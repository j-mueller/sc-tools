{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}
module Convex.TradingBot.NodeClient(
  backtestingClient
) where

import           Cardano.Api.Shelley           (AssetName, Block (..),
                                                BlockHeader (..),
                                                BlockInMode (..), BlockNo,
                                                CardanoMode, Env, Lovelace (..),
                                                NetworkId, PolicyId,
                                                Quantity (..), SlotNo)
import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  (Lens', anon, at, makeLenses,
                                                use, (%=), (&), (.=), (.~),
                                                (^.))
import           Control.Monad                 (guard, unless, when)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.State.Strict    (MonadState, execStateT)
import           Control.Monad.Trans.Maybe     (runMaybeT)
import qualified Convex.Constants              as Constants
import           Convex.Event                  (NewOutputEvent (..),
                                                ResolvedInputs (..),
                                                TxWithEvents (..), extract)
import           Convex.MonadLog               (MonadLog, MonadLogKatipT (..),
                                                logInfoS, logWarnS)
import           Convex.NodeClient.Fold        (CatchingUp (..), catchingUp,
                                                foldClient)
import           Convex.NodeClient.Resuming    (resumingClient)
import           Convex.NodeClient.Types       (PipelinedLedgerStateClient)
import           Convex.TradingBot.LPPoolEvent (LPPoolEvent (..))
import qualified Convex.TradingBot.LPPoolEvent as LPPoolEvent
import           Convex.TradingBot.Portfolio   (Portfolio,
                                                defaultPortfolioConfig,
                                                emptyPortfolio,
                                                execSimulatedPortfolioT)
import qualified Convex.TradingBot.Portfolio   as Portfolio
import           Convex.TradingBot.Prices      (LPPrices)
import qualified Convex.TradingBot.Prices      as Prices
import           Convex.TradingBot.Rules       (Signal (..))
import qualified Convex.TradingBot.Rules       as Rules
import           Convex.TradingBot.Stats       (LPStats)
import qualified Convex.TradingBot.Stats       as Stats
import           Data.Foldable                 (toList, traverse_)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe, mapMaybe)
import qualified Katip                         as K
import           Prelude                       hiding (log)

data ClientState =
  ClientState
    { _resolvedInputs :: !(ResolvedInputs LPPoolEvent)
    , _lpStats        :: !LPStats
    , _lpPrices       :: !(Map (PolicyId, AssetName) LPPrices)
    , _portfolio      :: !Portfolio
    }

makeLenses ''ClientState

initialState :: ClientState
initialState = ClientState mempty mempty mempty (emptyPortfolio $ Lovelace 3_000_000_000)

backtestingClient :: STM.TMVar Portfolio -> K.LogEnv -> K.Namespace -> NetworkId -> Env -> PipelinedLedgerStateClient
backtestingClient resultVar logEnv ns networkId env =
  resumingClient [Constants.lessRecent] $ \_ ->
    foldClient
      initialState
      env
      (applyBlock resultVar logEnv ns networkId)

applyBlock :: STM.TMVar Portfolio -> K.LogEnv -> K.Namespace -> NetworkId -> CatchingUp -> ClientState -> BlockInMode CardanoMode -> IO (Maybe ClientState)
applyBlock resultVar le initialNamespace _networkId c oldState block = K.runKatipContextT le () initialNamespace $ runMonadLogKatipT $ runMaybeT $ do
  let (newEvents, newResolvedInputs) = extract LPPoolEvent.extract (oldState ^. resolvedInputs) block
      BlockInMode (Block blockHeader _) _ = block
      BlockHeader currentSlot _ currentBlockNo = blockHeader
      newStats =
        foldMap (foldMap Stats.fromEvent . toList . twEvents) newEvents
        <> Stats.fromResolvedInputs newResolvedInputs
      totalStats = (oldState ^. lpStats) <> newStats
      newState = oldState
                  & resolvedInputs .~ newResolvedInputs
                  & lpStats        .~ totalStats
  flip execStateT newState $ do
    updatePrices c currentBlockNo currentSlot (getPrices $ oldState ^. resolvedInputs) (getPrices newResolvedInputs)
    unless (catchingUp c) $ do
      p <- use portfolio
      liftIO (STM.atomically $ STM.putTMVar resultVar p)
      guard False

logUnless :: MonadLog m => Bool -> String -> m ()
logUnless w m = unless w (logInfoS m)

pricesFor :: (PolicyId, AssetName) -> Lens' ClientState LPPrices
pricesFor k = lpPrices . at k . anon Prices.empty Prices.null

updatePrices :: (MonadLog m, MonadState ClientState m) => CatchingUp -> BlockNo -> SlotNo -> Map (PolicyId, AssetName) (Lovelace, Quantity) -> Map (PolicyId, AssetName) (Lovelace, Quantity) -> m ()
updatePrices c blck slt oldPrices newPrices = flip traverse_ (Map.toList newPrices) $ \(k, newPrice) -> do
  let oldPrice = Map.findWithDefault (0, 0) k oldPrices
      pAt = Prices.pricesAt blck slt oldPrice newPrice
      p = fromMaybe mempty $ Prices.price $ Prices._stats pAt
  when (Prices.pmVolume p > 0) $ do
    logUnless (catchingUp c) $ " " <> show (snd k) <> ": " <> Prices.showPriceMeasure p
    pricesFor k %= Prices.prepend pAt
    newPrices' <- use (pricesFor k)
    portf <- use portfolio
    portf' <- execSimulatedPortfolioT defaultPortfolioConfig portf $ do
      let pricePoint = (fst k, snd k, snd newPrice, fst newPrice)
      Portfolio.update pricePoint
      case Rules.movingAverage newPrices' of
        Buy  -> Portfolio.buy 1.0 pricePoint
        Sell -> Portfolio.sell 1.0 pricePoint
        _    -> pure ()
    portfolio .= portf'
    when (Portfolio._tradeCount portf == 0 && Portfolio._tradeCount portf' > 0) $ do
      logWarnS $ "First trade: " <> show blck <> " in slot " <> show slt

getPrices :: ResolvedInputs LPPoolEvent -> Map (PolicyId, AssetName) (Lovelace, Quantity)
getPrices (ResolvedInputs inputs) =
  let f (_, neEvent -> LPPoolEvent{lpePolicyId, lpeAssetName, lpeLovelace, lpeNativeTokenAmount}) = Just ((lpePolicyId, lpeAssetName), (lpeLovelace, lpeNativeTokenAmount))
  in Map.fromList $ mapMaybe f $ Map.toList inputs

-- TODO:
-- backtesting for multiple sets of rules
-- annealing lib.
-- execution
-- Add other LP dexes
