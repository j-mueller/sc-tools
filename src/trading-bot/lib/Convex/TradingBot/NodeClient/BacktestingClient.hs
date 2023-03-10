{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
module Convex.TradingBot.NodeClient.BacktestingClient(
  backtestingClient,
  getOrderbookPrices
) where

import           Cardano.Api.Shelley           (AssetName, Block (..),
                                                BlockHeader (..),
                                                BlockInMode (..), BlockNo,
                                                CardanoMode, Env, Lovelace (..),
                                                NetworkId, PolicyId,
                                                Quantity (..), SlotNo, Value)
import qualified Cardano.Api.Shelley           as C
import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  (Lens', _2, anon, at, makeLenses,
                                                use, view, (%=), (&), (.=),
                                                (.~), (^.))
import           Control.Monad                 (guard, unless, void, when)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.State.Strict    (MonadState, execStateT)
import           Control.Monad.Trans.Maybe     (runMaybeT)
import qualified Convex.Constants              as Constants
import           Convex.Event                  (Event (..), NewOutputEvent (..),
                                                OutputSpentEvent (..),
                                                ResolvedInputs (..),
                                                TxWithEvents (..), extract)
import qualified Convex.Lenses                 as L
import           Convex.MonadLog               (MonadLog, MonadLogKatipT (..),
                                                logUnless)
import           Convex.Muesli.LP.BuildTx      (buyOrderFromScriptData)
import           Convex.Muesli.LP.Types        (BuyOrder (..), valueOf)
import           Convex.NodeClient.Fold        (CatchingUp (..), catchingUp,
                                                foldClient)
import           Convex.NodeClient.Resuming    (resumingClient)
import           Convex.NodeClient.Types       (PipelinedLedgerStateClient)
import           Convex.TradingBot.LPPoolEvent (LPPoolEvent (..),
                                                OrderbookEvent (..))
import qualified Convex.TradingBot.LPPoolEvent as LPPoolEvent
import           Convex.TradingBot.Portfolio   (Portfolio,
                                                defaultPortfolioConfig,
                                                emptyPortfolio)
import qualified Convex.TradingBot.Portfolio   as Portfolio
import           Convex.TradingBot.Prices      (LPPrices)
import qualified Convex.TradingBot.Prices      as Prices
import           Convex.TradingBot.Rules       (Rule, Signal (..))
import           Convex.TradingBot.Stats       (LPStats)
import qualified Convex.TradingBot.Stats       as Stats
import           Data.Either                   (partitionEithers)
import           Data.Foldable                 (toList, traverse_)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe, mapMaybe)
import qualified Katip                         as K
import           Prelude                       hiding (log)

data ClientState =
  ClientState
    { _resolvedInputs :: !(ResolvedInputs (Either LPPoolEvent OrderbookEvent))
    , _lpStats        :: !LPStats
    , _lastPrices     :: !(Map (PolicyId, AssetName) (Lovelace, Quantity))
    , _lpPrices       :: !(Map (PolicyId, AssetName) LPPrices)
    , _portfolio      :: !(Portfolio, Value)
    }

makeLenses ''ClientState

initialState :: ClientState
initialState = ClientState mempty mempty mempty mempty (emptyPortfolio, C.lovelaceToValue $ Lovelace 3_000_000_000)

backtestingClient :: Rule -> STM.TMVar (Portfolio, Value) -> K.LogEnv -> K.Namespace -> NetworkId -> Env -> PipelinedLedgerStateClient
backtestingClient rule resultVar logEnv ns networkId env =
  resumingClient [Constants.lessRecent] $ \_ ->
    foldClient
      initialState
      env
      (applyBlock rule resultVar logEnv ns networkId)

applyBlock :: Rule -> STM.TMVar (Portfolio, Value) -> K.LogEnv -> K.Namespace -> NetworkId -> CatchingUp -> ClientState -> BlockInMode CardanoMode -> IO (Maybe ClientState)
applyBlock rule resultVar le initialNamespace networkId c oldState block = K.runKatipContextT le () initialNamespace $ runMonadLogKatipT $ runMaybeT $ do
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
    updatePrices rule c currentBlockNo currentSlot
      (Map.unionsWith (<>) (getOrderbookPrices networkId <$> newEvents))
    unless (catchingUp c) $ do
      p <- use portfolio
      liftIO (STM.atomically $ STM.putTMVar resultVar p)
      guard False

pricesFor :: (PolicyId, AssetName) -> Lens' ClientState LPPrices
pricesFor k = lpPrices . at k . anon Prices.empty Prices.null

updatePrices :: (MonadLog m, MonadState ClientState m) => Rule -> CatchingUp -> BlockNo -> SlotNo -> Map (PolicyId, AssetName) (Lovelace, Quantity) -> m ()
updatePrices rule c blck slt newPrices = do
  oldPrices <- use lastPrices
  flip traverse_ (Map.toList newPrices) $ \(k, newPrice) -> do
    let oldPrice = Map.findWithDefault (0, 0) k oldPrices
        pAt = Prices.pricesAt blck slt oldPrice newPrice
        p = fromMaybe mempty $ Prices.price $ Prices._stats pAt
    when (Prices.pmVolume p > 0) $ do
      logUnless (catchingUp c) $ " " <> show (snd k) <> ": " <> Prices.showPriceMeasure p
      pricesFor k %= Prices.prepend pAt
      newPrices' <- use (pricesFor k)
      portf <- use portfolio
      portf' <- Portfolio.execSimulatedPortfolioT defaultPortfolioConfig portf $ do
        let pricePoint = (fst k, snd k, snd newPrice, fst newPrice)
        void (Portfolio.update pricePoint)
        case rule newPrices' of
          Buy  -> void (Portfolio.buy 1.0 pricePoint)
          Sell -> void (Portfolio.sell 1.0 pricePoint)
          _    -> pure ()
      portfolio .= portf'
  lastPrices .= oldPrices <> newPrices

getOrderbookPrices :: NetworkId -> TxWithEvents (Either LPPoolEvent OrderbookEvent) -> Map (PolicyId, AssetName) (Lovelace, Quantity)
getOrderbookPrices networkId es =
  case snd $ partitionEithers $ fmap sequence $ toList $ twEvents es of
    [AnOutputSpentEvent a, AnOutputSpentEvent b] ->
      let f OutputSpentEvent{oseDatum, oseTxOutput=NewOutputEvent{neOutput}} =
            let vl = C.selectLovelace $ view (L._TxOut . _2 . L._TxOutValue) $ C.fromShelleyTxOut C.ShelleyBasedEraBabbage neOutput
                dt = C.fromAlonzoData oseDatum
            in case buyOrderFromScriptData networkId vl dt of
                Just BuyOrder{buyCurrency, buyQuantity, buyPrice} ->
                  Just (buyCurrency, (valueOf buyQuantity buyPrice, buyQuantity))
                _ -> Nothing -- FIXME: sellOrderFromScriptData
      in Map.fromListWith (<>) $ mapMaybe f [a, b]
    _ -> mempty

-- TODO:
-- backtesting for multiple sets of rules
-- execution
-- Add other LP dexes
