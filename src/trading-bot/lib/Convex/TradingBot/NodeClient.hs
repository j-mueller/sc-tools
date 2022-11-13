{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}
module Convex.TradingBot.NodeClient(
  muesliClient
) where

import           Cardano.Api.Shelley        (AssetName, Block (..),
                                             BlockHeader (..), BlockInMode (..),
                                             BlockNo, CardanoMode, Env,
                                             Lovelace (..), NetworkId, PolicyId,
                                             Quantity (..), SlotNo)
import           Control.Lens               (anon, at, makeLenses, (%=), (&),
                                             (.~), (^.))
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (MonadState, execStateT)
import           Control.Monad.Trans.Maybe  (runMaybeT)
import qualified Convex.Constants           as Constants
import           Convex.Event               (NewOutputEvent (..),
                                             ResolvedInputs (..),
                                             TxWithEvents (..), extract)
import           Convex.Muesli.LP.Constants (ScriptType (..), scriptType)
import           Convex.Muesli.LP.Types     (adaPair)
import           Convex.NodeClient.Fold     (CatchingUp (..), catchingUp,
                                             foldClient)
import           Convex.NodeClient.Resuming (resumingClient)
import           Convex.NodeClient.Types    (PipelinedLedgerStateClient)
import           Convex.TradingBot.Prices   (AssetPrices (..), LPPrices)
import qualified Convex.TradingBot.Prices   as Prices
import           Convex.TradingBot.Stats    (LPStats)
import qualified Convex.TradingBot.Stats    as Stats
import           Data.Foldable              (toList, traverse_)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (mapMaybe)
import           Prelude                    hiding (log)

data ClientState =
  ClientState
    { _resolvedInputs :: !(ResolvedInputs ScriptType)
    , _lpStats        :: !LPStats
    , _lpPrices       :: !(Map (PolicyId, AssetName) LPPrices)
    }

makeLenses ''ClientState

initialState :: ClientState
initialState = ClientState mempty mempty mempty

muesliClient :: NetworkId -> Env -> PipelinedLedgerStateClient
muesliClient networkId env =
  resumingClient [Constants.recent] $ \_ ->
    foldClient
      initialState
      env
      (applyBlock networkId)

applyBlock :: NetworkId -> CatchingUp -> ClientState -> BlockInMode CardanoMode -> IO (Maybe ClientState)
applyBlock _networkId c oldState block = runMaybeT $ do
  let (newEvents, newResolvedInputs) = extract scriptType (oldState ^. resolvedInputs) block
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

logUnless :: MonadIO m => Bool -> String -> m ()
logUnless w m = unless w (log m)

log :: MonadIO m => String -> m ()
log = liftIO . putStrLn

updatePrices :: (MonadIO m, MonadState ClientState m) => CatchingUp -> BlockNo -> SlotNo -> Map (PolicyId, AssetName) (Lovelace, Quantity) -> Map (PolicyId, AssetName) (Lovelace, Quantity) -> m ()
updatePrices c blck slt oldPrices newPrices = flip traverse_ (Map.toList newPrices) $ \(k, newPrice) -> do
  let oldPrice = Map.findWithDefault (0, 0) k oldPrices
      pAt = Prices.pricesAt blck slt oldPrice newPrice
      AssetPrices{apPrice} = Prices._stats pAt
  when (Prices.pmVolume apPrice > 0) $ do
    logUnless (catchingUp c) $ " " <> show (snd k) <> ": " <> Prices.showPriceMeasure apPrice
    lpPrices . at k . anon Prices.empty Prices.null %= Prices.prepend pAt

getPrices :: ResolvedInputs ScriptType -> Map (PolicyId, AssetName) (Lovelace, Quantity)
getPrices (ResolvedInputs inputs) =
  let f (_, neEvent -> PoolScript (Right (adaPair -> Just k, Just v))) = Just (k, v)
      f _                                                = Nothing
  in Map.fromList $ mapMaybe f $ Map.toList inputs

-- ScriptType -> LPPoolEvent (PolicyId, AssetName, Lovelace, Quantity)
-- change type of 'extract'
-- Add other LP dexes
-- rules
-- position mgmt
-- execution
