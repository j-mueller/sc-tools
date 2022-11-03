{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module Convex.Muesli.NodeClient(
  muesliClient
) where

import           Cardano.Api.Shelley        (AssetId, BlockInMode, CardanoMode,
                                             ChainPoint, Env, NetworkId,
                                             Quantity (..), ScriptHash, TxIn)
import qualified Cardano.Api.Shelley        as C
import           Control.Lens               (anon, at, makeLenses, use, (&),
                                             (.=), (.~), (?=), (^.))
import           Control.Monad              (guard, join, unless)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (MonadState, execStateT)
import           Control.Monad.Trans.Maybe  (runMaybeT)
import qualified Convex.Constants           as Constants
import           Convex.Event               (NewOutputEvent (..),
                                             OutputSpentEvent (..),
                                             ResolvedInputs, TxWithEvents (..),
                                             extract, splitEvent, txIn)
import           Convex.Muesli.Constants    (MuesliVersion, muesliHash,
                                             muesliVersion)
import           Convex.Muesli.KnownOrder   (KnownOrder (..))
import qualified Convex.Muesli.KnownOrder   as KnownOrder
import           Convex.Muesli.Match        (Match (..), Valid)
import qualified Convex.Muesli.Match        as Match
import           Convex.NodeClient.Fold     (CatchingUp (..), catchingUp,
                                             foldClient)
import           Convex.NodeClient.Resuming (resumingClient)
import           Convex.NodeClient.Types    (PipelinedLedgerStateClient)
import           Data.Either                (partitionEithers)
import           Data.Foldable              (fold, toList, traverse_)
import qualified Data.IntMap                as IntMap
import           Data.IntMap.Strict         (IntMap)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, mapMaybe)
import           Data.Validation            (Validation (..))
import           Prelude                    hiding (log)

data MuesliState =
  MuesliState
    { _resolvedInputs  :: !(ResolvedInputs MuesliVersion)
    , _potentialOrders :: !(Map AssetId (IntMap (Map AssetId (IntMap (Map TxIn (KnownOrder, NewOutputEvent MuesliVersion)))))) -- orders indexed by bid and ask
    , _txInLocations   :: !(Map TxIn ((AssetId, Quantity), (AssetId, Quantity)))
    }

makeLenses ''MuesliState

initialState :: MuesliState
initialState =
  MuesliState
    { _resolvedInputs = mempty
    , _txInLocations = mempty
    , _potentialOrders = mempty
    }

muesliClient :: NetworkId -> Env -> PipelinedLedgerStateClient
muesliClient _networkId env =
  resumingClient [Constants.lessRecent] $ \_ ->
    foldClient
      initialState
      env
      applyBlock

applyBlock :: CatchingUp -> MuesliState -> BlockInMode CardanoMode -> IO (Maybe MuesliState)
applyBlock (catchingUp -> isCatchingUp) oldState block = do
  let (newEvents, newResolvedInputs) = extract muesliVersion (oldState ^. resolvedInputs) block
      newState = oldState & resolvedInputs .~ newResolvedInputs

  runMaybeT $ flip execStateT newState $ do
    logUnless (null newEvents) ("Found " <> show (length newEvents) <> " events.")

    potentialMatches <- join <$> traverse applyTx newEvents
    flip traverse_ potentialMatches $ \(order, event, matches) -> do
      insertMatch order event
      unless isCatchingUp $ do
        validMatches <- catMaybes <$> traverse validate matches
        traverse_ addMatch validMatches

    logUnless isCatchingUp "Caught up"
    pure ()

insertMatch :: (MonadState MuesliState m) => KnownOrder -> NewOutputEvent MuesliVersion -> m ()
insertMatch orderOne@KnownOrder{orderAsk=(askC, askQ)} orderOneEvent = do
  let offerValue = Match.getOfferValue orderOneEvent
      txi = txIn orderOneEvent
  flip traverse_ (C.valueToList offerValue) $ \(bidC, bidQ) -> do
    potentialOrders
      . at bidC . anon IntMap.empty IntMap.null
      . at (quantityInt bidQ) . anon Map.empty Map.null
      . at askC . anon IntMap.empty IntMap.null
      . at (quantityInt askQ) . anon Map.empty Map.null
      . at txi ?= (orderOne, orderOneEvent)
    txInLocations . at txi .= Just ((bidC, bidQ), (askC, askQ))


applyTx :: (MonadState MuesliState m) => TxWithEvents MuesliVersion -> m [(KnownOrder, NewOutputEvent MuesliVersion, [Match])]
applyTx TxWithEvents{twEvents, twSlot, twTx} = do
  let (spent, produced) = partitionEithers $ fmap splitEvent $ toList twEvents
  case spent of
    [outputSpent1, outputSpent2] -> do
      traverse_ (deleteTxIn . oseTxIn) spent
      -- let created = neSlot . oseTxOutput
          -- fulfilmentTime = twSlot - max (created outputSpent1) (created outputSpent2)
      -- tell (Stats.countOrderFulfilledTotal fulfilmentTime)
      -- tell (Stats.countOrderFulfilledTotal fulfilmentTime)
      -- handleTx fulfilmentTime twTx
    -- [_] -> tell Stats.countOrderCancelled
    _ -> pure ()
  catMaybes <$> traverse applyNewOutputEvent produced

deleteTxIn :: MonadState MuesliState m => TxIn -> m ()
deleteTxIn txi = do
  loc <- use (txInLocations . at txi)
  flip traverse_ loc $ \((bidC, bidQ), (askC, askQ)) -> do
    potentialOrders
      . at bidC . anon IntMap.empty IntMap.null
      . at (quantityInt bidQ) . anon Map.empty Map.null
      . at askC . anon IntMap.empty IntMap.null
      . at (quantityInt askQ) . anon Map.empty Map.null
      . at txi
      .= Nothing
    txInLocations . at txi .= Nothing

applyNewOutputEvent :: (MonadState MuesliState m) => NewOutputEvent MuesliVersion -> m (Maybe (KnownOrder, NewOutputEvent MuesliVersion, [Match]))
applyNewOutputEvent orderOneEvent@NewOutputEvent{neTxMetadata} = do
  let offerValue = Match.getOfferValue orderOneEvent
  case KnownOrder.knownOrderFromMetadata neTxMetadata of
    Left _ -> do
      -- tell Stats.countMetadataFailed
      return Nothing
    Right orderOne@KnownOrder{orderAsk=(askC, askQ)} -> do
      potentialMatches <- fmap join <$> flip traverse (C.valueToList offerValue) $ \(bidC, bidQ) -> do
        offersForCurrency <- use (potentialOrders . at askC . anon IntMap.empty IntMap.null)
        let (_, potentialOffers) = IntMap.split (pred $ quantityInt askQ) offersForCurrency
            k = fold (mapMaybe (\(_, otherCurrencies) -> Map.lookup bidC otherCurrencies) (IntMap.toList potentialOffers))
            (potentialOffers', _) = IntMap.split (succ $ quantityInt bidQ) k
            mx = foldMap (Map.toList . snd) (IntMap.toAscList potentialOffers')
        flip traverse mx $ \(_, (orderTwo, orderTwoEvent)) -> do
          return Match{orderOne, orderOneEvent, orderTwo, orderTwoEvent}
      let numMatches = length potentialMatches
      -- tell (Stats.countTotalMatches numMatches)
      return (Just (orderOne, orderOneEvent, potentialMatches))

log :: MonadIO m => String -> m ()
log = liftIO . putStrLn

logUnless :: MonadIO m => Bool -> String -> m ()
logUnless w m = unless w (log m)

quantityInt :: Quantity -> Int
quantityInt (Quantity q) = fromIntegral q

validate :: (MonadIO m, Monad m) => Match -> m (Maybe (Valid Match))
validate match = case Match.validateMatch match of
  Failure{} -> do
    liftIO (putStrLn "Invalid match")
    pure Nothing
  Success validMatch -> do
    -- tell Stats.countValidMatch
    pure (Just validMatch)

addMatch :: MonadIO m => Valid Match -> m ()
addMatch _ = liftIO (putStrLn "Found a match!!")
