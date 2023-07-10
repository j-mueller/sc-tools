{-# LANGUAGE GADTs        #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
{-| Helper functions for querying a local @cardano-node@ using the socket interface
-}
module Convex.Devnet.NodeQueries(
  querySystemStart,
  queryEraHistory,
  queryTip,
  queryTipBlock,
  queryTipSlotNo,
  queryUTxO,
  waitForTxn,
  waitForTxIn,
  waitForTxInSpend,
  localNodeConnectInfo,
  loadConnectInfo
) where

import           Cardano.Api                                        (Address,
                                                                     BabbageEra,
                                                                     BlockNo,
                                                                     CardanoMode,
                                                                     EraHistory (..),
                                                                     NetworkId,
                                                                     QueryInMode,
                                                                     ShelleyAddr,
                                                                     SlotNo, Tx,
                                                                     TxIn, UTxO)
import qualified Cardano.Api                                        as C
import           Cardano.Slotting.Slot                              (WithOrigin)
import           Cardano.Slotting.Time                              (SlotLength,
                                                                     SystemStart)
import           Control.Concurrent                                 (threadDelay)
import           Control.Exception                                  (Exception,
                                                                     throwIO)
import           Control.Monad                                      (unless,
                                                                     when)
import           Control.Monad.Catch                                (MonadThrow)
import           Control.Monad.IO.Class                             (MonadIO (..))
import           Convex.Devnet.Utils                                (failure)
import           Convex.NodeQueries                                 (loadConnectInfo)
import           Convex.Utils                                       (txnUtxos)
import qualified Convex.Utxos                                       as Utxos
import qualified Data.Set                                           as Set
import           Data.Word                                          (Word64)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import           Ouroboros.Consensus.HardFork.History               (interpretQuery,
                                                                     slotToSlotLength)
import           Prelude

data QueryException
  = QueryAcquireException String
  | QueryEraMismatchException EraMismatch
  deriving (Eq, Show)

instance Exception QueryException

-- | Get the 'SystemStart' from the node
querySystemStart ::
  NetworkId ->
    -- ^ network Id to use for node query
  FilePath ->
    -- ^ Node socket
  IO SystemStart
querySystemStart = queryLocalState C.QuerySystemStart

-- | Get the 'EraHistory' from the node
queryEraHistory ::
  NetworkId ->
    -- ^ network Id to use for node query
  FilePath ->
    -- ^ Node socket
  IO (EraHistory CardanoMode)
queryEraHistory = queryLocalState (C.QueryEraHistory C.CardanoModeIsMultiEra)

queryLocalState :: QueryInMode CardanoMode b -> NetworkId -> FilePath -> IO b
queryLocalState query networkId socket = do
  C.queryNodeLocalState (localNodeConnectInfo networkId socket) Nothing query >>= \case
    Left err -> do
      failure $ "querySystemStart: Failed with " <> show err
    Right result -> pure result


localNodeConnectInfo :: NetworkId -> FilePath -> C.LocalNodeConnectInfo C.CardanoMode
localNodeConnectInfo = C.LocalNodeConnectInfo cardanoModeParams

cardanoModeParams :: C.ConsensusModeParams C.CardanoMode
cardanoModeParams = C.CardanoModeParams $ C.EpochSlots defaultByronEpochSlots
 where
  -- NOTE(AB): extracted from Parsers in cardano-cli, this is needed to run in 'cardanoMode' which
  -- is the default for cardano-cli
  defaultByronEpochSlots = 21600 :: Word64

queryTipBlock :: NetworkId -> FilePath -> IO (WithOrigin BlockNo)
queryTipBlock = queryLocalState C.QueryChainBlockNo

-- | Get the tip (slot no. and block hash) from the node
queryTip ::
  NetworkId ->
    -- ^ network Id to use for node query
  FilePath ->
    -- ^ Node socket
  IO (SlotNo, SlotLength, C.Hash C.BlockHeader)
queryTip networkId socket = queryLocalState (C.QueryChainPoint C.CardanoMode) networkId socket >>= \case
  C.ChainPointAtGenesis -> failure "queryTip: chain point at genesis"
  C.ChainPoint slot hsh -> getSlotLength slot >>= (\i -> pure (slot, i, hsh))

  where
    getSlotLength :: SlotNo -> IO SlotLength
    getSlotLength slotNo = do
      (EraHistory _ interpreter) <- queryEraHistory networkId socket
      case interpretQuery interpreter (slotToSlotLength slotNo) of
        Left err      -> failure $ "queryTip: Failed with " <> show err
        Right slength -> pure $ slength

-- | Get the slot no of the current tip from the node
queryTipSlotNo ::
  NetworkId ->
    -- ^ network Id to use for node query
  FilePath ->
    -- ^ Node socket
  IO (SlotNo, SlotLength)
queryTipSlotNo networkId socket = queryTip networkId socket >>= (\(s, l, _) -> pure (s, l))

-- | Query UTxO for all given addresses at given point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxO :: NetworkId -> FilePath -> [Address ShelleyAddr] -> IO (UTxO C.BabbageEra)
queryUTxO networkId socket addresses =
  let query =
        C.QueryInEra
          C.BabbageEraInCardanoMode
          ( C.QueryInShelleyBasedEra
              C.ShelleyBasedEraBabbage
              ( C.QueryUTxO
                  (C.QueryUTxOByAddress (Set.fromList $ map C.AddressShelley addresses))
              )
          )
   in queryLocalState query networkId socket >>= throwOnEraMismatch

throwOnEraMismatch :: (MonadThrow m, MonadIO m) => Either EraMismatch a -> m a
throwOnEraMismatch res =
  case res of
    Left eraMismatch -> liftIO $ throwIO $ QueryEraMismatchException eraMismatch
    Right result     -> pure result

{-| Wait until the output appears on the chain
-}
waitForTxIn :: NetworkId -> FilePath -> TxIn -> IO ()
waitForTxIn networkId socket txIn = do
  let query =
        C.QueryInEra
          C.BabbageEraInCardanoMode
          ( C.QueryInShelleyBasedEra
              C.ShelleyBasedEraBabbage
              ( C.QueryUTxO
                  (C.QueryUTxOByTxIn (Set.singleton txIn))
              )
          )
      go = do
        utxo <- Utxos.fromApiUtxo <$> (queryLocalState query networkId socket >>= throwOnEraMismatch)
        when (utxo == mempty) $ do
          threadDelay 2_000_000
          go
  go

waitForTxn :: NetworkId -> FilePath -> Tx BabbageEra -> IO ()
waitForTxn network socket (head . fmap fst . txnUtxos -> txi) = waitForTxIn network socket txi

{-| Wait until the 'TxIn' is not part of the utxo set anymore
-}
waitForTxInSpend :: NetworkId -> FilePath -> TxIn -> IO ()
waitForTxInSpend networkId socket txIn = do
  let query =
        C.QueryInEra
          C.BabbageEraInCardanoMode
          ( C.QueryInShelleyBasedEra
              C.ShelleyBasedEraBabbage
              ( C.QueryUTxO
                  (C.QueryUTxOByTxIn (Set.singleton txIn))
              )
          )
      go = do
        utxo <- Utxos.fromApiUtxo <$> (queryLocalState query networkId socket >>= throwOnEraMismatch)
        unless (utxo == mempty) $ do
          threadDelay 2_000_000
          go
  go
