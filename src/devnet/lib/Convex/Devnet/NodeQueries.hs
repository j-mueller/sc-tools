{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
module Convex.Devnet.NodeQueries(
  querySystemStart,
  queryEraHistory,
  queryTipBlock,
  queryTipSlotNo,
  queryUTxO,
  waitForTxn,
  waitForTxIn,
  localNodeConnectInfo,
  loadConnectInfo
) where

import           Cardano.Api                                        (Address,
                                                                     BabbageEra,
                                                                     BlockNo,
                                                                     CardanoMode,
                                                                     EraHistory,
                                                                     NetworkId,
                                                                     QueryInMode,
                                                                     ShelleyAddr,
                                                                     SlotNo, Tx,
                                                                     TxIn, UTxO)
import qualified Cardano.Api                                        as C
import           Cardano.Slotting.Slot                              (WithOrigin)
import           Cardano.Slotting.Time                              (SystemStart)
import           Control.Concurrent                                 (threadDelay)
import           Control.Exception                                  (Exception,
                                                                     throwIO)
import           Control.Monad                                      (when)
import           Control.Monad.Catch                                (MonadThrow)
import           Control.Monad.IO.Class                             (MonadIO (..))
import           Convex.Devnet.Utils                                (failure)
import           Convex.NodeQueries                                 (loadConnectInfo)
import           Convex.Utils                                       (txnUtxos)
import qualified Convex.Utxos                                       as Utxos
import qualified Data.Set                                           as Set
import           Data.Word                                          (Word64)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

import           Prelude

data QueryException
  = QueryAcquireException String
  | QueryEraMismatchException EraMismatch
  deriving (Eq, Show)

instance Exception QueryException

querySystemStart :: NetworkId -> FilePath -> IO SystemStart
querySystemStart = queryLocalState C.QuerySystemStart

queryEraHistory :: NetworkId -> FilePath -> IO (EraHistory CardanoMode)
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

queryTipSlotNo :: NetworkId -> FilePath -> IO SlotNo
queryTipSlotNo networkId socket = queryLocalState (C.QueryChainPoint C.CardanoMode) networkId socket >>= \case
  C.ChainPointAtGenesis -> failure "queryTipSlotNo: chain point at genesis"
  C.ChainPoint slot _   -> pure slot

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
waitForTxn network socket (head . txnUtxos -> txi) = waitForTxIn network socket txi
