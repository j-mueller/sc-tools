{-# LANGUAGE GADTs          #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE ViewPatterns   #-}
{-| Helper functions for querying a local @cardano-node@ using the socket interface
-}
module Convex.Devnet.NodeQueries(
  querySystemStart,
  queryEpoch,
  queryEraHistory,
  queryTip,
  queryTipBlock,
  queryTipSlotNo,
  queryUTxO,
  queryUTxOWhole,
  queryUTxOByTxIn,
  waitForTxn,
  waitForTxIn,
  waitForTxInSpend,
  localNodeConnectInfo,
  loadConnectInfo,
  queryEra
) where

import           Cardano.Api                                        (Address,
                                                                     BabbageEra,
                                                                     BlockNo,
                                                                     EraHistory (..),
                                                                     LocalNodeConnectInfo (..),
                                                                     NetworkId,
                                                                     QueryInMode,
                                                                     QueryUTxOFilter,
                                                                     ShelleyAddr,
                                                                     SlotNo, Tx,
                                                                     TxIn, UTxO)
import qualified Cardano.Api                                        as C
import           Cardano.Slotting.Slot                              (WithOrigin)
import           Cardano.Slotting.Time                              (SlotLength,
                                                                     SystemStart)
import           Control.Concurrent                                 (threadDelay)
import           Control.Exception                                  (Exception,
                                                                     throw,
                                                                     throwIO)
import           Control.Monad                                      (unless,
                                                                     when)
import           Control.Monad.Catch                                (MonadThrow)
import           Control.Monad.Except                               (runExceptT)
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
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type    as T
import           Prelude

data QueryException
  = QueryAcquireException String
  | QueryEraMismatchException EraMismatch
  | QueryUnsupportedEra C.AnyCardanoEra
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
  IO EraHistory
queryEraHistory = queryLocalState C.QueryEraHistory

queryLocalState :: QueryInMode b -> NetworkId -> FilePath -> IO b
queryLocalState query networkId socket = do
  runExceptT (C.queryNodeLocalState (localNodeConnectInfo networkId socket) T.VolatileTip query) >>= \case
    Left err -> do
      failure $ "queryLocalState: Failed with " <> show err
    Right result -> pure result


localNodeConnectInfo :: NetworkId -> FilePath -> C.LocalNodeConnectInfo
localNodeConnectInfo localNodeNetworkId (C.File -> localNodeSocketPath) =
  C.LocalNodeConnectInfo
    { localConsensusModeParams = cardanoModeParams
    , localNodeNetworkId
    , localNodeSocketPath
    }

cardanoModeParams :: C.ConsensusModeParams
cardanoModeParams = C.CardanoModeParams $ C.EpochSlots defaultByronEpochSlots
 where
  -- NOTE(AB): extracted from Parsers in cardano-cli, this is needed to run in 'cardanoMode' which
  -- is the default for cardano-cli
  defaultByronEpochSlots = 21600 :: Word64

queryTipBlock :: NetworkId -> FilePath -> IO (WithOrigin BlockNo)
queryTipBlock = queryLocalState C.QueryChainBlockNo

queryEra :: NetworkId -> FilePath -> IO C.AnyCardanoEra
queryEra = queryLocalState C.QueryCurrentEra

queryEpoch :: NetworkId -> FilePath -> IO C.EpochNo
queryEpoch nid path = do
  result <- queryLocalState
             (C.QueryInEra
                (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryEpoch)
             )
             nid path
  case result of
    Left err -> do
      fail ("queryEpoch: failed with: " <> show err)
    Right k -> pure k

-- | Get the tip (slot no. and block hash) from the node
queryTip ::
  NetworkId ->
    -- ^ network Id to use for node query
  FilePath ->
    -- ^ Node socket
  IO (SlotNo, SlotLength, C.Hash C.BlockHeader)
queryTip networkId socket = queryLocalState C.QueryChainPoint networkId socket >>= \case
  C.ChainPointAtGenesis -> failure "queryTip: chain point at genesis"
  C.ChainPoint slot hsh -> getSlotLength slot >>= (\i -> pure (slot, i, hsh))

  where
    getSlotLength :: SlotNo -> IO SlotLength
    getSlotLength slotNo = do
      (EraHistory interpreter) <- queryEraHistory networkId socket
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
queryUTxOFilterBabbage :: NetworkId -> FilePath -> QueryUTxOFilter -> IO (UTxO C.BabbageEra)
queryUTxOFilterBabbage networkId socket flt =
  let query =
        C.QueryInEra
          ( C.QueryInShelleyBasedEra
              C.ShelleyBasedEraBabbage
              ( C.QueryUTxO flt)
          )
   in queryLocalState query networkId socket >>= throwOnEraMismatch

-- | Query UTxO for all given addresses at given point.
--
-- Throws at least 'QueryException' if query fails.
-- queryUTxOByTxInBabbage :: NetworkId -> FilePath -> Set TxIn -> IO (UTxO C.BabbageEra)
-- queryUTxOByTxInBabbage networkId socket flt =
--   let query =
--         C.QueryInEra
--           ( C.QueryInShelleyBasedEra
--               C.ShelleyBasedEraBabbage
--               ( C.QueryUTxO flt)
--           )
--    in queryLocalState query networkId socket >>= throwOnEraMismatch

-- | Query UTxO for all given addresses at given point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOFilterConway :: NetworkId -> FilePath -> QueryUTxOFilter -> IO (UTxO C.ConwayEra)
queryUTxOFilterConway networkId socket flt =
  let query =
        C.QueryInEra
          ( C.QueryInShelleyBasedEra
              C.ShelleyBasedEraConway
              ( C.QueryUTxO flt)
          )
   in queryLocalState query networkId socket >>= throwOnEraMismatch

{-| Query the node for the current era and perform an action based on the result
-- Throws 'QueryException' if the current era is not one of (Babbage, Conway)
-}
withSupportedEra ::
  NetworkId ->
  FilePath ->
  (forall era. C.BabbageEraOnwards era -> IO a) -> -- ^
  IO a
withSupportedEra networkId socket handler = do
  queryEra networkId socket >>= \case
    C.AnyCardanoEra C.BabbageEra -> handler C.BabbageEraOnwardsBabbage
    C.AnyCardanoEra C.ConwayEra  -> handler C.BabbageEraOnwardsConway
    otherEra -> throw (QueryUnsupportedEra otherEra)

-- | Query UTxO for the outputs in the set of 'TxIn's
--
-- Throws at least 'QueryException' if query fails.
queryUTxOByTxIn :: NetworkId -> FilePath -> Set.Set TxIn -> IO (C.InAnyCardanoEra UTxO)
queryUTxOByTxIn networkId socket txIns =
  withSupportedEra networkId socket $ \case
    C.BabbageEraOnwardsBabbage -> C.inAnyCardanoEra C.BabbageEra <$> queryUTxOFilterBabbage networkId socket (C.QueryUTxOByTxIn txIns)
    C.BabbageEraOnwardsConway -> C.inAnyCardanoEra C.ConwayEra <$> queryUTxOFilterConway networkId socket (C.QueryUTxOByTxIn txIns)

-- | Query UTxO for all given addresses at given point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxO :: NetworkId -> FilePath -> [Address ShelleyAddr] -> IO (C.InAnyCardanoEra UTxO)
queryUTxO networkId socket addresses =
  withSupportedEra networkId socket $ \case
    C.BabbageEraOnwardsBabbage -> C.inAnyCardanoEra C.BabbageEra <$> queryUTxOFilterBabbage networkId socket (C.QueryUTxOByAddress (Set.fromList $ map C.AddressShelley addresses))
    C.BabbageEraOnwardsConway -> C.inAnyCardanoEra C.ConwayEra <$> queryUTxOFilterConway networkId socket (C.QueryUTxOByAddress (Set.fromList $ map C.AddressShelley addresses))

-- | Query the entire UTxO set
--
-- Throws at least 'QueryException' if query fails.
queryUTxOWhole :: NetworkId -> FilePath -> IO (C.InAnyCardanoEra UTxO)
queryUTxOWhole networkId socket =
  queryEra networkId socket >>= \case
    C.AnyCardanoEra C.BabbageEra ->
        C.inAnyCardanoEra C.BabbageEra <$>
          queryUTxOFilterBabbage networkId socket C.QueryUTxOWhole
    C.AnyCardanoEra C.ConwayEra ->
      C.inAnyCardanoEra C.ConwayEra <$>
          queryUTxOFilterConway networkId socket C.QueryUTxOWhole
    otherEra -> throw (QueryUnsupportedEra otherEra)

throwOnEraMismatch :: (MonadThrow m, MonadIO m) => Either EraMismatch a -> m a
throwOnEraMismatch res =
  case res of
    Left eraMismatch -> liftIO $ throwIO $ QueryEraMismatchException eraMismatch
    Right result     -> pure result

{-| Wait until the output appears on the chain
-}
waitForTxIn :: NetworkId -> FilePath -> TxIn -> IO ()
waitForTxIn networkId socket txIn = do
  let go :: forall era. C.BabbageEraOnwards era -> IO ()
      go era = do
        let query :: QueryInMode (Either EraMismatch (UTxO era))
            query =
              C.QueryInEra
                ( C.QueryInShelleyBasedEra
                    (C.babbageEraOnwardsToShelleyBasedEra era)
                    ( C.QueryUTxO
                        (C.QueryUTxOByTxIn (Set.singleton txIn))
                    )
                )
            loop = do
              utxo <- Utxos.fromApiUtxo () . C.inAnyCardanoEra (C.toCardanoEra $ C.babbageEraOnwardsToShelleyBasedEra era) <$> (queryLocalState query networkId socket >>= throwOnEraMismatch)
              when (utxo == mempty) $ do
                threadDelay 2_000_000
                loop
        loop
  withSupportedEra networkId socket go

waitForTxn :: NetworkId -> FilePath -> Tx BabbageEra -> IO ()
waitForTxn network socket (head . fmap fst . txnUtxos -> txi) = waitForTxIn network socket txi

{-| Wait until the 'TxIn' is not part of the utxo set anymore
-}
waitForTxInSpend :: NetworkId -> FilePath -> TxIn -> IO ()
waitForTxInSpend networkId socket txIn = do
  let go :: forall era. C.BabbageEraOnwards era -> IO ()
      go era = do
        let query =
              C.QueryInEra
                ( C.QueryInShelleyBasedEra
                    (C.babbageEraOnwardsToShelleyBasedEra era)
                    ( C.QueryUTxO
                        (C.QueryUTxOByTxIn (Set.singleton txIn))
                    )
                )
            loop = do
              utxo <- Utxos.fromApiUtxo () . C.inAnyCardanoEra (C.toCardanoEra $ C.babbageEraOnwardsToShelleyBasedEra era) <$> (queryLocalState query networkId socket >>= throwOnEraMismatch)
              unless (utxo == mempty) $ do
                threadDelay 2_000_000
                loop
        loop
  withSupportedEra networkId socket go
