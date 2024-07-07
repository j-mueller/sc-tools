{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-| Typeclasses for blockchain operations
-}
module Convex.Class(

  -- * Monad blockchain
  MonadBlockchain(..),
  trySendTx,
  SendTxFailed(..),
  singleUTxO,

  -- * Monad mockchain
  MonadMockchain(..),
  MonadBlockchainError(..),
  getSlot,
  setSlot,
  setPOSIXTime,
  nextSlot,
  setTimeToValidRange,
  getUtxo,
  setUtxo,

  -- * MonadUtxoQuery
  MonadUtxoQuery(..),
  utxosByPaymentCredential,

  -- * MonadDatumQuery
  MonadDatumQuery(..),

  -- * Implementation
  MonadBlockchainCardanoNodeT(..),
  runMonadBlockchainCardanoNodeT
) where

import qualified Cardano.Api                                       as C
import           Cardano.Api.Shelley                               (BabbageEra,
                                                                    EraHistory (..),
                                                                    Hash,
                                                                    HashableScriptData,
                                                                    LedgerProtocolParameters (..),
                                                                    LocalNodeConnectInfo,
                                                                    NetworkId,
                                                                    PaymentCredential,
                                                                    PoolId,
                                                                    ScriptData,
                                                                    SlotNo, Tx,
                                                                    TxId)
import           Cardano.Ledger.Shelley.API                        (Coin (..),
                                                                    UTxO)
import           Cardano.Slotting.Time                             (SlotLength,
                                                                    SystemStart)
import           Control.Lens                                      (_1, view)
import           Control.Monad.Except                              (MonadError,
                                                                    catchError,
                                                                    runExceptT,
                                                                    throwError)
import           Control.Monad.IO.Class                            (MonadIO (..))
import           Control.Monad.Reader                              (MonadTrans,
                                                                    ReaderT (..),
                                                                    ask, asks,
                                                                    lift)
import qualified Control.Monad.State                               as LazyState
import qualified Control.Monad.State.Strict                        as StrictState
import           Control.Monad.Trans.Except                        (ExceptT (..))
import           Control.Monad.Trans.Except.Result                 (ResultT)
import           Convex.Constants                                  (ERA)
import           Convex.MonadLog                                   (MonadLog (..),
                                                                    MonadLogIgnoreT (..),
                                                                    logInfoS,
                                                                    logWarn,
                                                                    logWarnS)
import           Convex.Utils                                      (posixTimeToSlotUnsafe,
                                                                    slotToUtcTime)
import           Data.Aeson                                        (FromJSON,
                                                                    ToJSON)
import           Data.Bifunctor                                    (Bifunctor (..))
import           Data.Map                                          (Map)
import qualified Data.Map                                          as Map
import           Data.Set                                          (Set)
import qualified Data.Set                                          as Set
import           Data.Time.Clock                                   (UTCTime)
import           GHC.Generics                                      (Generic)
import           Ouroboros.Consensus.HardFork.History              (interpretQuery,
                                                                    slotToSlotLength)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type   as T
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import qualified PlutusLedgerApi.V1                                as PV1
import           Prettyprinter                                     (Pretty (..),
                                                                    (<+>))
import           Test.QuickCheck.Monadic                           (PropertyM)

{-| Send transactions and resolve tx inputs.
-}
class Monad m => MonadBlockchain m where
  -- see note Note [sendTx Failure]
  sendTx                  :: Tx BabbageEra -> m (Either SendTxFailed TxId) -- ^ Submit a transaction to the network
  utxoByTxIn              :: Set C.TxIn -> m (C.UTxO C.BabbageEra) -- ^ Resolve tx inputs
  queryProtocolParameters :: m (LedgerProtocolParameters C.BabbageEra) -- ^ Get the protocol parameters
  queryStakeAddresses     :: Set C.StakeCredential -> NetworkId -> m (Map C.StakeAddress C.Quantity, Map C.StakeAddress PoolId) -- ^ Get stake rewards
  queryStakePools         :: m (Set PoolId) -- ^ Get the stake pools
  querySystemStart        :: m SystemStart
  queryEraHistory         :: m EraHistory
  querySlotNo             :: m (SlotNo, SlotLength, UTCTime)
                          -- ^ returns the current slot number, slot length and begin utc time for slot.
                          -- Slot 0 is returned when at genesis.
  networkId               :: m NetworkId -- ^ Get the network id

{-| Try sending the transaction to the node, failing with 'error' if 'sendTx'
  was not successful.
-}
trySendTx :: MonadBlockchain m => Tx BabbageEra -> m TxId
trySendTx = fmap (either (error . show) id) . sendTx

deriving newtype instance MonadBlockchain m => MonadBlockchain (MonadLogIgnoreT m)

instance MonadBlockchain m => MonadBlockchain (ResultT m) where
  sendTx = lift . sendTx
  utxoByTxIn = lift . utxoByTxIn
  queryProtocolParameters = lift queryProtocolParameters
  queryStakeAddresses creds = lift . queryStakeAddresses creds
  queryStakePools = lift queryStakePools
  querySystemStart = lift querySystemStart
  queryEraHistory = lift queryEraHistory
  querySlotNo = lift querySlotNo
  networkId = lift networkId

instance MonadBlockchain m => MonadBlockchain (ExceptT e m) where
  sendTx = lift . sendTx
  utxoByTxIn = lift . utxoByTxIn
  queryProtocolParameters = lift queryProtocolParameters
  queryStakeAddresses creds = lift . queryStakeAddresses creds
  queryStakePools = lift queryStakePools
  querySystemStart = lift querySystemStart
  queryEraHistory = lift queryEraHistory
  querySlotNo = lift querySlotNo
  networkId = lift networkId

instance MonadBlockchain m => MonadBlockchain (ReaderT e m) where
  sendTx = lift . sendTx
  utxoByTxIn = lift . utxoByTxIn
  queryProtocolParameters = lift queryProtocolParameters
  queryStakeAddresses creds = lift . queryStakeAddresses creds
  queryStakePools = lift queryStakePools
  querySystemStart = lift querySystemStart
  queryEraHistory = lift queryEraHistory
  querySlotNo = lift querySlotNo
  networkId = lift networkId

instance MonadBlockchain m => MonadBlockchain (StrictState.StateT e m) where
  sendTx = lift . sendTx
  utxoByTxIn = lift . utxoByTxIn
  queryProtocolParameters = lift queryProtocolParameters
  queryStakeAddresses creds = lift . queryStakeAddresses creds
  queryStakePools = lift queryStakePools
  querySystemStart = lift querySystemStart
  queryEraHistory = lift queryEraHistory
  querySlotNo = lift querySlotNo
  networkId = lift networkId

instance MonadBlockchain m => MonadBlockchain (LazyState.StateT e m) where
  sendTx = lift . sendTx
  utxoByTxIn = lift . utxoByTxIn
  queryProtocolParameters = lift queryProtocolParameters
  queryStakeAddresses creds = lift . queryStakeAddresses creds
  queryStakePools = lift queryStakePools
  querySystemStart = lift querySystemStart
  queryEraHistory = lift queryEraHistory
  querySlotNo = lift querySlotNo
  networkId = lift networkId

-- | Look up  a single UTxO
singleUTxO :: MonadBlockchain m => C.TxIn -> m (Maybe (C.TxOut C.CtxUTxO C.BabbageEra))
singleUTxO txi =  utxoByTxIn (Set.singleton txi) >>= \case
  C.UTxO (Map.toList -> [(_, o)]) -> pure (Just o)
  _ -> pure Nothing

{-| Modify the mockchain internals
-}
class MonadBlockchain m => MonadMockchain m where
  setReward :: C.StakeCredential -> Coin -> m ()
  modifySlot :: (SlotNo -> (SlotNo, a)) -> m a
  modifyUtxo :: (UTxO ERA -> (UTxO ERA, a)) -> m a

deriving newtype instance MonadMockchain m => MonadMockchain (MonadLogIgnoreT m)

instance MonadMockchain m => MonadMockchain (ResultT m) where
  setReward cred = lift . setReward cred
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo

instance MonadMockchain m => MonadMockchain (ReaderT e m) where
  setReward cred = lift . setReward cred
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo

instance MonadMockchain m => MonadMockchain (ExceptT e m) where
  setReward cred = lift . setReward cred
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo

instance MonadMockchain m => MonadMockchain (StrictState.StateT e m) where
  setReward cred = lift . setReward cred
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo

instance MonadMockchain m => MonadMockchain (LazyState.StateT e m) where
  setReward cred = lift . setReward cred
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo

{-| Get the current slot number
-}
getSlot :: MonadMockchain m => m SlotNo
getSlot = modifySlot (\s -> (s, s))

{-| Get the current slot number
-}
setSlot :: MonadMockchain m => SlotNo -> m ()
setSlot s = modifySlot (\_ -> (s, ()))

{-| Get the UTxO set |-}
getUtxo :: MonadMockchain m => m (UTxO ERA)
getUtxo = modifyUtxo (\s -> (s, s))

{-| Set the UTxO set |-}
setUtxo :: MonadMockchain m => UTxO ERA -> m ()
setUtxo u = modifyUtxo (const (u, ()))

{-| Set the slot number to the slot that contains the given POSIX time.
-}
setPOSIXTime :: (MonadFail m, MonadMockchain m) => PV1.POSIXTime -> m ()
setPOSIXTime tm =
  (posixTimeToSlotUnsafe <$> queryEraHistory <*> querySystemStart <*> pure tm) >>= either fail (setSlot . view _1)

{-| Change the clock so that the current slot time is within the given validity range.
This MAY move the clock backwards!
-}
setTimeToValidRange :: MonadMockchain m => (C.TxValidityLowerBound C.BabbageEra, C.TxValidityUpperBound C.BabbageEra) -> m ()
setTimeToValidRange = \case
  (C.TxValidityLowerBound _ lowerSlot, _)        -> setSlot lowerSlot
  (_, C.TxValidityUpperBound _ (Just upperSlot)) -> setSlot (pred upperSlot)
  _                                              -> pure ()

{-| Increase the slot number by 1.
-}
nextSlot :: MonadMockchain m => m ()
nextSlot = modifySlot (\s -> (succ s, ()))

data MonadBlockchainError e =
  MonadBlockchainError e
  | FailWith String
  deriving stock (Functor, Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

{- Note [MonadUtxoQuery design]

The 'MonadUtxoQuery' class provides a lookup function that tells us the
unspent transaction outputs locked by one of a set of payment credentials.

The reason why this is not part of 'MonadBlockchain' is that the latter can
be implemented efficiently using only a running cardano-node, while 'MonadUtxoQuery'
requires a separate indexer. The classes are split to give callers more fine-grained
control over the capabilities they require.

-}

-- | A capability typeclass that provides methods for querying a chain indexer.
--   See note [MonadUtxoQuery design].
--   NOTE: There are currently no implementations of this class in sc-tools.
class Monad m => MonadUtxoQuery m where
  -- | Given a set of payment credentials, retrieve all UTxOs associated with
  -- those payment credentials according to the current indexed blockchain state.
  utxosByPaymentCredentials :: Set PaymentCredential -> m (C.UTxO BabbageEra)

instance MonadUtxoQuery m => MonadUtxoQuery (ResultT m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (ExceptT e m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (ReaderT e m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (StrictState.StateT s m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (LazyState.StateT s m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (MonadBlockchainCardanoNodeT e m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (MonadLogIgnoreT m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

-- | Given a single payment credential, find the UTxOs with that credential
utxosByPaymentCredential :: MonadUtxoQuery m => PaymentCredential -> m (C.UTxO BabbageEra)
utxosByPaymentCredential = utxosByPaymentCredentials . Set.singleton

{- Note [MonadDatumQuery design]

Initially only part of MonadMockchain, but now as a separate typeclass.
Useful for resolving datum hashes from Mockchain or a chain-indexer.
-}

class Monad m => MonadDatumQuery m where
  queryDatumFromHash :: Hash ScriptData -> m (Maybe HashableScriptData)

instance MonadDatumQuery m => MonadDatumQuery (ResultT m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance MonadDatumQuery m => MonadDatumQuery (ExceptT e m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance MonadDatumQuery m => MonadDatumQuery (ReaderT e m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance MonadDatumQuery m => MonadDatumQuery (StrictState.StateT s m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance MonadDatumQuery m => MonadDatumQuery (LazyState.StateT s m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance MonadDatumQuery m => MonadDatumQuery (MonadBlockchainCardanoNodeT e m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance MonadDatumQuery m => MonadDatumQuery (MonadLogIgnoreT m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance MonadDatumQuery m => MonadDatumQuery (PropertyM m) where
  queryDatumFromHash= lift . queryDatumFromHash

{- Note [sendTx Failure]

It would be nice to return a more accurate error type than 'SendTxFailed',
but our two implementations of 'MonadBlockchain' (mockchain and cardano-node backend)
have different errors and it does not seem possible to find a common type.

-}

-- | Error message obtained when a transaction was not accepted by the node.
--
newtype SendTxFailed = SendTxFailed { unSendTxFailed :: String }
  deriving stock (Eq, Ord, Show)

instance Pretty SendTxFailed where
  pretty (SendTxFailed msg) = "sendTx: Submission failed:" <+> pretty msg

{-| 'MonadBlockchain' implementation that connects to a cardano node
-}
newtype MonadBlockchainCardanoNodeT e m a = MonadBlockchainCardanoNodeT { unMonadBlockchainCardanoNodeT :: ReaderT LocalNodeConnectInfo (ExceptT (MonadBlockchainError e) m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadError e (MonadBlockchainCardanoNodeT e m) where
  throwError = MonadBlockchainCardanoNodeT . throwError . MonadBlockchainError
  catchError (MonadBlockchainCardanoNodeT action) handler = MonadBlockchainCardanoNodeT $ catchError action (\case { MonadBlockchainError e -> unMonadBlockchainCardanoNodeT (handler e); e' -> throwError e' })

runMonadBlockchainCardanoNodeT :: LocalNodeConnectInfo -> MonadBlockchainCardanoNodeT e m a -> m (Either (MonadBlockchainError e) a)
runMonadBlockchainCardanoNodeT info (MonadBlockchainCardanoNodeT action) = runExceptT (runReaderT action info)

runQuery :: (MonadIO m, MonadLog m) => C.QueryInMode a -> MonadBlockchainCardanoNodeT e m a
runQuery qry = MonadBlockchainCardanoNodeT $ do
  info <- ask
  result <- liftIO (runExceptT $ C.queryNodeLocalState info T.VolatileTip qry)
  case result of
    Left err -> do
      let msg = "runQuery: Query failed: " <> show err
      logWarnS msg
      throwError $ FailWith msg
    Right result' -> do
      pure result'

runQuery' :: (MonadIO m, MonadLog m, Show e1) => C.QueryInMode (Either e1 a) -> MonadBlockchainCardanoNodeT e2 m a
runQuery' qry = runQuery qry >>= \case
  Left err -> MonadBlockchainCardanoNodeT $ do
    let msg = "runQuery': Era mismatch: " <> show err
    logWarnS msg
    throwError $ FailWith msg
  Right result' -> pure result'

instance (MonadLog m, MonadIO m) => MonadBlockchain (MonadBlockchainCardanoNodeT e m) where
  sendTx tx = MonadBlockchainCardanoNodeT $ do
    let txId = C.getTxId (C.getTxBody tx)
    info <- ask
    result <- liftIO (C.submitTxToNodeLocal info (C.TxInMode C.ShelleyBasedEraBabbage tx))
    case result of
      SubmitSuccess -> do
        logInfoS ("sendTx: Submitted " <> show txId)
        pure (Right txId)
      SubmitFail reason -> do
        let msg = SendTxFailed (show reason)
        logWarn msg
        pure (Left msg)

  utxoByTxIn txIns =
    runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage (C.QueryUTxO (C.QueryUTxOByTxIn txIns))))

  queryProtocolParameters = do
    LedgerProtocolParameters <$> runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryProtocolParameters))

  queryStakeAddresses creds nid =
    first (fmap C.lovelaceToQuantity) <$> runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage (C.QueryStakeAddresses creds nid)))

  queryStakePools =
    runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryStakePools))

  querySystemStart = runQuery C.QuerySystemStart

  queryEraHistory = runQuery C.QueryEraHistory

  querySlotNo = do
    (eraHistory@(EraHistory interpreter), systemStart) <- (,) <$> queryEraHistory <*> querySystemStart
    slotNo <- runQuery C.QueryChainPoint >>= \case
                C.ChainPointAtGenesis  -> pure $ fromIntegral (0 :: Integer)
                C.ChainPoint slot _hsh -> pure slot
    MonadBlockchainCardanoNodeT $ do
      let logErr err = do
            let msg = "querySlotNo: Failed with " <> err
            logWarnS msg
            throwError $ FailWith msg
      utctime <- either logErr pure (slotToUtcTime eraHistory systemStart slotNo)
      either (logErr . show) (\l -> pure (slotNo, l, utctime)) (interpretQuery interpreter $ slotToSlotLength slotNo)

  networkId = MonadBlockchainCardanoNodeT (asks C.localNodeNetworkId)

instance MonadTrans (MonadBlockchainCardanoNodeT e) where
  lift = MonadBlockchainCardanoNodeT . lift .lift

instance (MonadLog m) => MonadLog (MonadBlockchainCardanoNodeT e m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'
  logDebug' = lift . logDebug'

instance (MonadLog m) => MonadFail (MonadBlockchainCardanoNodeT e m) where
  fail = MonadBlockchainCardanoNodeT . throwError . FailWith
