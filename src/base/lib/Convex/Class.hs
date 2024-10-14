{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia            #-}
{-| Typeclasses for blockchain operations
-}
module Convex.Class(

  -- * Monad blockchain
  MonadBlockchain(..),
  trySendTx,
  singleUTxO,

  -- * Monad mockchain
  MonadMockchain(..),

  -- * Mockchain state & lenses
  MockChainState (..),
  env,
  poolState,
  transactions,
  failedTransactions,
  datums,
  _Phase1Error,
  _Phase2Error,

  -- * Other types
  ExUnitsError(..),
  ValidationError(..),
  BlockchainException(..),
  _VExUnits,
  _PredicateFailures,
  _ApplyTxFailure,

  -- * Utilities
  getMockChainState,
  putMockChainState,
  setReward,
  modifySlot,
  getSlot,
  setSlot,
  setPOSIXTime,
  nextSlot,
  setTimeToValidRange,
  modifyUtxo,
  getUtxo,
  setUtxo,
  getTxs,

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
import           Cardano.Api.Shelley                               (EraHistory (..),
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
import qualified Cardano.Api.Shelley                               as C
import           Cardano.Ledger.Alonzo.Plutus.Evaluate             (CollectError)
import qualified Cardano.Ledger.Core                               as Core
import           Cardano.Ledger.Crypto                             (StandardCrypto)
import           Cardano.Ledger.Plutus.Evaluate                    (PlutusWithContext (..))
import           Cardano.Ledger.Shelley.API                        (ApplyTxError,
                                                                    Coin (..),
                                                                    MempoolEnv,
                                                                    MempoolState,
                                                                    UTxO (..),
                                                                    Validated,
                                                                    extractTx)
import           Cardano.Ledger.Shelley.LedgerState                (certDStateL,
                                                                    dsUnifiedL,
                                                                    lsCertStateL,
                                                                    rewards)
import           Cardano.Ledger.UMap                               (RDPair (..),
                                                                    adjust,
                                                                    compactCoinOrError)
import           Cardano.Slotting.Time                             (SlotLength,
                                                                    SystemStart)
import           Control.Exception                                 (Exception,
                                                                    throwIO)
import           Control.Lens                                      (_1, set, to,
                                                                    view, (^.))
import           Control.Lens.TH                                   (makeLensesFor,
                                                                    makePrisms)
import           Control.Monad.Except                              (MonadError,
                                                                    runExceptT)
import           Control.Monad.IO.Class                            (MonadIO (..))
import           Control.Monad.Primitive                           (PrimMonad)
import           Control.Monad.Reader                              (MonadTrans,
                                                                    ReaderT (..),
                                                                    ask, asks,
                                                                    lift)
import qualified Control.Monad.State                               as LazyState
import qualified Control.Monad.State.Strict                        as StrictState
import           Control.Monad.Trans.Except                        (ExceptT (..))
import           Control.Monad.Trans.Except.Result                 (ResultT)
import qualified Convex.CardanoApi.Lenses                          as L
import           Convex.MonadLog                                   (MonadLog (..),
                                                                    MonadLogIgnoreT (..),
                                                                    MonadLogKatipT (..))
import           Convex.NodeParams                                 (NodeParams,
                                                                    pParams)
import           Convex.Utils                                      (posixTimeToSlotUnsafe,
                                                                    slotToUtcTime)
import           Convex.Utxos                                      (UtxoSet)
import           Data.Bifunctor                                    (Bifunctor (..))
import           Data.Functor                                      ((<&>))
import           Data.Map                                          (Map)
import qualified Data.Map                                          as Map
import           Data.Set                                          (Set)
import qualified Data.Set                                          as Set
import           Data.Time.Clock                                   (UTCTime)
import           Katip.Monadic                                     (KatipContextT (..))
import           Ouroboros.Consensus.HardFork.History              (interpretQuery,
                                                                    slotToSlotLength)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type   as T
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import qualified PlutusLedgerApi.V1                                as PV1
import           Test.QuickCheck.Monadic                           (PropertyM)

-- Error types
data ExUnitsError era =
  Phase1Error (C.TransactionValidityError era)
  | Phase2Error C.ScriptExecutionError
  deriving stock Show
  deriving anyclass Exception

makePrisms ''ExUnitsError

-- see https://github.com/j-mueller/sc-tools/issues/214
data ValidationError era =
  ValidationErrorInMode !C.TxValidationErrorInCardanoMode
  | VExUnits !(ExUnitsError era)
  | PredicateFailures ![CollectError (C.ShelleyLedgerEra era)]
  | ApplyTxFailure !(ApplyTxError (C.ShelleyLedgerEra era))
  deriving anyclass Exception

instance C.IsAlonzoBasedEra era => Show (ValidationError era) where
  show err = C.alonzoEraOnwardsConstraints @era C.alonzoBasedEra $ "ValidationError: " <> case err of
    ValidationErrorInMode err' -> "ValidationErrorInMode: " <> show err'
    VExUnits err'              -> "VExUnits: " <> show err'
    PredicateFailures errs'    -> "PredicateFailures: " <> show errs'
    ApplyTxFailure err'        -> "ApplyTxFailure: " <> show err'

makePrisms ''ValidationError

{-| Send transactions and resolve tx inputs.
-}
class Monad m => MonadBlockchain era m | m -> era where
  sendTx                  :: Tx era -> m (Either (ValidationError era) TxId) -- ^ Submit a transaction to the network
  utxoByTxIn              :: Set C.TxIn -> m (C.UTxO era) -- ^ Resolve tx inputs
  queryProtocolParameters :: m (LedgerProtocolParameters era) -- ^ Get the protocol parameters
  queryStakeAddresses     :: Set C.StakeCredential -> NetworkId -> m (Map C.StakeAddress C.Quantity, Map C.StakeAddress PoolId) -- ^ Get stake rewards
  queryStakePools         :: m (Set PoolId) -- ^ Get the stake pools
  querySystemStart        :: m SystemStart
  queryEraHistory         :: m C.EraHistory
  querySlotNo             :: m (C.SlotNo, SlotLength, UTCTime)
                          -- ^ returns the current slot number, slot length and begin utc time for slot.
                          -- Slot 0 is returned when at genesis.
  queryNetworkId          :: m C.NetworkId -- ^ Get the network id

  default sendTx :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => Tx era -> m (Either (ValidationError era) TxId)
  sendTx = lift . sendTx

  default utxoByTxIn :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => Set C.TxIn -> m (C.UTxO era)
  utxoByTxIn = lift . utxoByTxIn

  default queryProtocolParameters :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m (LedgerProtocolParameters era)
  queryProtocolParameters = lift queryProtocolParameters

  default queryStakeAddresses :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => Set C.StakeCredential -> NetworkId -> m (Map C.StakeAddress C.Quantity, Map C.StakeAddress PoolId) -- ^ Get stake rewards
  queryStakeAddresses = (lift .) . queryStakeAddresses

  default queryStakePools :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m (Set PoolId)
  queryStakePools = lift queryStakePools

  default querySystemStart :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m SystemStart
  querySystemStart = lift querySystemStart

  default queryEraHistory :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m EraHistory
  queryEraHistory = lift queryEraHistory

  default querySlotNo :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m (SlotNo, SlotLength, UTCTime)
  querySlotNo = lift querySlotNo

  default queryNetworkId :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m NetworkId
  queryNetworkId = lift queryNetworkId


trySendTx :: (MonadBlockchain era m, C.IsAlonzoBasedEra era) => Tx era -> m TxId
trySendTx = fmap (either (error . show) id) . sendTx

deriving newtype instance MonadBlockchain era m => MonadBlockchain era (KatipContextT m)
deriving newtype instance MonadBlockchain era m => MonadBlockchain era (MonadLogKatipT m)
deriving newtype instance MonadBlockchain era m => MonadBlockchain era (MonadLogIgnoreT m)

instance MonadBlockchain era m => MonadBlockchain era (ResultT m)
instance MonadBlockchain era m => MonadBlockchain era (ExceptT e m)
instance MonadBlockchain era m => MonadBlockchain era (ReaderT e m)
instance MonadBlockchain era m => MonadBlockchain era (StrictState.StateT e m)
instance MonadBlockchain era m => MonadBlockchain era (LazyState.StateT e m)
instance MonadBlockchain era m => MonadBlockchain era (PropertyM m)

-- | Look up  a single UTxO
singleUTxO :: MonadBlockchain era m => C.TxIn -> m (Maybe (C.TxOut C.CtxUTxO era))
singleUTxO txi =  utxoByTxIn (Set.singleton txi) >>= \case
  C.UTxO (Map.toList -> [(_, o)]) -> pure (Just o)
  _ -> pure Nothing

{-| State of the mockchain
-}
data MockChainState era =
  MockChainState
    { mcsEnv                :: MempoolEnv (C.ShelleyLedgerEra era)
    , mcsPoolState          :: MempoolState (C.ShelleyLedgerEra era)
    , mcsTransactions       :: [(Validated (Core.Tx (C.ShelleyLedgerEra era)), [PlutusWithContext StandardCrypto])] -- ^ Transactions that were submitted to the mockchain and validated
    , mcsFailedTransactions :: [(Tx era, ValidationError era)] -- ^ Transactions that were submitted to the mockchain, but failed with a validation error
    , mcsDatums             :: Map (Hash ScriptData) HashableScriptData
    }

makeLensesFor
  [ ("mcsEnv", "env")
  , ("mcsPoolState", "poolState")
  , ("mcsTransactions", "transactions")
  , ("mcsFailedTransactions", "failedTransactions")
  , ("mcsDatums", "datums")
  ] ''MockChainState

{-| Modify the mockchain internals
-}
class MonadBlockchain era m => MonadMockchain era m where
  modifyMockChainState :: (MockChainState era -> (a, MockChainState era)) -> m a
  askNodeParams :: m (NodeParams era)

  default modifyMockChainState :: (MonadTrans t, m ~ t n, MonadMockchain era n) => (MockChainState era -> (a, MockChainState era)) -> m a
  modifyMockChainState = lift . modifyMockChainState

  default askNodeParams :: (MonadTrans t, m ~ t n, MonadMockchain era n) => m (NodeParams era)
  askNodeParams = lift askNodeParams

deriving newtype instance MonadMockchain era m => MonadMockchain era (MonadLogIgnoreT m)

instance MonadMockchain era m => MonadMockchain era (ResultT m)
instance MonadMockchain era m => MonadMockchain era (ReaderT e m)
instance MonadMockchain era m => MonadMockchain era (ExceptT e m)
instance MonadMockchain era m => MonadMockchain era (StrictState.StateT e m)
instance MonadMockchain era m => MonadMockchain era (LazyState.StateT e m)
instance MonadMockchain era m => MonadMockchain era (PropertyM m)

getMockChainState :: MonadMockchain era m => m (MockChainState era)
getMockChainState = modifyMockChainState (\s -> (s, s))

putMockChainState :: MonadMockchain era m => MockChainState era -> m ()
putMockChainState s = modifyMockChainState (const ((), s))

setReward :: forall era m. (Core.EraCrypto (C.ShelleyLedgerEra era) ~ StandardCrypto, MonadMockchain era m) => C.StakeCredential -> Coin -> m ()
setReward cred coin = do
  mcs <- getMockChainState
  let
    dState = mcs ^. poolState . lsCertStateL . certDStateL
    umap =
      adjust
        (\rd -> rd {rdReward=compactCoinOrError coin})
        (C.toShelleyStakeCredential cred)
        (rewards dState)
  putMockChainState (set (poolState . lsCertStateL . certDStateL . dsUnifiedL) umap mcs)

modifySlot :: MonadMockchain era m => (SlotNo -> (SlotNo, a)) -> m a
modifySlot f = modifyMockChainState $ \s ->
  let (s', a) = f (s ^. env . L.slot)
  in (a, set (env . L.slot) s' s)

{-| Get the current slot number
-}
getSlot :: MonadMockchain era m => m SlotNo
getSlot = modifySlot (\s -> (s, s))

{-| Get the current slot number
-}
setSlot :: MonadMockchain era m => SlotNo -> m ()
setSlot s = modifySlot (const (s, ()))

modifyUtxo :: forall era m a.
  (C.IsShelleyBasedEra era, MonadMockchain era m)
  => (UTxO (C.ShelleyLedgerEra era) -> (UTxO (C.ShelleyLedgerEra era), a)) -> m a
modifyUtxo f = C.shelleyBasedEraConstraints @era C.shelleyBasedEra $
  askNodeParams >>= \np -> modifyMockChainState $ \s ->
  let (u', a) = f (s ^. poolState . L.utxoState . L._UTxOState (pParams np) . _1)
  in (a, set (poolState . L.utxoState . L._UTxOState (pParams np) . _1) u' s)

{-| Get the UTxO set |-}
getUtxo :: (MonadMockchain era m, C.IsShelleyBasedEra era) => m (UTxO (C.ShelleyLedgerEra era))
getUtxo = modifyUtxo (\s -> (s, s))

{-| Set the UTxO set |-}
setUtxo :: (MonadMockchain era m, C.IsShelleyBasedEra era) => UTxO (C.ShelleyLedgerEra era) -> m ()
setUtxo u = modifyUtxo (const (u, ()))

{-| Return all Tx's from the ledger state -}
getTxs :: (MonadMockchain era m, C.IsShelleyBasedEra era) => m [C.Tx era]
getTxs = getMockChainState <&> view (transactions . traverse . _1 . to ((: []) . C.ShelleyTx C.shelleyBasedEra . extractTx))

{-| Return all Tx's from the ledger state -}

{-| Set the slot number to the slot that contains the given POSIX time.
-}
setPOSIXTime :: (MonadFail m, MonadMockchain era m) => PV1.POSIXTime -> m ()
setPOSIXTime tm =
  (posixTimeToSlotUnsafe <$> queryEraHistory <*> querySystemStart <*> pure tm) >>= either fail (setSlot . view _1)

{-| Change the clock so that the current slot time is within the given validity range.
This MAY move the clock backwards!
-}
setTimeToValidRange :: MonadMockchain era m => (C.TxValidityLowerBound era, C.TxValidityUpperBound era) -> m ()
setTimeToValidRange = \case
  (C.TxValidityLowerBound _ lowerSlot, _)        -> setSlot lowerSlot
  (_, C.TxValidityUpperBound _ (Just upperSlot)) -> setSlot (pred upperSlot)
  _                                              -> pure ()

{-| Increase the slot number by 1.
-}
nextSlot :: MonadMockchain era m => m ()
nextSlot = modifySlot (\s -> (succ s, ()))

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
  -- those payment credentials according to the current indexed blockchain
  -- state. Each UTXO also possibly has the resolved datum (meaning that if we
  -- only have the datum hash, the implementation should try and resolve it to
  -- the actual datum).
  utxosByPaymentCredentials :: Set PaymentCredential -> m (UtxoSet C.CtxUTxO (Maybe C.HashableScriptData))

  default utxosByPaymentCredentials :: (MonadTrans t, m ~ t n, MonadUtxoQuery n) => Set PaymentCredential -> m (UtxoSet C.CtxUTxO (Maybe C.HashableScriptData))
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (ResultT m) where
instance MonadUtxoQuery m => MonadUtxoQuery (ExceptT e m)
instance MonadUtxoQuery m => MonadUtxoQuery (ReaderT e m)
instance MonadUtxoQuery m => MonadUtxoQuery (StrictState.StateT s m)
instance MonadUtxoQuery m => MonadUtxoQuery (LazyState.StateT s m)
instance MonadUtxoQuery m => MonadUtxoQuery (MonadBlockchainCardanoNodeT era m)
instance MonadUtxoQuery m => MonadUtxoQuery (MonadLogIgnoreT m)
instance MonadUtxoQuery m => MonadUtxoQuery (PropertyM m)

-- | Given a single payment credential, find the UTxOs with that credential
utxosByPaymentCredential :: MonadUtxoQuery m => PaymentCredential -> m (UtxoSet C.CtxUTxO (Maybe C.HashableScriptData))
utxosByPaymentCredential = utxosByPaymentCredentials . Set.singleton

{- Note [MonadDatumQuery design]

Initially only part of MonadMockchain, but now as a separate typeclass.
Useful for resolving datum hashes from Mockchain or a chain-indexer.
-}

class Monad m => MonadDatumQuery m where
  queryDatumFromHash :: C.Hash C.ScriptData -> m (Maybe C.HashableScriptData)

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

instance MonadDatumQuery m => MonadDatumQuery (MonadBlockchainCardanoNodeT era m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance MonadDatumQuery m => MonadDatumQuery (MonadLogIgnoreT m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance MonadDatumQuery m => MonadDatumQuery (PropertyM m) where
  queryDatumFromHash= lift . queryDatumFromHash


{-| 'MonadBlockchain' implementation that connects to a cardano node
-}
newtype BlockchainException = BlockchainException String
  deriving stock Show
  deriving anyclass Exception

newtype MonadBlockchainCardanoNodeT era m a = MonadBlockchainCardanoNodeT { unMonadBlockchainCardanoNodeT :: ReaderT LocalNodeConnectInfo m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError e, MonadFail, PrimMonad)

runMonadBlockchainCardanoNodeT :: LocalNodeConnectInfo -> MonadBlockchainCardanoNodeT era m a -> m a
runMonadBlockchainCardanoNodeT info (MonadBlockchainCardanoNodeT action) = runReaderT action info

runQuery :: MonadIO m => C.QueryInMode a -> MonadBlockchainCardanoNodeT era m a
runQuery qry = MonadBlockchainCardanoNodeT $ do
  info <- ask
  result <- liftIO (runExceptT $ C.queryNodeLocalState info T.VolatileTip qry)
  case result of
    Left err -> do
      let msg = "runQuery: Query failed: " <> show err
      liftIO $ throwIO $ BlockchainException msg
    Right result' -> do
      pure result'

runQuery' :: (MonadIO m, Show e1) => C.QueryInMode (Either e1 a) -> MonadBlockchainCardanoNodeT era m a
runQuery' qry = runQuery qry >>= \case
  Left err -> MonadBlockchainCardanoNodeT $ do
    let msg = "runQuery': Era mismatch: " <> show err
    liftIO $ throwIO $ BlockchainException msg
  Right result' -> pure result'

instance (MonadIO m, C.IsShelleyBasedEra era) => MonadBlockchain era (MonadBlockchainCardanoNodeT era m) where
  sendTx tx = MonadBlockchainCardanoNodeT $ do
    let txId = C.getTxId (C.getTxBody tx)
    info <- ask
    result <- liftIO (C.submitTxToNodeLocal info (C.TxInMode C.shelleyBasedEra tx))
    pure $ case result of
      SubmitSuccess ->
        Right txId
      SubmitFail reason ->
        Left $ ValidationErrorInMode reason

  utxoByTxIn txIns =
    runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra C.shelleyBasedEra (C.QueryUTxO (C.QueryUTxOByTxIn txIns))))

  queryProtocolParameters = do
    LedgerProtocolParameters <$> runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra C.shelleyBasedEra C.QueryProtocolParameters))

  queryStakeAddresses creds nid =
    first (fmap C.lovelaceToQuantity) <$> runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra (C.shelleyBasedEra @era) (C.QueryStakeAddresses creds nid)))

  queryStakePools =
    runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra (C.shelleyBasedEra @era) C.QueryStakePools))

  querySystemStart = runQuery C.QuerySystemStart

  queryEraHistory = runQuery C.QueryEraHistory

  querySlotNo = do
    (eraHistory@(C.EraHistory interpreter), systemStart) <- (,) <$> queryEraHistory <*> querySystemStart
    slotNo <- runQuery C.QueryChainPoint >>= \case
                C.ChainPointAtGenesis  -> pure $ fromIntegral (0 :: Integer)
                C.ChainPoint slot _hsh -> pure slot
    MonadBlockchainCardanoNodeT $ do
      let logErr err = do
            let msg = "querySlotNo: Failed with " <> err
            liftIO $ throwIO $ BlockchainException msg
      utctime <- either logErr pure (slotToUtcTime eraHistory systemStart slotNo)
      either (logErr . show) (\l -> pure (slotNo, l, utctime)) (interpretQuery interpreter $ slotToSlotLength slotNo)

  queryNetworkId = MonadBlockchainCardanoNodeT (asks C.localNodeNetworkId)

instance MonadTrans (MonadBlockchainCardanoNodeT era) where
  lift = MonadBlockchainCardanoNodeT . lift

instance (MonadLog m) => MonadLog (MonadBlockchainCardanoNodeT era m)
