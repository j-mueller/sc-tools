{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Typeclasses for blockchain operations
module Convex.Class (
  -- * Monad blockchain
  MonadBlockchain (..),
  trySendTx,
  singleUTxO,

  -- * Monad mockchain
  MonadMockchain (..),

  -- * Mockchain state & lenses
  MockChainState (..),
  env,
  poolState,
  transactions,
  failedTransactions,
  datums,
  txById,

  -- * Other types
  ExUnitsError (..),
  AsExUnitsError (..),
  ValidationError (..),
  AsValidationError (..),
  BlockchainException (..),

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
  getTxById,

  -- * MonadUtxoQuery
  MonadUtxoQuery (..),
  utxosByPaymentCredential,

  -- * MonadDatumQuery
  MonadDatumQuery (..),

  -- * Implementation
  MonadBlockchainCardanoNodeT (..),
  runMonadBlockchainCardanoNodeT,
) where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError)
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Plutus.Evaluate (PlutusWithContext (..))
import Cardano.Ledger.Shelley.API (
  ApplyTxError,
  Coin (..),
  MempoolEnv,
  MempoolState,
  UTxO (..),
  Validated,
  extractTx,
 )
import Cardano.Ledger.Shelley.LedgerState (
  certDStateL,
  dsUnifiedL,
  lsCertStateL,
  rewards,
 )
import Cardano.Ledger.UMap (
  RDPair (..),
  adjust,
  compactCoinOrError,
 )
import Cardano.Slotting.Time (
  SlotLength,
  SystemStart,
 )
import Control.Exception (
  Exception,
  throwIO,
 )
import Control.Lens (
  Prism',
  at,
  set,
  to,
  view,
  (^.),
  _1,
 )
import Control.Lens.TH (
  makeClassyPrisms,
  makeLensesFor,
 )
import Control.Monad.Except (
  MonadError,
  runExceptT,
 )
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader (
  MonadTrans,
  ReaderT (..),
  ask,
  asks,
  lift,
 )
import Control.Monad.State qualified as LazyState
import Control.Monad.State.Strict qualified as StrictState
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Except.Result (ResultT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Convex.CardanoApi.Lenses qualified as L
import Convex.MonadLog (
  MonadLog (..),
  MonadLogIgnoreT (..),
  MonadLogKatipT (..),
 )
import Convex.NodeParams (
  NodeParams,
  pParams,
 )
import Convex.Utils (
  posixTimeToSlotUnsafe,
  slotToUtcTime,
 )
import Convex.Utxos (UtxoSet)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime)
import Katip.Monadic (KatipContextT (..))
import Ouroboros.Consensus.HardFork.History (
  interpretQuery,
  slotToSlotLength,
 )
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as T
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import PlutusLedgerApi.V1 qualified as PV1
import Test.QuickCheck.Monadic (PropertyM)

-- Error types
data ExUnitsError era
  = Phase1Error (C.TransactionValidityError era)
  | Phase2Error C.ScriptExecutionError
  deriving stock (Show)
  deriving anyclass (Exception)

{- | [Note] Classy prism errors

Classy prisms are useful to create hierarchies of errors.

If 'ExUnitsError' has classy prisms, then all functions returning these errors could also return
'ValidationErrors's thusly:

Start by creating an 'AsExUnitsError' instance for 'ValidationError's:
@
instance AsExUnitsError (ValidationError era) era where
  _ExUnitsError :: L.Prism' (ValidationError era) (ExUnitsError era)
  _ExUnitsError = _VExUnits . _ExUnitsError
@

Then functions throwing ExUnitsError throw like this:
@
fun :: (MonadError e m, AsExUnitsError e era) => m ()
fun = do
  ...
  throwError $ L.review _ExUnitsError $ Phase1Error bla
@
and we are not forcing a specific error @e@, but only that @e@ can be created via the prism from an @ExUnitsError@.

It is also possible to have multiple types of errors:
@
fun :: (MonadError e m, AsExUnitsError e era, AsValidationError e era, AsSomeOtherError e) => m ()
fun = do
  ...
  throwError $ L.review _ExUnitsError $ Phase1Error bla
  ...
  throwError $ L.review _PredicateFailures $ []
  ...
  throwError $ L.review _OtherError
@
Note that @OtherError@ is "parallel" to @_ExUnitsError@ and not a descendent, i.e. not related.

The whole tree of exceptions can be mapped in this fashion, which increases the interoperability
between error-throwing functions without the need for explicit 'modifyError' calls.
-}
makeClassyPrisms ''ExUnitsError

-- see https://github.com/j-mueller/sc-tools/issues/214
data ValidationError era
  = ValidationErrorInMode !C.TxValidationErrorInCardanoMode
  | VExUnits !(ExUnitsError era)
  | PredicateFailures ![CollectError (C.ShelleyLedgerEra era)]
  | ApplyTxFailure !(ApplyTxError (C.ShelleyLedgerEra era))
  deriving anyclass (Exception)

instance (C.IsAlonzoBasedEra era) => Show (ValidationError era) where
  show err =
    C.alonzoEraOnwardsConstraints @era C.alonzoBasedEra $
      "ValidationError: " <> case err of
        ValidationErrorInMode err' -> "ValidationErrorInMode: " <> show err'
        VExUnits err' -> "VExUnits: " <> show err'
        PredicateFailures errs' -> "PredicateFailures: " <> show errs'
        ApplyTxFailure err' -> "ApplyTxFailure: " <> show err'

makeClassyPrisms ''ValidationError

instance AsExUnitsError (ValidationError era) era where
  _ExUnitsError :: Prism' (ValidationError era) (ExUnitsError era)
  _ExUnitsError = _VExUnits . _ExUnitsError

-- | Send transactions and resolve tx inputs.
class (Monad m) => MonadBlockchain era m | m -> era where
  sendTx
    :: C.Tx era
    -> m (Either (ValidationError era) C.TxId)
    -- ^ Submit a transaction to the network

  utxoByTxIn
    :: Set C.TxIn
    -> m (C.UTxO era)
    -- ^ Resolve tx inputs

  queryProtocolParameters :: m (C.LedgerProtocolParameters era)
    -- ^ Get the protocol parameters

  queryStakeAddresses
    :: Set C.StakeCredential
    -> C.NetworkId
    -> m (Map C.StakeAddress C.Quantity, Map C.StakeAddress C.PoolId)
    -- ^ Get stake rewards

  queryStakePools :: m (Set C.PoolId)
    -- ^ Get the stake pools

  queryStakeVoteDelegatees
    :: Set C.StakeCredential
    -> m (Map C.StakeCredential (Ledger.DRep Ledger.StandardCrypto))
    -- ^ Get the delegatees of the stake addresses

  querySystemStart :: m SystemStart
  queryEraHistory :: m C.EraHistory
  querySlotNo :: m (C.SlotNo, SlotLength, UTCTime)
    -- ^ returns the current slot number, slot length and begin utc time for slot.
    -- Slot 0 is returned when at genesis.

  queryNetworkId :: m C.NetworkId
    -- ^ Get the network id

  default sendTx :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => C.Tx era -> m (Either (ValidationError era) C.TxId)
  sendTx = lift . sendTx

  default utxoByTxIn :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => Set C.TxIn -> m (C.UTxO era)
  utxoByTxIn = lift . utxoByTxIn

  default queryProtocolParameters :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m (C.LedgerProtocolParameters era)
  queryProtocolParameters = lift queryProtocolParameters

  default queryStakeAddresses
    :: (MonadTrans t, m ~ t n, MonadBlockchain era n)
    => Set C.StakeCredential
    -> C.NetworkId
    -> m (Map C.StakeAddress C.Quantity, Map C.StakeAddress C.PoolId)
    -- ^ Get stake rewards
  queryStakeAddresses = (lift .) . queryStakeAddresses

  default queryStakePools :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m (Set C.PoolId)
  queryStakePools = lift queryStakePools

  default queryStakeVoteDelegatees
    :: (MonadTrans t, m ~ t n, MonadBlockchain era n)
    => Set C.StakeCredential
    -> m (Map C.StakeCredential (Ledger.DRep Ledger.StandardCrypto))
  queryStakeVoteDelegatees = lift . queryStakeVoteDelegatees

  default querySystemStart :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m SystemStart
  querySystemStart = lift querySystemStart

  default queryEraHistory :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m C.EraHistory
  queryEraHistory = lift queryEraHistory

  default querySlotNo :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m (C.SlotNo, SlotLength, UTCTime)
  querySlotNo = lift querySlotNo

  default queryNetworkId :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m C.NetworkId
  queryNetworkId = lift queryNetworkId

trySendTx :: (MonadBlockchain era m, C.IsAlonzoBasedEra era) => C.Tx era -> m C.TxId
trySendTx = fmap (either (error . show) id) . sendTx

deriving newtype instance (MonadBlockchain era m) => MonadBlockchain era (KatipContextT m)
deriving newtype instance (MonadBlockchain era m) => MonadBlockchain era (MonadLogKatipT m)
deriving newtype instance (MonadBlockchain era m) => MonadBlockchain era (MonadLogIgnoreT m)

instance (MonadBlockchain era m) => MonadBlockchain era (ResultT m)
instance (MonadBlockchain era m) => MonadBlockchain era (MaybeT m)
instance (MonadBlockchain era m) => MonadBlockchain era (ExceptT e m)
instance (MonadBlockchain era m) => MonadBlockchain era (ReaderT e m)
instance (MonadBlockchain era m) => MonadBlockchain era (StrictState.StateT e m)
instance (MonadBlockchain era m) => MonadBlockchain era (LazyState.StateT e m)
instance (MonadBlockchain era m) => MonadBlockchain era (PropertyM m)

-- | Look up  a single UTxO
singleUTxO :: (MonadBlockchain era m) => C.TxIn -> m (Maybe (C.TxOut C.CtxUTxO era))
singleUTxO txi =
  utxoByTxIn (Set.singleton txi) >>= \case
    C.UTxO (Map.toList -> [(_, o)]) -> pure (Just o)
    _ -> pure Nothing

-- | State of the mockchain
data MockChainState era
  = MockChainState
  { mcsEnv :: MempoolEnv (C.ShelleyLedgerEra era)
  , mcsPoolState :: MempoolState (C.ShelleyLedgerEra era)
  , mcsTransactions :: [(Validated (Core.Tx (C.ShelleyLedgerEra era)), [PlutusWithContext StandardCrypto])]
  -- ^ Transactions that were submitted to the mockchain and validated
  , mcsFailedTransactions :: [(C.Tx era, ValidationError era)]
  -- ^ Transactions that were submitted to the mockchain, but failed with a validation error
  , mcsDatums :: Map (C.Hash C.ScriptData) C.HashableScriptData
  , mcsTxById :: Map C.TxId (C.Tx era)
  -- ^ Index of transactions by ID
  }

makeLensesFor
  [ ("mcsEnv", "env")
  , ("mcsPoolState", "poolState")
  , ("mcsTransactions", "transactions")
  , ("mcsFailedTransactions", "failedTransactions")
  , ("mcsDatums", "datums")
  , ("mcsTxById", "txById")
  ]
  ''MockChainState

-- | Modify the mockchain internals
class (MonadBlockchain era m) => MonadMockchain era m where
  modifyMockChainState :: (MockChainState era -> (a, MockChainState era)) -> m a
  askNodeParams :: m (NodeParams era)

  default modifyMockChainState :: (MonadTrans t, m ~ t n, MonadMockchain era n) => (MockChainState era -> (a, MockChainState era)) -> m a
  modifyMockChainState = lift . modifyMockChainState

  default askNodeParams :: (MonadTrans t, m ~ t n, MonadMockchain era n) => m (NodeParams era)
  askNodeParams = lift askNodeParams

deriving newtype instance (MonadMockchain era m) => MonadMockchain era (MonadLogIgnoreT m)

instance (MonadMockchain era m) => MonadMockchain era (ResultT m)
instance (MonadMockchain era m) => MonadMockchain era (ReaderT e m)
instance (MonadMockchain era m) => MonadMockchain era (ExceptT e m)
instance (MonadMockchain era m) => MonadMockchain era (StrictState.StateT e m)
instance (MonadMockchain era m) => MonadMockchain era (LazyState.StateT e m)
instance (MonadMockchain era m) => MonadMockchain era (PropertyM m)

getMockChainState :: (MonadMockchain era m) => m (MockChainState era)
getMockChainState = modifyMockChainState (\s -> (s, s))

putMockChainState :: (MonadMockchain era m) => MockChainState era -> m ()
putMockChainState s = modifyMockChainState (const ((), s))

setReward :: forall era m. (Core.EraCrypto (C.ShelleyLedgerEra era) ~ StandardCrypto, MonadMockchain era m) => C.StakeCredential -> Coin -> m ()
setReward cred coin = do
  mcs <- getMockChainState
  let
    dState = mcs ^. poolState . lsCertStateL . certDStateL
    umap =
      adjust
        (\rd -> rd{rdReward = compactCoinOrError coin})
        (C.toShelleyStakeCredential cred)
        (rewards dState)
  putMockChainState (set (poolState . lsCertStateL . certDStateL . dsUnifiedL) umap mcs)

modifySlot :: (MonadMockchain era m) => (C.SlotNo -> (C.SlotNo, a)) -> m a
modifySlot f = modifyMockChainState $ \s ->
  let (s', a) = f (s ^. env . L.slot)
   in (a, set (env . L.slot) s' s)

-- | Get the current slot number
getSlot :: (MonadMockchain era m) => m C.SlotNo
getSlot = modifySlot (\s -> (s, s))

-- | Get the current slot number
setSlot :: (MonadMockchain era m) => C.SlotNo -> m ()
setSlot s = modifySlot (const (s, ()))

modifyUtxo
  :: forall era m a
   . (C.IsShelleyBasedEra era, MonadMockchain era m)
  => (UTxO (C.ShelleyLedgerEra era) -> (UTxO (C.ShelleyLedgerEra era), a)) -> m a
modifyUtxo f =
  C.shelleyBasedEraConstraints @era C.shelleyBasedEra $
    askNodeParams >>= \np -> modifyMockChainState $ \s ->
      let (u', a) = f (s ^. poolState . L.utxoState . L._UTxOState (pParams np) . _1)
       in (a, set (poolState . L.utxoState . L._UTxOState (pParams np) . _1) u' s)

-- | Get the UTxO set |
getUtxo :: (MonadMockchain era m, C.IsShelleyBasedEra era) => m (UTxO (C.ShelleyLedgerEra era))
getUtxo = modifyUtxo (\s -> (s, s))

-- | Set the UTxO set |
setUtxo :: (MonadMockchain era m, C.IsShelleyBasedEra era) => UTxO (C.ShelleyLedgerEra era) -> m ()
setUtxo u = modifyUtxo (const (u, ()))

-- | Return all Tx's from the ledger state
getTxs :: (MonadMockchain era m, C.IsShelleyBasedEra era) => m [C.Tx era]
getTxs = getMockChainState <&> view (transactions . traverse . _1 . to ((: []) . C.ShelleyTx C.shelleyBasedEra . extractTx))

-- | Look up the transaction by its ID
getTxById :: (MonadMockchain era m) => C.TxId -> m (Maybe (C.Tx era))
getTxById txI = getMockChainState <&> view (txById . at txI)

-- | Set the slot number to the slot that contains the given POSIX time.
setPOSIXTime :: (MonadFail m, MonadMockchain era m) => PV1.POSIXTime -> m ()
setPOSIXTime tm =
  (posixTimeToSlotUnsafe <$> queryEraHistory <*> querySystemStart <*> pure tm) >>= either fail (setSlot . view _1)

{- | Change the clock so that the current slot time is within the given validity range.
This MAY move the clock backwards!
-}
setTimeToValidRange :: (MonadMockchain era m) => (C.TxValidityLowerBound era, C.TxValidityUpperBound era) -> m ()
setTimeToValidRange = \case
  (C.TxValidityLowerBound _ lowerSlot, _) -> setSlot lowerSlot
  (_, C.TxValidityUpperBound _ (Just upperSlot)) -> setSlot (pred upperSlot)
  _ -> pure ()

-- | Increase the slot number by 1.
nextSlot :: (MonadMockchain era m) => m ()
nextSlot = modifySlot (\s -> (succ s, ()))

{- Note [MonadUtxoQuery design]

The 'MonadUtxoQuery' class provides a lookup function that tells us the
unspent transaction outputs locked by one of a set of payment credentials.

The reason why this is not part of 'MonadBlockchain' is that the latter can
be implemented efficiently using only a running cardano-node, while 'MonadUtxoQuery'
requires a separate indexer. The classes are split to give callers more fine-grained
control over the capabilities they require.

-}

{- | A capability typeclass that provides methods for querying a chain indexer.
  See note [MonadUtxoQuery design].
-}
class (Monad m) => MonadUtxoQuery m where
  -- | Given a set of payment credentials, retrieve all UTxOs associated with
  -- those payment credentials according to the current indexed blockchain
  -- state. Each UTXO also possibly has the resolved datum (meaning that if we
  -- only have the datum hash, the implementation should try and resolve it to
  -- the actual datum).
  utxosByPaymentCredentials :: Set C.PaymentCredential -> m (UtxoSet C.CtxUTxO (Maybe C.HashableScriptData))
  default utxosByPaymentCredentials :: (MonadTrans t, m ~ t n, MonadUtxoQuery n) => Set C.PaymentCredential -> m (UtxoSet C.CtxUTxO (Maybe C.HashableScriptData))
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance (MonadUtxoQuery m) => MonadUtxoQuery (ResultT m)
instance (MonadUtxoQuery m) => MonadUtxoQuery (ExceptT e m)
instance (MonadUtxoQuery m) => MonadUtxoQuery (ReaderT e m)
instance (MonadUtxoQuery m) => MonadUtxoQuery (StrictState.StateT s m)
instance (MonadUtxoQuery m) => MonadUtxoQuery (LazyState.StateT s m)
instance (MonadUtxoQuery m) => MonadUtxoQuery (MonadBlockchainCardanoNodeT era m)
instance (MonadUtxoQuery m) => MonadUtxoQuery (MonadLogIgnoreT m)
instance (MonadUtxoQuery m) => MonadUtxoQuery (PropertyM m)

-- | Given a single payment credential, find the UTxOs with that credential
utxosByPaymentCredential :: (MonadUtxoQuery m) => C.PaymentCredential -> m (UtxoSet C.CtxUTxO (Maybe C.HashableScriptData))
utxosByPaymentCredential = utxosByPaymentCredentials . Set.singleton

{- Note [MonadDatumQuery design]

Initially only part of MonadMockchain, but now as a separate typeclass.
Useful for resolving datum hashes from Mockchain or a chain-indexer.
-}

class (Monad m) => MonadDatumQuery m where
  queryDatumFromHash :: C.Hash C.ScriptData -> m (Maybe C.HashableScriptData)

instance (MonadDatumQuery m) => MonadDatumQuery (ResultT m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance (MonadDatumQuery m) => MonadDatumQuery (ExceptT e m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance (MonadDatumQuery m) => MonadDatumQuery (ReaderT e m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance (MonadDatumQuery m) => MonadDatumQuery (StrictState.StateT s m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance (MonadDatumQuery m) => MonadDatumQuery (LazyState.StateT s m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance (MonadDatumQuery m) => MonadDatumQuery (MonadBlockchainCardanoNodeT era m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance (MonadDatumQuery m) => MonadDatumQuery (MonadLogIgnoreT m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance (MonadDatumQuery m) => MonadDatumQuery (PropertyM m) where
  queryDatumFromHash = lift . queryDatumFromHash

-- | 'MonadBlockchain' implementation that connects to a cardano node
newtype BlockchainException = BlockchainException String
  deriving stock (Show)
  deriving anyclass (Exception)

newtype MonadBlockchainCardanoNodeT era m a = MonadBlockchainCardanoNodeT {unMonadBlockchainCardanoNodeT :: ReaderT C.LocalNodeConnectInfo m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError e, MonadFail, PrimMonad)

runMonadBlockchainCardanoNodeT :: C.LocalNodeConnectInfo -> MonadBlockchainCardanoNodeT era m a -> m a
runMonadBlockchainCardanoNodeT info (MonadBlockchainCardanoNodeT action) = runReaderT action info

runQuery :: (MonadIO m) => C.QueryInMode a -> MonadBlockchainCardanoNodeT era m a
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
runQuery' qry =
  runQuery qry >>= \case
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
    C.LedgerProtocolParameters <$> runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra C.shelleyBasedEra C.QueryProtocolParameters))

  queryStakeAddresses creds nid =
    first (fmap C.lovelaceToQuantity) <$> runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra (C.shelleyBasedEra @era) (C.QueryStakeAddresses creds nid)))

  queryStakePools =
    runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra (C.shelleyBasedEra @era) C.QueryStakePools))

  queryStakeVoteDelegatees creds =
    runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra (C.shelleyBasedEra @era) (C.QueryStakeVoteDelegatees creds)))

  querySystemStart = runQuery C.QuerySystemStart

  queryEraHistory = runQuery C.QueryEraHistory

  querySlotNo = do
    (eraHistory@(C.EraHistory interpreter), systemStart) <- (,) <$> queryEraHistory <*> querySystemStart
    slotNo <-
      runQuery C.QueryChainPoint >>= \case
        C.ChainPointAtGenesis -> pure $ fromIntegral (0 :: Integer)
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
