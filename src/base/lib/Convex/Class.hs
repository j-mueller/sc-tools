{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Typeclass for blockchain operations
-}
module Convex.Class(
  MonadBlockchain(..),
  MonadMockchain(..),
  MonadBlockchainError(..),
  getSlot,
  setSlot,
  setPOSIXTime,
  nextSlot,
  setTimeToValidRange,
  getUtxo,
  setUtxo,

  -- * Implementation
  MonadBlockchainCardanoNodeT(..),
  runMonadBlockchainCardanoNodeT
) where

import qualified Cardano.Api                                       as C
import           Cardano.Api.Shelley                               (BabbageEra,
                                                                    CardanoMode,
                                                                    EraHistory (..),
                                                                    Hash,
                                                                    LocalNodeConnectInfo,
                                                                    NetworkId,
                                                                    PoolId,
                                                                    ProtocolParameters,
                                                                    ScriptData,
                                                                    SlotNo, Tx,
                                                                    TxId)
import qualified Cardano.Ledger.Core                               as Core
import           Cardano.Ledger.Shelley.API                        (UTxO)
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
import           Convex.Era                                        (ERA)
import qualified Convex.Era                                        as Ledger.Era
import           Convex.MonadLog                                   (MonadLog (..),
                                                                    MonadLogIgnoreT (..),
                                                                    logInfoS,
                                                                    logWarnS)
import           Convex.Utils                                      (posixTimeToSlotUnsafe,
                                                                    slotToUtcTime)
import           Data.Aeson                                        (FromJSON,
                                                                    ToJSON)
import           Data.Set                                          (Set)
import           Data.Time.Clock                                   (UTCTime)
import           GHC.Generics                                      (Generic)
import           Ouroboros.Consensus.HardFork.History              (interpretQuery,
                                                                    slotToSlotLength)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import qualified Plutus.V1.Ledger.Time                             as PV1

{-| Send transactions and resolve tx inputs.
-}
class Monad m => MonadBlockchain m where
  sendTx                  :: Tx BabbageEra -> m TxId -- ^ Submit a transaction to the network
  utxoByTxIn              :: Set C.TxIn -> m (C.UTxO C.BabbageEra) -- ^ Resolve tx inputs
  queryProtocolParameters :: m (ProtocolParameters, Core.PParams Ledger.Era.ERA) -- ^ Get the protocol parameters
  queryStakePools         :: m (Set PoolId) -- ^ Get the stake pools
  querySystemStart        :: m SystemStart
  queryEraHistory         :: m (EraHistory CardanoMode)
  querySlotNo             :: m (SlotNo, SlotLength, UTCTime)
                          -- ^ returns the current slot number, slot length and begin utc time for slot.
                          -- Slot 0 is returned when at genesis.
  networkId               :: m NetworkId -- ^ Get the network id

deriving newtype instance MonadBlockchain m => MonadBlockchain (MonadLogIgnoreT m)

instance MonadBlockchain m => MonadBlockchain (ResultT m) where
  sendTx = lift . sendTx
  utxoByTxIn = lift . utxoByTxIn
  queryProtocolParameters = lift queryProtocolParameters
  queryStakePools = lift queryStakePools
  querySystemStart = lift querySystemStart
  queryEraHistory = lift queryEraHistory
  querySlotNo = lift querySlotNo
  networkId = lift networkId

instance MonadBlockchain m => MonadBlockchain (ExceptT e m) where
  sendTx = lift . sendTx
  utxoByTxIn = lift . utxoByTxIn
  queryProtocolParameters = lift queryProtocolParameters
  queryStakePools = lift queryStakePools
  querySystemStart = lift querySystemStart
  queryEraHistory = lift queryEraHistory
  querySlotNo = lift querySlotNo
  networkId = lift networkId

instance MonadBlockchain m => MonadBlockchain (ReaderT e m) where
  sendTx = lift . sendTx
  utxoByTxIn = lift . utxoByTxIn
  queryProtocolParameters = lift queryProtocolParameters
  queryStakePools = lift queryStakePools
  querySystemStart = lift querySystemStart
  queryEraHistory = lift queryEraHistory
  querySlotNo = lift querySlotNo
  networkId = lift networkId

instance MonadBlockchain m => MonadBlockchain (StrictState.StateT e m) where
  sendTx = lift . sendTx
  utxoByTxIn = lift . utxoByTxIn
  queryProtocolParameters = lift queryProtocolParameters
  queryStakePools = lift queryStakePools
  querySystemStart = lift querySystemStart
  queryEraHistory = lift queryEraHistory
  querySlotNo = lift querySlotNo
  networkId = lift networkId

instance MonadBlockchain m => MonadBlockchain (LazyState.StateT e m) where
  sendTx = lift . sendTx
  utxoByTxIn = lift . utxoByTxIn
  queryProtocolParameters = lift queryProtocolParameters
  queryStakePools = lift queryStakePools
  querySystemStart = lift querySystemStart
  queryEraHistory = lift queryEraHistory
  querySlotNo = lift querySlotNo
  networkId = lift networkId

{-| Modify the mockchain internals
-}
class MonadBlockchain m => MonadMockchain m where
  modifySlot :: (SlotNo -> (SlotNo, a)) -> m a
  modifyUtxo :: (UTxO ERA -> (UTxO ERA, a)) -> m a

  {-| Look up the datum of a script hash, taking into account
  all datums that were part of transactions submitted with @sendTx@.
  -}
  resolveDatumHash :: Hash ScriptData -> m (Maybe ScriptData)

deriving newtype instance MonadMockchain m => MonadMockchain (MonadLogIgnoreT m)

instance MonadMockchain m => MonadMockchain (ResultT m) where
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo
  resolveDatumHash = lift . resolveDatumHash

instance MonadMockchain m => MonadMockchain (ReaderT e m) where
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo
  resolveDatumHash = lift . resolveDatumHash

instance MonadMockchain m => MonadMockchain (ExceptT e m) where
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo
  resolveDatumHash = lift . resolveDatumHash

instance MonadMockchain m => MonadMockchain (StrictState.StateT e m) where
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo
  resolveDatumHash = lift . resolveDatumHash

instance MonadMockchain m => MonadMockchain (LazyState.StateT e m) where
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo
  resolveDatumHash = lift . resolveDatumHash

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
  (C.TxValidityLowerBound _ lowerSlot, _) -> setSlot lowerSlot
  (_, C.TxValidityUpperBound _ upperSlot) -> setSlot (pred upperSlot)
  _                                       -> pure ()

{-| Increase the slot number by 1.
-}
nextSlot :: MonadMockchain m => m ()
nextSlot = modifySlot (\s -> (succ s, ()))

data MonadBlockchainError e =
  MonadBlockchainError e
  | FailWith String
  deriving stock (Eq, Functor, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Show e => Show (MonadBlockchainError e) where
  show (MonadBlockchainError e) = show e
  show (FailWith str)           = str

{-| 'MonadBlockchain' implementation that connects to a cardano node
-}
newtype MonadBlockchainCardanoNodeT e m a = MonadBlockchainCardanoNodeT { unMonadBlockchainCardanoNodeT :: ReaderT (LocalNodeConnectInfo CardanoMode) (ExceptT (MonadBlockchainError e) m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadError e (MonadBlockchainCardanoNodeT e m) where
  throwError = MonadBlockchainCardanoNodeT . throwError . MonadBlockchainError
  catchError (MonadBlockchainCardanoNodeT action) handler = MonadBlockchainCardanoNodeT $ catchError action (\case { MonadBlockchainError e -> unMonadBlockchainCardanoNodeT (handler e); e' -> throwError e' })

runMonadBlockchainCardanoNodeT :: LocalNodeConnectInfo CardanoMode -> MonadBlockchainCardanoNodeT e m a -> m (Either (MonadBlockchainError e) a)
runMonadBlockchainCardanoNodeT info (MonadBlockchainCardanoNodeT action) = runExceptT (runReaderT action info)

runQuery :: (MonadIO m, MonadLog m) => C.QueryInMode CardanoMode a -> MonadBlockchainCardanoNodeT e m a
runQuery qry = MonadBlockchainCardanoNodeT $ do
  info <- ask
  result <- liftIO (C.queryNodeLocalState info Nothing qry)
  case result of
    Left err -> do
      let msg = "runQuery: Query failed: " <> show err
      logWarnS msg
      throwError $ FailWith msg
    Right result' -> do
      pure result'

runQuery' :: (MonadIO m, MonadLog m, Show e1) => C.QueryInMode CardanoMode (Either e1 a) -> MonadBlockchainCardanoNodeT e2 m a
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
    result <- liftIO (C.submitTxToNodeLocal info (C.TxInMode tx C.BabbageEraInCardanoMode))
    -- TODO: Error should be reflected in return type of 'sendTx'
    case result of
      SubmitSuccess -> do
        logInfoS ("sendTx: Submitted " <> show txId)
        pure txId
      SubmitFail reason -> do
        let msg = "sendTx: Submission failed: " <> show reason
        logWarnS msg
        throwError $ FailWith msg

  utxoByTxIn txIns =
    runQuery' (C.QueryInEra C.BabbageEraInCardanoMode (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage (C.QueryUTxO (C.QueryUTxOByTxIn txIns))))

  queryProtocolParameters = do
    p <- runQuery' (C.QueryInEra C.BabbageEraInCardanoMode (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryProtocolParameters))
    return (p, C.toLedgerPParams C.ShelleyBasedEraBabbage p)

  queryStakePools =
    runQuery' (C.QueryInEra C.BabbageEraInCardanoMode (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryStakePools))

  querySystemStart = runQuery C.QuerySystemStart

  queryEraHistory = runQuery (C.QueryEraHistory C.CardanoModeIsMultiEra)

  querySlotNo = do
    (eraHistory@(EraHistory _ interpreter), systemStart) <- (,) <$> queryEraHistory <*> querySystemStart
    slotNo <- runQuery (C.QueryChainPoint C.CardanoMode) >>= \case
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
