{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Typeclass for blockchain operations
-}
module Convex.Class(
  MonadBlockchain(..),
  MonadMockchain(..),
  getSlot,
  setSlot,
  setPOSIXTime,
  nextSlot,
  setTimeToValidRange,

  -- * Implementation
  MonadBlockchainCardanoNodeT(..),
  runMonadBlockchainCardanoNodeT
) where

import qualified Cardano.Api                                       as C
import           Cardano.Api.Shelley                               (BabbageEra,
                                                                    CardanoMode,
                                                                    EraHistory,
                                                                    LocalNodeConnectInfo,
                                                                    NetworkId,
                                                                    PoolId,
                                                                    ProtocolParameters,
                                                                    SlotNo, Tx,
                                                                    TxId)
import           Cardano.Ledger.Shelley.API                        (UTxO)
import           Cardano.Slotting.Time                             (SystemStart)
import           Control.Lens                                      (_1, view)
import           Control.Monad.Except                              (MonadError)
import           Control.Monad.IO.Class                            (MonadIO (..))
import           Control.Monad.Reader                              (MonadTrans,
                                                                    ReaderT (..),
                                                                    ask, asks,
                                                                    lift)
import           Control.Monad.Trans.Except                        (ExceptT)
import           Control.Monad.Trans.Except.Result                 (ResultT)
import           Convex.Era                                        (ERA)
import           Convex.MonadLog                                   (MonadLog (..),
                                                                    logInfoS,
                                                                    logWarnS)
import           Convex.Utils                                      (posixTimeToSlotUnsafe)
import           Data.Set                                          (Set)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import qualified Plutus.V1.Ledger.Time                             as PV1

{-| Send transactions and resolve tx inputs.
-}
class Monad m => MonadBlockchain m where
  sendTx                  :: Tx BabbageEra -> m TxId -- ^ Submit a transaction to the network
  utxoByTxIn              :: Set C.TxIn -> m (C.UTxO C.BabbageEra) -- ^ Resolve tx inputs
  queryProtocolParameters :: m ProtocolParameters -- ^ Get the protocol parameters
  queryStakePools         :: m (Set PoolId) -- ^ Get the stake pools
  querySystemStart        :: m SystemStart
  queryEraHistory         :: m (EraHistory CardanoMode)
  networkId               :: m NetworkId -- ^ Get the network id

instance MonadBlockchain m => MonadBlockchain (ResultT m) where
  sendTx = lift . sendTx
  utxoByTxIn = lift . utxoByTxIn
  queryProtocolParameters = lift queryProtocolParameters
  queryStakePools = lift queryStakePools
  querySystemStart = lift querySystemStart
  queryEraHistory = lift queryEraHistory
  networkId = lift networkId

instance MonadBlockchain m => MonadBlockchain (ExceptT e m) where
  sendTx = lift . sendTx
  utxoByTxIn = lift . utxoByTxIn
  queryProtocolParameters = lift queryProtocolParameters
  queryStakePools = lift queryStakePools
  querySystemStart = lift querySystemStart
  queryEraHistory = lift queryEraHistory
  networkId = lift networkId

{-| Modify the mockchain internals
-}
class MonadBlockchain m => MonadMockchain m where
  modifySlot :: (SlotNo -> (SlotNo, a)) -> m a
  modifyUtxo :: (UTxO ERA -> (UTxO ERA, a)) -> m a

instance MonadMockchain m => MonadMockchain (ResultT m) where
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo

instance MonadMockchain m => MonadMockchain (ExceptT e m) where
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

{-| 'MonadBlockchain' implementation that connects to a cardano node
-}
newtype MonadBlockchainCardanoNodeT m a = MonadBlockchainCardanoNodeT { unMonadBlockchainCardanoNodeT :: ReaderT (LocalNodeConnectInfo CardanoMode) m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

deriving newtype instance MonadError e m => MonadError e (MonadBlockchainCardanoNodeT m)

runMonadBlockchainCardanoNodeT :: LocalNodeConnectInfo CardanoMode -> MonadBlockchainCardanoNodeT m a -> m a
runMonadBlockchainCardanoNodeT info (MonadBlockchainCardanoNodeT action) = runReaderT action info

runQuery :: (MonadIO m, MonadLog m, MonadFail m) => C.QueryInMode CardanoMode a -> MonadBlockchainCardanoNodeT m a
runQuery qry = MonadBlockchainCardanoNodeT $ do
  info <- ask
  result <- liftIO (C.queryNodeLocalState info Nothing qry)
  case result of
    Left err -> do
      let msg = "runQuery: Query failed: " <> show err
      logWarnS msg
      fail msg
    Right result' -> do
      pure result'

runQuery' :: (MonadIO m, MonadLog m, Show err, MonadFail m) => C.QueryInMode CardanoMode (Either err a) -> MonadBlockchainCardanoNodeT m a
runQuery' qry = runQuery qry >>= \case
  Left err -> MonadBlockchainCardanoNodeT $ do
    let msg = "runQuery': Era mismatch: " <> show err
    logWarnS msg
    fail msg
  Right result' -> MonadBlockchainCardanoNodeT $ do
    logInfoS "runQuery': Success"
    pure result'

instance (MonadFail m, MonadLog m, MonadIO m) => MonadBlockchain (MonadBlockchainCardanoNodeT m) where
  sendTx tx = MonadBlockchainCardanoNodeT $ do
    let txId = C.getTxId (C.getTxBody tx)
    info <- ask
    result <- liftIO (C.submitTxToNodeLocal info (C.TxInMode tx C.BabbageEraInCardanoMode))
    -- TODO: Error should be reflected in return type of 'sendTx'
    case result of
      SubmitSuccess -> do
        logInfoS ("sendTx: Submitted " <> show txId)
      SubmitFail reason -> do
        logWarnS $ "sendTx: Submission failed: " <> show reason
    pure txId

  utxoByTxIn txIns =
    runQuery' (C.QueryInEra C.BabbageEraInCardanoMode (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage (C.QueryUTxO (C.QueryUTxOByTxIn txIns))))

  queryProtocolParameters =
    runQuery' (C.QueryInEra C.BabbageEraInCardanoMode (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryProtocolParameters))

  queryStakePools =
    runQuery' (C.QueryInEra C.BabbageEraInCardanoMode (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryStakePools))

  querySystemStart = runQuery C.QuerySystemStart

  queryEraHistory = runQuery (C.QueryEraHistory C.CardanoModeIsMultiEra)

  networkId = MonadBlockchainCardanoNodeT (asks C.localNodeNetworkId)

instance MonadLog m => MonadLog (MonadBlockchainCardanoNodeT m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'

instance (MonadLog m, MonadFail m) => MonadFail (MonadBlockchainCardanoNodeT m) where
  fail s = MonadBlockchainCardanoNodeT $ do
    logWarnS s
    lift (fail s)
