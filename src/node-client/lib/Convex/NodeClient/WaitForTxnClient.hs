{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE UndecidableInstances #-}
{-| A node client that waits for a transaction to appear on the chain
-}
module Convex.NodeClient.WaitForTxnClient(
  runWaitForTxn,
  MonadBlockchainWaitingT(..),
  runMonadBlockchainWaitingT
) where

import           Cardano.Api                (BlockInMode, CardanoMode,
                                             ChainPoint, Env,
                                             LocalNodeConnectInfo, TxId, LedgerState)
import qualified Cardano.Api                as C
import           Control.Concurrent         (forkIO)
import           Control.Concurrent.STM     (TMVar, atomically, newEmptyTMVar,
                                             putTMVar, takeTMVar)
import           Control.Monad.Except       (MonadError (..))
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader       (MonadTrans, ReaderT (..), ask,
                                             lift)
import           Convex.Class               (MonadBlockchain (..))
import           Convex.MonadLog            (MonadLog (..), logInfoS)
import           Convex.NodeClient.Fold     (CatchingUp (..), foldClient, LedgerStateArgs (..), LedgerStateUpdate, LedgerStateMode (..))
import           Convex.NodeClient.Resuming (resumingClient)
import           Convex.NodeClient.Types    (PipelinedLedgerStateClient,
                                             protocols)
import qualified Convex.NodeQueries         as NodeQueries

{-| Start a 'waitForTxnClient' in a separate thread. Returns a TMVar that will contain the block that has the given
transaction.
-}
runWaitForTxn ::
    LocalNodeConnectInfo CardanoMode
  -> Env
  -> TxId
  -> IO (TMVar (BlockInMode CardanoMode))
runWaitForTxn connectInfo env txi = do
  tip' <- NodeQueries.queryTip connectInfo
  tmv <- atomically newEmptyTMVar
  _ <- forkIO $ C.connectToLocalNode connectInfo (protocols $ waitForTxnClient tmv tip' txi env)
  pure tmv

{-| Scan the new blocks until the transaction appears
-}
waitForTxnClient ::
     TMVar (BlockInMode CardanoMode)
  -> ChainPoint
  -> TxId
  -> Env
  -> PipelinedLedgerStateClient
waitForTxnClient tmv cp txId env =
  resumingClient [cp] $ \_ ->
    foldClient () NoLedgerStateArgs env (applyBlock tmv txId)

applyBlock :: TMVar (BlockInMode CardanoMode) -> TxId -> CatchingUp -> () -> LedgerStateUpdate 'NoLedgerState -> BlockInMode CardanoMode -> IO (Maybe ())
applyBlock tmv txi _ () _ block = do
  case block of
    C.BlockInMode blck C.BabbageEraInCardanoMode ->
      if checkTxIds txi blck
        then do
          liftIO $ atomically $ putTMVar tmv block
          pure Nothing
        else pure (Just ())
    _ -> pure (Just ())

checkTxIds :: TxId -> C.Block C.BabbageEra -> Bool
checkTxIds txi ((C.Block _ txns)) = any (checkTxId txi) txns

checkTxId :: TxId -> C.Tx C.BabbageEra -> Bool
checkTxId txi tx = txi == C.getTxId (C.getTxBody tx)

newtype MonadBlockchainWaitingT m a = MonadBlockchainWaitingT{unMonadBlockchainWaitingT :: ReaderT (LocalNodeConnectInfo CardanoMode, LedgerState, Env) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail)

runMonadBlockchainWaitingT :: LocalNodeConnectInfo CardanoMode -> LedgerState -> Env -> MonadBlockchainWaitingT m a -> m a
runMonadBlockchainWaitingT connectInfo initialLedgerState env (MonadBlockchainWaitingT action) = runReaderT action (connectInfo, initialLedgerState, env)

instance MonadError e m => MonadError e (MonadBlockchainWaitingT m) where
  throwError = lift . throwError
  catchError m _ = m

instance MonadTrans MonadBlockchainWaitingT where
  lift = MonadBlockchainWaitingT . lift

instance (MonadLog m) => MonadLog (MonadBlockchainWaitingT m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'
  logDebug' = lift . logDebug'

instance (MonadIO m, MonadBlockchain m, MonadLog m) => MonadBlockchain (MonadBlockchainWaitingT m) where
  sendTx tx = MonadBlockchainWaitingT $ do
    let txi = C.getTxId (C.getTxBody tx)
    (info, _ledgerState0, env) <- ask
    tmv <- liftIO (runWaitForTxn info env txi)
    k <- sendTx tx
    logInfoS $ "MonadBlockchainWaitingT.sendTx: Waiting for " <> show txi <> " to appear on the chain"
    _ <- liftIO (atomically $ takeTMVar tmv)
    logInfoS $ "MonadBlockchainWaitingT.sendTx: Found " <> show txi
    pure k

  utxoByTxIn txIns = MonadBlockchainWaitingT (utxoByTxIn txIns)

  queryProtocolParameters = MonadBlockchainWaitingT queryProtocolParameters

  queryStakePools = MonadBlockchainWaitingT queryStakePools

  querySystemStart = MonadBlockchainWaitingT querySystemStart

  queryEraHistory = MonadBlockchainWaitingT queryEraHistory

  querySlotNo = MonadBlockchainWaitingT querySlotNo

  networkId = MonadBlockchainWaitingT networkId
