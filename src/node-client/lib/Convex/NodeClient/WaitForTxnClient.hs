{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A node client that waits for a transaction to appear on the chain
module Convex.NodeClient.WaitForTxnClient (
  runWaitForTxn,
  MonadBlockchainWaitingT (..),
  runMonadBlockchainWaitingT,
) where

import Cardano.Api (
  BlockInMode,
  ChainPoint,
  Env,
  LocalNodeConnectInfo,
  TxId,
 )
import Cardano.Api qualified as C
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (
  TMVar,
  atomically,
  newEmptyTMVarIO,
  putTMVar,
  takeTMVar,
 )
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader (
  MonadTrans,
  ReaderT (..),
  ask,
  lift,
 )
import Convex.Class (
  MonadBlockchain (..),
  MonadDatumQuery,
  MonadUtxoQuery,
 )
import Convex.MonadLog (MonadLog (..), logInfoS)
import Convex.NodeClient.Fold (
  CatchingUp (..),
  LedgerStateArgs (..),
  LedgerStateMode (..),
  LedgerStateUpdate,
  foldClient,
 )
import Convex.NodeClient.Resuming (resumingClient)
import Convex.NodeClient.Types (
  PipelinedLedgerStateClient,
  protocols,
 )
import Convex.NodeQueries qualified as NodeQueries

{- | Start a 'waitForTxnClient' in a separate thread. Returns a TMVar that will contain the block that has the given
transaction.
-}
runWaitForTxn :: LocalNodeConnectInfo -> Env -> TxId -> ChainPoint -> IO (TMVar BlockInMode)
runWaitForTxn connectInfo env txi tip' = do
  tmv <- newEmptyTMVarIO
  _ <- forkIO $ C.connectToLocalNode connectInfo (protocols $ waitForTxnClient tmv tip' txi env)
  pure tmv

-- | Scan the new blocks until the transaction appears
waitForTxnClient :: TMVar BlockInMode -> ChainPoint -> TxId -> Env -> PipelinedLedgerStateClient
waitForTxnClient tmv cp txId env =
  resumingClient [cp] $ \_ ->
    foldClient () NoLedgerStateArgs env (applyBlock tmv txId)

applyBlock :: TMVar BlockInMode -> TxId -> CatchingUp -> () -> LedgerStateUpdate 'NoLedgerState -> BlockInMode -> IO (Maybe ())
applyBlock tmv txi _ () _ block = do
  case block of
    C.BlockInMode _era blck ->
      if checkTxIds txi blck
        then do
          liftIO $ atomically $ putTMVar tmv block
          pure Nothing
        else pure (Just ())

checkTxIds :: TxId -> C.Block era -> Bool
checkTxIds txi ((C.Block _ txns)) = any (checkTxId txi) txns

checkTxId :: TxId -> C.Tx era -> Bool
checkTxId txi tx = txi == C.getTxId (C.getTxBody tx)

newtype MonadBlockchainWaitingT era m a = MonadBlockchainWaitingT {unMonadBlockchainWaitingT :: ReaderT (LocalNodeConnectInfo, Env) m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadUtxoQuery, MonadDatumQuery, MonadError e, MonadLog, PrimMonad)

runMonadBlockchainWaitingT :: LocalNodeConnectInfo -> Env -> MonadBlockchainWaitingT era m a -> m a
runMonadBlockchainWaitingT connectInfo env (MonadBlockchainWaitingT action) = runReaderT action (connectInfo, env)

instance MonadTrans (MonadBlockchainWaitingT era) where
  lift = MonadBlockchainWaitingT . lift

instance (MonadIO m, MonadBlockchain era m, MonadLog m) => MonadBlockchain era (MonadBlockchainWaitingT era m) where
  sendTx tx = MonadBlockchainWaitingT $ do
    let txi = C.getTxId (C.getTxBody tx)
    (info, env) <- ask
    oldTip <- liftIO (NodeQueries.queryChainPoint info)
    k <- sendTx tx
    case k of
      Left e -> pure $ Left e
      Right x -> do
        logInfoS $ "MonadBlockchainWaitingT.sendTx: Waiting for " <> show txi <> " to appear on the chain"
        _ <- liftIO (runWaitForTxn info env txi oldTip >>= atomically . takeTMVar)
        logInfoS $ "MonadBlockchainWaitingT.sendTx: Found " <> show txi
        pure $ Right x
