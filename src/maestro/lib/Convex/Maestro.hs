{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- | Maestro-backed implementation of MonadBlockchain
module Convex.Maestro (
  MaestroT (..),
  evalMaestroT,
  runMaestroT,
) where

import Cardano.Api qualified as C
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Convex.Class (MonadBlockchain (..))
import Convex.Maestro.MonadBlockchain qualified as MonadBlockchain
import Maestro.Client.Env qualified as Env

-- | Monad transformer carrying a Maestro client environment
newtype MaestroT m a = MaestroT {unMaestroT :: ReaderT (Env.MaestroEnv 'Env.V1) m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Run with the provided Maestro environment, discarding it afterwards
evalMaestroT :: Env.MaestroEnv 'Env.V1 -> MaestroT m a -> m a
evalMaestroT env = (`runReaderT` env) . unMaestroT

-- | Alias to 'evalMaestroT' for symmetry with other backends
runMaestroT :: Env.MaestroEnv 'Env.V1 -> MaestroT m a -> m a
runMaestroT = evalMaestroT

instance MonadTrans MaestroT where
  lift = MaestroT . lift

instance (MonadIO m) => MonadBlockchain C.ConwayEra (MaestroT m) where
  sendTx tx = MaestroT $ do
    env <- ask
    liftIO (MonadBlockchain.sendTxMaestro False env tx)

  utxoByTxIn txIns = MaestroT $ do
    env <- ask
    liftIO (MonadBlockchain.getUtxoByTxIn env txIns)

  queryProtocolParameters = MaestroT $ do
    env <- ask
    liftIO (MonadBlockchain.getProtocolParams env)

  queryStakeAddresses creds _nid = MaestroT $ do
    env <- ask
    liftIO (MonadBlockchain.getStakeAddresses env creds)

  queryStakePools = MaestroT $ do
    env <- ask
    liftIO (MonadBlockchain.getStakePools env)

  queryStakeVoteDelegatees creds = MaestroT $ do
    env <- ask
    liftIO (MonadBlockchain.getStakeVoteDelegatees env creds)

  querySystemStart = MaestroT $ do
    env <- ask
    liftIO (MonadBlockchain.getSystemStart env)

  queryEraHistory = MaestroT $ do
    env <- ask
    liftIO (MonadBlockchain.getEraHistory env)

  querySlotNo = MaestroT $ do
    env <- ask
    liftIO (MonadBlockchain.getSlotNo env)

  queryNetworkId = MaestroT $ do
    env <- ask
    liftIO (MonadBlockchain.getNetworkId env)
