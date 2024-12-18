{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Convex.NodeClient.Types (
  PipelinedLedgerStateClient (..),
  runNodeClient,
  protocols,

  -- * Sync points
  ChainPoint (..),
  fromChainTip,
) where

import Cardano.Api (
  BlockInMode (..),
  BlockNo (..),
  ChainPoint (..),
  ChainSyncClientPipelined,
  ChainTip (..),
  Env (..),
  InitialLedgerStateError,
  LocalChainSyncClient (LocalChainSyncClientPipelined),
  LocalNodeClientProtocols (..),
  LocalNodeClientProtocolsInMode,
  LocalNodeConnectInfo (..),
  connectToLocalNode,
 )
import Cardano.Slotting.Slot (WithOrigin (At, Origin))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Convex.NodeQueries (loadConnectInfo)
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined qualified as CSP

newtype PipelinedLedgerStateClient
  = PipelinedLedgerStateClient
  { getPipelinedLedgerStateClient :: ChainSyncClientPipelined BlockInMode ChainPoint ChainTip IO ()
  }

runNodeClient
  :: FilePath
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  -> FilePath
  -- ^ Path to local cardano-node socket. This is the path specified by the @--socket-path@ command line option when running the node.
  -> (LocalNodeConnectInfo -> Env -> IO PipelinedLedgerStateClient)
  -- ^ Client
  -> ExceptT InitialLedgerStateError IO ()
  -- ^ Final state
runNodeClient nodeConfigFilePath socketPath client = do
  (connectInfo, env) <- loadConnectInfo nodeConfigFilePath socketPath
  c <- liftIO (client connectInfo env)
  lift $ connectToLocalNode connectInfo (protocols c)

protocols :: PipelinedLedgerStateClient -> LocalNodeClientProtocolsInMode
protocols client =
  LocalNodeClientProtocols
    { localChainSyncClient = LocalChainSyncClientPipelined (chainSyncClient client)
    , localTxSubmissionClient = Nothing
    , localStateQueryClient = Nothing
    , localTxMonitoringClient = Nothing
    }

chainSyncClient :: PipelinedLedgerStateClient -> ChainSyncClientPipelined BlockInMode ChainPoint ChainTip IO ()
chainSyncClient PipelinedLedgerStateClient{getPipelinedLedgerStateClient} =
  CSP.ChainSyncClientPipelined $
    let CSP.ChainSyncClientPipelined{CSP.runChainSyncClientPipelined} = getPipelinedLedgerStateClient
     in runChainSyncClientPipelined

fromChainTip :: ChainTip -> WithOrigin BlockNo
fromChainTip ct = case ct of
  ChainTipAtGenesis -> Origin
  ChainTip _ _ bno -> At bno
