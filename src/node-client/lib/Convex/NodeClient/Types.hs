{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Convex.NodeClient.Types(
  PipelinedLedgerStateClient(..),
  ClientBlock,
  runNodeClient,
  loadConnectInfo,
  -- * Sync points
  ChainPoint(..),
  fromChainTip
  ) where

import           Cardano.Api                                          (BlockInMode (..),
                                                                       BlockNo (..),
                                                                       CardanoMode,
                                                                       ChainPoint (..),
                                                                       ChainSyncClientPipelined,
                                                                       ChainTip (..),
                                                                       ConsensusModeParams (..),
                                                                       Env (..),
                                                                       EpochSlots (..),
                                                                       InitialLedgerStateError,
                                                                       LocalChainSyncClient (LocalChainSyncClientPipelined),
                                                                       LocalNodeClientProtocols (..),
                                                                       LocalNodeClientProtocolsInMode,
                                                                       LocalNodeConnectInfo (..),
                                                                       NetworkId (Mainnet, Testnet),
                                                                       NetworkMagic (..),
                                                                       connectToLocalNode,
                                                                       envSecurityParam)
import qualified Cardano.Api                                          as CAPI
import qualified Cardano.Chain.Genesis
import           Cardano.Crypto                                       (ProtocolMagicId (unProtocolMagicId),
                                                                       RequiresNetworkMagic (..))
import           Cardano.Slotting.Slot                                (WithOrigin (At, Origin))
import           Control.Monad.Except                                 (MonadError,
                                                                       throwError)
import           Control.Monad.IO.Class                               (MonadIO (..))
import           Control.Monad.Trans.Class                            (lift)
import           Control.Monad.Trans.Except                           (ExceptT,
                                                                       runExceptT)
import           Data.SOP.Strict                                      (NP ((:*)))
import qualified Ouroboros.Consensus.Cardano.CanHardFork              as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator              as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras   as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics       as HFC
import qualified Ouroboros.Network.Protocol.ChainSync.ClientPipelined as CSP

{-|
-}
newtype PipelinedLedgerStateClient =
  PipelinedLedgerStateClient
    { getPipelinedLedgerStateClient :: ChainSyncClientPipelined (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
    }

{-| Load the node config file and create 'LocalNodeConnectInfo' and 'Env' values that can be used to talk to the node.
-}
loadConnectInfo :: (MonadError InitialLedgerStateError m, MonadIO m) => FilePath -> FilePath -> m (LocalNodeConnectInfo CardanoMode, Env)
loadConnectInfo nodeConfigFilePath socketPath = do
  (env, _) <- liftIO (runExceptT (CAPI.initialLedgerState nodeConfigFilePath)) >>= either throwError pure

  -- Derive the NetworkId as described in network-magic.md from the
  -- cardano-ledger-specs repo.
  let byronConfig
        = (\(Consensus.WrapPartialLedgerConfig (Consensus.ByronPartialLedgerConfig bc _) :* _) -> bc)
        . HFC.getPerEraLedgerConfig
        . HFC.hardForkLedgerConfigPerEra
        $ envLedgerConfig env

      networkMagic
        = NetworkMagic
        $ unProtocolMagicId
        $ Cardano.Chain.Genesis.gdProtocolMagicId
        $ Cardano.Chain.Genesis.configGenesisData byronConfig

      networkId = case Cardano.Chain.Genesis.configReqNetMagic byronConfig of
        RequiresNoMagic -> Mainnet
        RequiresMagic   -> Testnet networkMagic

      cardanoModeParams = CardanoModeParams . EpochSlots $ 10 * envSecurityParam env

  -- Connect to the node.
  let connectInfo :: LocalNodeConnectInfo CardanoMode
      connectInfo =
          LocalNodeConnectInfo {
            localConsensusModeParams = cardanoModeParams,
            localNodeNetworkId       = networkId,
            localNodeSocketPath      = socketPath
          }
  pure (connectInfo, env)

runNodeClient ::
  FilePath
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  -> FilePath
  -- ^ Path to local cardano-node socket. This is the path specified by the @--socket-path@ command line option when running the node.
  -> (LocalNodeConnectInfo CardanoMode -> Env -> IO PipelinedLedgerStateClient)
  -- ^ Client
  -> ExceptT InitialLedgerStateError IO ()
  -- ^ The final state
runNodeClient nodeConfigFilePath socketPath client = do
  (connectInfo, env) <- loadConnectInfo nodeConfigFilePath socketPath
  c <- liftIO (client connectInfo env)
  lift $ connectToLocalNode connectInfo (protocols c)

protocols :: PipelinedLedgerStateClient -> LocalNodeClientProtocolsInMode CardanoMode
protocols client =
  LocalNodeClientProtocols {
    localChainSyncClient    = LocalChainSyncClientPipelined (chainSyncClient client),
    localTxSubmissionClient = Nothing,
    localStateQueryClient   = Nothing,
    localTxMonitoringClient = Nothing
  }

chainSyncClient :: PipelinedLedgerStateClient -> ChainSyncClientPipelined (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
chainSyncClient PipelinedLedgerStateClient{getPipelinedLedgerStateClient} = CSP.ChainSyncClientPipelined $
  let CSP.ChainSyncClientPipelined{CSP.runChainSyncClientPipelined} = getPipelinedLedgerStateClient
  in runChainSyncClientPipelined

type ClientBlock = BlockInMode CardanoMode

fromChainTip :: ChainTip -> WithOrigin BlockNo
fromChainTip ct = case ct of
  ChainTipAtGenesis -> Origin
  ChainTip _ _ bno  -> At bno
