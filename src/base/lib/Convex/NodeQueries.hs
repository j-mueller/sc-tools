{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-| Conveniences for working with a local @cardano-node@
-}
module Convex.NodeQueries(
  loadConnectInfo,
  queryEraHistory,
  querySystemStart,
  queryLocalState,
  queryTip,
  queryProtocolParameters,
  queryStakePools,
  queryStakeAddresses
) where

import           Cardano.Api                                        (ChainPoint,
                                                                     ConsensusModeParams (..),
                                                                     Env (..),
                                                                     EpochSlots (..),
                                                                     EraHistory,
                                                                     InitialLedgerStateError,
                                                                     LocalNodeConnectInfo (..),
                                                                     Lovelace,
                                                                     NetworkId (Mainnet, Testnet),
                                                                     NetworkMagic (..),
                                                                     SystemStart,
                                                                     envSecurityParam)
import qualified Cardano.Api                                        as CAPI
import           Cardano.Api.Shelley                                (PoolId,
                                                                     StakeAddress,
                                                                     StakeCredential)
import qualified Cardano.Chain.Genesis
import           Cardano.Crypto                                     (RequiresNetworkMagic (..),
                                                                     getProtocolMagic)
import           Cardano.Ledger.Core                                (PParams)
import           Control.Monad.Except                               (MonadError,
                                                                     throwError)
import           Control.Monad.IO.Class                             (MonadIO (..))
import           Control.Monad.Trans.Except                         (runExceptT)
import           Data.Map                                           (Map)
import           Data.Set                                           (Set)
import           Data.SOP.Strict                                    (NP ((:*)))
import qualified Ouroboros.Consensus.Cardano.CanHardFork            as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator            as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics     as HFC
import           Ouroboros.Consensus.Shelley.Eras                   (StandardBabbage)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type    as T

{-| Load the node config file and create 'LocalNodeConnectInfo' and 'Env' values that can be used to talk to the node.
-}
loadConnectInfo ::
  (MonadError InitialLedgerStateError m, MonadIO m)
  => FilePath
  -- ^ Node config file (JSON)
  -> FilePath
  -- ^ Node socket
  -> m (LocalNodeConnectInfo, Env)
loadConnectInfo nodeConfigFilePath socketPath = do
  (env, _) <- liftIO (runExceptT (CAPI.initialLedgerState (CAPI.File nodeConfigFilePath))) >>= either throwError pure

  -- Derive the NetworkId as described in network-magic.md from the
  -- cardano-ledger-specs repo.
  let byronConfig
        = (\(Consensus.WrapPartialLedgerConfig (Consensus.ByronPartialLedgerConfig bc _) :* _) -> bc)
        . HFC.getPerEraLedgerConfig
        . HFC.hardForkLedgerConfigPerEra
        $ envLedgerConfig env

      networkMagic
        = getProtocolMagic
        $ Cardano.Chain.Genesis.configProtocolMagic byronConfig

      networkId = case Cardano.Chain.Genesis.configReqNetMagic byronConfig of
        RequiresNoMagic -> Mainnet
        RequiresMagic   -> Testnet (NetworkMagic networkMagic)

      cardanoModeParams = CardanoModeParams . EpochSlots $ 10 * envSecurityParam env

  -- Connect to the node.
  let connectInfo :: LocalNodeConnectInfo
      connectInfo =
          LocalNodeConnectInfo {
            localConsensusModeParams = cardanoModeParams,
            localNodeNetworkId       = networkId,
            localNodeSocketPath      = CAPI.File socketPath
          }
  pure (connectInfo, env)

-- | Get the system start from the local cardano node
querySystemStart :: LocalNodeConnectInfo -> IO SystemStart
querySystemStart = queryLocalState CAPI.QuerySystemStart

-- | Get the era history from the local cardano node
queryEraHistory :: LocalNodeConnectInfo -> IO EraHistory
queryEraHistory = queryLocalState CAPI.QueryEraHistory

-- | Get the tip from the local cardano node
queryTip :: LocalNodeConnectInfo -> IO ChainPoint
queryTip = queryLocalState CAPI.QueryChainPoint

queryStakePools :: LocalNodeConnectInfo -> IO (Set PoolId)
queryStakePools connectInfo = do
  result <- queryLocalState
              (CAPI.QueryInEra (CAPI.QueryInShelleyBasedEra CAPI.ShelleyBasedEraBabbage CAPI.QueryStakePools))
              connectInfo
  case result of
    Left err -> do
      fail ("queryStakePools: failed with: " <> show err)
    Right k -> pure k

queryStakeAddresses :: LocalNodeConnectInfo -> Set StakeCredential -> NetworkId -> IO (Map StakeAddress Lovelace, Map StakeAddress PoolId)
queryStakeAddresses info creds nid = do
  result <- queryLocalState
              (CAPI.QueryInEra (CAPI.QueryInShelleyBasedEra CAPI.ShelleyBasedEraBabbage (CAPI.QueryStakeAddresses creds nid)))
              info
  case result of
    Left err -> do
      fail ("queryStakeAddresses: failed with: " <> show err)
    Right k -> pure k

-- | Run a local state query on the local cardano node, using the volatile tip
queryLocalState :: CAPI.QueryInMode b -> LocalNodeConnectInfo -> IO b
queryLocalState query connectInfo = do
  CAPI.queryNodeLocalState connectInfo T.VolatileTip query >>= \case
    Left err -> do
      fail ("queryLocalState: Failed with " <> show err)
    Right result -> pure result

-- | Get the protocol parameters from the local cardano node
queryProtocolParameters :: LocalNodeConnectInfo -> IO (PParams StandardBabbage)
queryProtocolParameters connectInfo = do
  result <- queryLocalState (CAPI.QueryInEra (CAPI.QueryInShelleyBasedEra CAPI.ShelleyBasedEraBabbage CAPI.QueryProtocolParameters)) connectInfo
  case result of
    Left err -> do
      fail ("queryProtocolParameters: failed with: " <> show err)
    Right x -> pure x
