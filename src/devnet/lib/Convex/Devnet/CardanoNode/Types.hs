{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
module Convex.Devnet.CardanoNode.Types (
  Port,
  PortsConfig (..),
  defaultPortsConfig,
  ConfigFilePath(..),
  RunningNode (..),
  RunningStakePoolNode (..),
  StakePoolNodeParams (..),
  defaultStakePoolNodeParams,
  -- * Node arguments
  CardanoNodeArgs(..),
  defaultCardanoNodeArgs,
  cardanoNodeProcess,
  -- * Genesis config changes
  GenesisConfigChanges (..),
  forkIntoConwayInEpoch,
  allowLargeTransactions,
  setEpochLength
) where

import           Cardano.Api                      (Env, LocalNodeConnectInfo,
                                                   NetworkId)
import           Cardano.Api.Shelley              (KesKey, Key (..),
                                                   OperationalCertificateIssueCounter,
                                                   ShelleyGenesis, StakeKey,
                                                   StakePoolKey, VrfKey)
import           Cardano.Ledger.BaseTypes         (EpochSize)
import qualified Cardano.Ledger.Core              as Core
import           Cardano.Ledger.Shelley.API       (Coin)
import           Cardano.Ledger.Shelley.Genesis   (ShelleyGenesis (..))
import           Control.Lens                     (over, set)
import           Data.Aeson                       (FromJSON, ToJSON)
import qualified Data.Aeson                       as Aeson
import           Data.Aeson.Lens                  (atKey)
import           Data.Ratio                       ((%))
import           GHC.Generics                     (Generic)
import           Numeric.Natural                  (Natural)
import           Ouroboros.Consensus.Shelley.Eras (ShelleyEra, StandardCrypto)
import           System.FilePath                  ((</>))
import           System.Process                   (CreateProcess (..), proc)

type Port = Int

-- | Configuration of ports from the perspective of a peer in the context of a
-- fully sockected topology.
data PortsConfig = PortsConfig
  { -- | Our node TCP port.
    ours  :: Port
  , -- | Other peers TCP ports.
    peers :: [Port]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default value for 'PortsConfig'
defaultPortsConfig :: PortsConfig
defaultPortsConfig = PortsConfig 3001 []

newtype ConfigFilePath = ConfigFilePath FilePath
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

{-| Describes a running pool node
-}
data RunningNode = RunningNode
  { rnNodeSocket         :: FilePath -- ^ Cardano node socket
  , rnNetworkId          :: NetworkId -- ^ Network ID used by the cardano node
  , rnNodeConfigFilePath :: ConfigFilePath -- ^ Directory of the config files
  , rnNodeArgs           :: CardanoNodeArgs
  , rnNodeConfigFile     :: FilePath -- ^ Cardano node config file (JSON)
  , rnConnectInfo        :: (LocalNodeConnectInfo, Env) -- ^ Connection info for node queries
  }

{-| Describes a running stake pool node
-}
data RunningStakePoolNode = RunningStakePoolNode
  { rspnNode         :: RunningNode -- ^ Running ardano node
  , rspnStakeKey     :: SigningKey StakeKey
  , rspnVrfKey       :: SigningKey VrfKey
  , rspnKesKey       :: SigningKey KesKey
  , rspnStakePoolKey :: SigningKey StakePoolKey
  , rspnCounter      :: OperationalCertificateIssueCounter
  }

{- | Stake pool node params
-}
data StakePoolNodeParams = StakePoolNodeParams
  { spnCost   :: Coin
  , spnMargin :: Rational
  , spnPledge :: Coin
  }

defaultStakePoolNodeParams :: StakePoolNodeParams
defaultStakePoolNodeParams = StakePoolNodeParams 0 (3 % 100) 0

{-| Modifications to apply to the default genesis configurations
-}
data GenesisConfigChanges =
  GenesisConfigChanges
    { cfShelley :: ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto

    -- this is spiritually a 'Cardano.Ledger.Alonzo.Genesis.AlonzoGenesis' value
    -- we can't JSON roundtrip it here because the cardano node that we use in
    -- CI uses a slightly different JSON encoding and will trip even if we
    -- just write 'toJSON . fromJSON' without modifying the value
    -- Note that the problem is with the ToJSON instance!
    , cfAlonzo  :: Aeson.Value -> Aeson.Value


    -- this is spiritually a 'Cardano.Ledger.Conway.Genesis.ConwayGenesis' value
    -- we can't JSON roundtrip it here because the cardano node that we use in
    -- CI uses a slightly different JSON encoding and will trip even if we
    -- just write 'toJSON . fromJSON' without modifying the value
    -- Note that the problem is with the ToJSON instance!
    , cfConway :: Aeson.Value -> Aeson.Value

    -- | Changes to the node config file
    , cfNodeConfig :: Aeson.Value -> Aeson.Value
    }

instance Semigroup GenesisConfigChanges where
  l <> r =
    GenesisConfigChanges
      { cfShelley = cfShelley r . cfShelley l
      , cfAlonzo  = cfAlonzo  r . cfAlonzo l
      , cfConway  = cfConway  r . cfConway l
      , cfNodeConfig = cfNodeConfig r . cfNodeConfig l
      }

instance Monoid GenesisConfigChanges where
  mempty = GenesisConfigChanges id id id id

-- | Set the 'TestConwayHardForkAtEpoch' field to the given value (can be 0)
forkIntoConwayInEpoch :: Natural -> GenesisConfigChanges
forkIntoConwayInEpoch n =
  mempty{ cfNodeConfig = set (atKey "TestConwayHardForkAtEpoch") (Just $ Aeson.toJSON n) }

{-| Change the alonzo genesis config to allow transactions with up to twice the normal size
-}
allowLargeTransactions :: GenesisConfigChanges
allowLargeTransactions =
  let change :: ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto
      change g = g{sgProtocolParams = double (sgProtocolParams g)}
      double :: Core.PParams (ShelleyEra StandardCrypto) -> Core.PParams (ShelleyEra StandardCrypto)
      double = over (Core.ppLens . Core.hkdMaxTxSizeL) (*2)
  in mempty{cfShelley = change}

{-| Set the epoch length in the shelley genesis configuration. Note that the parameter is a multiple of
  the slot length. With the default slot length of 0.1s, an epoch length of 100 would result in
  10 second epochs.
-}
setEpochLength :: EpochSize -> GenesisConfigChanges
setEpochLength n =
  let change :: ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto
      change g = g{sgEpochLength = n}
  in mempty{cfShelley = change}

-- | Arguments given to the 'cardano-node' command-line to run a node.
data CardanoNodeArgs = CardanoNodeArgs
  { nodeSocket             :: FilePath
  , nodeConfigFile         :: FilePath
  , nodeByronGenesisFile   :: FilePath
  , nodeShelleyGenesisFile :: FilePath
  , nodeAlonzoGenesisFile  :: FilePath
  , nodeConwayGenesisFile  :: FilePath
  , nodeTopologyFile       :: FilePath
  , nodeDatabaseDir        :: FilePath
  , nodeDlgCertFile        :: Maybe FilePath
  , nodeSignKeyFile        :: Maybe FilePath
  , nodeOpCertFile         :: Maybe FilePath
  , nodeKesKeyFile         :: Maybe FilePath
  , nodeVrfKeyFile         :: Maybe FilePath
  , nodePort               :: Maybe Port
  }

defaultCardanoNodeArgs :: ConfigFilePath -> CardanoNodeArgs
defaultCardanoNodeArgs (ConfigFilePath configPath) =
  CardanoNodeArgs
    { nodeSocket = "node.socket"
    , nodeConfigFile = configPath </> "cardano-node.json"
    , nodeByronGenesisFile = configPath </> "genesis-byron.json"
    , nodeShelleyGenesisFile = configPath </> "genesis-shelley.json"
    , nodeAlonzoGenesisFile = configPath </> "genesis-alonzo.json"
    , nodeConwayGenesisFile = configPath </> "genesis-conway.json"
    , nodeTopologyFile = "topology.json"
    , nodeDatabaseDir = "db"
    , nodeDlgCertFile = Nothing
    , nodeSignKeyFile = Nothing
    , nodeOpCertFile = Nothing
    , nodeKesKeyFile = Nothing
    , nodeVrfKeyFile = Nothing
    , nodePort = Nothing
    }

-- | Generate command-line arguments for launching @cardano-node@.
cardanoNodeProcess :: Maybe FilePath -> CardanoNodeArgs -> CreateProcess
cardanoNodeProcess cwd args =
  (proc "cardano-node" strArgs){cwd}
 where
  CardanoNodeArgs
    { nodeConfigFile
    , nodeTopologyFile
    , nodeDatabaseDir
    , nodeSocket
    , nodePort
    , nodeSignKeyFile
    , nodeDlgCertFile
    , nodeOpCertFile
    , nodeKesKeyFile
    , nodeVrfKeyFile
    } = args

  strArgs =
    "run" :
    mconcat
      [ ["--config", nodeConfigFile]
      , ["--topology", nodeTopologyFile]
      , ["--database-path", nodeDatabaseDir]
      , ["--socket-path", nodeSocket]
      , opt "--port" (show <$> nodePort)
      , opt "--byron-signing-key" nodeSignKeyFile
      , opt "--byron-delegation-certificate" nodeDlgCertFile
      , opt "--shelley-operational-certificate" nodeOpCertFile
      , opt "--shelley-kes-key" nodeKesKeyFile
      , opt "--shelley-vrf-key" nodeVrfKeyFile
      ]

  opt :: a -> Maybe a -> [a]
  opt arg = \case
    Nothing  -> []
    Just val -> [arg, val]
