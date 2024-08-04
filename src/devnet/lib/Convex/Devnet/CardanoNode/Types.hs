{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
module Convex.Devnet.CardanoNode.Types (
  Port,
  PortsConfig (..),
  defaultPortsConfig,
  RunningNode (..),
  RunningStakePoolNode (..),
  StakePoolNodeParams (..),
  defaultStakePoolNodeParams,
  -- * Genesis config changes
  GenesisConfigChanges (..),
  forkIntoConwayInEpoch,
  allowLargeTransactions
) where

import           Cardano.Api                      (Env, LocalNodeConnectInfo,
                                                   NetworkId)
import           Cardano.Api.Shelley              (KesKey, Key (..),
                                                   OperationalCertificateIssueCounter,
                                                   ShelleyGenesis, StakeKey,
                                                   StakePoolKey, VrfKey)
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

{-| Describes a running pool node
-}
data RunningNode = RunningNode
  { rnNodeSocket     :: FilePath -- ^ Cardano node socket
  , rnNetworkId      :: NetworkId -- ^ Network ID used by the cardano node
  , rnNodeConfigFile :: FilePath -- ^ Cardano node config file (JSON)
  , rnConnectInfo    :: (LocalNodeConnectInfo, Env) -- ^ Connection info for node queries
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
