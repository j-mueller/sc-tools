module Convex.Devnet.CardanoNode.Types (
  RunningNode (..),
  RunningStakePoolNode(..)
) where

import           Cardano.Api                      (CardanoMode, Env,
                                                   LocalNodeConnectInfo,
                                                   NetworkId)
import           Cardano.Api.Shelley              (StakeKey, Key(..), VrfKey,
                                                   KesKey, StakePoolKey,
                                                   OperationalCertificateIssueCounter)

{-| Describes a running pool node
-}
data RunningNode = RunningNode
  { rnNodeSocket     :: FilePath -- ^ Cardano node socket
  , rnNetworkId      :: NetworkId -- ^ Network ID used by the cardano node
  , rnNodeConfigFile :: FilePath -- ^ Cardano node config file (JSON)
  , rnConnectInfo    :: (LocalNodeConnectInfo CardanoMode, Env) -- ^ Connection info for node queries
  }

{-| Describes a running stake pool node
-}
data RunningStakePoolNode = RunningStakePoolNode
  { rspnNode :: RunningNode -- ^ Running ardano node
  , rspnStakeKey :: SigningKey StakeKey
  , rspnVrfKey :: SigningKey VrfKey
  , rspnKesKey :: SigningKey KesKey
  , rspnStakePoolKey :: SigningKey StakePoolKey
  , rspnCounter :: OperationalCertificateIssueCounter
  }
