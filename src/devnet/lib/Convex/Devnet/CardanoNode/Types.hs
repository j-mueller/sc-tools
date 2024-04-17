module Convex.Devnet.CardanoNode.Types (
  RunningNode (..),
) where

import           Cardano.Api                      (CardanoMode, Env,
                                                   LocalNodeConnectInfo,
                                                   NetworkId)

data RunningNode = RunningNode
  { rnNodeSocket     :: FilePath -- ^ Cardano node socket
  , rnNetworkId      :: NetworkId -- ^ Network ID used by the cardano node
  , rnNodeConfigFile :: FilePath -- ^ Cardano node config file (JSON)
  , rnConnectInfo    :: (LocalNodeConnectInfo CardanoMode, Env) -- ^ Connection info for node queries
  }
