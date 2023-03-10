{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell    #-}
module Convex.NodeParams(
  NodeParams(..),
  networkId,
  protocolParameters,
  systemStart,
  eraHistory,
  stakePools,
  slotLength
) where

import           Cardano.Api.Shelley   (CardanoMode, EraHistory, NetworkId (..),
                                        PoolId, ProtocolParameters (..))
import           Cardano.Slotting.Time (SlotLength, SystemStart)
import           Control.Lens.TH       (makeLensesFor)
import           Data.Set              as Set (Set)

data NodeParams =
  NodeParams
    { npNetworkId          :: NetworkId
    , npProtocolParameters :: ProtocolParameters
    , npSystemStart        :: SystemStart
    , npEraHistory         :: EraHistory CardanoMode
    , npStakePools         :: Set PoolId
    , npSlotLength         :: SlotLength
    }

makeLensesFor
  [ ("npNetworkId", "networkId")
  , ("npProtocolParameters", "protocolParameters")
  , ("npSystemStart", "systemStart")
  , ("npEraHistory", "eraHistory")
  , ("npStakePools", "stakePools")
  , ("npSlotLength", "slotLength")
  ] ''NodeParams
