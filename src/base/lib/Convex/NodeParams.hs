{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module Convex.NodeParams (
  NodeParams (..),
  networkId,
  ledgerProtocolParameters,
  protocolParameters,
  systemStart,
  eraHistory,
  stakePools,
  slotLength,
  pParams,

  -- * Lenses for @ProtocolParameters@, re-exported from cardano-ledger.

  -- See 'Cardano.Api.Ledger' for an explanation of the fields and a full list
  L.ppProtocolVersionL,
  L.hkdMaxBHSizeL,
  L.hkdMaxBBSizeL,
  L.hkdMaxTxSizeL,
  L.hkdMinFeeAL,
  L.hkdMinFeeBL,
  L.hkdPoolDepositL,
  L.hkdPricesL,
  L.hkdMaxTxExUnitsL,
  L.hkdMaxBlockExUnitsL,
  L.hkdMaxValSizeL,
  L.hkdCollateralPercentageL,
  L.hkdMaxCollateralInputsL,
  L.hkdMinPoolCostL,
  L.hkdCostModelsL,
) where

import Cardano.Api.Ledger (PParams)
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley (
  EraHistory,
  LedgerProtocolParameters (..),
  NetworkId (..),
  PoolId,
  ShelleyLedgerEra,
 )
import Cardano.Ledger.Alonzo.PParams qualified as L
import Cardano.Slotting.Time (SlotLength, SystemStart)
import Control.Lens.TH (makeLensesFor)
import Data.Set as Set (Set)

data NodeParams era
  = NodeParams
  { npNetworkId :: NetworkId
  , npProtocolParameters :: LedgerProtocolParameters era
  , npSystemStart :: SystemStart
  , npEraHistory :: EraHistory
  , npStakePools :: Set PoolId
  , npSlotLength :: SlotLength
  }

makeLensesFor
  [ ("npNetworkId", "networkId")
  , ("npProtocolParameters", "ledgerProtocolParameters")
  , ("npSystemStart", "systemStart")
  , ("npEraHistory", "eraHistory")
  , ("npStakePools", "stakePools")
  , ("npSlotLength", "slotLength")
  ]
  ''NodeParams

makeLensesFor
  [ ("unLedgerProtocolParameters", "protocolParameters")
  ]
  ''LedgerProtocolParameters

-- | Convert `Params` to cardano-ledger `PParams`
pParams :: NodeParams era -> PParams (ShelleyLedgerEra era)
pParams NodeParams{npProtocolParameters} = case npProtocolParameters of
  LedgerProtocolParameters p -> p
