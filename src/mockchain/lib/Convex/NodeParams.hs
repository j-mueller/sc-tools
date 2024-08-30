{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell    #-}
module Convex.NodeParams(
  NodeParams(..),
  networkId,
  ledgerProtocolParameters,
  protocolParameters,
  systemStart,
  eraHistory,
  stakePools,
  slotLength,

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
  L.hkdCostModelsL
) where

import           Cardano.Api                   (ConwayEra)
import qualified Cardano.Api.Ledger            as L
import           Cardano.Api.Shelley           (EraHistory,
                                                LedgerProtocolParameters,
                                                NetworkId (..), PoolId)
import qualified Cardano.Ledger.Alonzo.PParams as L
import           Cardano.Slotting.Time         (SlotLength, SystemStart)
import           Control.Lens.TH               (makeLensesFor)
import           Data.Set                      as Set (Set)

data NodeParams =
  NodeParams
    { npNetworkId          :: NetworkId
    , npProtocolParameters :: LedgerProtocolParameters ConwayEra
    , npSystemStart        :: SystemStart
    , npEraHistory         :: EraHistory
    , npStakePools         :: Set PoolId
    , npSlotLength         :: SlotLength
    }

makeLensesFor
  [ ("npNetworkId", "networkId")
  , ("npProtocolParameters", "ledgerProtocolParameters")
  , ("npSystemStart", "systemStart")
  , ("npEraHistory", "eraHistory")
  , ("npStakePools", "stakePools")
  , ("npSlotLength", "slotLength")
  ] ''NodeParams

makeLensesFor
  [ ("unLedgerProtocolParameters", "protocolParameters")
  ] ''LedgerProtocolParameters

