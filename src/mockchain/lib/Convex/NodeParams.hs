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

  -- * Lenses for @ProtocolParameters@
  -- See https://input-output-hk.github.io/cardano-node/cardano-api/lib/Cardano-Api-Shelley.html#t:ProtocolParameters
  -- for an explanation of the fields
  protocolVersion,
  decentralization,
  extraPraosEntropy,
  maxBlockHeaderSize,
  maxBlockBodySize,
  maxTxSize,
  txFeeFixed,
  txFeeFerByte,
  minUTxOValue,
  stakeAddressDeposit,
  stakePoolDeposit,
  minPoolCost,
  poolRetireMaxEpoch,
  stakePoolTargetNum,
  poolPledgeInfluence,
  monetaryExpansion,
  treasuryCut,
  costModels,
  prices,
  maxTxExUnits,
  maxBlockExUnits,
  maxValueSize,
  collateralPercent,
  maxCollateralInputs,
  uTxOCostPerByte
) where

import           Cardano.Api           (BabbageEra)
import           Cardano.Api.Shelley   (EraHistory, LedgerProtocolParameters,
                                        NetworkId (..), PoolId,
                                        ProtocolParameters (..))
import           Cardano.Slotting.Time (SlotLength, SystemStart)
import           Control.Lens.TH       (makeLensesFor)
import           Data.Set              as Set (Set)

data NodeParams =
  NodeParams
    { npNetworkId          :: NetworkId
    , npProtocolParameters :: LedgerProtocolParameters BabbageEra
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

makeLensesFor
  [ ("protocolParamProtocolVersion", "protocolVersion")
  , ("protocolParamDecentralization", "decentralization")
  , ("protocolParamExtraPraosEntropy", "extraPraosEntropy")
  , ("protocolParamMaxBlockHeaderSize", "maxBlockHeaderSize")
  , ("protocolParamMaxBlockBodySize", "maxBlockBodySize")
  , ("protocolParamMaxTxSize", "maxTxSize")
  , ("protocolParamTxFeeFixed", "txFeeFixed")
  , ("protocolParamTxFeePerByte", "txFeeFerByte")
  , ("protocolParamMinUTxOValue", "minUTxOValue")
  , ("protocolParamStakeAddressDeposit", "stakeAddressDeposit")
  , ("protocolParamStakePoolDeposit", "stakePoolDeposit")
  , ("protocolParamMinPoolCost", "minPoolCost")
  , ("protocolParamPoolRetireMaxEpoch", "poolRetireMaxEpoch")
  , ("protocolParamStakePoolTargetNum", "stakePoolTargetNum")
  , ("protocolParamPoolPledgeInfluence", "poolPledgeInfluence")
  , ("protocolParamMonetaryExpansion", "monetaryExpansion")
  , ("protocolParamTreasuryCut", "treasuryCut")
  , ("protocolParamUTxOCostPerWord", "uTxOCostPerWord")
  , ("protocolParamCostModels", "costModels")
  , ("protocolParamPrices", "prices")
  , ("protocolParamMaxTxExUnits", "maxTxExUnits")
  , ("protocolParamMaxBlockExUnits", "maxBlockExUnits")
  , ("protocolParamMaxValueSize", "maxValueSize")
  , ("protocolParamCollateralPercent", "collateralPercent")
  , ("protocolParamMaxCollateralInputs", "maxCollateralInputs")
  , ("protocolParamUTxOCostPerByte", "uTxOCostPerByte")
  ] ''ProtocolParameters
