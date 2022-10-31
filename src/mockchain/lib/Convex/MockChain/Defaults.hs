{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
module Convex.MockChain.Defaults(
  eraHistory,
  epochSize,
  slotLength,
  protocolParameters,
  networkId,
  systemStart,
  globals,
  genesisDefaultsFromParams,
  pParams,
  nodeParams
) where

import           Cardano.Api.Shelley                  (AnyPlutusScriptVersion (..),
                                                       CardanoMode,
                                                       ConsensusMode (..),
                                                       CostModel (..),
                                                       EpochNo (..),
                                                       EraHistory (EraHistory),
                                                       ExecutionUnitPrices (..),
                                                       ExecutionUnits (..),
                                                       Lovelace (..),
                                                       NetworkId (..),
                                                       NetworkMagic (..),
                                                       PlutusScriptVersion (..),
                                                       ProtocolParameters (..),
                                                       ShelleyBasedEra (..),
                                                       shelleyGenesisDefaults,
                                                       toLedgerPParams)
import           Cardano.Ledger.Babbage               (BabbageEra)
import           Cardano.Ledger.Babbage.PParams       (retractPP)
import           Cardano.Ledger.Babbage.Translation   (coinsPerUTxOWordToCoinsPerUTxOByte)
import           Cardano.Ledger.BaseTypes             (boundRational)
import           Cardano.Ledger.Core                  (PParams)
import           Cardano.Ledger.Crypto                (StandardCrypto)
import           Cardano.Ledger.Shelley.API           (Coin (..), Globals,
                                                       ShelleyGenesis (..),
                                                       mkShelleyGlobals)
import qualified Cardano.Ledger.Shelley.API           as C.Ledger
import           Cardano.Ledger.Slot                  (EpochSize (..))
import           Cardano.Slotting.EpochInfo           (fixedEpochInfo)
import           Cardano.Slotting.Time                (SlotLength,
                                                       SystemStart (..),
                                                       mkSlotLength)
import           Convex.NodeParams                    (NodeParams (..))
import           Data.Map                             (fromList)
import           Data.Maybe                           (fromMaybe)
import           Data.Ratio                           ((%))
import           Data.SOP.Strict                      (K (K), NP (..))
import           Data.Time.Calendar                   (fromGregorian)
import           Data.Time.Clock                      (UTCTime (..))
import qualified Ouroboros.Consensus.HardFork.History as Ouroboros
import qualified Ouroboros.Consensus.Util.Counting    as Ouroboros
import           PlutusCore                           (defaultCostModelParams)

type MockchainEra = BabbageEra StandardCrypto

networkId :: NetworkId
networkId = Testnet (NetworkMagic 0)

startTime :: UTCTime
startTime = UTCTime (fromGregorian 2022 01 01) 0

systemStart :: SystemStart
systemStart = SystemStart startTime

-- Defaults are from plutus-apps/plutus-ledger/Ledger.Params

eraHistory :: EraHistory CardanoMode
eraHistory =
  EraHistory CardanoMode (Ouroboros.mkInterpreter $ Ouroboros.summaryWithExactly list)
    where
      one = Ouroboros.nonEmptyHead $ Ouroboros.getSummary $ Ouroboros.neverForksSummary epochSize slotLength
      list = Ouroboros.Exactly $ K one :* K one :* K one :* K one :* K one :* K one :* Nil

-- | A sensible default 'EpochSize' value for the emulator
epochSize :: EpochSize
epochSize = EpochSize 432000

-- | Slot length of 1 second
slotLength :: SlotLength
slotLength = mkSlotLength 1_000 -- 1 second

protocolParameters :: ProtocolParameters
protocolParameters =
  ProtocolParameters
    { protocolParamProtocolVersion = (6,0)
    , protocolParamDecentralization = Just (3 % 5)
    , protocolParamExtraPraosEntropy = Nothing
    , protocolParamMaxBlockHeaderSize = 1_100
    , protocolParamMaxBlockBodySize = 65_536
    , protocolParamMaxTxSize = 16_384
    , protocolParamTxFeeFixed = 155_381
    , protocolParamTxFeePerByte = 44
    , protocolParamMinUTxOValue = Just (Lovelace 1_500_000)
    , protocolParamStakeAddressDeposit = Lovelace 2_000_000
    , protocolParamStakePoolDeposit = Lovelace 500_000_000
    , protocolParamMinPoolCost = Lovelace 340_000_000
    , protocolParamPoolRetireMaxEpoch = EpochNo 18
    , protocolParamStakePoolTargetNum = 150
    , protocolParamPoolPledgeInfluence = 3 % 10
    , protocolParamMonetaryExpansion = 3 % 1_000
    , protocolParamTreasuryCut = 1 % 5
    , protocolParamUTxOCostPerWord = Nothing -- Obsolete from babbage onwards
    , protocolParamCostModels = fromList
      [ (AnyPlutusScriptVersion PlutusScriptV1, CostModel $ fromMaybe (error "Ledger.Params: defaultCostModelParams is broken") defaultCostModelParams)
      , (AnyPlutusScriptVersion PlutusScriptV2, CostModel $ fromMaybe (error "Ledger.Params: defaultCostModelParams is broken") defaultCostModelParams) ]
    , protocolParamPrices = Just (ExecutionUnitPrices {priceExecutionSteps = 721 % 10_000_000, priceExecutionMemory = 577 % 10_000})
    , protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 1_0000_000_000, executionMemory = 16_000_000})
    , protocolParamMaxBlockExUnits = Just (ExecutionUnits {executionSteps = 4_0000_000_000, executionMemory = 80_000_000})
    , protocolParamMaxValueSize = Just 5_000
    , protocolParamCollateralPercent = Just 150
    , protocolParamMaxCollateralInputs = Just 3
    , protocolParamUTxOCostPerByte =
        let (Coin coinsPerUTxOByte) = coinsPerUTxOWordToCoinsPerUTxOByte $ Coin 34_482
         in Just $ Lovelace coinsPerUTxOByte
    }

globals :: NodeParams -> Globals
globals params@NodeParams { npProtocolParameters, npSlotLength } = mkShelleyGlobals
  (genesisDefaultsFromParams params)
  (fixedEpochInfo epochSize npSlotLength)
  (fst $ protocolParamProtocolVersion npProtocolParameters)

genesisDefaultsFromParams :: NodeParams -> ShelleyGenesis MockchainEra
genesisDefaultsFromParams params@NodeParams { npNetworkId } = shelleyGenesisDefaults
  { sgSystemStart = startTime
  , sgNetworkMagic = case npNetworkId of Testnet (NetworkMagic nm) -> nm; _ -> 0
  , sgNetworkId = case npNetworkId of Testnet _ -> C.Ledger.Testnet; Mainnet -> C.Ledger.Mainnet
  , sgProtocolParams = retractPP (Coin 0) d C.Ledger.NeutralNonce $ pParams params
  }
  where
    d = fromMaybe (error "3 % 5 should be valid UnitInterval") $ boundRational (3 % 5)

-- | Convert `Params` to cardano-ledger `PParams`
pParams :: NodeParams -> PParams MockchainEra
pParams NodeParams { npProtocolParameters } = toLedgerPParams ShelleyBasedEraBabbage npProtocolParameters

{-| 'NodeParams' with default values for testing
-}
nodeParams :: NodeParams
nodeParams =
  NodeParams
    { npNetworkId = networkId
    , npProtocolParameters = protocolParameters
    , npSystemStart = systemStart
    , npEraHistory = eraHistory
    , npStakePools = mempty
    , npSlotLength = slotLength
    }
