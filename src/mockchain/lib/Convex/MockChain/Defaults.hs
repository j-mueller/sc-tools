{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
module Convex.MockChain.Defaults(
  eraHistory,
  epochSize,
  slotLength,
  protocolParameters,
  bundledProtocolParameters,
  ledgerProtocolParameters,
  networkId,
  systemStart,
  globals,
  genesisDefaultsFromParams,
  pParams,
  protVer,
  nodeParams
) where

import           Cardano.Api.Shelley                  (AnyPlutusScriptVersion (..),
                                                       EraHistory (EraHistory),
                                                       ExecutionUnitPrices (..),
                                                       ExecutionUnits (..),
                                                       NetworkId (..),
                                                       NetworkMagic (..),
                                                       PlutusScriptVersion (..),
                                                       ProtocolParameters (..),
                                                       ShelleyBasedEra (..),
                                                       shelleyGenesisDefaults,
                                                       toLedgerPParams)
import qualified Cardano.Api.Shelley                  as C
import           Cardano.Ledger.Alonzo.PParams        (DowngradeAlonzoPParams (..))
import           Cardano.Ledger.Babbage.Core          (CoinPerByte (..),
                                                       CoinPerWord (..))
import           Cardano.Ledger.Babbage.PParams       (DowngradeBabbagePParams (..),
                                                       coinsPerUTxOWordToCoinsPerUTxOByte)
import           Cardano.Ledger.BaseTypes             (EpochInterval (..),
                                                       ProtVer, boundRational)
import qualified Cardano.Ledger.Binary.Version        as Version
import           Cardano.Ledger.Core                  (PParams,
                                                       downgradePParams)
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
import           Data.SOP                             (K (K))
import           Data.SOP.Counting                    (Exactly (..))
import           Data.SOP.NonEmpty                    (nonEmptyHead)
import           Data.SOP.Strict                      (NP (..))
import           Data.Time.Calendar                   (fromGregorian)
import           Data.Time.Clock                      (UTCTime (..))
import qualified Ouroboros.Consensus.Block.Abstract   as Ouroboros
import qualified Ouroboros.Consensus.HardFork.History as Ouroboros
import           Ouroboros.Consensus.Shelley.Eras     (StandardConway)

networkId :: NetworkId
networkId = Testnet (NetworkMagic 0)

startTime :: UTCTime
startTime = UTCTime (fromGregorian 2022 01 01) 0

systemStart :: SystemStart
systemStart = SystemStart startTime

-- Defaults are from plutus-apps/plutus-ledger/Ledger.Params

eraHistory :: EraHistory
eraHistory =
  EraHistory (Ouroboros.mkInterpreter $ Ouroboros.summaryWithExactly list) -- $ Ouroboros.summaryWithExactly list)
    where
      one = nonEmptyHead $ Ouroboros.getSummary $ Ouroboros.neverForksSummary epochSize slotLength window
      list = Exactly $ K one :* K one :* K one :* K one :* K one :* K one :* K one :* Nil

      -- NB: Not sure what to put here. Looks like this is usually 2 * max-rollbacks.
      window = Ouroboros.GenesisWindow (2 * 2160)

-- | A sensible default 'EpochSize' value for the emulator
epochSize :: EpochSize
epochSize = EpochSize 432000

-- | Slot length of 1 second
slotLength :: SlotLength
slotLength = mkSlotLength 1 -- 1 second

protocolParameters :: ProtocolParameters
protocolParameters =

  -- cost models from https://github.com/input-output-hk/cardano-node/blob/master/cardano-testnet/src/Testnet/Defaults.hs
  let defaultV1CostModel = C.CostModel
                            [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4
                            , 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100
                            , 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525
                            , 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000, 216773, 62
                            , 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32
                            , 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473
                            , 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32
                            , 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0
                            , 1, 1, 196500, 453240, 220, 0, 1, 1, 806990, 30482, 4, 1927926, 82523, 4, 265318, 0
                            , 4, 0, 85931, 32, 205665, 812, 1, 1, 41182, 32, 212342, 32, 31220, 32, 32696, 32, 43357
                            , 32, 32247, 32, 38314, 32, 57996947, 18975, 10
                            ]
      defaultV2CostModel = C.CostModel
                            [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4
                            , 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100
                            , 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525
                            , 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000, 216773, 62
                            , 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32
                            , 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473
                            , 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32
                            , 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0
                            , 1, 1, 196500, 453240, 220, 0, 1, 1, 1159724, 392670, 0, 2, 806990, 30482, 4, 1927926
                            , 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812, 1, 1, 41182, 32, 212342, 32, 31220
                            , 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32, 35892428, 10, 9462713, 1021, 10, 38887044
                            , 32947, 10
                            ]

      defaultV3CostModel = C.CostModel
                            [ 100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4
                            , 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100
                            , 16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189
                            , 769, 4, 2, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 1000, 42921
                            , 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1
                            , 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999
                            , 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391
                            , 32, 11546, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 90434, 519, 0
                            , 1, 74433, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 85848, 123203
                            , 7305, -900, 1716, 549, 57, 85848, 0, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325
                            , 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142
                            , 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10
                            , 43574283, 26308, 10, 16000, 100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1, 52538055
                            , 3756, 18, 267929, 18, 76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919, 12, 901022
                            , 1, 166917843, 4307, 36, 284546, 36, 158221314, 26549, 36, 74698472, 36, 333849714, 1
                            , 254006273, 72, 2174038, 72, 2261318, 64571, 4, 207616, 8310, 4, 1293828, 28716, 63, 0, 1
                            , 1006041, 43623, 251, 0, 1
                            ]

  in ProtocolParameters
      { protocolParamProtocolVersion = (7,0)
      , protocolParamDecentralization = Just (3 % 5)
      , protocolParamExtraPraosEntropy = Nothing
      , protocolParamMaxBlockHeaderSize = 1_100
      , protocolParamMaxBlockBodySize = 65_536
      , protocolParamMaxTxSize = 16_384
      , protocolParamTxFeeFixed = 155_381
      , protocolParamTxFeePerByte = 44
      , protocolParamMinUTxOValue = Just (Coin 1_500_000)
      , protocolParamStakeAddressDeposit = Coin 2_000_000
      , protocolParamStakePoolDeposit = Coin 500_000_000
      , protocolParamMinPoolCost = Coin 340_000_000
      , protocolParamPoolRetireMaxEpoch = EpochInterval 18
      , protocolParamStakePoolTargetNum = 150
      , protocolParamPoolPledgeInfluence = 3 % 10
      , protocolParamMonetaryExpansion = 3 % 1_000
      , protocolParamTreasuryCut = 1 % 5
      , protocolParamCostModels = fromList
        [ (AnyPlutusScriptVersion PlutusScriptV1, defaultV1CostModel)
        , (AnyPlutusScriptVersion PlutusScriptV2, defaultV2CostModel)
        , (AnyPlutusScriptVersion PlutusScriptV3, defaultV3CostModel)
        ]
      , protocolParamPrices = Just (ExecutionUnitPrices {priceExecutionSteps = 721 % 10_000_000, priceExecutionMemory = 577 % 10_000})
      , protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 1_0000_000_000, executionMemory = 16_000_000})
      , protocolParamMaxBlockExUnits = Just (ExecutionUnits {executionSteps = 4_0000_000_000, executionMemory = 80_000_000})
      , protocolParamMaxValueSize = Just 5_000
      , protocolParamCollateralPercent = Just 150
      , protocolParamMaxCollateralInputs = Just 3
      , protocolParamUTxOCostPerByte =
          let (CoinPerByte (Coin coinsPerUTxOByte)) = coinsPerUTxOWordToCoinsPerUTxOByte $ CoinPerWord $ Coin 34_482
          in Just $ Coin coinsPerUTxOByte
      }

ledgerProtocolParameters :: PParams StandardConway
ledgerProtocolParameters =
  either (error . (<>) "ledgerProtocolParameters: toLedgerPParams failed with " . show) id $ toLedgerPParams ShelleyBasedEraConway protocolParameters

globals :: NodeParams -> Globals
globals params@NodeParams { npProtocolParameters, npSlotLength } = mkShelleyGlobals
  (genesisDefaultsFromParams params)
  (fixedEpochInfo epochSize npSlotLength)
  (fromMaybe (error "globals: Invalid version") $ Version.mkVersion $ fst $ protocolParamProtocolVersion $ C.fromLedgerPParams C.ShelleyBasedEraConway $ C.unLedgerProtocolParameters npProtocolParameters)

protVer :: NodeParams -> ProtVer
protVer = lederPPProtVer . C.fromLedgerPParams C.ShelleyBasedEraConway . C.unLedgerProtocolParameters . npProtocolParameters

lederPPProtVer :: ProtocolParameters -> ProtVer
lederPPProtVer k =
  let (majorProtVer, minorProtVer) = protocolParamProtocolVersion k
  in fromMaybe (error $ "globals: Invalid major protocol version: " <> show majorProtVer) $
      (`C.Ledger.ProtVer` minorProtVer) <$> Version.mkVersion majorProtVer

genesisDefaultsFromParams :: NodeParams -> ShelleyGenesis StandardCrypto
genesisDefaultsFromParams params@NodeParams { npNetworkId } = shelleyGenesisDefaults
  { sgSystemStart = startTime
  , sgNetworkMagic = case npNetworkId of Testnet (NetworkMagic nm) -> nm; _ -> 0
  , sgNetworkId = case npNetworkId of Testnet _ -> C.Ledger.Testnet; Mainnet -> C.Ledger.Mainnet
  , sgProtocolParams =
      downgradePParams ()
      $ downgradePParams ()
      $ downgradePParams DowngradeAlonzoPParams{dappMinUTxOValue=Coin 0}
      $ downgradePParams DowngradeBabbagePParams{dbppD=d, dbppExtraEntropy=C.Ledger.NeutralNonce}
      $ downgradePParams ()
      $ pParams params
  }
  where
    d = fromMaybe (error "3 % 5 should be valid UnitInterval") $ boundRational (3 % 5)

-- | Convert `Params` to cardano-ledger `PParams`
pParams :: NodeParams -> PParams StandardConway
pParams NodeParams { npProtocolParameters } = case npProtocolParameters of
  C.LedgerProtocolParameters p -> p

{-| 'NodeParams' with default values for testing
-}
nodeParams :: NodeParams
nodeParams =
  NodeParams
    { npNetworkId = networkId
    , npProtocolParameters = bundledProtocolParameters
    , npSystemStart = systemStart
    , npEraHistory = eraHistory
    , npStakePools = mempty
    , npSlotLength = slotLength
    }

bundledProtocolParameters :: C.LedgerProtocolParameters C.ConwayEra
bundledProtocolParameters = C.LedgerProtocolParameters $ either (error . (<>) "toLedgerPParams failed: " . show) id $ C.toLedgerPParams C.ShelleyBasedEraConway protocolParameters
