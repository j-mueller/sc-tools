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

import qualified Cardano.Api                          as C
import           Cardano.Api.Shelley                  (AnyPlutusScriptVersion (..),
                                                       CardanoMode,
                                                       ConsensusMode (..),
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
import           Cardano.Ledger.Alonzo.PParams        (DowngradeAlonzoPParams (..))
import           Cardano.Ledger.Babbage               (Babbage)
import           Cardano.Ledger.Babbage.Core          (CoinPerByte (..),
                                                       CoinPerWord (..))
import           Cardano.Ledger.Babbage.PParams       (DowngradeBabbagePParams (..),
                                                       coinsPerUTxOWordToCoinsPerUTxOByte)
import           Cardano.Ledger.BaseTypes             (ProtVer, boundRational)
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
import           Data.SOP.Counting                    (Exactly (..),
                                                       nonEmptyHead)
import           Data.SOP.Strict                      (K (K), NP (..))
import           Data.Time.Calendar                   (fromGregorian)
import           Data.Time.Clock                      (UTCTime (..))
import qualified Ouroboros.Consensus.HardFork.History as Ouroboros
import           Ouroboros.Consensus.Shelley.Eras     (StandardBabbage)

networkId :: NetworkId
networkId = Testnet (NetworkMagic 0)

startTime :: UTCTime
startTime = UTCTime (fromGregorian 2022 01 01) 0

systemStart :: SystemStart
systemStart = SystemStart startTime

-- Defaults are from plutus-apps/plutus-ledger/Ledger.Params

eraHistory :: EraHistory CardanoMode
eraHistory =
  EraHistory CardanoMode (Ouroboros.mkInterpreter $ Ouroboros.summaryWithExactly list) -- $ Ouroboros.summaryWithExactly list)
    where
      one = nonEmptyHead $ Ouroboros.getSummary $ Ouroboros.neverForksSummary epochSize slotLength
      list = Exactly $ K one :* K one :* K one :* K one :* K one :* K one :* K one :* Nil

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

  in ProtocolParameters
      { protocolParamProtocolVersion = (7,0)
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
        [ (AnyPlutusScriptVersion PlutusScriptV1, defaultV1CostModel)
        , (AnyPlutusScriptVersion PlutusScriptV2, defaultV2CostModel) ]
      , protocolParamPrices = Just (ExecutionUnitPrices {priceExecutionSteps = 721 % 10_000_000, priceExecutionMemory = 577 % 10_000})
      , protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 1_0000_000_000, executionMemory = 16_000_000})
      , protocolParamMaxBlockExUnits = Just (ExecutionUnits {executionSteps = 4_0000_000_000, executionMemory = 80_000_000})
      , protocolParamMaxValueSize = Just 5_000
      , protocolParamCollateralPercent = Just 150
      , protocolParamMaxCollateralInputs = Just 3
      , protocolParamUTxOCostPerByte =
          let (CoinPerByte (Coin coinsPerUTxOByte)) = coinsPerUTxOWordToCoinsPerUTxOByte $ CoinPerWord $ Coin 34_482
          in Just $ Lovelace coinsPerUTxOByte
      }

ledgerProtocolParameters :: PParams StandardBabbage
ledgerProtocolParameters =
  either (error . (<>) "ledgerProtocolParameters: toLedgerPParams failed with " . show) id $ toLedgerPParams ShelleyBasedEraBabbage protocolParameters

globals :: NodeParams -> Globals
globals params@NodeParams { npProtocolParameters, npSlotLength } = mkShelleyGlobals
  (genesisDefaultsFromParams params)
  (fixedEpochInfo epochSize npSlotLength)
  (fromMaybe (error "globals: Invalid version") $ Version.mkVersion $ fst $ protocolParamProtocolVersion $ C.unbundleProtocolParams npProtocolParameters)

protVer :: NodeParams -> ProtVer
protVer = lederPPProtVer . C.unbundleProtocolParams . npProtocolParameters

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
      $ pParams params
  }
  where
    d = fromMaybe (error "3 % 5 should be valid UnitInterval") $ boundRational (3 % 5)

-- | Convert `Params` to cardano-ledger `PParams`
pParams :: NodeParams -> PParams Babbage
pParams NodeParams { npProtocolParameters } = case npProtocolParameters of
  C.BundleAsShelleyBasedProtocolParameters _ _ p -> p

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

bundledProtocolParameters :: C.BundledProtocolParameters C.BabbageEra
bundledProtocolParameters = either (error . (<>) "nodeParams: bundleProtocolParams failed: " . show) id (C.bundleProtocolParams C.BabbageEra protocolParameters)
