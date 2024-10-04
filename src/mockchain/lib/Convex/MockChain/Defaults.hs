{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
module Convex.MockChain.Defaults(
  eraHistory,
  epochSize,
  slotLength,
  protocolParameters,
  bundledProtocolParameters,
  networkId,
  systemStart,
  globals,
  genesisDefaultsFromParams,
  pParams,
  protVer,
  nodeParams
) where

import qualified Cardano.Api.Ledger                   as L
import           Cardano.Api.Shelley                  (EraHistory (EraHistory),
                                                       NetworkId (..),
                                                       NetworkMagic (..),
                                                       shelleyGenesisDefaults)
import qualified Cardano.Api.Shelley                  as C
import           Cardano.Ledger.Alonzo.PParams        (DowngradeAlonzoPParams (..))
import qualified Cardano.Ledger.Alonzo.PParams        as L
import           Cardano.Ledger.Babbage.PParams       (DowngradeBabbagePParams (..))
import qualified Cardano.Ledger.Babbage.PParams       as L
import           Cardano.Ledger.BaseTypes             (ProtVer, boundRational)
import qualified Cardano.Ledger.Conway.PParams        as L
import           Cardano.Ledger.Core                  (PParams,
                                                       downgradePParams)
import           Cardano.Ledger.Crypto                (StandardCrypto)
import qualified Cardano.Ledger.Plutus.CostModels     as CostModels
import           Cardano.Ledger.Plutus.ExUnits        (ExUnits (..))
import           Cardano.Ledger.Plutus.Language       (Language (..))
import           Cardano.Ledger.Shelley.API           (Coin (..), Globals,
                                                       ShelleyGenesis (..),
                                                       mkShelleyGlobals)
import qualified Cardano.Ledger.Shelley.API           as C.Ledger
import           Cardano.Ledger.Slot                  (EpochSize (..))
import           Cardano.Slotting.EpochInfo           (fixedEpochInfo)
import           Cardano.Slotting.Time                (SlotLength,
                                                       SystemStart (..),
                                                       mkSlotLength)
import           Control.Lens                         (view, (&), (.~))
import           Convex.NodeParams                    (NodeParams (..))
import           Data.Int                             (Int64)
import qualified Data.Map                             as Map
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
import           Ouroboros.Consensus.Shelley.Eras     (ConwayEra,
                                                       StandardConway)

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
epochSize = EpochSize 432_000

-- | Slot length of 1 second
slotLength :: SlotLength
slotLength = mkSlotLength 1 -- 1 second

protocolParameters :: PParams StandardConway
protocolParameters = L.PParams $
    L.emptyPParamsIdentity @(ConwayEra StandardCrypto)
      & L.hkdMaxBHSizeL .~ 1_100
      & L.hkdMaxBBSizeL .~ 90_112
      & L.hkdMaxTxSizeL .~ 16_384
      & L.hkdMinFeeAL .~ 44
      & L.hkdMinFeeBL .~ 155_381
      & L.hkdPoolDepositL .~ 500_000_000
      & L.hkdCoinsPerUTxOByteL .~ L.CoinPerByte 4_310
      & L.hkdPricesL .~ L.Prices
          { L.prMem   = C.unsafeBoundedRational (577 % 10_000)
          , L.prSteps = C.unsafeBoundedRational (721 % 10_000_000)
          }
      & L.hkdMaxTxExUnitsL .~ ExUnits { exUnitsSteps = 1_0000_000_000, exUnitsMem = 14_000_000}
      & L.hkdMaxBlockExUnitsL .~ ExUnits{ exUnitsSteps = 20_000_000_000, exUnitsMem = 62_000_000 }
      & L.hkdMaxValSizeL .~ 5_000
      & L.hkdCollateralPercentageL .~ 150
      & L.hkdMaxCollateralInputsL .~ 3
      & L.hkdMinPoolCostL .~ 200_000
      & L.hkdCostModelsL .~ CostModels.mkCostModels (Map.fromList [(PlutusV1, v1CostModel), (PlutusV2, v2CostModel), (PlutusV3, v3CostModel)])
      & L.hkdMinFeeRefScriptCostPerByteL .~ C.unsafeBoundedRational 15
      & L.hkdMinPoolCostL .~ 170_000_000
      & L.hkdEMaxL .~ L.EpochInterval 18

unsafeMkCostModel :: Language -> [Int64] -> CostModels.CostModel
unsafeMkCostModel lang = either (error . show) id . CostModels.mkCostModel lang

v1CostModel :: CostModels.CostModel
v1CostModel = unsafeMkCostModel PlutusV1
  [
            100788,
            420,
            1,
            1,
            1000,
            173,
            0,
            1,
            1000,
            59957,
            4,
            1,
            11183,
            32,
            201305,
            8356,
            4,
            16000,
            100,
            16000,
            100,
            16000,
            100,
            16000,
            100,
            16000,
            100,
            16000,
            100,
            100,
            100,
            16000,
            100,
            94375,
            32,
            132994,
            32,
            61462,
            4,
            72010,
            178,
            0,
            1,
            22151,
            32,
            91189,
            769,
            4,
            2,
            85848,
            228465,
            122,
            0,
            1,
            1,
            1000,
            42921,
            4,
            2,
            24548,
            29498,
            38,
            1,
            898148,
            27279,
            1,
            51775,
            558,
            1,
            39184,
            1000,
            60594,
            1,
            141895,
            32,
            83150,
            32,
            15299,
            32,
            76049,
            1,
            13169,
            4,
            22100,
            10,
            28999,
            74,
            1,
            28999,
            74,
            1,
            43285,
            552,
            1,
            44749,
            541,
            1,
            33852,
            32,
            68246,
            32,
            72362,
            32,
            7243,
            32,
            7391,
            32,
            11546,
            32,
            85848,
            228465,
            122,
            0,
            1,
            1,
            90434,
            519,
            0,
            1,
            74433,
            32,
            85848,
            228465,
            122,
            0,
            1,
            1,
            85848,
            228465,
            122,
            0,
            1,
            1,
            270652,
            22588,
            4,
            1457325,
            64566,
            4,
            20467,
            1,
            4,
            0,
            141992,
            32,
            100788,
            420,
            1,
            1,
            81663,
            32,
            59498,
            32,
            20142,
            32,
            24588,
            32,
            20744,
            32,
            25933,
            32,
            24623,
            32,
            53384111,
            14333,
            10
        ]

v2CostModel :: CostModels.CostModel
v2CostModel = unsafeMkCostModel PlutusV2
    [
            100788,
            420,
            1,
            1,
            1000,
            173,
            0,
            1,
            1000,
            59957,
            4,
            1,
            11183,
            32,
            201305,
            8356,
            4,
            16000,
            100,
            16000,
            100,
            16000,
            100,
            16000,
            100,
            16000,
            100,
            16000,
            100,
            100,
            100,
            16000,
            100,
            94375,
            32,
            132994,
            32,
            61462,
            4,
            72010,
            178,
            0,
            1,
            22151,
            32,
            91189,
            769,
            4,
            2,
            85848,
            228465,
            122,
            0,
            1,
            1,
            1000,
            42921,
            4,
            2,
            24548,
            29498,
            38,
            1,
            898148,
            27279,
            1,
            51775,
            558,
            1,
            39184,
            1000,
            60594,
            1,
            141895,
            32,
            83150,
            32,
            15299,
            32,
            76049,
            1,
            13169,
            4,
            22100,
            10,
            28999,
            74,
            1,
            28999,
            74,
            1,
            43285,
            552,
            1,
            44749,
            541,
            1,
            33852,
            32,
            68246,
            32,
            72362,
            32,
            7243,
            32,
            7391,
            32,
            11546,
            32,
            85848,
            228465,
            122,
            0,
            1,
            1,
            90434,
            519,
            0,
            1,
            74433,
            32,
            85848,
            228465,
            122,
            0,
            1,
            1,
            85848,
            228465,
            122,
            0,
            1,
            1,
            955506,
            213312,
            0,
            2,
            270652,
            22588,
            4,
            1457325,
            64566,
            4,
            20467,
            1,
            4,
            0,
            141992,
            32,
            100788,
            420,
            1,
            1,
            81663,
            32,
            59498,
            32,
            20142,
            32,
            24588,
            32,
            20744,
            32,
            25933,
            32,
            24623,
            32,
            43053543,
            10,
            53384111,
            14333,
            10,
            43574283,
            26308,
            10
        ]

v3CostModel :: CostModels.CostModel
v3CostModel = unsafeMkCostModel PlutusV3
  [
            100788,
            420,
            1,
            1,
            1000,
            173,
            0,
            1,
            1000,
            59957,
            4,
            1,
            11183,
            32,
            201305,
            8356,
            4,
            16000,
            100,
            16000,
            100,
            16000,
            100,
            16000,
            100,
            16000,
            100,
            16000,
            100,
            100,
            100,
            16000,
            100,
            94375,
            32,
            132994,
            32,
            61462,
            4,
            72010,
            178,
            0,
            1,
            22151,
            32,
            91189,
            769,
            4,
            2,
            85848,
            123203,
            7305,
            -900,
            1716,
            549,
            57,
            85848,
            0,
            1,
            1,
            1000,
            42921,
            4,
            2,
            24548,
            29498,
            38,
            1,
            898148,
            27279,
            1,
            51775,
            558,
            1,
            39184,
            1000,
            60594,
            1,
            141895,
            32,
            83150,
            32,
            15299,
            32,
            76049,
            1,
            13169,
            4,
            22100,
            10,
            28999,
            74,
            1,
            28999,
            74,
            1,
            43285,
            552,
            1,
            44749,
            541,
            1,
            33852,
            32,
            68246,
            32,
            72362,
            32,
            7243,
            32,
            7391,
            32,
            11546,
            32,
            85848,
            123203,
            7305,
            -900,
            1716,
            549,
            57,
            85848,
            0,
            1,
            90434,
            519,
            0,
            1,
            74433,
            32,
            85848,
            123203,
            7305,
            -900,
            1716,
            549,
            57,
            85848,
            0,
            1,
            1,
            85848,
            123203,
            7305,
            -900,
            1716,
            549,
            57,
            85848,
            0,
            1,
            955506,
            213312,
            0,
            2,
            270652,
            22588,
            4,
            1457325,
            64566,
            4,
            20467,
            1,
            4,
            0,
            141992,
            32,
            100788,
            420,
            1,
            1,
            81663,
            32,
            59498,
            32,
            20142,
            32,
            24588,
            32,
            20744,
            32,
            25933,
            32,
            24623,
            32,
            43053543,
            10,
            53384111,
            14333,
            10,
            43574283,
            26308,
            10,
            16000,
            100,
            16000,
            100,
            962335,
            18,
            2780678,
            6,
            442008,
            1,
            52538055,
            3756,
            18,
            267929,
            18,
            76433006,
            8868,
            18,
            52948122,
            18,
            1995836,
            36,
            3227919,
            12,
            901022,
            1,
            166917843,
            4307,
            36,
            284546,
            36,
            158221314,
            26549,
            36,
            74698472,
            36,
            333849714,
            1,
            254006273,
            72,
            2174038,
            72,
            2261318,
            64571,
            4,
            207616,
            8310,
            4,
            1293828,
            28716,
            63,
            0,
            1,
            1006041,
            43623,
            251,
            0,
            1
        ]

globals :: C.IsShelleyBasedEra era => NodeParams era -> Globals
globals params@NodeParams { npSlotLength } = mkShelleyGlobals
  (genesisDefaultsFromParams params)
  (fixedEpochInfo epochSize npSlotLength)
  (C.Ledger.pvMajor $ view L.ppProtocolVersionL protocolParameters)

protVer :: L.EraPParams (C.ShelleyLedgerEra era) => NodeParams era -> ProtVer
protVer = view L.ppProtocolVersionL . C.unLedgerProtocolParameters . npProtocolParameters

genesisDefaultsFromParams :: forall era. C.IsShelleyBasedEra era => NodeParams era -> ShelleyGenesis StandardCrypto
genesisDefaultsFromParams params@NodeParams { npNetworkId } = shelleyGenesisDefaults
  { sgSystemStart = startTime
  , sgNetworkMagic = case npNetworkId of Testnet (NetworkMagic nm) -> nm; _ -> 0
  , sgNetworkId = case npNetworkId of Testnet _ -> C.Ledger.Testnet; Mainnet -> C.Ledger.Mainnet
  , sgProtocolParams = ($ pParams params) $ case C.shelleyBasedEra @era of
      C.ShelleyBasedEraShelley -> id
      C.ShelleyBasedEraAllegra -> downgradeAllegra
      C.ShelleyBasedEraMary    -> downgradeMary
      C.ShelleyBasedEraAlonzo  -> downgradeAlonzo
      C.ShelleyBasedEraBabbage -> downgradeBabbage
      C.ShelleyBasedEraConway  -> downgradeConway

  }
  where
    d = fromMaybe (error "3 % 5 should be valid UnitInterval") $ boundRational (3 % 5)

    downgradeAllegra :: PParams (C.ShelleyLedgerEra C.AllegraEra) -> PParams (C.ShelleyLedgerEra C.ShelleyEra)
    downgradeAllegra = downgradePParams ()

    downgradeMary :: PParams (C.ShelleyLedgerEra C.MaryEra) -> PParams (C.ShelleyLedgerEra C.ShelleyEra)
    downgradeMary = downgradeAllegra . downgradePParams ()

    downgradeAlonzo :: PParams (C.ShelleyLedgerEra C.AlonzoEra) -> PParams (C.ShelleyLedgerEra C.ShelleyEra)
    downgradeAlonzo = downgradeMary . downgradePParams DowngradeAlonzoPParams{dappMinUTxOValue=Coin 0}

    downgradeBabbage :: PParams (C.ShelleyLedgerEra C.BabbageEra) -> PParams (C.ShelleyLedgerEra C.ShelleyEra)
    downgradeBabbage = downgradeAlonzo . downgradePParams DowngradeBabbagePParams{dbppD=d, dbppExtraEntropy=C.Ledger.NeutralNonce}

    downgradeConway :: PParams (C.ShelleyLedgerEra C.ConwayEra) -> PParams (C.ShelleyLedgerEra C.ShelleyEra)
    downgradeConway = downgradeBabbage . downgradePParams ()



-- | Convert `Params` to cardano-ledger `PParams`
pParams :: NodeParams era -> PParams (C.ShelleyLedgerEra era)
pParams NodeParams { npProtocolParameters } = case npProtocolParameters of
  C.LedgerProtocolParameters p -> p

{-| 'NodeParams' with default values for testing
-}
nodeParams :: NodeParams C.ConwayEra
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
bundledProtocolParameters = C.LedgerProtocolParameters protocolParameters
