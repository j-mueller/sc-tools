{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Convex.Maestro.Types (
  poolId,
  toMaestroTxIn,
  CurrentEra,
  toCardanoApiUTxO,
  decodeTxOutCbor,
  toCardanoApiTxIn,
  textToIsString,
  toCardanoApiProtocolParams,
  toCardanoApiAddress,
  toCardanoApiSystemStart,
  toLedgerEraBound,
  toLedgerEraParams,
  toLedgerEraSummary,
  toCardanoApiSlotNo,
  toMaestroStakeAddress,
  toCardanoApiTxId,
  maestroSubmitResult,
) where

import Cardano.Api.Shelley qualified as C
import Cardano.Binary (DecoderError (..))
import Control.Monad.Except (MonadError (..), throwError)
import Data.ByteString.Base16 qualified as Base16
import Data.Text.Encoding qualified as Text.Encoding
import Maestro.Types.Common (Address, Bech32StringOf (Bech32StringOf), HexStringOf (..))
import Maestro.Types.V1 (AsAda (..), AsBytes (..), AsLovelace (..), MaestroRational (..), MemoryCpuWith (..), OutputReference (..), ProtocolParameters (..), ProtocolVersion (..))
import Maestro.Types.V1.Common (PoolId, TxHash (..))
import Maestro.Types.V1.Transactions (UtxoWithBytes (..))

-- or Babbage

import Cardano.Api.Ledger qualified as L
import Cardano.Ledger.Alonzo.PParams qualified as L
import Cardano.Ledger.Babbage.PParams qualified as L
import Cardano.Ledger.BaseTypes qualified as BaseTypes
import Cardano.Ledger.Binary qualified as L
import Cardano.Ledger.Binary.Plain (decodeFullDecoder)
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Plutus.CostModels qualified as CostModels
import Cardano.Ledger.Plutus.Language qualified as Plutus.Language
import Cardano.Slotting.Slot qualified as CSlot
import Cardano.Slotting.Time qualified as CTime
import Control.Lens
import Convex.Class (ValidationError)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (Coercible, coerce)
import Data.Int (Int64)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text qualified as Text
import Data.Time (LocalTime)
import Data.Time qualified as Time
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Maestro.Types.V1 qualified as Maestro
import Numeric.Natural (Natural)
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import Ouroboros.Consensus.Shelley.Eras qualified as LEras

type CurrentEra = C.ConwayEra
type LCurrentEra = LEras.ConwayEra

data DecodingError
  = Base16DecodeError String
  | CBORError DecoderError
  deriving stock (Eq, Show)

-- | Convert a 'Bech32StringOf PoolId' to a 'Cardano.Api.PoolId'
poolId :: Bech32StringOf PoolId -> C.PoolId
poolId (Bech32StringOf text) =
  either (error . show) id $ C.deserialiseFromBech32 text

-- | Convert a 'Cardano.Api.TxIn' to a Maestro 'OutputReference'
toMaestroTxIn :: C.TxIn -> OutputReference
toMaestroTxIn (C.TxIn txId (C.TxIx txIx)) = OutputReference (TxHash (C.serialiseToRawBytesHexText txId)) (fromIntegral txIx)

maestroSubmitResult :: Text.Text -> Either (ValidationError CurrentEra) C.TxId
maestroSubmitResult submitResult = either (const (mapToValidationError submitResult)) Right . C.deserialiseFromRawBytesHex . Text.Encoding.encodeUtf8 $ submitResult
 where
  mapToValidationError :: Text.Text -> Either (ValidationError CurrentEra) C.TxId
  mapToValidationError = Left . error . show

toMaestroStakeAddress :: C.StakeAddress -> Maestro.Bech32StringOf Maestro.RewardAddress
toMaestroStakeAddress = Maestro.Bech32StringOf . C.serialiseToBech32

{- | Convert a 'Maestro.Types.V1.Common.OutputReference' to a 'Cardano.Api.TxIn'
>>> toCardanoApiTxIn (OutputReference (TxHash "80ef5e073e9d703182cf7368a9b65caedee0b2477798430246234d297fba4a6c") (fromIntegral 0))
TxIn "80ef5e073e9d703182cf7368a9b65caedee0b2477798430246234d297fba4a6c" (TxIx 0)
-}
toCardanoApiTxIn :: OutputReference -> C.TxIn
toCardanoApiTxIn (OutputReference (TxHash (textToIsString -> txId)) (fromIntegral -> txIx)) = C.TxIn txId (C.TxIx txIx)

toCardanoApiTxId :: TxHash -> C.TxId
toCardanoApiTxId (TxHash (textToIsString -> txId)) = C.TxId txId

textToIsString :: (Coercible a Text.Text, IsString b) => a -> b
textToIsString = fromString . Text.unpack . coerce

toCardanoApiAddress :: (C.IsCardanoEra era) => Bech32StringOf Address -> Maybe (C.AddressInEra era)
toCardanoApiAddress (Bech32StringOf text) =
  C.deserialiseAddress (C.proxyToAsType Proxy) text

instance (Typeable ctx) => L.FromCBOR (C.TxOut ctx CurrentEra) where
  fromCBOR = C.fromShelleyTxOut C.ShelleyBasedEraConway <$> L.fromCBOR

instance L.ToCBOR (C.TxOut C.CtxUTxO CurrentEra) where
  toCBOR = L.toCBOR . C.toShelleyTxOut C.ShelleyBasedEraConway

{- | Decode a 'C.TxOut' from a 'ByteString' containing CBOR hex of the 'TxOut'
>>> decodeTxOutCbor (Text.Encoding.encodeUtf8 "82583900a2024cfb0f99fe862eba72550dfc7342a0d822af698f4764ece448a0bb1a2d7a92db3276410580742f4959339243a5fa93b41ad7c09687351b00000002303add4e")
Right (TxOut (AddressInEra (ShelleyAddressInEra ShelleyBasedEraConway) (ShelleyAddress Testnet (KeyHashObj (KeyHash {unKeyHash = "a2024cfb0f99fe862eba72550dfc7342a0d822af698f4764ece448a0"})) (StakeRefBase (KeyHashObj (KeyHash {unKeyHash = "bb1a2d7a92db3276410580742f4959339243a5fa93b41ad7c0968735"}))))) (TxOutValueShelleyBased ShelleyBasedEraConway (MaryValue (Coin 9399098702) (MultiAsset (fromList [])))) TxOutDatumNone ReferenceScriptNone)
-}
decodeTxOutCbor
  :: BS.ByteString
  -> Either DecodingError (C.TxOut C.CtxUTxO CurrentEra)
decodeTxOutCbor hexText = do
  raw <- first (Base16DecodeError . show) (Base16.decode hexText)
  -- Decode a ledger TxOut for that era
  first CBORError $ decodeFullDecoder "TxOut" L.fromCBOR (BSL.fromStrict raw)

toCardanoApiUTxO :: UtxoWithBytes -> Either DecodingError (C.TxIn, C.TxOut C.CtxUTxO CurrentEra)
toCardanoApiUTxO UtxoWithBytes{utxoWithBytesAddress = _, utxoWithBytesAssets = _, utxoWithBytesDatum = _, utxoWithBytesIndex, utxoWithBytesReferenceScript = _, utxoWithBytesTxHash, utxoWithBytesTxoutCbor} =
  maybe (throwError (Base16DecodeError "Could not decode base16")) pure utxoWithBytesTxoutCbor >>= \(HexStringOf txoutCbor) ->
    (,) (toCardanoApiTxIn (OutputReference (TxHash (textToIsString utxoWithBytesTxHash)) utxoWithBytesIndex)) <$> decodeTxOutCbor (Text.Encoding.encodeUtf8 txoutCbor)

toExUnits :: MemoryCpuWith Natural -> L.ExUnits
toExUnits (MemoryCpuWith{memoryCpuWithMemory = memory, memoryCpuWithCpu = cpu}) = L.ExUnits{L.exUnitsMem = memory, L.exUnitsSteps = cpu}

toPricesRational :: MemoryCpuWith MaestroRational -> L.Prices
toPricesRational (MemoryCpuWith (MaestroRational memory) (MaestroRational cpu)) =
  L.Prices
    { L.prMem = C.unsafeBoundedRational memory
    , L.prSteps = C.unsafeBoundedRational cpu
    }

toCardanoApiCoin :: AsAda -> L.Coin
toCardanoApiCoin (AsAda (AsLovelace l)) = L.Coin (fromIntegral l)

toCardanoApiBoundedRational :: (HasCallStack, Typeable r, BaseTypes.BoundedRational r) => MaestroRational -> r
toCardanoApiBoundedRational (MaestroRational r) = C.unsafeBoundedRational r

toLovelace :: AsAda -> C.Lovelace
toLovelace (AsAda (AsLovelace l)) = L.Coin . toInteger $ l

toCardanoApiProtVer :: ProtocolVersion -> L.ProtVer
toCardanoApiProtVer ProtocolVersion{protocolVersionMajor, protocolVersionMinor} =
  L.ProtVer
    { L.pvMajor = fromMaybe (error "Invalid protocol version major") (L.mkVersion protocolVersionMajor)
    , L.pvMinor = protocolVersionMinor
    }

toCardanoApiSystemStart :: LocalTime -> C.SystemStart
toCardanoApiSystemStart = C.SystemStart . Time.localTimeToUTC Time.utc

toLedgerCostModel :: Maestro.CostModels -> CostModels.CostModels
toLedgerCostModel
  Maestro.CostModels{Maestro.costModelsPlutusV1, Maestro.costModelsPlutusV2, Maestro.costModelsPlutusV3} =
    CostModels.mkCostModels $
      M.fromList
        [
          ( Plutus.Language.PlutusV1
          , either (error "Couldn't build PlutusV1 cost model") id $ CostModels.mkCostModel Plutus.Language.PlutusV1 $ coerce @_ @[Int64] costModelsPlutusV1
          )
        ,
          ( Plutus.Language.PlutusV2
          , either (error "Couldn't build PlutusV2 cost model") id $ CostModels.mkCostModel Plutus.Language.PlutusV2 $ coerce @_ @[Int64] costModelsPlutusV2
          )
        ,
          ( Plutus.Language.PlutusV3
          , either (error "Couldn't build PlutusV3 cost model") id $ CostModels.mkCostModel Plutus.Language.PlutusV3 $ coerce @_ @[Int64] costModelsPlutusV3
          )
        ]

toCardanoApiSlotNo :: Maestro.SlotNo -> C.SlotNo
toCardanoApiSlotNo (Maestro.SlotNo n) = C.SlotNo n

toCardanoApiProtocolParams :: ProtocolParameters -> L.PParams LCurrentEra
toCardanoApiProtocolParams
  ProtocolParameters
    { protocolParametersCollateralPercentage
    , protocolParametersConstitutionalCommitteeMaxTermLength
    , protocolParametersConstitutionalCommitteeMinSize
    , protocolParametersDelegateRepresentativeDeposit
    , protocolParametersDelegateRepresentativeMaxIdleTime
    , protocolParametersDelegateRepresentativeVotingThresholds
    , protocolParametersDesiredNumberOfStakePools
    , protocolParametersGovernanceActionDeposit
    , protocolParametersGovernanceActionLifetime
    , protocolParametersMaxBlockBodySize
    , protocolParametersMaxBlockHeaderSize
    , protocolParametersMaxCollateralInputs
    , protocolParametersMaxExecutionUnitsPerBlock
    , protocolParametersMaxExecutionUnitsPerTransaction
    , protocolParametersMaxTransactionSize
    , protocolParametersMaxValueSize
    , protocolParametersMinFeeCoefficient
    , protocolParametersMinFeeConstant
    , protocolParametersMinFeeReferenceScripts
    , protocolParametersMinStakePoolCost
    , protocolParametersMinUtxoDepositCoefficient
    , protocolParametersMonetaryExpansion
    , protocolParametersPlutusCostModels
    , protocolParametersScriptExecutionPrices
    , protocolParametersStakeCredentialDeposit
    , protocolParametersStakePoolDeposit
    , protocolParametersStakePoolPledgeInfluence
    , protocolParametersStakePoolRetirementEpochBound
    , protocolParametersStakePoolVotingThresholds
    , protocolParametersTreasuryExpansion
    , protocolParametersVersion
    } =
    L.PParams
      ( L.emptyPParamsIdentity @LCurrentEra
          & L.hkdMinFeeAL .~ L.Coin (fromIntegral protocolParametersMinFeeCoefficient)
          & L.hkdMinFeeBL .~ toCardanoApiCoin protocolParametersMinFeeConstant
          & L.hkdMaxBBSizeL .~ fromIntegral (asBytesBytes protocolParametersMaxBlockBodySize)
          & L.hkdMaxTxSizeL .~ fromIntegral (asBytesBytes protocolParametersMaxTransactionSize)
          & L.hkdMaxBHSizeL .~ fromIntegral (asBytesBytes protocolParametersMaxBlockHeaderSize)
          & L.hkdKeyDepositL .~ toCardanoApiCoin protocolParametersStakeCredentialDeposit
          & L.hkdPoolDepositL .~ toCardanoApiCoin protocolParametersStakePoolDeposit
          & L.hkdEMaxL .~ L.EpochInterval (fromIntegral protocolParametersStakePoolRetirementEpochBound)
          & L.hkdNOptL .~ fromIntegral protocolParametersDesiredNumberOfStakePools
          & L.hkdA0L .~ toCardanoApiBoundedRational protocolParametersStakePoolPledgeInfluence
          & L.hkdRhoL .~ toCardanoApiBoundedRational protocolParametersMonetaryExpansion
          & L.hkdTauL .~ toCardanoApiBoundedRational protocolParametersTreasuryExpansion
          & L.hkdMinPoolCostL .~ toCardanoApiCoin protocolParametersMinStakePoolCost
          & L.hkdCostModelsL .~ toLedgerCostModel protocolParametersPlutusCostModels
          & L.hkdPricesL
            .~ toPricesRational protocolParametersScriptExecutionPrices
          & L.hkdMaxTxExUnitsL
            .~ toExUnits protocolParametersMaxExecutionUnitsPerTransaction
          & L.hkdMaxBlockExUnitsL
            .~ toExUnits protocolParametersMaxExecutionUnitsPerBlock
          & L.hkdMaxValSizeL .~ fromIntegral (asBytesBytes protocolParametersMaxValueSize)
          & L.hkdCollateralPercentageL .~ fromIntegral protocolParametersCollateralPercentage
          & L.hkdMaxCollateralInputsL .~ fromIntegral protocolParametersMaxCollateralInputs
          & L.hkdCoinsPerUTxOByteL .~ L.CoinPerByte (L.Coin (fromIntegral protocolParametersMinUtxoDepositCoefficient)) -- UNSURE mapping
          -- Conway-specific values
          -- see note [Protocol Parameter Conversion]
          & L.hkdPoolVotingThresholdsL
            .~ L.PoolVotingThresholds
              { L.pvtMotionNoConfidence = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.stakePoolVotingThresholdsNoConfidence protocolParametersStakePoolVotingThresholds
              , L.pvtCommitteeNormal = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.constitutionalCommitteeDefault $ Maestro.stakePoolVotingThresholdsConstitutionalCommittee protocolParametersStakePoolVotingThresholds
              , L.pvtCommitteeNoConfidence = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.constitutionalCommitteeStateOfNoConfidence $ Maestro.stakePoolVotingThresholdsConstitutionalCommittee protocolParametersStakePoolVotingThresholds
              , L.pvtHardForkInitiation = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.stakePoolVotingThresholdsHardForkInitiation protocolParametersStakePoolVotingThresholds
              , L.pvtPPSecurityGroup = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.ppUpdateStakePoolSecurity $ Maestro.stakePoolVotingThresholdsProtocolParametersUpdate protocolParametersStakePoolVotingThresholds
              }
          & L.hkdDRepVotingThresholdsL
            .~ L.DRepVotingThresholds
              { L.dvtMotionNoConfidence = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.drepVotingThresholdsNoConfidence protocolParametersDelegateRepresentativeVotingThresholds
              , L.dvtCommitteeNormal = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.constitutionalCommitteeDefault $ Maestro.drepVotingThresholdsConstitutionalCommittee protocolParametersDelegateRepresentativeVotingThresholds
              , L.dvtCommitteeNoConfidence = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.constitutionalCommitteeStateOfNoConfidence $ Maestro.drepVotingThresholdsConstitutionalCommittee protocolParametersDelegateRepresentativeVotingThresholds
              , L.dvtUpdateToConstitution = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.drepVotingThresholdsConstitution protocolParametersDelegateRepresentativeVotingThresholds
              , L.dvtHardForkInitiation = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.drepVotingThresholdsHardForkInitiation protocolParametersDelegateRepresentativeVotingThresholds
              , L.dvtPPNetworkGroup = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.ppUpdateDrepNetwork $ Maestro.drepVotingThresholdsProtocolParametersUpdate protocolParametersDelegateRepresentativeVotingThresholds
              , L.dvtPPEconomicGroup = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.ppUpdateDrepEconomic $ Maestro.drepVotingThresholdsProtocolParametersUpdate protocolParametersDelegateRepresentativeVotingThresholds
              , L.dvtPPTechnicalGroup = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.ppUpdateDrepTechnical $ Maestro.drepVotingThresholdsProtocolParametersUpdate protocolParametersDelegateRepresentativeVotingThresholds
              , L.dvtPPGovGroup = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.ppUpdateDrepGovernance $ Maestro.drepVotingThresholdsProtocolParametersUpdate protocolParametersDelegateRepresentativeVotingThresholds
              , L.dvtTreasuryWithdrawal = C.unsafeBoundedRational $ Maestro.unMaestroRational $ Maestro.drepVotingThresholdsTreasuryWithdrawals protocolParametersDelegateRepresentativeVotingThresholds
              }
          & L.hkdCommitteeMinSizeL .~ fromIntegral protocolParametersConstitutionalCommitteeMinSize
          & L.hkdCommitteeMaxTermLengthL .~ BaseTypes.EpochInterval (fromIntegral protocolParametersConstitutionalCommitteeMaxTermLength)
          & L.hkdGovActionLifetimeL .~ BaseTypes.EpochInterval (fromIntegral protocolParametersGovernanceActionLifetime)
          & L.hkdGovActionDepositL .~ toLovelace protocolParametersGovernanceActionDeposit
          & L.hkdDRepDepositL .~ toLovelace protocolParametersDelegateRepresentativeDeposit
          & L.hkdDRepActivityL .~ BaseTypes.EpochInterval (fromIntegral protocolParametersDelegateRepresentativeMaxIdleTime)
          & L.hkdMinFeeRefScriptCostPerByteL .~ C.unsafeBoundedRational @BaseTypes.NonNegativeInterval (Maestro.minFeeReferenceScriptsBase protocolParametersMinFeeReferenceScripts)
      )
      & L.ppProtocolVersionL .~ toCardanoApiProtVer protocolParametersVersion

toLedgerEraBound :: Maestro.EraBound -> Ouroboros.Bound
toLedgerEraBound Maestro.EraBound{Maestro.eraBoundEpoch, Maestro.eraBoundSlot, Maestro.eraBoundTime} =
  Ouroboros.Bound
    { Ouroboros.boundTime = CTime.RelativeTime $ Maestro.eraBoundTimeSeconds eraBoundTime
    , Ouroboros.boundSlot = CSlot.SlotNo $ fromIntegral eraBoundSlot
    , Ouroboros.boundEpoch = CSlot.EpochNo $ fromIntegral eraBoundEpoch
    }

toLedgerEraParams :: Maestro.EraParameters -> Ouroboros.EraParams
toLedgerEraParams Maestro.EraParameters{Maestro.eraParametersEpochLength, Maestro.eraParametersSlotLength, Maestro.eraParametersSafeZone} =
  Ouroboros.EraParams
    { Ouroboros.eraEpochSize = CSlot.EpochSize $ fromIntegral eraParametersEpochLength
    , Ouroboros.eraSlotLength = CTime.mkSlotLength $ Maestro.epochSlotLengthMilliseconds eraParametersSlotLength / 1000
    , Ouroboros.eraSafeZone = Ouroboros.StandardSafeZone $ fromJust eraParametersSafeZone
    , Ouroboros.eraGenesisWin = fromIntegral $ fromJust eraParametersSafeZone -- TODO: Get it from provider? It is supposed to be 3k/f where k is security parameter (at present 2160) and f is active slot coefficient. Usually ledger set the safe zone size such that it guarantees at least k blocks...
    }

toLedgerEraSummary :: Maestro.EraSummary -> Ouroboros.EraSummary
toLedgerEraSummary Maestro.EraSummary{Maestro.eraSummaryStart, Maestro.eraSummaryEnd, Maestro.eraSummaryParameters} =
  Ouroboros.EraSummary
    { Ouroboros.eraStart = toLedgerEraBound eraSummaryStart
    , Ouroboros.eraEnd = maybe Ouroboros.EraUnbounded (Ouroboros.EraEnd . toLedgerEraBound) eraSummaryEnd
    , Ouroboros.eraParams = toLedgerEraParams eraSummaryParameters
    }
