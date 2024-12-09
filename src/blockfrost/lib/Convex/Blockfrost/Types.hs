{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-| Conversion between blockfrost and @cardano-api@ types
-}
module Convex.Blockfrost.Types(
  toLovelace,
  toQuantity,
  toPolicyId,
  toTxHash,
  fromTxHash,
  toAssetId,
  toAddress,
  toStakeAddress,
  toDatum,
  toScriptHash,
  -- * Transaction outputs
  TxOutUnresolvedScript(..),
  utxoOutput,
  addressUtxo,
  addressUtxoTxIn,
  ScriptResolutionFailure(..),
  resolveScript,
  -- * Protocol parameters
  protocolParametersConway,
  -- * CBOR
  toCBORString,
  decodeTransactionCBOR,
  -- * Payment credential
  fromPaymentCredential,
  fromStakeAddress,
  -- * Genesis related
  systemStart,
  -- * Misc.
  poolId,
  eraSummary,
  slot,
  -- * API queries
  pagedStream
) where

import           Blockfrost.Client                              (CostModelsRaw (..),
                                                                 Epoch (..),
                                                                 EpochLength (..),
                                                                 NetworkEraBound (..),
                                                                 NetworkEraParameters (..),
                                                                 NetworkEraSummary (..),
                                                                 PoolId (..),
                                                                 ProtocolParams (..),
                                                                 Slot (..))
import qualified Blockfrost.Client                              as Client
import           Blockfrost.Client.Types                        (MonadBlockfrost)
import qualified Blockfrost.Client.Types                        as Types
import           Blockfrost.Types.Cardano.Addresses             (AddressUtxo (..))
import           Blockfrost.Types.Cardano.Genesis               (Genesis (..))
import           Blockfrost.Types.Cardano.Scripts               (InlineDatum (..),
                                                                 Script (..),
                                                                 ScriptCBOR (..),
                                                                 ScriptDatumCBOR (..),
                                                                 ScriptType (..))
import           Blockfrost.Types.Cardano.Transactions          (TransactionCBOR (..),
                                                                 UtxoOutput (..))
import           Blockfrost.Types.Shared.Ada                    (Lovelaces)
import           Blockfrost.Types.Shared.Address                (Address (..))
import           Blockfrost.Types.Shared.Amount                 (Amount (..))
import           Blockfrost.Types.Shared.CBOR                   (CBORString (..))
import           Blockfrost.Types.Shared.DatumHash              (DatumHash (..))
import           Blockfrost.Types.Shared.PolicyId               (PolicyId (..))
import           Blockfrost.Types.Shared.Quantity               (Quantity (..))
import           Blockfrost.Types.Shared.ScriptHash             (ScriptHash (..))
import           Blockfrost.Types.Shared.TxHash                 (TxHash (..))
import           Cardano.Api                                    (HasTypeProxy (..))
import qualified Cardano.Api.Ledger                             as C.Ledger
import qualified Cardano.Api.Ledger                             as L
import           Cardano.Api.SerialiseBech32                    (SerialiseAsBech32 (..))
import           Cardano.Api.SerialiseUsing                     (UsingRawBytesHex (..))
import           Cardano.Api.Shelley                            (Lovelace)
import qualified Cardano.Api.Shelley                            as C
import           Cardano.Binary                                 (DecoderError)
import qualified Cardano.Ledger.Alonzo.PParams                  as L
import qualified Cardano.Ledger.Babbage.PParams                 as L
import qualified Cardano.Ledger.BaseTypes                       as BaseTypes
import           Cardano.Ledger.Binary.Encoding                 (EncCBOR)
import qualified Cardano.Ledger.Binary.Version                  as Version
import qualified Cardano.Ledger.Conway.PParams                  as L
import           Cardano.Ledger.Core                            (PParams)
import qualified Cardano.Ledger.Plutus.CostModels               as CostModels
import qualified Cardano.Ledger.Plutus.Language                 as Plutus.Language
import           Cardano.Slotting.Slot                          (EpochSize (..))
import           Cardano.Slotting.Time                          (RelativeTime (..),
                                                                 SystemStart (..),
                                                                 mkSlotLength)
import           Control.Applicative                            (Alternative (..))
import           Control.Lens                                   (_4, (&), (.~),
                                                                 (<&>))
import           Control.Monad.Except                           (MonadError (..),
                                                                 runExceptT,
                                                                 throwError)
import           Control.Monad.Trans.Class                      (lift)
import qualified Convex.CardanoApi.Lenses                       as L
import           Convex.Utils                                   (inBabbage)
import qualified Data.ByteString.Base16                         as Base16
import qualified Data.ByteString.Lazy                           as BSL
import           Data.Coerce                                    (Coercible,
                                                                 coerce)
import           Data.Int                                       (Int64)
import qualified Data.Map                                       as Map
import           Data.Maybe                                     (fromMaybe,
                                                                 mapMaybe)
import           Data.Proxy                                     (Proxy (..))
import           Data.String                                    (IsString (..))
import qualified Data.Text                                      as Text
import qualified Data.Text.Encoding                             as Text.Encoding
import qualified Data.Time.Clock.POSIX                          as Clock
import qualified GHC.IsList                                     as L
import           GHC.Num.Natural                                (Natural)
import qualified Money
import qualified Ouroboros.Consensus.Block.Abstract             as Ouroboros
import           Ouroboros.Consensus.HardFork.History.EraParams (EraParams (..),
                                                                 SafeZone (StandardSafeZone))
import           Ouroboros.Consensus.HardFork.History.Summary   (Bound (..),
                                                                 EraEnd (..),
                                                                 EraSummary (..))
import           Ouroboros.Consensus.Shelley.Eras               (StandardConway)
import qualified Streaming.Prelude                              as S
import           Streaming.Prelude                              (Of, Stream)

toLovelace :: Lovelaces -> Lovelace
toLovelace = C.Ledger.Coin . toInteger

toQuantity :: Quantity -> C.Quantity
toQuantity = coerce

toPolicyId :: PolicyId -> C.PolicyId
toPolicyId = textToIsString

toTxHash :: TxHash -> C.TxId
toTxHash = textToIsString

fromTxHash :: C.TxId -> TxHash
fromTxHash = TxHash . C.serialiseToRawBytesHexText

textToIsString :: (Coercible a Text.Text, IsString b) => a -> b
textToIsString = fromString . Text.unpack . coerce

hexTextToByteString :: C.SerialiseAsRawBytes a => Text.Text -> a
hexTextToByteString t =
  let UsingRawBytesHex x = fromString (Text.unpack t)
  in x

quantity :: Quantity -> Natural
quantity (Quantity n) = fromInteger n

-- pool1axzm26vduyuxgw0x9ddh4vkvn7q5hyd558l0t9c08p556lf2zaj
poolId :: PoolId -> C.PoolId
poolId (PoolId text) =
  either (error . show) id $ C.deserialiseFromBech32 (proxyToAsType $ Proxy @(C.Hash C.StakePoolKey)) text

toAssetId :: Amount -> (C.AssetId, C.Quantity)
toAssetId = \case
  AdaAmount lvl -> (C.AdaAssetId, C.lovelaceToQuantity $ toLovelace lvl)
  AssetAmount disc ->
    -- concatenation of asset policy ID and hex-encoded asset_name
    let txt                     = Money.someDiscreteCurrency disc
        (policyText, assetName) = Text.splitAt 56 txt
        amount = Money.someDiscreteAmount disc
        -- TODO: We could also consider Money.someDiscreteScale
        --       but it looks like blockfrost just uses unitScale for native assets
    in (C.AssetId (textToIsString policyText) (hexTextToByteString assetName), C.Quantity amount)

toAddress :: C.IsCardanoEra era => Address -> Maybe (C.AddressInEra era)
toAddress (Address text) = C.deserialiseAddress (C.proxyToAsType Proxy) text

{-| Encode the 'C.PaymentCredential' as a blockfrost 'Address'
-}
-- NOTE: The payment credential is only 1/3 of the address (the other parts are network ID and stake credential).
--       However, blockfrost still accepts this as an argument for the "utxo at address" query
--       See https://github.com/blockfrost/blockfrost-haskell/issues/68
fromPaymentCredential :: C.PaymentCredential -> Address
fromPaymentCredential = \case
  C.PaymentCredentialByKey key       -> Address $ C.serialiseToBech32 $ CustomBech32Payment key
  C.PaymentCredentialByScript script -> Address $ C.serialiseToBech32 $ CustomBech32Payment script


newtype CustomBech32Payment a = CustomBech32Payment a

instance C.HasTypeProxy a => C.HasTypeProxy (CustomBech32Payment a) where
  newtype AsType (CustomBech32Payment a) = CustomBech32PaymentType (AsType a)
  proxyToAsType _proxy = CustomBech32PaymentType (proxyToAsType Proxy)

instance C.SerialiseAsRawBytes a => C.SerialiseAsRawBytes (CustomBech32Payment a) where
  serialiseToRawBytes (CustomBech32Payment a) = C.serialiseToRawBytes a
  deserialiseFromRawBytes _asType = fmap CustomBech32Payment . C.deserialiseFromRawBytes (proxyToAsType Proxy)

-- The following two instances of @SerialiseAsBech32@ are used for generating payment credential queries that blockfrost understands
-- See: https://github.com/blockfrost/blockfrost-utils/blob/master/src/validation.ts#L109-L128
instance C.SerialiseAsBech32 (CustomBech32Payment (C.Hash C.PaymentKey)) where
  bech32PrefixFor _ = "addr_vkh"
  bech32PrefixesPermitted _ = ["addr_vkh"]

instance C.SerialiseAsBech32 (CustomBech32Payment C.ScriptHash) where
  bech32PrefixFor _ = "script"
  bech32PrefixesPermitted _ = ["script"]

fromStakeAddress :: C.StakeAddress -> Address
fromStakeAddress = Address . C.serialiseToBech32

newtype CustomBech32Stake a = CustomBech32Stake a

instance C.HasTypeProxy a => C.HasTypeProxy (CustomBech32Stake a) where
  newtype AsType (CustomBech32Stake a) = CustomBech32StakeType (AsType a)
  proxyToAsType _proxy = CustomBech32StakeType (proxyToAsType Proxy)

instance C.SerialiseAsRawBytes a => C.SerialiseAsRawBytes (CustomBech32Stake a) where
  serialiseToRawBytes (CustomBech32Stake a) = C.serialiseToRawBytes a
  deserialiseFromRawBytes _asType = fmap CustomBech32Stake . C.deserialiseFromRawBytes (proxyToAsType Proxy)

-- The following two instances of @SerialiseAsBech32@ are used for generating payment credential queries that blockfrost understands
-- See: https://github.com/blockfrost/blockfrost-utils/blob/master/src/validation.ts#L109-L128
instance C.SerialiseAsBech32 (CustomBech32Stake (C.Hash C.StakeKey)) where
  bech32PrefixFor _ = "stake"
  bech32PrefixesPermitted _ = ["stake"]

instance C.SerialiseAsBech32 (CustomBech32Stake C.ScriptHash) where
  bech32PrefixFor _ = "stake"
  bech32PrefixesPermitted _ = ["stake"]

toStakeAddress :: Address -> Maybe C.StakeAddress
toStakeAddress (Address text) = C.deserialiseAddress (C.proxyToAsType Proxy) text

toDatumHash :: DatumHash -> C.Hash C.ScriptData
toDatumHash = textToIsString

toScriptHash :: ScriptHash -> C.ScriptHash
toScriptHash = textToIsString

toDatum :: InlineDatum -> Either DecoderError C.HashableScriptData
toDatum (InlineDatum (ScriptDatumCBOR text)) =
  C.deserialiseFromCBOR (C.proxyToAsType Proxy) (Text.Encoding.encodeUtf8 text)

{-| A @cardano-api@ tx output with a reference script that we only know
the hash of.

Note that the 'txuOutput' has its reference script set to 'C.ReferenceScriptNone'
while in reality (in the ledger's UTxO set) it has a reference script.

If you don't care about the reference script, it's okay to use the 'txuOutput'
as it is. If you do care about reference scripts, you need to get the 'C.Script'
for the given hash (possibly by performing a lookup using the blockfrost API)
and set the output's reference script field to the script.

-}
data TxOutUnresolvedScript era =
  TxOutUnresolvedScript
    { txuOutput     :: C.TxOut C.CtxUTxO era
    , txuScriptHash :: ScriptHash
    }

{-| Failed to resolve a script hash to a plutus script
-}
data ScriptResolutionFailure =
  ScriptNotFound ScriptHash
  | ScriptDecodingError ScriptType ScriptHash DecodingError
  deriving stock (Eq, Show)

data DecodingError
  = Base16DecodeError String
  | CBORError DecoderError
  deriving stock (Eq, Show)

decodeScriptCbor :: forall lang m. (MonadError ScriptResolutionFailure m, C.IsScriptLanguage lang) => ScriptType -> ScriptHash -> Text.Text -> m (C.Script lang)
decodeScriptCbor tp hsh text =
  either (throwError . ScriptDecodingError tp hsh . Base16DecodeError) pure (Base16.decode $ Text.Encoding.encodeUtf8 text)
  >>= either (throwError . ScriptDecodingError tp hsh . CBORError) pure . C.deserialiseFromCBOR (C.proxyToAsType $ Proxy @(C.Script lang))

{-| Load this output's reference script from blockfrost and return the full output
-}
resolveScript :: forall era m. (C.IsBabbageBasedEra era, MonadBlockfrost m) => TxOutUnresolvedScript era -> m (Either ScriptResolutionFailure (C.TxOut C.CtxUTxO era))
resolveScript TxOutUnresolvedScript{txuOutput, txuScriptHash} = runExceptT $ inBabbage @era $ do
  ScriptCBOR{_scriptCborCbor} <- lift (Client.getScriptCBOR txuScriptHash)
  case _scriptCborCbor of
    Nothing -> throwError $ ScriptNotFound txuScriptHash
    Just text -> do
      Script{_scriptType} <- lift (Client.getScript txuScriptHash) -- We need this call to figure out what language the script is
      refScript <- case _scriptType of
        PlutusV1 ->
          decodeScriptCbor _scriptType txuScriptHash text <&> C.ReferenceScript (C.babbageBasedEra @era) . C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1)
        PlutusV2 ->
          decodeScriptCbor _scriptType txuScriptHash text <&> C.ReferenceScript (C.babbageBasedEra @era) . C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2)
        PlutusV3 ->
          decodeScriptCbor _scriptType txuScriptHash text <&> C.ReferenceScript (C.babbageBasedEra @era) . C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3)
        Timelock ->
          decodeScriptCbor _scriptType txuScriptHash text <&> C.ReferenceScript (C.babbageBasedEra @era) . C.ScriptInAnyLang C.SimpleScriptLanguage
      return (txuOutput & L._TxOut . _4 .~ refScript)

{-| Convert a blockfrost 'UtxoOutput' to a @cardano-api@ 'C.TxOut C.CtxUTxO era',
returning 'TxOutUnresolvedScript' if the output has a reference script.
-}
utxoOutput ::
  forall era.
  ( C.IsBabbageBasedEra era
  )
  => UtxoOutput
  -> Either (TxOutUnresolvedScript era) (C.TxOut C.CtxUTxO era)
utxoOutput UtxoOutput{_utxoOutputAddress, _utxoOutputAmount, _utxoOutputDataHash, _utxoOutputInlineDatum, _utxoOutputReferenceScriptHash} =
  convertOutput _utxoOutputAddress _utxoOutputAmount _utxoOutputDataHash _utxoOutputInlineDatum _utxoOutputReferenceScriptHash

convertOutput :: forall era. (C.IsBabbageBasedEra era) => Address -> [Amount] -> Maybe DatumHash -> Maybe InlineDatum -> Maybe ScriptHash -> Either (TxOutUnresolvedScript era) (C.TxOut C.CtxUTxO era)
convertOutput addr_ amount dataHash inlineDatum refScriptHash = inBabbage @era $
  let addr = fromMaybe (error "utxoOutput: Unable to deserialise address") $ toAddress @era addr_
      val  = C.TxOutValueShelleyBased
              C.shelleyBasedEra
              (C.toLedgerValue @era C.maryBasedEra $ foldMap (L.fromList . return . toAssetId) amount)
      dat  = fmap (C.TxOutDatumHash  C.alonzoBasedEra . toDatumHash) dataHash
              <|> fmap (C.TxOutDatumInline C.babbageBasedEra) (inlineDatum >>= either (const Nothing) Just . toDatum)

      txuOutput = C.TxOut addr val (fromMaybe C.TxOutDatumNone dat) C.ReferenceScriptNone

  in case refScriptHash of
      Nothing -> Right txuOutput
      Just txuScriptHash ->
        Left TxOutUnresolvedScript{txuOutput, txuScriptHash}

{-| The utxo reference 'C.TxIn' of the 'AddressUtxo'
-}
addressUtxoTxIn :: AddressUtxo -> C.TxIn
addressUtxoTxIn AddressUtxo{_addressUtxoTxHash, _addressUtxoOutputIndex} =
  C.TxIn (toTxHash _addressUtxoTxHash) (C.TxIx $ fromIntegral _addressUtxoOutputIndex)

{-| Convert a blockfrost 'AddressUtxo' to a @cardano-api@ 'C.TxOut C.CtxUTxO era',
returning 'TxOutUnresolvedScript' if the output has a reference script.
-}
addressUtxo ::
  forall era.
  ( C.IsBabbageBasedEra era
  )
  => AddressUtxo
  -> Either (TxOutUnresolvedScript era) (C.TxOut C.CtxUTxO era)
addressUtxo AddressUtxo{_addressUtxoAddress, _addressUtxoAmount, _addressUtxoDataHash, _addressUtxoInlineDatum, _addressUtxoReferenceScriptHash} =
  convertOutput _addressUtxoAddress _addressUtxoAmount _addressUtxoDataHash _addressUtxoInlineDatum _addressUtxoReferenceScriptHash

{-| Serialise a value to CBOR
-}
toCBORString :: EncCBOR v => v -> CBORString
toCBORString = CBORString . BSL.fromStrict . C.Ledger.serialize' Version.shelleyProtVer

{-| Decode a full transaction from a CBOR hex string
-}
decodeTransactionCBOR :: MonadError DecodingError m => TransactionCBOR -> m (C.Tx C.ConwayEra)
decodeTransactionCBOR TransactionCBOR{_transactionCBORCbor} =
  either (throwError . Base16DecodeError) pure (Base16.decode $ Text.Encoding.encodeUtf8 _transactionCBORCbor)
  >>= either (throwError . CBORError) pure . C.deserialiseFromCBOR (C.proxyToAsType $ Proxy @(C.Tx C.ConwayEra))

{-| The 'SystemStart' value
-}
systemStart :: Genesis -> SystemStart
systemStart =
  SystemStart . Clock.posixSecondsToUTCTime . _genesisSystemStart

{-| Stream a list of results from a paged query
-}
pagedStream :: Monad m => (Types.Paged -> m [a]) -> Stream (Of a) m ()
pagedStream action = flip S.for S.each $ flip S.unfoldr 1 $ \pageNumber -> do
  let paged = Client.Paged{Client.countPerPage = 100, Client.pageNumber = pageNumber}
  action paged >>= \case
    [] -> pure (Left ())
    xs -> pure (Right (xs, succ pageNumber))

slot :: Slot -> C.SlotNo
slot (Slot n) = C.SlotNo (fromInteger n)

epoch :: Epoch -> C.EpochNo
epoch (Epoch e) = C.EpochNo (fromInteger e)

epochSize :: EpochLength -> EpochSize
epochSize = coerce

eraSummary :: NetworkEraSummary -> EraSummary
eraSummary NetworkEraSummary{_networkEraStart, _networkEraEnd, _networkEraParameters} =
  let eraEnd =
        let NetworkEraBound{_boundEpoch, _boundSlot, _boundTime} = _networkEraEnd
        in EraEnd Bound{boundTime = RelativeTime _boundTime, boundSlot = slot _boundSlot, boundEpoch = epoch _boundEpoch}
      eraStart =
        let NetworkEraBound{_boundEpoch, _boundSlot, _boundTime} = _networkEraStart
        in Bound{boundTime = RelativeTime _boundTime, boundSlot = slot _boundSlot, boundEpoch = epoch _boundEpoch}
      eraParams =
        let NetworkEraParameters{_parametersEpochLength, _parametersSlotLength, _parametersSafeZone} = _networkEraParameters
        in EraParams
            { eraEpochSize = epochSize _parametersEpochLength
            , eraSlotLength = mkSlotLength _parametersSlotLength
            , eraSafeZone = StandardSafeZone _parametersSafeZone
            , eraGenesisWin = Ouroboros.GenesisWindow (2 * 2160) -- 2 * max-rollbacks
            }
  in EraSummary
      { eraEnd
      , eraStart
      , eraParams
      }

costModels :: CostModelsRaw -> L.CostModels
costModels =
  let unsafeMkCostModel :: Plutus.Language.Language -> [Int64] -> CostModels.CostModel
      unsafeMkCostModel lang = either (error . show) id . CostModels.mkCostModel lang
      mkModel (scriptType, cost) = do
        l <- plutusLanguage scriptType
        pure (l, unsafeMkCostModel l (fromInteger <$> cost))
  in
      CostModels.mkCostModels
      . Map.fromList
      . mapMaybe mkModel
      . Map.toList
      . unCostModelsRaw

plutusLanguage :: ScriptType -> Maybe Plutus.Language.Language
plutusLanguage = \case
  PlutusV1 -> Just Plutus.Language.PlutusV1
  PlutusV2 -> Just Plutus.Language.PlutusV2
  PlutusV3 -> Just Plutus.Language.PlutusV3
  Timelock -> Nothing

{- Note [Protocol Parameter Conversion]

The protocol parameters type varies from era to era.

Blockfrost captures all possible protocol parameters in a single 'ProtocolParams'
type. In the Conway era, a number of fields were added to the protocol
parameters that appear as optional ('Maybe') in the 'ProtocolParams' but are
in fact mandatory. Some examples are "min ref script cost per byte" and
the drep / pool voting related parameters.

When converting from 'ProtocolParams' to conway-era params, if one of those
mandatory fields is missing, we use the default from the conway genesis file
on mainnet.

-}

{-| Convert the 'ProtocolParams' to conway-era ledger params.
See note [Protocol Parameter Conversion]
-}
protocolParametersConway :: ProtocolParams -> PParams StandardConway
protocolParametersConway pp =
  let votingThresholdFromRational = C.unsafeBoundedRational @BaseTypes.UnitInterval . fromMaybe 0.51 in
  L.PParams $
    L.emptyPParamsIdentity @StandardConway
      & L.hkdMinFeeAL .~ L.Coin (_protocolParamsMinFeeA pp)
      & L.hkdMinFeeBL .~ L.Coin (_protocolParamsMinFeeB pp)
      & L.hkdMaxBBSizeL .~ fromInteger (_protocolParamsMaxBlockSize pp)
      & L.hkdMaxTxSizeL .~ fromInteger (_protocolParamsMaxTxSize pp)
      & L.hkdMaxBHSizeL .~ fromInteger (_protocolParamsMaxBlockHeaderSize  pp)
      & L.hkdKeyDepositL .~ toLovelace (_protocolParamsKeyDeposit  pp)
      & L.hkdPoolDepositL .~ toLovelace (_protocolParamsKeyDeposit  pp)
      & L.hkdEMaxL .~ L.EpochInterval (fromInteger (_protocolParamsEMax pp))
      & L.hkdNOptL .~ fromInteger (_protocolParamsNOpt  pp)
      & L.hkdA0L .~ C.unsafeBoundedRational (_protocolParamsA0 pp) -- TODO: Is unsafeBoundedRational ok to use here?
      & L.hkdRhoL .~ C.unsafeBoundedRational (_protocolParamsRho pp)
      & L.hkdTauL .~ C.unsafeBoundedRational (_protocolParamsTau pp)
      -- & L.hkdDL .~ _ (_protocolParamsDecentralisationParam pp)
      -- & L.hkdExtraEntropyL .~
      --     maybe BaseTypes.NeutralNonce (BaseTypes.Nonce . _) (_protocolParamsExtraEntropy pp)
      -- & L.hkdExtraEntropyL .~ _ (_protocolParamsExtraEntropy pp)
      -- & L.ppProtocolVersionL .~
      --     L.ProtVer
      --       { L.pvMajor = _ (_protocolParamsProtocolMajorVer  pp)
      --       , L.pvMinor = _ (_protocolParamsProtocolMinorVer  pp)
      --       }
      & L.hkdMinPoolCostL .~ toLovelace (_protocolParamsMinPoolCost pp)
      & L.hkdCostModelsL .~ costModels (_protocolParamsCostModelsRaw pp)
      & L.hkdPricesL .~ L.Prices
          { L.prMem   = C.unsafeBoundedRational (_protocolParamsPriceMem pp)
          , L.prSteps = C.unsafeBoundedRational (_protocolParamsPriceStep  pp)
          }
      & L.hkdMaxTxExUnitsL .~ L.ExUnits
          { L.exUnitsSteps = quantity (_protocolParamsMaxTxExSteps  pp)
          , L.exUnitsMem = quantity (_protocolParamsMaxTxExMem pp)
          }
      & L.hkdMaxBlockExUnitsL .~ L.ExUnits
          { L.exUnitsSteps = quantity (_protocolParamsMaxBlockExSteps pp)
          , L.exUnitsMem = quantity (_protocolParamsMaxBlockExMem pp)
          }
      & L.hkdMaxValSizeL .~ quantity (_protocolParamsMaxValSize pp)
      & L.hkdCollateralPercentageL .~ fromInteger (_protocolParamsCollateralPercent pp)
      & L.hkdMaxCollateralInputsL .~ fromInteger (_protocolParamsMaxCollateralInputs pp)
      & L.hkdCoinsPerUTxOByteL .~ L.CoinPerByte (toLovelace (_protocolParamsCoinsPerUtxoSize pp))

      -- Conway-specific values
      -- see note [Protocol Parameter Conversion]
      & L.hkdPoolVotingThresholdsL .~
          L.PoolVotingThresholds
            { L.pvtMotionNoConfidence    = votingThresholdFromRational (_protocolParamsPvtMotionNoConfidence pp)
            , L.pvtCommitteeNormal       = votingThresholdFromRational (_protocolParamsPvtCommitteeNormal pp)
            , L.pvtCommitteeNoConfidence = votingThresholdFromRational (_protocolParamsPvtCommitteeNoConfidence pp)
            , L.pvtHardForkInitiation    = votingThresholdFromRational (_protocolParamsPvtHardForkInitiation pp)
            , L.pvtPPSecurityGroup       = votingThresholdFromRational (_protocolParamsPvtppSecurityGroup pp)
            }
      & L.hkdDRepVotingThresholdsL .~
          L.DRepVotingThresholds
            { L.dvtMotionNoConfidence    = votingThresholdFromRational (_protocolParamsDvtMotionNoConfidence pp)
            , L.dvtCommitteeNormal       = votingThresholdFromRational (_protocolParamsDvtCommitteeNormal pp)
            , L.dvtCommitteeNoConfidence = votingThresholdFromRational (_protocolParamsDvtCommitteeNoConfidence pp)
            , L.dvtUpdateToConstitution  = votingThresholdFromRational (_protocolParamsDvtUpdateToConstitution pp)
            , L.dvtHardForkInitiation    = votingThresholdFromRational (_protocolParamsDvtHardForkInitiation pp)
            , L.dvtPPNetworkGroup        = votingThresholdFromRational (_protocolParamsDvtPPNetworkGroup pp)
            , L.dvtPPEconomicGroup       = votingThresholdFromRational (_protocolParamsDvtPPEconomicGroup pp)
            , L.dvtPPTechnicalGroup      = votingThresholdFromRational (_protocolParamsDvtPPTechnicalGroup pp)
            , L.dvtPPGovGroup            = votingThresholdFromRational (_protocolParamsDvtPPGovGroup pp)
            , L.dvtTreasuryWithdrawal    = votingThresholdFromRational (_protocolParamsDvtTreasuryWithdrawal pp)
            }
      & L.hkdCommitteeMinSizeL .~ maybe 7 quantity (_protocolParamsCommitteeMinSize pp)
      & L.hkdCommitteeMaxTermLengthL .~ BaseTypes.EpochInterval (maybe 146 (fromIntegral . quantity) (_protocolParamsCommitteeMaxTermLength pp))
      & L.hkdGovActionLifetimeL .~ BaseTypes.EpochInterval (maybe 6 (fromIntegral . quantity) (_protocolParamsGovActionLifetime pp))
      & L.hkdGovActionDepositL .~ maybe 100_000_000_000 toLovelace (_protocolParamsGovActionDeposit pp)
      & L.hkdDRepDepositL .~ maybe 500_000_000 toLovelace (_protocolParamsDrepDeposit pp)
      & L.hkdDRepActivityL .~ BaseTypes.EpochInterval (maybe 20 (fromIntegral . quantity)  (_protocolParamsDrepActivity pp))
      & L.hkdMinFeeRefScriptCostPerByteL .~ C.unsafeBoundedRational (fromMaybe 15 (_protocolParamsMinFeeRefScriptCostPerByte pp))
