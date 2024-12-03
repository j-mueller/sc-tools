{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-| Conversion between blockfrost and @cardano-api@ types
-}
module Convex.Blockfrost.Types(
  toLovelace,
  toQuantity,
  toPolicyId,
  toTxHash,
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
  -- * CBOR
  toCBORString,
  -- * Payment credential
  fromPaymentCredential,
  -- * Genesis related
  systemStart
) where

import qualified Blockfrost.Client.Cardano.Scripts     as Client
import           Blockfrost.Client.Types               (MonadBlockfrost)
import           Blockfrost.Types.Cardano.Addresses    (AddressUtxo (..))
import           Blockfrost.Types.Cardano.Scripts      (InlineDatum (..),
                                                        Script (..),
                                                        ScriptCBOR (..),
                                                        ScriptDatumCBOR (..),
                                                        ScriptType (..))
import           Blockfrost.Types.Cardano.Transactions (UtxoOutput (..))
import           Blockfrost.Types.Shared.Ada           (Lovelaces)
import           Blockfrost.Types.Shared.Address       (Address (..))
import           Blockfrost.Types.Shared.Amount        (Amount (..))
import           Blockfrost.Types.Shared.CBOR          (CBORString (..))
import Cardano.Slotting.Time (SystemStart(..))
import Blockfrost.Types.Cardano.Genesis (Genesis(..))
import           Blockfrost.Types.Shared.DatumHash     (DatumHash (..))
import           Blockfrost.Types.Shared.PolicyId      (PolicyId (..))
import           Blockfrost.Types.Shared.Quantity      (Quantity (..))
import           Blockfrost.Types.Shared.ScriptHash    (ScriptHash (..))
import           Blockfrost.Types.Shared.TxHash        (TxHash (..))
import           Cardano.Api                           (HasTypeProxy (..))
import qualified Cardano.Api.Ledger                    as C.Ledger
import           Cardano.Api.SerialiseBech32           (SerialiseAsBech32 (..))
import Data.Time.Clock.POSIX qualified as Clock
import           Cardano.Api.SerialiseUsing            (UsingRawBytesHex (..))
import           Cardano.Api.Shelley                   (Lovelace)
import qualified Cardano.Api.Shelley                   as C
import           Cardano.Binary                        (DecoderError)
import           Cardano.Ledger.Binary.Encoding        (EncCBOR)
import qualified Cardano.Ledger.Binary.Version         as Version
import           Control.Applicative                   (Alternative (..))
import           Control.Lens                          (_4, (&), (.~), (<&>))
import           Control.Monad.Except                  (MonadError (..),
                                                        runExceptT, throwError)
import           Control.Monad.Trans.Class             (lift)
import qualified Convex.CardanoApi.Lenses              as L
import           Convex.Utils                          (inBabbage)
import qualified Data.ByteString.Base16                as Base16
import qualified Data.ByteString.Lazy                  as BSL
import           Data.Coerce                           (Coercible, coerce)
import           Data.Maybe                            (fromMaybe)
import           Data.Proxy                            (Proxy (..))
import           Data.String                           (IsString (..))
import qualified Data.Text                             as Text
import qualified Data.Text.Encoding                    as Text.Encoding
import qualified GHC.IsList                            as L
import qualified Money

toLovelace :: Lovelaces -> Lovelace
toLovelace = C.Ledger.Coin . toInteger

toQuantity :: Quantity -> C.Quantity
toQuantity = coerce

toPolicyId :: PolicyId -> C.PolicyId
toPolicyId = textToIsString

toTxHash :: TxHash -> C.TxId
toTxHash = textToIsString

textToIsString :: (Coercible a Text.Text, IsString b) => a -> b
textToIsString = fromString . Text.unpack . coerce

hexTextToByteString :: C.SerialiseAsRawBytes a => Text.Text -> a
hexTextToByteString t =
  let UsingRawBytesHex x = fromString (Text.unpack t)
  in x

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
  C.PaymentCredentialByKey key       -> Address $ C.serialiseToBech32 $ CustomBech32 key
  C.PaymentCredentialByScript script -> Address $ C.serialiseToBech32 $ CustomBech32 script

newtype CustomBech32 a = CustomBech32 a

instance C.HasTypeProxy a => C.HasTypeProxy (CustomBech32 a) where
  newtype AsType (CustomBech32 a) = CustomBech32Type (AsType a)
  proxyToAsType _proxy = CustomBech32Type (proxyToAsType Proxy)

instance C.SerialiseAsRawBytes a => C.SerialiseAsRawBytes (CustomBech32 a) where
  serialiseToRawBytes (CustomBech32 a) = C.serialiseToRawBytes a
  deserialiseFromRawBytes _asType = fmap CustomBech32 . C.deserialiseFromRawBytes (proxyToAsType Proxy)

-- The following two instances of @SerialiseAsBech32@ are used for generating payment credential queries that blockfrost understands
-- See: https://github.com/blockfrost/blockfrost-utils/blob/master/src/validation.ts#L109-L128
instance C.SerialiseAsBech32 (CustomBech32 (C.Hash C.PaymentKey)) where
  bech32PrefixFor _ = "addr_vkh"
  bech32PrefixesPermitted _ = ["addr_vkh"]

instance C.SerialiseAsBech32 (CustomBech32 C.ScriptHash) where
  bech32PrefixFor _ = "script"
  bech32PrefixesPermitted _ = ["script"]

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
  | Base16DecodeError ScriptType ScriptHash String
  | CBORError ScriptType ScriptHash DecoderError

decodeScriptCbor :: forall lang m. (MonadError ScriptResolutionFailure m, C.IsScriptLanguage lang) => ScriptType -> ScriptHash -> Text.Text -> m (C.Script lang)
decodeScriptCbor tp hsh text =
  either (throwError . Base16DecodeError tp hsh) pure (Base16.decode $ Text.Encoding.encodeUtf8 text)
  >>= either (throwError . CBORError tp hsh) pure . C.deserialiseFromCBOR (C.proxyToAsType $ Proxy @(C.Script lang))

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

{-| The 'SystemStart' value
-}
systemStart :: Genesis -> SystemStart
systemStart =
  SystemStart . Clock.posixSecondsToUTCTime . _genesisSystemStart