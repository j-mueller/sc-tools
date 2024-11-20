{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
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
  toTransactionOutput
) where

import           Blockfrost.Types.Cardano.Scripts      (InlineDatum (..),
                                                        ScriptDatumCBOR (..))
import           Blockfrost.Types.Cardano.Transactions (UtxoOutput (..))
import           Blockfrost.Types.Shared.Ada           (Lovelaces)
import           Blockfrost.Types.Shared.Address       (Address (..))
import           Blockfrost.Types.Shared.Amount        (Amount (..))
import           Blockfrost.Types.Shared.DatumHash     (DatumHash (..))
import           Blockfrost.Types.Shared.PolicyId      (PolicyId (..))
import           Blockfrost.Types.Shared.Quantity      (Quantity (..))
import           Blockfrost.Types.Shared.ScriptHash    (ScriptHash (..))
import           Blockfrost.Types.Shared.TxHash        (TxHash (..))
import qualified Cardano.Api.Ledger                    as C.Ledger
import           Cardano.Api.Shelley                   (Lovelace)
import qualified Cardano.Api.Shelley                   as C
import           Cardano.Binary                        (DecoderError)
import           Control.Applicative                   (Alternative (..))
import           Convex.Utils                          (inBabbage)
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

toAssetId :: Amount -> (C.AssetId, C.Quantity)
toAssetId = \case
  AdaAmount lvl -> (C.AdaAssetId, C.lovelaceToQuantity $ toLovelace lvl)
  AssetAmount disc ->
    -- concatenation of asset policy ID and hex-encoded asset_name
    let (policyText, assetName) = Text.splitAt 56 (Money.someDiscreteCurrency disc)
        amount = Money.someDiscreteAmount disc
        -- TODO: We could also consider Money.someDiscreteScale
        --       but it looks like blockfrost just uses unitScale for native assets
    in (C.AssetId (textToIsString policyText) (textToIsString assetName), C.Quantity amount)

toAddress :: C.IsCardanoEra era => Address -> Maybe (C.AddressInEra era)
toAddress (Address text) = C.deserialiseAddress (C.proxyToAsType Proxy) text

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

{-| Convert a blockfrost 'UtxoOutput' to a @cardano-api@ 'C.TxOut C.CtxUTxO era',
returning 'TxOutUnresolvedScript' if the output has a reference script.
-}
toTransactionOutput ::
  forall era.
  ( C.IsBabbageBasedEra era
  )
  => UtxoOutput
  -> Either (TxOutUnresolvedScript era) (C.TxOut C.CtxUTxO era)
toTransactionOutput output = inBabbage @era $
  let UtxoOutput{_utxoOutputAddress, _utxoOutputAmount, _utxoOutputDataHash, _utxoOutputInlineDatum, _utxoOutputReferenceScriptHash} = output
      addr = fromMaybe (error "toTransactionOutput: Unable to deserialise address") $ toAddress @era _utxoOutputAddress
      val  = C.TxOutValueShelleyBased
              C.shelleyBasedEra
              (C.toLedgerValue @era C.maryBasedEra $ foldMap (L.fromList . return . toAssetId) _utxoOutputAmount)
      dat  = fmap (C.TxOutDatumHash  C.alonzoBasedEra . toDatumHash) _utxoOutputDataHash
              <|> fmap (C.TxOutDatumInline C.babbageBasedEra) (_utxoOutputInlineDatum >>= either (const Nothing) Just . toDatum)

      txuOutput = C.TxOut addr val (fromMaybe C.TxOutDatumNone dat) C.ReferenceScriptNone

  in case _utxoOutputReferenceScriptHash of
      Nothing -> Right txuOutput
      Just txuScriptHash ->
        Left TxOutUnresolvedScript{txuOutput, txuScriptHash}
