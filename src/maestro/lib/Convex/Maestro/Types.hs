{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Convex.Maestro.Types (
  poolId,
  toMaestroTxIn,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Binary (DecoderError (DecoderErrorCustom))

-- or Babbage
import Cardano.Ledger.Binary qualified as L
import Cardano.Ledger.Binary.Plain (decodeFullDecoder)
import Cardano.Ledger.Conway qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.Data (Proxy (..))
import Data.Text.Encoding qualified as Text.Encoding
import Data.Typeable (Typeable)
import Maestro.Types.Common (Bech32StringOf (Bech32StringOf), HexStringOf (..))
import Maestro.Types.V1 (OutputReference (..))
import Maestro.Types.V1.Common (PoolId, TxHash (..))
import Maestro.Types.V1.Transactions (UtxoWithBytes (..))

data DecodingError
  = Base16DecodeError String
  | CBORError Cardano.Binary.DecoderError
  deriving stock (Eq, Show)

-- | Convert a 'Bech32StringOf PoolId' to a 'Cardano.Api.PoolId'
poolId :: Bech32StringOf PoolId -> C.PoolId
poolId (Bech32StringOf text) =
  either (error . show) id $ C.deserialiseFromBech32 text

-- | Convert a 'Cardano.Api.TxIn' to a Maestro 'OutputReference'
toMaestroTxIn :: C.TxIn -> OutputReference
toMaestroTxIn (C.TxIn txId (C.TxIx txIx)) = OutputReference (TxHash (C.serialiseToRawBytesHexText txId)) (fromIntegral txIx)

-- convertOutput :: forall era. (C.IsBabbageBasedEra era) => Address -> [Amount] -> Maybe DatumHash -> Maybe InlineDatum -> Maybe ScriptHash -> Either (TxOutUnresolvedScript era) (C.TxOut C.CtxUTxO era)
-- convertOutput addr_ amount dataHash inlineDatum refScriptHash =
--   inBabbage @era $
--     let addr = fromMaybe (error "utxoOutput: Unable to deserialise address") $ toAddress @era addr_
--         val =
--           C.TxOutValueShelleyBased
--             C.shelleyBasedEra
--             (C.toLedgerValue @era C.maryBasedEra $ foldMap (L.fromList . return . toAssetId) amount)
--         inlinedat = fmap (C.TxOutDatumInline C.babbageBasedEra) (inlineDatum >>= either (const Nothing) Just . toDatum)
--         datumhash = fmap (C.TxOutDatumHash C.alonzoBasedEra . toDatumHash) dataHash
--         dat = inlinedat <|> datumhash
--         txuOutput = C.TxOut addr val (fromMaybe C.TxOutDatumNone dat) C.ReferenceScriptNone
--      in case refScriptHash of
--           Nothing -> Right txuOutput
--           Just txuScriptHash ->
--             Left TxOutUnresolvedScript{txuOutput, txuScriptHash}

instance (Typeable ctx) => L.FromCBOR (C.TxOut ctx C.ConwayEra) where
  fromCBOR = C.fromShelleyTxOut C.ShelleyBasedEraConway <$> L.fromCBOR

instance L.ToCBOR (C.TxOut C.CtxUTxO C.ConwayEra) where
  toCBOR = L.toCBOR . C.toShelleyTxOut C.ShelleyBasedEraConway

decodeTxOutCbor
  :: forall era ctx
   . (L.FromCBOR era)
  => C.ShelleyBasedEra era
  -> BS.ByteString -- hex text bytes (without 0x), e.g. from HexStringOf
  -> Either DecodingError (C.TxOut C.CtxUTxO era)
decodeTxOutCbor sbe hexText = do
  raw <- first (Base16DecodeError . show) (Base16.decode hexText)

  -- Decode a ledger TxOut for that era
  ledgerTxOut :: (C.TxOut C.CtxUTxO era) <- either (throwError . CBORError) pure $ decodeFullDecoder "TxOut" L.fromCBOR (BSL.fromStrict raw)
  error "Not implemented"

-- ledgerTxOut <- either (throwError . CBORError) (fromCBOR (BSL.fromStrict raw))
-- first CBORError $
--   L.decodeFullDecoder
--     "TxOut"
--     (L.fromCBOR @(Ledger.TxOut (C.ShelleyLedgerEra era)))
--     pv
--     raw
-- -- Convert to Cardano.Api
-- utxo <- C.fromLedgerUTxO sbe ledgerTxOut
-- pure (C.fromLedgerUTxO sbe ledgerTxOut)

toCardanoApiUTxO :: forall era m. (MonadError DecodingError m) => UtxoWithBytes -> m (C.TxOut C.CtxUTxO era)
toCardanoApiUTxO UtxoWithBytes{utxoWithBytesAddress, utxoWithBytesAssets, utxoWithBytesDatum, utxoWithBytesIndex, utxoWithBytesReferenceScript, utxoWithBytesTxHash, utxoWithBytesTxoutCbor} =
  error "Not implemented"

-- maybe (throwError (Base16DecodeError "Could not decode base16")) pure utxoWithBytesTxoutCbor >>= \(HexStringOf txoutCbor) ->
--   either (throwError . Base16DecodeError) pure (Base16.decode $ Text.Encoding.encodeUtf8 txoutCbor)
--   >>= either (throwError . CBORError) pure . C.deserialiseFromCBOR (C.proxyToAsType $ Proxy @(C.TxOut C.CtxUTxO era))
