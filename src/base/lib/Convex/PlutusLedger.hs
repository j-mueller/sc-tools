{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-| Translating between cardano-api/cardano-ledger and plutus representations
-}
module Convex.PlutusLedger(
  -- * Script hashes
  transScriptHash,
  unTransScriptHash,
  unTransScriptDataHash,
  unTransTxOutDatumHash,

  -- * Key hashes
  transPubKeyHash,
  unTransPubKeyHash,

  transStakeKeyHash,
  unTransStakeKeyHash,

  transStakePoolKeyHash,
  unTransStakePoolKeyHash,

  -- * Asset names
  transAssetName,
  toMaryAssetName,
  unTransAssetName,

  transPolicyId,
  unTransPolicyId,

  transAssetId,
  unTransAssetId,

  -- * Credentials and addresses
  transCredential,
  unTransCredential,

  transStakeCredential,
  unTransStakeCredential,

  transStakeAddressReference,
  unTransStakeAddressReference,

  unTransAddressInEra,
  transAddressInEra,

  -- * Tx IDs
  unTransTxOutRef,
  transTxOutRef,

  -- * POSIX Time
  unTransPOSIXTime,
  transPOSIXTime,

  -- * Value
  unTransTxOutValue,

  transValue,
  unTransValue,

  -- * Scripts
  unTransPlutusScript,

) where

import qualified Cardano.Api.Shelley        as C
import           Cardano.Ledger.BaseTypes   (CertIx (..), TxIx (..))
import           Cardano.Ledger.Credential  (Ptr (..))
import qualified Cardano.Ledger.Mary.Value  as Mary (AssetName (..))
import qualified Codec.Serialise            as Codec
import qualified Data.ByteString.Lazy       as BSL
import           Data.ByteString.Short      (fromShort)
import qualified Data.ByteString.Short      as Short
import           Data.Functor               ((<&>))
import           Data.Time.Clock.POSIX      (POSIXTime)
import           PlutusLedgerApi.Common     (SerialisedScript)
import qualified PlutusLedgerApi.V1         as PV1
import qualified PlutusLedgerApi.V1.Scripts as P
import qualified PlutusLedgerApi.V1.Value   as Value
import qualified PlutusTx.AssocMap          as Map
import qualified PlutusTx.Prelude           as PlutusTx

-- | Translate a script hash from @cardano-api@ to @plutus@
transScriptHash :: C.ScriptHash -> PV1.ScriptHash
transScriptHash h = PV1.ScriptHash (PV1.toBuiltin (C.serialiseToRawBytes h)) -- TODO: is serialiseToRawBytes the correct thing to do here?

-- | Translate a script hash from @plutus@ to @cardano-api@
unTransScriptHash :: PV1.ScriptHash -> Either C.SerialiseAsRawBytesError C.ScriptHash
unTransScriptHash (PV1.ScriptHash vh) =
  C.deserialiseFromRawBytes C.AsScriptHash $ PlutusTx.fromBuiltin vh

-- | Translate an asset name from @cardano-api@ to @plutus@
transAssetName :: Mary.AssetName -> PV1.TokenName
transAssetName (Mary.AssetName bs) = PV1.TokenName (PV1.toBuiltin (fromShort bs))

unTransAssetName :: PV1.TokenName -> C.AssetName
unTransAssetName (PV1.TokenName bs) = C.AssetName $ PV1.fromBuiltin bs

-- | Translate a policy ID from @cardano-api@ to @plutus@
transPolicyId :: C.PolicyId -> PV1.CurrencySymbol
transPolicyId (C.PolicyId scriptHash) = PV1.CurrencySymbol $ PlutusTx.toBuiltin (C.serialiseToRawBytes scriptHash)

unTransPolicyId :: PV1.CurrencySymbol -> Either C.SerialiseAsRawBytesError C.PolicyId
unTransPolicyId (PV1.CurrencySymbol bs) =
  C.deserialiseFromRawBytes C.AsPolicyId (PlutusTx.fromBuiltin bs)

-- | Translate an asset ID from @cardano-api@ to @plutus@
transAssetId :: C.AssetId -> Value.AssetClass
transAssetId C.AdaAssetId = Value.assetClass PV1.adaSymbol PV1.adaToken
transAssetId (C.AssetId policyId assetName) =
    Value.assetClass
        (transPolicyId $ policyId)
        (transAssetName $ toMaryAssetName assetName)

toMaryAssetName :: C.AssetName -> Mary.AssetName
toMaryAssetName (C.AssetName n) = Mary.AssetName $ Short.toShort n

unTransAssetId :: Value.AssetClass -> Either C.SerialiseAsRawBytesError C.AssetId
unTransAssetId (Value.AssetClass (currencySymbol, tokenName))
    | currencySymbol == PV1.adaSymbol && tokenName == PV1.adaToken =
        pure C.AdaAssetId
    | otherwise =
        C.AssetId
            <$> unTransPolicyId currencySymbol
            <*> pure (unTransAssetName tokenName)

unTransPubKeyHash :: PV1.PubKeyHash -> Either C.SerialiseAsRawBytesError (C.Hash C.PaymentKey)
unTransPubKeyHash (PV1.PubKeyHash pkh) =
  let bsx = PlutusTx.fromBuiltin pkh
  in C.deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) bsx

transPubKeyHash :: C.Hash C.PaymentKey -> PV1.PubKeyHash
transPubKeyHash = PV1.PubKeyHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

transStakeKeyHash :: C.Hash C.StakeKey -> PV1.PubKeyHash
transStakeKeyHash = PV1.PubKeyHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

unTransStakeKeyHash :: PV1.PubKeyHash -> Either C.SerialiseAsRawBytesError (C.Hash C.StakeKey)
unTransStakeKeyHash (PV1.PubKeyHash pkh) =
  let bsx = PlutusTx.fromBuiltin pkh
  in C.deserialiseFromRawBytes (C.AsHash C.AsStakeKey) bsx

transStakePoolKeyHash :: C.Hash C.StakePoolKey -> PV1.PubKeyHash
transStakePoolKeyHash = PV1.PubKeyHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

unTransStakePoolKeyHash :: PV1.PubKeyHash -> Either C.SerialiseAsRawBytesError (C.Hash C.StakePoolKey)
unTransStakePoolKeyHash (PV1.PubKeyHash pkh) =
  let bsx = PlutusTx.fromBuiltin pkh
  in C.deserialiseFromRawBytes (C.AsHash C.AsStakePoolKey) bsx

unTransCredential :: PV1.Credential -> Either C.SerialiseAsRawBytesError C.PaymentCredential
unTransCredential = \case
  PV1.PubKeyCredential c -> C.PaymentCredentialByKey <$> unTransPubKeyHash c
  PV1.ScriptCredential c -> C.PaymentCredentialByScript <$> unTransScriptHash c

transCredential :: C.PaymentCredential -> PV1.Credential
transCredential = \case
  C.PaymentCredentialByKey k    -> PV1.PubKeyCredential (transPubKeyHash k)
  C.PaymentCredentialByScript k -> PV1.ScriptCredential (transScriptHash k)

transStakeAddressReference :: C.StakeAddressReference -> Maybe PV1.StakingCredential
transStakeAddressReference = \case
  C.StakeAddressByValue x -> Just (PV1.StakingHash $ transStakeCredential x)
  C.StakeAddressByPointer (C.StakeAddressPointer (Ptr (C.SlotNo slotNo) (TxIx txIx) (CertIx ptrIx))) -> Just (PV1.StakingPtr (fromIntegral slotNo) (fromIntegral txIx) (fromIntegral ptrIx))
  C.NoStakeAddress -> Nothing

transStakeCredential :: C.StakeCredential -> PV1.Credential
transStakeCredential (C.StakeCredentialByKey stakeKeyHash) = PV1.PubKeyCredential (transStakeKeyHash stakeKeyHash)
transStakeCredential (C.StakeCredentialByScript scriptHash) = PV1.ScriptCredential (transScriptHash scriptHash)

unTransStakeCredential :: PV1.Credential -> Either C.SerialiseAsRawBytesError C.StakeCredential
unTransStakeCredential (PV1.PubKeyCredential pubKeyHash) = C.StakeCredentialByKey <$> unTransStakeKeyHash pubKeyHash
unTransStakeCredential (PV1.ScriptCredential scriptHash) = C.StakeCredentialByScript <$> unTransScriptHash scriptHash

unTransStakeAddressReference :: Maybe PV1.StakingCredential -> Either C.SerialiseAsRawBytesError C.StakeAddressReference
unTransStakeAddressReference Nothing = Right C.NoStakeAddress
unTransStakeAddressReference (Just (PV1.StakingHash credential)) =
  C.StakeAddressByValue <$> unTransStakeCredential credential
unTransStakeAddressReference (Just (PV1.StakingPtr slotNo txIx ptrIx)) =
  Right (C.StakeAddressByPointer (C.StakeAddressPointer (Ptr (C.SlotNo $ fromIntegral slotNo) (TxIx $ fromIntegral txIx) (CertIx $ fromIntegral ptrIx))))

unTransAddressInEra :: C.NetworkId -> PV1.Address -> Either C.SerialiseAsRawBytesError (C.AddressInEra C.ConwayEra)
unTransAddressInEra networkId (PV1.Address cred staking) =
  C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraConway) <$>
    (C.makeShelleyAddress networkId
      <$> unTransCredential cred
      <*> unTransStakeAddressReference staking
      )

-- | @cardano-api@ address to @plutus@ address. Returns 'Nothing' for
-- | byron addresses.
transAddressInEra :: C.AddressInEra C.ConwayEra -> Maybe PV1.Address
transAddressInEra = \case
  C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraConway) (C.ShelleyAddress _ p s) ->
    Just $ PV1.Address
      (transCredential $ C.fromShelleyPaymentCredential p)
      (transStakeAddressReference $ C.fromShelleyStakeReference s)
  C.AddressInEra C.ByronAddressInAnyEra _ -> Nothing

unTransTxOutRef :: PV1.TxOutRef -> Either C.SerialiseAsRawBytesError C.TxIn
unTransTxOutRef PV1.TxOutRef{PV1.txOutRefId=PV1.TxId bs, PV1.txOutRefIdx} =
  let i = C.deserialiseFromRawBytes C.AsTxId $ PlutusTx.fromBuiltin bs
  in C.TxIn <$> i <*> pure (C.TxIx $ fromIntegral txOutRefIdx)

transTxOutRef :: C.TxIn -> PV1.TxOutRef
transTxOutRef (C.TxIn txId (C.TxIx ix)) =
  let i = PV1.TxId $ PlutusTx.toBuiltin $ C.serialiseToRawBytes txId
  in PV1.TxOutRef i (fromIntegral ix)

transPOSIXTime :: POSIXTime -> PV1.POSIXTime
transPOSIXTime posixTimeSeconds = PV1.POSIXTime (floor @Rational (1000 * realToFrac posixTimeSeconds))

unTransPOSIXTime :: PV1.POSIXTime -> POSIXTime
unTransPOSIXTime (PV1.POSIXTime pt) = realToFrac @Rational $ fromIntegral pt / 1000

unTransTxOutValue :: PV1.Value -> Either C.SerialiseAsRawBytesError (C.TxOutValue C.ConwayEra)
unTransTxOutValue value = C.TxOutValueShelleyBased C.ShelleyBasedEraConway . C.toMaryValue <$> unTransValue value

unTransValue :: PV1.Value -> Either C.SerialiseAsRawBytesError C.Value
unTransValue =
    fmap C.valueFromList . traverse toSingleton . Value.flattenValue
  where
    toSingleton (cs, tn, q) =
        unTransAssetId (Value.assetClass cs tn) <&> (, C.Quantity q)

transValue :: C.Value -> PV1.Value
transValue =
  let t (assetId, C.Quantity quantity) =
        let Value.AssetClass (sym, tn) = transAssetId assetId
        in (sym, Map.singleton tn quantity)
  in PV1.Value . Map.safeFromList . fmap t . C.valueToList

unTransPlutusScript
    :: C.SerialiseAsRawBytes plutusScript
    => C.AsType plutusScript
    -> SerialisedScript
    -> Either C.SerialiseAsRawBytesError plutusScript
unTransPlutusScript asPlutusScriptType =
  C.deserialiseFromRawBytes asPlutusScriptType . BSL.toStrict . Codec.serialise

unTransScriptDataHash :: P.DatumHash -> Either C.SerialiseAsRawBytesError (C.Hash C.ScriptData)
unTransScriptDataHash (P.DatumHash bs) =
  C.deserialiseFromRawBytes (C.AsHash C.AsScriptData) (PlutusTx.fromBuiltin bs)

unTransTxOutDatumHash :: P.DatumHash -> Either C.SerialiseAsRawBytesError (C.TxOutDatum ctx C.ConwayEra)
unTransTxOutDatumHash datumHash = C.TxOutDatumHash C.AlonzoEraOnwardsConway <$> unTransScriptDataHash datumHash
