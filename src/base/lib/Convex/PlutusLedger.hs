{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
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
  unTransValue,

  -- * Scripts
  unTransPlutusScript,

  -- * Intervals
  _Interval,
  _UpperBound,
  _LowerBound,
  _NegInf,
  _PosInf,
  _Finite,
  _FiniteInterval

) where

import qualified Cardano.Api.Shelley       as C
import           Cardano.Ledger.BaseTypes  (CertIx (..), TxIx (..))
import           Cardano.Ledger.Credential (Ptr (..))
import qualified Cardano.Ledger.Mary.Value as Mary (AssetName (..))
import qualified Codec.Serialise           as Codec
import           Control.Lens              (Iso', Prism', iso, prism')
import qualified Data.ByteString.Lazy      as BSL
import           Data.ByteString.Short     (fromShort)
import qualified Data.ByteString.Short     as Short
import           Data.Functor              ((<&>))
import           Data.Time.Clock.POSIX     (POSIXTime)
import qualified Plutus.V1.Ledger.Api      as PV1
import           Plutus.V1.Ledger.Interval (Closure, Extended (..),
                                            Interval (..), LowerBound (..),
                                            UpperBound (..))
import qualified Plutus.V1.Ledger.Scripts  as P
import qualified Plutus.V1.Ledger.Value    as Value
import qualified PlutusTx.Prelude          as PlutusTx

transScriptHash :: C.ScriptHash -> PV1.ValidatorHash
transScriptHash h = PV1.ValidatorHash (PV1.toBuiltin (C.serialiseToRawBytes h)) -- TODO: is serialiseToRawBytes the correct thing to do here?

unTransScriptHash :: PV1.ValidatorHash -> Maybe C.ScriptHash
unTransScriptHash (PV1.ValidatorHash vh) =
  C.deserialiseFromRawBytes C.AsScriptHash $ PlutusTx.fromBuiltin vh

transAssetName :: Mary.AssetName -> PV1.TokenName
transAssetName (Mary.AssetName bs) = PV1.TokenName (PV1.toBuiltin (fromShort bs))

unTransAssetName :: PV1.TokenName -> C.AssetName
unTransAssetName (PV1.TokenName bs) = C.AssetName $ PV1.fromBuiltin bs

transPolicyId :: C.PolicyId -> PV1.MintingPolicyHash
transPolicyId (C.PolicyId scriptHash) = PV1.MintingPolicyHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes scriptHash)

unTransPolicyId :: PV1.MintingPolicyHash -> Maybe C.PolicyId
unTransPolicyId (PV1.MintingPolicyHash bs) =
  C.deserialiseFromRawBytes C.AsPolicyId (PlutusTx.fromBuiltin bs)

transAssetId :: C.AssetId -> Value.AssetClass
transAssetId C.AdaAssetId = Value.assetClass PV1.adaSymbol PV1.adaToken
transAssetId (C.AssetId policyId assetName) =
    Value.assetClass
        (Value.mpsSymbol . transPolicyId $ policyId)
        (transAssetName $ toMaryAssetName assetName)

toMaryAssetName :: C.AssetName -> Mary.AssetName
toMaryAssetName (C.AssetName n) = Mary.AssetName $ Short.toShort n

unTransAssetId :: Value.AssetClass -> Maybe C.AssetId
unTransAssetId (Value.AssetClass (currencySymbol, tokenName))
    | currencySymbol == PV1.adaSymbol && tokenName == PV1.adaToken =
        pure C.AdaAssetId
    | otherwise =
        C.AssetId
            <$> unTransPolicyId (Value.currencyMPSHash currencySymbol)
            <*> pure (unTransAssetName tokenName)

unTransPubKeyHash :: PV1.PubKeyHash -> Maybe (C.Hash C.PaymentKey)
unTransPubKeyHash (PV1.PubKeyHash pkh) =
  let bsx = PlutusTx.fromBuiltin pkh
  in C.deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) bsx

transPubKeyHash :: C.Hash C.PaymentKey -> PV1.PubKeyHash
transPubKeyHash = PV1.PubKeyHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

transStakeKeyHash :: C.Hash C.StakeKey -> PV1.PubKeyHash
transStakeKeyHash = PV1.PubKeyHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

unTransStakeKeyHash :: PV1.PubKeyHash -> Maybe (C.Hash C.StakeKey)
unTransStakeKeyHash (PV1.PubKeyHash pkh) =
  let bsx = PlutusTx.fromBuiltin pkh
  in C.deserialiseFromRawBytes (C.AsHash C.AsStakeKey) bsx

unTransCredential :: PV1.Credential -> Maybe C.PaymentCredential
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

unTransStakeCredential :: PV1.Credential -> Maybe C.StakeCredential
unTransStakeCredential (PV1.PubKeyCredential pubKeyHash) = C.StakeCredentialByKey <$> unTransStakeKeyHash pubKeyHash
unTransStakeCredential (PV1.ScriptCredential validatorHash) = C.StakeCredentialByScript <$> unTransScriptHash validatorHash

unTransStakeAddressReference :: Maybe PV1.StakingCredential -> Maybe C.StakeAddressReference
unTransStakeAddressReference Nothing = Just C.NoStakeAddress
unTransStakeAddressReference (Just (PV1.StakingHash credential)) =
  C.StakeAddressByValue <$> unTransStakeCredential credential
unTransStakeAddressReference (Just (PV1.StakingPtr slotNo txIx ptrIx)) =
  Just (C.StakeAddressByPointer (C.StakeAddressPointer (Ptr (C.SlotNo $ fromIntegral slotNo) (TxIx $ fromIntegral txIx) (CertIx $ fromIntegral ptrIx))))

unTransAddressInEra :: C.NetworkId -> PV1.Address -> Maybe (C.AddressInEra C.BabbageEra)
unTransAddressInEra networkId (PV1.Address cred staking) =
  C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) <$>
    (C.makeShelleyAddress networkId
      <$> unTransCredential cred
      <*> unTransStakeAddressReference staking
      )

-- | @cardano-api@ address to @plutus@ address. Returns 'Nothing' for
-- | byron addresses.
transAddressInEra :: C.AddressInEra C.BabbageEra -> Maybe PV1.Address
transAddressInEra = \case
  C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) (C.ShelleyAddress _ p s) ->
    Just $ PV1.Address
      (transCredential $ C.fromShelleyPaymentCredential p)
      (transStakeAddressReference $ C.fromShelleyStakeReference s)
  C.AddressInEra C.ByronAddressInAnyEra _ -> Nothing

unTransTxOutRef :: PV1.TxOutRef -> Maybe C.TxIn
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

unTransTxOutValue :: PV1.Value -> Maybe (C.TxOutValue C.BabbageEra)
unTransTxOutValue value = C.TxOutValue C.MultiAssetInBabbageEra <$> unTransValue value

unTransValue :: PV1.Value -> Maybe C.Value
unTransValue =
    fmap C.valueFromList . traverse toSingleton . Value.flattenValue
  where
    toSingleton (cs, tn, q) =
        unTransAssetId (Value.assetClass cs tn) <&> (, C.Quantity q)

unTransPlutusScript
    :: C.SerialiseAsRawBytes plutusScript
    => C.AsType plutusScript
    -> P.Script
    -> Maybe plutusScript
unTransPlutusScript asPlutusScriptType =
  C.deserialiseFromRawBytes asPlutusScriptType . BSL.toStrict . Codec.serialise

unTransScriptDataHash :: P.DatumHash -> Maybe (C.Hash C.ScriptData)
unTransScriptDataHash (P.DatumHash bs) =
  C.deserialiseFromRawBytes (C.AsHash C.AsScriptData) (PlutusTx.fromBuiltin bs)

unTransTxOutDatumHash :: P.DatumHash -> Maybe (C.TxOutDatum ctx C.BabbageEra)
unTransTxOutDatumHash datumHash = C.TxOutDatumHash C.ScriptDataInBabbageEra <$> unTransScriptDataHash datumHash

_Interval :: Iso' (Interval a) (LowerBound a, UpperBound a)
_Interval = iso from to where
  from Interval{ivFrom, ivTo} = (ivFrom, ivTo)
  to (ivFrom, ivTo) = Interval{ivFrom, ivTo}

_UpperBound :: Iso' (UpperBound a) (Extended a, Closure)
_UpperBound = iso from to where
  from (UpperBound a b) = (a, b)
  to = uncurry UpperBound

_LowerBound :: Iso' (LowerBound a) (Extended a, Closure)
_LowerBound = iso from to where
  from (LowerBound a b) = (a, b)
  to = uncurry LowerBound

_NegInf :: Prism' (Extended a) ()
_NegInf = prism' from to where
  from () = NegInf
  to = \case
    NegInf -> Just ()
    _      -> Nothing

_PosInf :: Prism' (Extended a) ()
_PosInf = prism' from to where
  from () = PosInf
  to = \case
    PosInf -> Just ()
    _      -> Nothing

_Finite :: Prism' (Extended a) a
_Finite = prism' from to where
  from = Finite
  to = \case
    Finite a -> Just a
    _        -> Nothing

-- | Access a finite interval with the two boundaries and closures. This is
--   convenient if you want to change the length of the interval for example.
_FiniteInterval :: Prism' (Interval a) ((a, a), (Closure, Closure))
_FiniteInterval = prism' from to where
  from ((l, u), (lc, uc)) = Interval (LowerBound (Finite l) lc) (UpperBound (Finite u) uc)
  to (Interval (LowerBound (Finite l) lc) (UpperBound (Finite u) uc)) = Just ((l, u), (lc, uc))
  to _ = Nothing
