{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-| Lenses for @cardano-api@ types
-}
module Convex.Lenses(
  -- * Tx body content lenses
  emptyTx,
  emptyTxOut,
  TxIn,
  txIns,
  txInsReference,
  txOuts,
  txMintValue,
  txFee,
  txFee',
  txValidityRange,
  txMetadata,
  txProtocolParams,
  txInsCollateral,
  txScriptValidity,
  txAuxScripts,
  txExtraKeyWits,
  txWithdrawals,
  txCertificates,

  -- * Prisms and Isos
  _TxMintValue,
  _TxInsReference,
  _Value,
  _TxOut,
  _TxOutValue,
  _ShelleyAddressInBabbageEra,
  _PaymentCredentialByKey,
  _ShelleyPaymentCredentialByKey,
  _PaymentCredentialByScript,
  _ShelleyPaymentCredentialByScript,
  _TxInsCollateral,
  _TxMetadata,
  _TxAuxScripts,
  _TxExtraKeyWitnesses,
  _TxWithdrawals,
  _TxCertificates,

  -- ** Validity intervals
  _TxValidityNoLowerBound,
  _TxValidityLowerBound,
  _TxValidityNoUpperBound,
  _TxValidityUpperBound,
  _TxValidityFiniteRange,

  -- ** Witnesses
  _KeyWitness,
  _ScriptWitness,
  _PlutusScriptWitnessV1,
  _PlutusScriptWitnessV2,

  -- ** Build tx
  _BuildTxWith,
  _ViewTx,

  -- * Ledger API types
  slot,
  _UTxOState,
  utxoState,

  -- * Addresses
  _AddressInEra,
  _Address,

  -- * Hashes
  _ScriptHash,
  _KeyHash,
  _PlutusPubKeyHash,
  _PaymentCredential,

  -- * Datums
  _TxOutDatumInline,
  _TxOutDatumInTx,
  _TxOutDatumHash,
  _ScriptData
) where

import           Cardano.Api                        (AddressInEra, AssetId,
                                                     BabbageEra, BuildTx,
                                                     BuildTxWith, CtxTx,
                                                     PolicyId, Quantity (..),
                                                     ScriptWitness, TxMintValue,
                                                     TxOut, TxOutDatum,
                                                     TxOutValue, Value, ViewTx,
                                                     WitCtxMint)
import           Cardano.Api.Shelley                (Address, ReferenceScript,
                                                     ShelleyAddr, SlotNo)
import qualified Cardano.Api.Shelley                as C
import qualified Cardano.Ledger.BaseTypes           as Shelley
import qualified Cardano.Ledger.Core                as Core
import qualified Cardano.Ledger.Credential          as Credential
import           Cardano.Ledger.Crypto              (StandardCrypto)
import           Cardano.Ledger.Era                 (Era)
import qualified Cardano.Ledger.Hashes              as Hashes
import qualified Cardano.Ledger.Keys                as Keys
import           Cardano.Ledger.Shelley.API         (Coin, LedgerEnv (..), UTxO,
                                                     UTxOState (..))
import           Cardano.Ledger.Shelley.LedgerState (LedgerState (..),
                                                     smartUTxOState)
import           Control.Lens                       (Iso', Lens', Prism', iso,
                                                     lens, prism')
import           Control.State.Transition           (STS (State))
import qualified Convex.Scripts                     as Scripts
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Proxy                         (Proxy (..))
import           Data.Word                          (Word64)
import qualified Plutus.V1.Ledger.Api               as PV1
import           Plutus.V1.Ledger.Crypto            (PubKeyHash (..))
import qualified PlutusTx.Prelude                   as PlutusTx

{-| 'TxBodyContent' with all fields set to empty, none, default values
-}
emptyTx :: C.TxBodyContent C.BuildTx BabbageEra
emptyTx =
  C.TxBodyContent
    { C.txIns = []
    , C.txInsCollateral = C.TxInsCollateralNone
    , C.txInsReference = C.TxInsReferenceNone
    , C.txOuts = []
    , C.txTotalCollateral = C.TxTotalCollateralNone
    , C.txReturnCollateral = C.TxReturnCollateralNone
    , C.txFee = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra 0
    , C.txValidityRange = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)
    , C.txMetadata = C.TxMetadataNone
    , C.txAuxScripts = C.TxAuxScriptsNone
    , C.txExtraKeyWits = C.TxExtraKeyWitnessesNone
    , C.txProtocolParams = C.BuildTxWith Nothing
    , C.txWithdrawals = C.TxWithdrawalsNone
    , C.txCertificates = C.TxCertificatesNone
    , C.txUpdateProposal = C.TxUpdateProposalNone
    , C.txMintValue = C.TxMintNone
    , C.txScriptValidity = C.TxScriptValidityNone
    }

{-| A transaction output with no value
-}
emptyTxOut :: AddressInEra BabbageEra -> C.TxOut C.CtxTx C.BabbageEra
emptyTxOut addr = C.TxOut addr (C.lovelaceToTxOutValue 0) C.TxOutDatumNone C.ReferenceScriptNone

type TxIn v = (C.TxIn, BuildTxWith v (C.Witness C.WitCtxTxIn BabbageEra))

txIns :: Lens' (C.TxBodyContent v BabbageEra) [TxIn v]
txIns = lens get set_ where
  get = C.txIns
  set_ body txIns' = body{C.txIns=txIns'}

txInsReference :: Lens' (C.TxBodyContent v BabbageEra) (C.TxInsReference v BabbageEra)
txInsReference = lens get set_ where
  get = C.txInsReference
  set_ body txInsRef' = body{C.txInsReference = txInsRef'}

-- Lenses for working with cardano-api transactions
txOuts :: Lens' (C.TxBodyContent v BabbageEra) [TxOut CtxTx BabbageEra]
txOuts = lens get set_ where
  get = C.txOuts
  set_ body txOuts' = body{C.txOuts=txOuts'}

txFee' :: Lens' (C.TxBodyContent v e) (C.TxFee e)
txFee' = lens get set_ where
  get           = C.txFee
  set_ body fee = body{C.txFee = fee}

txValidityRange :: Lens' (C.TxBodyContent v e) (C.TxValidityLowerBound e, C.TxValidityUpperBound e)
txValidityRange = lens get set_ where
  get = C.txValidityRange
  set_ body range = body{C.txValidityRange = range}

txFee :: Lens' (C.TxBodyContent v BabbageEra) C.Lovelace
txFee = lens get set_ where
  get :: C.TxBodyContent v BabbageEra -> C.Lovelace
  get b = case C.txFee b of { C.TxFeeExplicit C.TxFeesExplicitInBabbageEra t_fee -> t_fee; C.TxFeeImplicit{} -> error "not possible in babbage era" }
  set_ body fee = body{C.txFee = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra fee}

txProtocolParams :: Lens' (C.TxBodyContent v e) (BuildTxWith v (Maybe C.ProtocolParameters))
txProtocolParams = lens get set_ where
  get = C.txProtocolParams
  set_ body params = body{C.txProtocolParams = params}

txMintValue :: Lens' (C.TxBodyContent v BabbageEra) (TxMintValue v BabbageEra)
txMintValue = lens get set_ where
  get = C.txMintValue
  set_ body txMintValue' = body{C.txMintValue=txMintValue'}

txScriptValidity :: Lens' (C.TxBodyContent v e) (C.TxScriptValidity e)
txScriptValidity = lens get set_ where
  get = C.txScriptValidity
  set_ body v = body{C.txScriptValidity = v}

txInsCollateral :: Lens' (C.TxBodyContent v BabbageEra) (C.TxInsCollateral BabbageEra)
txInsCollateral = lens get set_ where
  get = C.txInsCollateral
  set_ body col = body{C.txInsCollateral = col}

txMetadata :: Lens' (C.TxBodyContent v BabbageEra) (C.TxMetadataInEra BabbageEra)
txMetadata = lens get set_ where
  get = C.txMetadata
  set_ body m = body{C.txMetadata=m}

txExtraKeyWits :: Lens' (C.TxBodyContent v BabbageEra) (C.TxExtraKeyWitnesses BabbageEra)
txExtraKeyWits = lens get set_ where
  get = C.txExtraKeyWits
  set_ body k = body{C.txExtraKeyWits = k}

txWithdrawals :: Lens' (C.TxBodyContent v BabbageEra) (C.TxWithdrawals v BabbageEra)
txWithdrawals = lens get set_ where
  get = C.txWithdrawals
  set_ body k = body{C.txWithdrawals = k}

txCertificates :: Lens' (C.TxBodyContent v BabbageEra) (C.TxCertificates v BabbageEra)
txCertificates = lens get set_ where
  get = C.txCertificates
  set_ body k = body{C.txCertificates = k}

_TxExtraKeyWitnesses :: Iso' (C.TxExtraKeyWitnesses BabbageEra) [C.Hash C.PaymentKey]
_TxExtraKeyWitnesses = iso from to where
  from :: C.TxExtraKeyWitnesses BabbageEra -> [C.Hash C.PaymentKey]
  from C.TxExtraKeyWitnessesNone      = []
  from (C.TxExtraKeyWitnesses _ keys) = keys

  to []   = C.TxExtraKeyWitnessesNone
  to keys = C.TxExtraKeyWitnesses C.ExtraKeyWitnessesInBabbageEra keys

_TxWithdrawals :: Iso' (C.TxWithdrawals v BabbageEra) [(C.StakeAddress, C.Lovelace, BuildTxWith v (C.Witness C.WitCtxStake BabbageEra))]
_TxWithdrawals = iso from to where
  from :: C.TxWithdrawals v BabbageEra -> [(C.StakeAddress, C.Lovelace, BuildTxWith v (C.Witness C.WitCtxStake BabbageEra))]
  from C.TxWithdrawalsNone    = []
  from (C.TxWithdrawals _ xs) = xs

  to [] = C.TxWithdrawalsNone
  to xs = C.TxWithdrawals C.WithdrawalsInBabbageEra xs

_TxCertificates :: Iso' (C.TxCertificates BuildTx BabbageEra) ([C.Certificate], Map C.StakeCredential (C.Witness C.WitCtxStake BabbageEra))
_TxCertificates = iso from to where
  from :: C.TxCertificates BuildTx BabbageEra -> ([C.Certificate], (Map C.StakeCredential (C.Witness C.WitCtxStake BabbageEra)))
  from C.TxCertificatesNone                     = ([], Map.empty)
  from (C.TxCertificates _ x (C.BuildTxWith y)) = (x, y)

  to :: ([C.Certificate], Map C.StakeCredential (C.Witness C.WitCtxStake BabbageEra)) -> C.TxCertificates BuildTx BabbageEra
  to ([], mp) | Map.null mp = C.TxCertificatesNone
  to (x, y) = C.TxCertificates C.CertificatesInBabbageEra x (C.BuildTxWith y)

txAuxScripts :: Lens' (C.TxBodyContent v BabbageEra) (C.TxAuxScripts BabbageEra)
txAuxScripts = lens get set_ where
  get = C.txAuxScripts
  set_ body s = body{C.txAuxScripts=s}

_TxAuxScripts :: Iso' (C.TxAuxScripts BabbageEra) [C.ScriptInEra BabbageEra]
_TxAuxScripts = iso from to where
  from :: C.TxAuxScripts BabbageEra -> [C.ScriptInEra BabbageEra]
  from = \case
    C.TxAuxScriptsNone -> []
    C.TxAuxScripts _ s -> s
  to s | null s = C.TxAuxScriptsNone
       | otherwise = C.TxAuxScripts C.AuxScriptsInBabbageEra s

_TxMetadata :: Iso' (C.TxMetadataInEra BabbageEra) (Map Word64 C.TxMetadataValue)
_TxMetadata = iso from to where
  from :: C.TxMetadataInEra BabbageEra -> (Map Word64 C.TxMetadataValue)
  from = \case
    C.TxMetadataNone                     -> Map.empty
    C.TxMetadataInEra _ (C.TxMetadata m) -> m
  to m | Map.null m = C.TxMetadataNone
       | otherwise  = C.TxMetadataInEra C.TxMetadataInBabbageEra (C.TxMetadata m)

_TxInsCollateral :: Iso' (C.TxInsCollateral BabbageEra) [C.TxIn]
_TxInsCollateral = iso from to where
  from :: C.TxInsCollateral BabbageEra -> [C.TxIn]
  from = \case
    C.TxInsCollateralNone  -> []
    C.TxInsCollateral _ xs -> xs
  to = \case
    [] -> C.TxInsCollateralNone
    xs -> C.TxInsCollateral C.CollateralInBabbageEra xs

_TxMintValue :: Iso' (TxMintValue BuildTx BabbageEra) (Value, Map PolicyId (ScriptWitness WitCtxMint BabbageEra))
_TxMintValue = iso from to where
  from :: TxMintValue BuildTx BabbageEra -> (Value, Map PolicyId (ScriptWitness WitCtxMint BabbageEra))
  from = \case
    C.TxMintNone                          -> (mempty, mempty)
    C.TxMintValue _ vl (C.BuildTxWith mp) -> (vl, mp)
  to (vl, mp)
    | Map.null mp && vl == mempty = C.TxMintNone
    | otherwise                   = C.TxMintValue C.MultiAssetInBabbageEra vl (C.BuildTxWith mp)

_TxInsReference :: Iso' (C.TxInsReference build BabbageEra) [C.TxIn]
_TxInsReference = iso from to where
  from :: C.TxInsReference build BabbageEra -> [C.TxIn]
  from = \case
    C.TxInsReferenceNone   -> []
    C.TxInsReference _ ins -> ins
  to = \case
    [] -> C.TxInsReferenceNone
    xs -> C.TxInsReference C.ReferenceTxInsScriptsInlineDatumsInBabbageEra xs


_Value :: Iso' Value (Map AssetId Quantity)
_Value = iso from to where
  -- the 'Value' constructor is not exposed so we have to take the long way around
  from = Map.fromList . C.valueToList
  to = C.valueFromList . Map.toList

_TxOut :: Iso' (TxOut ctx era) (AddressInEra era, TxOutValue era, TxOutDatum ctx era, ReferenceScript era)
_TxOut = iso from to where
  from (C.TxOut addr vl dt rs) = (addr, vl, dt, rs)
  to (addr, vl, dt, rs) = C.TxOut addr vl dt rs

_TxOutDatumHash :: Prism' (TxOutDatum ctx C.BabbageEra) (C.Hash C.ScriptData)
_TxOutDatumHash = prism' from to where
  to :: TxOutDatum ctx C.BabbageEra -> Maybe (C.Hash C.ScriptData)
  to (C.TxOutDatumHash _ h) = Just h
  to _                      = Nothing
  from :: C.Hash C.ScriptData -> TxOutDatum ctx C.BabbageEra
  from h = C.TxOutDatumHash C.ScriptDataInBabbageEra h

_TxOutDatumInTx :: Prism' (TxOutDatum CtxTx C.BabbageEra) C.ScriptData
_TxOutDatumInTx = prism' from to where
  to :: TxOutDatum CtxTx C.BabbageEra -> Maybe C.ScriptData
  to (C.TxOutDatumInTx _ k) = Just k
  to _                      = Nothing
  from :: C.ScriptData -> TxOutDatum CtxTx C.BabbageEra
  from cd = C.TxOutDatumInTx C.ScriptDataInBabbageEra cd

_TxOutDatumInline :: forall ctx. Prism' (TxOutDatum ctx C.BabbageEra) C.ScriptData
_TxOutDatumInline = prism' from to where
  to :: TxOutDatum ctx C.BabbageEra -> Maybe C.ScriptData
  to (C.TxOutDatumInline _ k) = Just k
  to _                        = Nothing
  from :: C.ScriptData -> TxOutDatum ctx C.BabbageEra
  from cd = C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra cd

_ShelleyAddressInBabbageEra :: Prism' (C.AddressInEra C.BabbageEra) (Shelley.Network, Credential.PaymentCredential StandardCrypto, Credential.StakeReference StandardCrypto)
_ShelleyAddressInBabbageEra = prism' from to where
  to :: C.AddressInEra C.BabbageEra -> Maybe (Shelley.Network, Credential.PaymentCredential StandardCrypto, Credential.StakeReference StandardCrypto)
  to x = case x of
    (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) (C.ShelleyAddress ntw pmt stakeRef)) -> Just (ntw, pmt, stakeRef)
    (C.AddressInEra (C.ByronAddressInAnyEra) _) -> Nothing
  from (ntw, pmt, stakeRef) = C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) (C.ShelleyAddress ntw pmt stakeRef)

_PaymentCredentialByKey :: Prism' C.PaymentCredential (C.Hash C.PaymentKey)
_PaymentCredentialByKey = prism' from to where
  from = C.PaymentCredentialByKey
  to (C.PaymentCredentialByKey k) = Just k
  to _                            = Nothing

_ShelleyPaymentCredentialByKey :: Prism' (Credential.PaymentCredential StandardCrypto) (Keys.KeyHash 'Keys.Payment StandardCrypto)
_ShelleyPaymentCredentialByKey = prism' from to where
  from = Credential.KeyHashObj
  to (Credential.KeyHashObj k)  = Just k
  to Credential.ScriptHashObj{} = Nothing

_PaymentCredentialByScript :: Prism' C.PaymentCredential C.ScriptHash
_PaymentCredentialByScript = prism' from to where
  from = C.PaymentCredentialByScript
  to (C.PaymentCredentialByScript k) = Just k
  to C.PaymentCredentialByKey{}      = Nothing

_ShelleyPaymentCredentialByScript :: Prism' (Credential.PaymentCredential StandardCrypto) (Hashes.ScriptHash StandardCrypto)
_ShelleyPaymentCredentialByScript = prism' from to where
  from = Credential.ScriptHashObj
  to (Credential.ScriptHashObj s) = Just s
  to Credential.KeyHashObj{}      = Nothing

_KeyWitness :: Prism' (C.Witness witctx era) (C.KeyWitnessInCtx witctx)
_KeyWitness = prism' from to where
  from = C.KeyWitness
  to :: C.Witness witctx era -> Maybe (C.KeyWitnessInCtx witctx)
  to = \case
    C.KeyWitness w    -> Just w
    C.ScriptWitness{} -> Nothing

_ScriptWitness :: Prism' (C.Witness witctx era) (C.ScriptWitnessInCtx witctx, C.ScriptWitness witctx era)
_ScriptWitness = prism' from to where
  from (a, b) = C.ScriptWitness a b
  to :: C.Witness witctx era -> Maybe (C.ScriptWitnessInCtx witctx, C.ScriptWitness witctx era)
  to = \case
    C.ScriptWitness a b -> Just (a, b)
    C.KeyWitness{}      -> Nothing

_ViewTx :: Iso' (C.BuildTxWith ViewTx a) ()
_ViewTx = iso from to where
  from :: C.BuildTxWith ViewTx a -> ()
  from = \case
    C.ViewTx{} -> ()
  to () = C.ViewTx{}

_BuildTxWith :: Iso' (C.BuildTxWith BuildTx a) a
_BuildTxWith = iso from to where
  from :: C.BuildTxWith BuildTx a -> a
  from = \case
    C.BuildTxWith a -> a
  to = C.BuildTxWith

_TxOutValue :: Iso' (TxOutValue BabbageEra) Value
_TxOutValue = iso from to where
  from = C.txOutValueToValue
  to = C.TxOutValue C.MultiAssetInBabbageEra

slot :: Lens' (LedgerEnv era) SlotNo
slot = lens get set_ where
  get = ledgerSlotNo
  set_ l s = l{ledgerSlotNo=s}

{-| 'UTxOState' iso. Note that this doesn't touch the '_stakeDistro' field. This is because the
stake distro is a function of @utxo :: UTxO era@ and can be computed by @updateStakeDistribution mempty mempty utxo@.
-}
_UTxOState :: forall era. Era era => Iso' (UTxOState era) (UTxO era, Coin, Coin, State (Core.EraRule "PPUP" era))
_UTxOState = iso from to where
  from UTxOState{_utxo, _deposited, _fees, _ppups} = (_utxo, _deposited, _fees, _ppups)
  to (utxo, deposited, fees, pups) = smartUTxOState @era utxo deposited fees pups

utxoState :: Lens' (LedgerState era) (UTxOState era)
utxoState = lens get set_ where
  get = lsUTxOState
  set_ l s = l{lsUTxOState=s}

_AddressInEra :: Prism' (AddressInEra BabbageEra) (Address ShelleyAddr)
_AddressInEra = prism' from to where
  to :: AddressInEra BabbageEra -> Maybe (Address ShelleyAddr)
  to (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) addr) = Just addr
  to _ = Nothing
  from = C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage)

_Address :: Iso' (Address ShelleyAddr) (Shelley.Network, Credential.PaymentCredential StandardCrypto, Credential.StakeReference StandardCrypto)
_Address = iso from to where
  from :: Address ShelleyAddr -> (Shelley.Network, Credential.PaymentCredential StandardCrypto, Credential.StakeReference StandardCrypto)
  from (C.ShelleyAddress n p s) = (n, p, s)
  to (n, p, s) = C.ShelleyAddress n p s

_KeyHash :: Iso' (Keys.KeyHash 'Keys.Payment StandardCrypto) (C.Hash C.PaymentKey)
_KeyHash = iso from to where
  from :: Keys.KeyHash 'Keys.Payment StandardCrypto -> C.Hash C.PaymentKey
  from hash = C.PaymentKeyHash hash
  to (C.PaymentKeyHash h) = h

_ScriptHash :: Iso' (Hashes.ScriptHash StandardCrypto) C.ScriptHash
_ScriptHash = iso from to where
  from :: Hashes.ScriptHash StandardCrypto -> C.ScriptHash
  from = C.fromShelleyScriptHash

  to :: C.ScriptHash -> Hashes.ScriptHash StandardCrypto
  to = C.toShelleyScriptHash

_PlutusPubKeyHash :: Prism' PubKeyHash (C.Hash C.PaymentKey)
_PlutusPubKeyHash = prism' from to where
  from :: C.Hash C.PaymentKey -> PubKeyHash
  from = PubKeyHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

  to :: PubKeyHash -> Maybe (C.Hash C.PaymentKey)
  to (PubKeyHash h) = C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @(C.Hash C.PaymentKey)) $ PlutusTx.fromBuiltin h

_PaymentCredential :: Iso' C.PaymentCredential (Credential.PaymentCredential StandardCrypto)
_PaymentCredential = iso from to where
  from :: C.PaymentCredential -> Credential.PaymentCredential StandardCrypto
  from (C.PaymentCredentialByKey (C.PaymentKeyHash kh)) = Credential.KeyHashObj kh
  from (C.PaymentCredentialByScript sh)                 = Credential.ScriptHashObj (C.toShelleyScriptHash sh)

  to = C.fromShelleyPaymentCredential

_ScriptData :: forall a. (PV1.FromData a, PV1.ToData a) => Prism' C.ScriptData a
_ScriptData = prism' from to where
  to :: C.ScriptData -> Maybe a
  to = Scripts.fromScriptData

  from :: a -> C.ScriptData
  from = Scripts.toScriptData

_PlutusScriptWitnessV1 :: forall era witctx. Prism' (C.ScriptWitness witctx era) (C.ScriptLanguageInEra C.PlutusScriptV1 era, C.PlutusScriptVersion C.PlutusScriptV1, C.PlutusScriptOrReferenceInput C.PlutusScriptV1, C.ScriptDatum witctx, C.ScriptRedeemer, C.ExecutionUnits)
_PlutusScriptWitnessV1 = prism' from to where
  from :: (C.ScriptLanguageInEra C.PlutusScriptV1 era, C.PlutusScriptVersion C.PlutusScriptV1, C.PlutusScriptOrReferenceInput C.PlutusScriptV1, C.ScriptDatum witctx, C.ScriptRedeemer, C.ExecutionUnits) -> C.ScriptWitness witctx era
  from (lang, v, i, dtr, red, ex) = C.PlutusScriptWitness lang v i dtr red ex

  to :: C.ScriptWitness witctx era -> Maybe (C.ScriptLanguageInEra C.PlutusScriptV1 era, C.PlutusScriptVersion C.PlutusScriptV1, C.PlutusScriptOrReferenceInput C.PlutusScriptV1, C.ScriptDatum witctx, C.ScriptRedeemer, C.ExecutionUnits)
  to (C.PlutusScriptWitness C.PlutusScriptV1InBabbage v i dtr red ex) = Just (C.PlutusScriptV1InBabbage, v, i, dtr, red, ex)
  to _ = Nothing

_PlutusScriptWitnessV2 :: forall era witctx. Prism' (C.ScriptWitness witctx era) (C.ScriptLanguageInEra C.PlutusScriptV2 era, C.PlutusScriptVersion C.PlutusScriptV2, C.PlutusScriptOrReferenceInput C.PlutusScriptV2, C.ScriptDatum witctx, C.ScriptRedeemer, C.ExecutionUnits)
_PlutusScriptWitnessV2 = prism' from to where
  from :: (C.ScriptLanguageInEra C.PlutusScriptV2 era, C.PlutusScriptVersion C.PlutusScriptV2, C.PlutusScriptOrReferenceInput C.PlutusScriptV2, C.ScriptDatum witctx, C.ScriptRedeemer, C.ExecutionUnits) -> C.ScriptWitness witctx era
  from (lang, v, i, dtr, red, ex) = C.PlutusScriptWitness lang v i dtr red ex

  to :: C.ScriptWitness witctx era -> Maybe (C.ScriptLanguageInEra C.PlutusScriptV2 era, C.PlutusScriptVersion C.PlutusScriptV2, C.PlutusScriptOrReferenceInput C.PlutusScriptV2, C.ScriptDatum witctx, C.ScriptRedeemer, C.ExecutionUnits)
  to (C.PlutusScriptWitness C.PlutusScriptV2InBabbage v i dtr red ex) = Just (C.PlutusScriptV2InBabbage, v, i, dtr, red, ex)
  to _ = Nothing

_TxValidityNoLowerBound :: forall era. Prism' (C.TxValidityLowerBound era) ()
_TxValidityNoLowerBound = prism' from to where
  from () = C.TxValidityNoLowerBound
  to = \case
    C.TxValidityNoLowerBound -> Just ()
    _                        -> Nothing

_TxValidityLowerBound :: forall era. Prism' (C.TxValidityLowerBound era) (C.ValidityLowerBoundSupportedInEra era, C.SlotNo)
_TxValidityLowerBound = prism' from to where
  from (s, e) = C.TxValidityLowerBound s e
  to = \case
    C.TxValidityLowerBound s e -> Just (s, e)
    _                          -> Nothing

_TxValidityNoUpperBound :: forall era. Prism' (C.TxValidityUpperBound era) (C.ValidityNoUpperBoundSupportedInEra era)
_TxValidityNoUpperBound = prism' from to where
  from = C.TxValidityNoUpperBound
  to = \case
    C.TxValidityNoUpperBound k -> Just k
    _                          -> Nothing

_TxValidityUpperBound :: forall era. Prism' (C.TxValidityUpperBound era) (C.ValidityUpperBoundSupportedInEra era, SlotNo)
_TxValidityUpperBound = prism' from to where
  from (k, s) = C.TxValidityUpperBound k s
  to = \case
    C.TxValidityUpperBound k s -> Just (k, s)
    _                          -> Nothing

_TxValidityFiniteRange :: Prism' (C.TxValidityLowerBound C.BabbageEra, C.TxValidityUpperBound C.BabbageEra) (SlotNo, SlotNo)
_TxValidityFiniteRange = prism' from to where
  from (l, u) = (C.TxValidityLowerBound C.ValidityLowerBoundInBabbageEra l, C.TxValidityUpperBound C.ValidityUpperBoundInBabbageEra u)
  to = \case
    (C.TxValidityLowerBound _ l, C.TxValidityUpperBound _ u) -> Just (l, u)
    _                                                        -> Nothing
