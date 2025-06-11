{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Lenses for @cardano-api@ types
module Convex.CardanoApi.Lenses (
  -- * Tx body content lenses
  emptyTx,
  emptyTxOut,
  TxIn,
  txIns,
  txInsReference,
  txInsReferenceTxIns,
  txOuts,
  txMintValue,
  txFee,
  txFee',
  txValidityLowerBound,
  txValidityUpperBound,
  txMetadata,
  txProtocolParams,
  txInsCollateral,
  txInsCollateralTxIns,
  txScriptValidity,
  txAuxScripts,
  txExtraKeyWits,
  txWithdrawals,
  txCertificates,
  txProposalProcedures,
  txVotingProcedures,
  txCurrentTreasuryValue,
  txTreasuryDonation,

  -- * Prisms and Isos
  _TxMintValue,
  _TxInsReferenceIso,
  _Value,
  _AssetId,
  _TxOut,
  _TxOutValue,
  _ShelleyAddress,
  _PaymentCredentialByKey,
  _ShelleyPaymentCredentialByKey,
  _PaymentCredentialByScript,
  _ShelleyPaymentCredentialByScript,
  _TxInsCollateralIso,
  _TxMetadata,
  _TxAuxScripts,
  _TxExtraKeyWitnesses,
  _TxWithdrawals,
  _TxCertificates,
  Certificates,

  -- ** Validity intervals
  _TxValidityNoLowerBound,
  _TxValidityLowerBound,
  _TxValidityUpperBound,
  _TxValidityFiniteRange,

  -- ** Witnesses
  _KeyWitness,
  _ScriptWitness,
  _PlutusScriptWitness,

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
  _TxOutSupplementalDatum,
  _TxOutDatumHash,
  _ScriptData,

  -- * Intervals
  _Interval,
  _UpperBound,
  _LowerBound,
  _NegInf,
  _PosInf,
  _Finite,
  _FiniteInterval,
) where

import Cardano.Api (
  AddressInEra,
  AssetId,
  BuildTx,
  BuildTxWith,
  CtxTx,
  PolicyId,
  Quantity (..),
  ScriptWitness,
  TxMintValue,
  TxOut,
  TxOutDatum,
  TxOutValue,
  Value,
  ViewTx,
  WitCtxMint,
 )
import Cardano.Api.Shelley (
  Address,
  ReferenceScript,
  ShelleyAddr,
  SlotNo,
 )
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.BaseTypes qualified as Shelley
import Cardano.Ledger.Credential qualified as Credential
import Cardano.Ledger.Hashes qualified as Hashes
import Cardano.Ledger.Keys qualified as Keys
import Cardano.Ledger.Shelley.API (
  Coin,
  LedgerEnv (..),
  UTxO,
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Governance (GovState)
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
 )
import Cardano.Ledger.State (EraStake, addInstantStake)
import Control.Lens (
  Getter,
  Iso',
  Lens',
  Prism',
  iso,
  lens,
  prism',
 )
import Control.Lens qualified as L
import Convex.Scripts qualified as Scripts
import Data.Bifunctor (first)
import Data.Map.Ordered.Strict (OMap)
import Data.Map.Ordered.Strict qualified as OMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Word (Word64)
import GHC.IsList (IsList (fromList, toList))
import PlutusLedgerApi.V1 (PubKeyHash (..))
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Interval (
  Closure,
  Extended (..),
  Interval (..),
  LowerBound (..),
  UpperBound (..),
 )
import PlutusTx.Prelude qualified as PlutusTx

{- | 'TxBodyContent' with all fields set to empty, none, default values
TODO Remove and replace with C.defaultTxBodyContent
-}
emptyTx :: (C.IsShelleyBasedEra era) => C.TxBodyContent C.BuildTx era
emptyTx =
  C.TxBodyContent
    { C.txIns = []
    , C.txInsCollateral = C.TxInsCollateralNone
    , C.txInsReference = C.TxInsReferenceNone
    , C.txOuts = []
    , C.txTotalCollateral = C.TxTotalCollateralNone
    , C.txReturnCollateral = C.TxReturnCollateralNone
    , C.txFee = C.TxFeeExplicit C.shelleyBasedEra 0
    , C.txValidityLowerBound = C.TxValidityNoLowerBound
    , C.txValidityUpperBound = C.TxValidityUpperBound C.shelleyBasedEra Nothing
    , C.txMetadata = C.TxMetadataNone
    , C.txAuxScripts = C.TxAuxScriptsNone
    , C.txExtraKeyWits = C.TxExtraKeyWitnessesNone
    , C.txProtocolParams = C.BuildTxWith Nothing
    , C.txWithdrawals = C.TxWithdrawalsNone
    , C.txCertificates = C.TxCertificatesNone
    , C.txUpdateProposal = C.TxUpdateProposalNone
    , C.txMintValue = C.TxMintNone
    , C.txScriptValidity = C.TxScriptValidityNone
    , C.txProposalProcedures = Nothing
    , C.txVotingProcedures = Nothing
    , C.txCurrentTreasuryValue = Nothing
    , C.txTreasuryDonation = Nothing
    }

-- | A transaction output with no value
emptyTxOut :: (C.IsShelleyBasedEra era) => AddressInEra era -> C.TxOut ctx era
emptyTxOut addr = C.TxOut addr (C.lovelaceToTxOutValue C.shelleyBasedEra 0) C.TxOutDatumNone C.ReferenceScriptNone

type TxIn ctx era = (C.TxIn, BuildTxWith ctx (C.Witness C.WitCtxTxIn era))

txIns :: Lens' (C.TxBodyContent ctx era) [TxIn ctx era]
txIns = lens get set_
 where
  get = C.txIns
  set_ body txIns' = body{C.txIns = txIns'}

txInsReference :: Lens' (C.TxBodyContent v era) (C.TxInsReference v era)
txInsReference = lens get set_
 where
  get = C.txInsReference
  set_ body txInsRef' = body{C.txInsReference = txInsRef'}

txInsReferenceTxIns :: Getter (C.TxInsReference build era) [C.TxIn]
txInsReferenceTxIns = L.to get_
 where
  get_ = \case
    C.TxInsReferenceNone -> []
    C.TxInsReference _era xs _refDatums -> xs

-- Lenses for working with cardano-api transactions
txOuts :: Lens' (C.TxBodyContent v era) [TxOut CtxTx era]
txOuts = lens get set_
 where
  get = C.txOuts
  set_ body txOuts' = body{C.txOuts = txOuts'}

txFee' :: Lens' (C.TxBodyContent v e) (C.TxFee e)
txFee' = lens get set_
 where
  get = C.txFee
  set_ body fee = body{C.txFee = fee}

txValidityUpperBound :: Lens' (C.TxBodyContent v e) (C.TxValidityUpperBound e)
txValidityUpperBound = lens get set_
 where
  get = C.txValidityUpperBound
  set_ body range = body{C.txValidityUpperBound = range}

txValidityLowerBound :: Lens' (C.TxBodyContent v e) (C.TxValidityLowerBound e)
txValidityLowerBound = lens get set_
 where
  get = C.txValidityLowerBound
  set_ body range = body{C.txValidityLowerBound = range}

txFee :: (C.IsShelleyBasedEra era) => Lens' (C.TxBodyContent v era) Coin
txFee = lens get set_
 where
  get :: C.TxBodyContent v era -> Coin
  get b = case C.txFee b of C.TxFeeExplicit _era t_fee -> t_fee
  set_ body fee = body{C.txFee = C.TxFeeExplicit C.shelleyBasedEra fee}

txProtocolParams :: Lens' (C.TxBodyContent v e) (BuildTxWith v (Maybe (C.LedgerProtocolParameters e)))
txProtocolParams = lens get set_
 where
  get = C.txProtocolParams
  set_ body params = body{C.txProtocolParams = params}

txMintValue :: Lens' (C.TxBodyContent v era) (TxMintValue v era)
txMintValue = lens get set_
 where
  get = C.txMintValue
  set_ body txMintValue' = body{C.txMintValue = txMintValue'}

txScriptValidity :: Lens' (C.TxBodyContent v e) (C.TxScriptValidity e)
txScriptValidity = lens get set_
 where
  get = C.txScriptValidity
  set_ body v = body{C.txScriptValidity = v}

txInsCollateral :: Lens' (C.TxBodyContent v era) (C.TxInsCollateral era)
txInsCollateral = lens get set_
 where
  get = C.txInsCollateral
  set_ body col = body{C.txInsCollateral = col}

txInsCollateralTxIns :: Getter (C.TxInsCollateral era) [C.TxIn]
txInsCollateralTxIns = L.to get_
 where
  get_ = \case
    C.TxInsCollateralNone -> []
    C.TxInsCollateral _ xs -> xs

txMetadata :: Lens' (C.TxBodyContent v era) (C.TxMetadataInEra era)
txMetadata = lens get set_
 where
  get = C.txMetadata
  set_ body m = body{C.txMetadata = m}

txExtraKeyWits :: Lens' (C.TxBodyContent v era) (C.TxExtraKeyWitnesses era)
txExtraKeyWits = lens get set_
 where
  get = C.txExtraKeyWits
  set_ body k = body{C.txExtraKeyWits = k}

txWithdrawals :: Lens' (C.TxBodyContent v era) (C.TxWithdrawals v era)
txWithdrawals = lens get set_
 where
  get = C.txWithdrawals
  set_ body k = body{C.txWithdrawals = k}

txCertificates :: Lens' (C.TxBodyContent v era) (C.TxCertificates v era)
txCertificates = lens get set_
 where
  get = C.txCertificates
  set_ body k = body{C.txCertificates = k}

txCurrentTreasuryValue :: Lens' (C.TxBodyContent v era) (Maybe (C.Featured C.ConwayEraOnwards era (Maybe Coin)))
txCurrentTreasuryValue = lens get set_
 where
  get = C.txCurrentTreasuryValue
  set_ body k = body{C.txCurrentTreasuryValue = k}

txTreasuryDonation :: Lens' (C.TxBodyContent v era) (Maybe (C.Featured C.ConwayEraOnwards era Coin))
txTreasuryDonation = lens get set_
 where
  get = C.txTreasuryDonation
  set_ body k = body{C.txTreasuryDonation = k}

txProposalProcedures :: Lens' (C.TxBodyContent v era) (Maybe (C.Featured C.ConwayEraOnwards era (C.TxProposalProcedures v era)))
txProposalProcedures = lens get set_
 where
  get = C.txProposalProcedures
  set_ body k = body{C.txProposalProcedures = k}

txVotingProcedures :: Lens' (C.TxBodyContent v era) (Maybe (C.Featured C.ConwayEraOnwards era (C.TxVotingProcedures v era)))
txVotingProcedures = lens get set_
 where
  get = C.txVotingProcedures
  set_ body k = body{C.txVotingProcedures = k}

_TxExtraKeyWitnesses :: (C.IsAlonzoBasedEra era) => Iso' (C.TxExtraKeyWitnesses era) [C.Hash C.PaymentKey]
_TxExtraKeyWitnesses = iso from to
 where
  from :: C.TxExtraKeyWitnesses era -> [C.Hash C.PaymentKey]
  from C.TxExtraKeyWitnessesNone = []
  from (C.TxExtraKeyWitnesses _ keys) = keys

  to [] = C.TxExtraKeyWitnessesNone
  to keys = C.TxExtraKeyWitnesses C.alonzoBasedEra keys

_TxWithdrawals :: (C.IsShelleyBasedEra era) => Iso' (C.TxWithdrawals v era) [(C.StakeAddress, Coin, BuildTxWith v (C.Witness C.WitCtxStake era))]
_TxWithdrawals = iso from to
 where
  from :: C.TxWithdrawals v era -> [(C.StakeAddress, Coin, BuildTxWith v (C.Witness C.WitCtxStake era))]
  from C.TxWithdrawalsNone = []
  from (C.TxWithdrawals _ xs) = xs

  to [] = C.TxWithdrawalsNone
  to xs = C.TxWithdrawals C.shelleyBasedEra xs

type Certificates build era =
  OMap
    (C.Certificate era)
    (BuildTxWith build (Maybe (C.StakeCredential, C.Witness C.WitCtxStake era)))

{- | The shelley-era constructor of 'C.TxCertificates'.
Consider using 'C.mkTxCertificates' and 'indexTxCertificates'
instead of constructing the 'Certificates' list directly.
-}
_TxCertificates
  :: forall era build
   . (C.IsShelleyBasedEra era)
  => Iso' (C.TxCertificates build era) (Certificates build era)
_TxCertificates = iso from to
 where
  from :: C.TxCertificates build era -> Certificates build era
  from (C.TxCertificates _era x) = x
  from _ = OMap.empty

  to :: Certificates build era -> C.TxCertificates build era
  to = C.TxCertificates C.shelleyBasedEra

txAuxScripts :: Lens' (C.TxBodyContent v era) (C.TxAuxScripts era)
txAuxScripts = lens get set_
 where
  get = C.txAuxScripts
  set_ body s = body{C.txAuxScripts = s}

_TxAuxScripts :: (C.IsAllegraBasedEra era) => Iso' (C.TxAuxScripts era) [C.ScriptInEra era]
_TxAuxScripts = iso from to
 where
  from :: C.TxAuxScripts era -> [C.ScriptInEra era]
  from = \case
    C.TxAuxScriptsNone -> []
    C.TxAuxScripts _ s -> s
  to s
    | null s = C.TxAuxScriptsNone
    | otherwise = C.TxAuxScripts C.allegraBasedEra s

_TxMetadata :: (C.IsShelleyBasedEra era) => Iso' (C.TxMetadataInEra era) (Map Word64 C.TxMetadataValue)
_TxMetadata = iso from to
 where
  from :: C.TxMetadataInEra era -> Map Word64 C.TxMetadataValue
  from = \case
    C.TxMetadataNone -> Map.empty
    C.TxMetadataInEra _ (C.TxMetadata m) -> m
  to m
    | Map.null m = C.TxMetadataNone
    | otherwise = C.TxMetadataInEra C.shelleyBasedEra (C.TxMetadata m)

_TxInsCollateralIso :: (C.IsAlonzoBasedEra era) => Iso' (C.TxInsCollateral era) [C.TxIn]
_TxInsCollateralIso = iso from to
 where
  from :: C.TxInsCollateral era -> [C.TxIn]
  from = \case
    C.TxInsCollateralNone -> []
    C.TxInsCollateral _ xs -> xs
  to = \case
    [] -> C.TxInsCollateralNone
    xs -> C.TxInsCollateral C.alonzoBasedEra xs

_TxMintValue :: forall era build. (C.IsMaryBasedEra era) => Iso' (TxMintValue build era) (Map PolicyId (Map C.AssetName C.Quantity, BuildTxWith build (ScriptWitness WitCtxMint era)))
_TxMintValue = iso from to
 where
  from :: TxMintValue build era -> Map PolicyId (Map C.AssetName C.Quantity, BuildTxWith build (ScriptWitness WitCtxMint era))
  from = \case
    C.TxMintNone -> mempty
    C.TxMintValue _ mp -> fromCapiMap mp
  to mp
    | Map.null mp = C.TxMintNone
    | otherwise = C.TxMintValue C.maryBasedEra $ toCapiMap mp

  toCapiMap = Map.map (first C.PolicyAssets) . removeZeroEntries

  fromCapiMap =
    -- Remove 0 quantity entries
    removeZeroEntries
      -- Convert to nested map structure
      . Map.map
        ( \(C.PolicyAssets innerMap, wit) ->
            (innerMap, wit)
        )

  removeZeroEntries =
    Map.filter
      ( \(innerMap, _wit) ->
          let filteredInner = Map.filter (/= 0) innerMap
           in not $ Map.null filteredInner
      )

_TxInsReferenceIso :: forall era build. (C.IsBabbageBasedEra era, Applicative (BuildTxWith build)) => Iso' (C.TxInsReference build era) ([C.TxIn], C.BuildTxWith build (Set C.HashableScriptData))
_TxInsReferenceIso = iso from to
 where
  from :: C.TxInsReference build era -> ([C.TxIn], C.BuildTxWith build (Set C.HashableScriptData))
  from = \case
    C.TxInsReferenceNone -> ([], mempty)
    C.TxInsReference _ ins refDatums -> (ins, refDatums)
  to = \case
    ([], _) -> C.TxInsReferenceNone
    (xs, datums) -> C.TxInsReference C.babbageBasedEra xs datums

_Value :: Iso' Value (Map AssetId Quantity)
_Value = iso from to
 where
  -- the 'Value' constructor is not exposed so we have to take the long way around
  from = Map.fromList . toList
  to = fromList . Map.toList

_AssetId :: Prism' C.AssetId (C.PolicyId, C.AssetName)
_AssetId = prism' from to
 where
  from (p, a) = C.AssetId p a
  to = \case
    C.AssetId p a -> Just (p, a)
    _1 -> Nothing

_TxOut :: Iso' (TxOut ctx era) (AddressInEra era, TxOutValue era, TxOutDatum ctx era, ReferenceScript era)
_TxOut = iso from to
 where
  from (C.TxOut addr vl dt rs) = (addr, vl, dt, rs)
  to (addr, vl, dt, rs) = C.TxOut addr vl dt rs

_TxOutDatumHash :: forall ctx era. (C.IsAlonzoBasedEra era) => Prism' (TxOutDatum ctx era) (C.Hash C.ScriptData)
_TxOutDatumHash = prism' from to
 where
  to :: TxOutDatum ctx era -> Maybe (C.Hash C.ScriptData)
  to (C.TxOutDatumHash _ h) = Just h
  to _ = Nothing
  from :: C.Hash C.ScriptData -> TxOutDatum ctx era
  from h = C.TxOutDatumHash C.alonzoBasedEra h

_TxOutSupplementalDatum :: forall era. (C.IsAlonzoBasedEra era) => Prism' (TxOutDatum CtxTx era) C.HashableScriptData
_TxOutSupplementalDatum = prism' from to
 where
  to :: TxOutDatum CtxTx era -> Maybe C.HashableScriptData
  to (C.TxOutSupplementalDatum _ k) = Just k
  to _ = Nothing
  from :: C.HashableScriptData -> TxOutDatum CtxTx era
  from cd = C.TxOutSupplementalDatum C.alonzoBasedEra cd

_TxOutDatumInline :: forall ctx era. (C.IsBabbageBasedEra era) => Prism' (TxOutDatum ctx era) C.HashableScriptData
_TxOutDatumInline = prism' from to
 where
  to :: TxOutDatum ctx era -> Maybe C.HashableScriptData
  to (C.TxOutDatumInline _ k) = Just k
  to _ = Nothing
  from :: C.HashableScriptData -> TxOutDatum ctx era
  from cd = C.TxOutDatumInline C.babbageBasedEra cd

_ShelleyAddress :: (C.IsShelleyBasedEra era) => Prism' (C.AddressInEra era) (Shelley.Network, Credential.PaymentCredential, Credential.StakeReference)
_ShelleyAddress = prism' from to
 where
  to :: C.AddressInEra era -> Maybe (Shelley.Network, Credential.PaymentCredential, Credential.StakeReference)
  to x = case x of
    (C.AddressInEra (C.ShelleyAddressInEra _era) (C.ShelleyAddress ntw pmt stakeRef)) -> Just (ntw, pmt, stakeRef)
    (C.AddressInEra (C.ByronAddressInAnyEra) _) -> Nothing
  from (ntw, pmt, stakeRef) = C.AddressInEra (C.ShelleyAddressInEra C.shelleyBasedEra) (C.ShelleyAddress ntw pmt stakeRef)

_PaymentCredentialByKey :: Prism' C.PaymentCredential (C.Hash C.PaymentKey)
_PaymentCredentialByKey = prism' from to
 where
  from = C.PaymentCredentialByKey
  to (C.PaymentCredentialByKey k) = Just k
  to _ = Nothing

_ShelleyPaymentCredentialByKey :: Prism' (Credential.PaymentCredential) (Keys.KeyHash 'Keys.Payment)
_ShelleyPaymentCredentialByKey = prism' from to
 where
  from = Credential.KeyHashObj
  to (Credential.KeyHashObj k) = Just k
  to Credential.ScriptHashObj{} = Nothing

_PaymentCredentialByScript :: Prism' C.PaymentCredential C.ScriptHash
_PaymentCredentialByScript = prism' from to
 where
  from = C.PaymentCredentialByScript
  to (C.PaymentCredentialByScript k) = Just k
  to C.PaymentCredentialByKey{} = Nothing

_ShelleyPaymentCredentialByScript :: Prism' (Credential.PaymentCredential) (Hashes.ScriptHash)
_ShelleyPaymentCredentialByScript = prism' from to
 where
  from = Credential.ScriptHashObj
  to (Credential.ScriptHashObj s) = Just s
  to Credential.KeyHashObj{} = Nothing

_KeyWitness :: Prism' (C.Witness witctx era) (C.KeyWitnessInCtx witctx)
_KeyWitness = prism' from to
 where
  from = C.KeyWitness
  to :: C.Witness witctx era -> Maybe (C.KeyWitnessInCtx witctx)
  to = \case
    C.KeyWitness w -> Just w
    C.ScriptWitness{} -> Nothing

_ScriptWitness :: Prism' (C.Witness witctx era) (C.ScriptWitnessInCtx witctx, C.ScriptWitness witctx era)
_ScriptWitness = prism' from to
 where
  from (a, b) = C.ScriptWitness a b
  to :: C.Witness witctx era -> Maybe (C.ScriptWitnessInCtx witctx, C.ScriptWitness witctx era)
  to = \case
    C.ScriptWitness a b -> Just (a, b)
    C.KeyWitness{} -> Nothing

_ViewTx :: Iso' (C.BuildTxWith ViewTx a) ()
_ViewTx = iso from to
 where
  from :: C.BuildTxWith ViewTx a -> ()
  from = \case
    C.ViewTx{} -> ()
  to () = C.ViewTx{}

_BuildTxWith :: Iso' (C.BuildTxWith BuildTx a) a
_BuildTxWith = iso from to
 where
  from :: C.BuildTxWith BuildTx a -> a
  from = \case
    C.BuildTxWith a -> a
  to = C.BuildTxWith

_TxOutValue
  :: forall era
   . (C.IsMaryBasedEra era)
  => Iso' (TxOutValue era) Value
_TxOutValue = iso from to
 where
  from = C.txOutValueToValue
  to :: Value -> TxOutValue era
  to = C.maryEraOnwardsConstraints @era C.maryBasedEra $ C.TxOutValueShelleyBased C.shelleyBasedEra . C.toMaryValue

slot :: Lens' (LedgerEnv era) SlotNo
slot = lens get set_
 where
  get = ledgerSlotNo
  set_ l s = l{ledgerSlotNo = s}

{- | 'UTxOState' iso. Note that this doesn't touch the 'utxosInstantStake' field. This is because the
stake distro is a function of @utxo :: UTxO era@ and can be computed by @addInstantStake utxosUtxo mempty@.
-}
_UTxOState :: forall era. (EraStake era) => Iso' (UTxOState era) (UTxO era, Coin, Coin, GovState era, Coin)
_UTxOState = iso from to
 where
  from UTxOState{utxosUtxo, utxosDeposited, utxosFees, utxosGovState, utxosDonation} = (utxosUtxo, utxosDeposited, utxosFees, utxosGovState, utxosDonation)
  to (utxosUtxo, utxosDeposited, utxosFees, utxosGovState, utxosDonation) = UTxOState{utxosUtxo, utxosDeposited, utxosFees, utxosGovState, utxosDonation, utxosInstantStake = addInstantStake utxosUtxo mempty}

utxoState :: Lens' (LedgerState era) (UTxOState era)
utxoState = lens get set_
 where
  get = lsUTxOState
  set_ l s = l{lsUTxOState = s}

_AddressInEra :: (C.IsShelleyBasedEra era) => Prism' (AddressInEra era) (Address ShelleyAddr)
_AddressInEra = prism' from to
 where
  to :: AddressInEra era -> Maybe (Address ShelleyAddr)
  to (C.AddressInEra (C.ShelleyAddressInEra _era) addr) = Just addr
  to _ = Nothing
  from = C.AddressInEra (C.ShelleyAddressInEra C.shelleyBasedEra)

_Address :: Iso' (Address ShelleyAddr) (Shelley.Network, Credential.PaymentCredential, Credential.StakeReference)
_Address = iso from to
 where
  from :: Address ShelleyAddr -> (Shelley.Network, Credential.PaymentCredential, Credential.StakeReference)
  from (C.ShelleyAddress n p s) = (n, p, s)
  to (n, p, s) = C.ShelleyAddress n p s

_KeyHash :: Iso' (Keys.KeyHash 'Keys.Payment) (C.Hash C.PaymentKey)
_KeyHash = iso from to
 where
  from :: Keys.KeyHash 'Keys.Payment -> C.Hash C.PaymentKey
  from hash = C.PaymentKeyHash hash
  to (C.PaymentKeyHash h) = h

_ScriptHash :: Iso' (Hashes.ScriptHash) C.ScriptHash
_ScriptHash = iso from to
 where
  from :: Hashes.ScriptHash -> C.ScriptHash
  from = C.fromShelleyScriptHash

  to :: C.ScriptHash -> Hashes.ScriptHash
  to = C.toShelleyScriptHash

_PlutusPubKeyHash :: Prism' PubKeyHash (C.Hash C.PaymentKey)
_PlutusPubKeyHash = prism' from to
 where
  from :: C.Hash C.PaymentKey -> PubKeyHash
  from = PubKeyHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

  to :: PubKeyHash -> Maybe (C.Hash C.PaymentKey)
  to (PubKeyHash h) = either (const Nothing) Just $ C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @(C.Hash C.PaymentKey)) $ PlutusTx.fromBuiltin h

_PaymentCredential :: Iso' C.PaymentCredential (Credential.PaymentCredential)
_PaymentCredential = iso from to
 where
  from :: C.PaymentCredential -> Credential.PaymentCredential
  from (C.PaymentCredentialByKey (C.PaymentKeyHash kh)) = Credential.KeyHashObj kh
  from (C.PaymentCredentialByScript sh) = Credential.ScriptHashObj (C.toShelleyScriptHash sh)

  to = C.fromShelleyPaymentCredential

_ScriptData :: forall a. (PV1.FromData a, PV1.ToData a) => Prism' C.ScriptData a
_ScriptData = prism' from to
 where
  to :: C.ScriptData -> Maybe a
  to = Scripts.fromScriptData

  from :: a -> C.ScriptData
  from = Scripts.toScriptData

_PlutusScriptWitness
  :: forall era lang witctx
   . (C.IsPlutusScriptLanguage lang)
  => C.PlutusScriptVersion lang
  -> Prism'
      (C.ScriptWitness witctx era)
      ( C.ScriptLanguageInEra lang era
      , C.PlutusScriptVersion lang
      , C.PlutusScriptOrReferenceInput lang
      , C.ScriptDatum witctx
      , C.ScriptRedeemer
      , C.ExecutionUnits
      )
_PlutusScriptWitness lang = prism' from (to lang)
 where
  from :: (C.ScriptLanguageInEra lang era, C.PlutusScriptVersion lang, C.PlutusScriptOrReferenceInput lang, C.ScriptDatum witctx, C.ScriptRedeemer, C.ExecutionUnits) -> C.ScriptWitness witctx era
  from (era, v, i, dtr, red, ex) = C.PlutusScriptWitness era v i dtr red ex

  to :: C.PlutusScriptVersion lang -> C.ScriptWitness witctx era -> Maybe (C.ScriptLanguageInEra lang era, C.PlutusScriptVersion lang, C.PlutusScriptOrReferenceInput lang, C.ScriptDatum witctx, C.ScriptRedeemer, C.ExecutionUnits)
  to C.PlutusScriptV1 (C.PlutusScriptWitness era C.PlutusScriptV1 i dtr red ex) = Just (era, lang, i, dtr, red, ex)
  to C.PlutusScriptV2 (C.PlutusScriptWitness era C.PlutusScriptV2 i dtr red ex) = Just (era, lang, i, dtr, red, ex)
  to C.PlutusScriptV3 (C.PlutusScriptWitness era C.PlutusScriptV3 i dtr red ex) = Just (era, lang, i, dtr, red, ex)
  to C.PlutusScriptV1 (C.PlutusScriptWitness{}) = Nothing
  to C.PlutusScriptV2 (C.PlutusScriptWitness{}) = Nothing
  to C.PlutusScriptV3 (C.PlutusScriptWitness{}) = Nothing
  to _ C.SimpleScriptWitness{} = Nothing

_TxValidityNoLowerBound :: forall era. Prism' (C.TxValidityLowerBound era) ()
_TxValidityNoLowerBound = prism' from to
 where
  from () = C.TxValidityNoLowerBound
  to = \case
    C.TxValidityNoLowerBound -> Just ()
    _ -> Nothing

_TxValidityLowerBound :: forall era. Prism' (C.TxValidityLowerBound era) (C.AllegraEraOnwards era, C.SlotNo)
_TxValidityLowerBound = prism' from to
 where
  from (s, e) = C.TxValidityLowerBound s e
  to = \case
    C.TxValidityLowerBound s e -> Just (s, e)
    _ -> Nothing

_TxValidityUpperBound :: forall era. Iso' (C.TxValidityUpperBound era) (C.ShelleyBasedEra era, Maybe SlotNo)
_TxValidityUpperBound = iso from to
 where
  from :: C.TxValidityUpperBound era -> (C.ShelleyBasedEra era, Maybe SlotNo)
  from = \case
    C.TxValidityUpperBound k s -> (k, s)

  to :: (C.ShelleyBasedEra era, Maybe SlotNo) -> C.TxValidityUpperBound era
  to (k, s) = C.TxValidityUpperBound k s

_TxValidityFiniteRange :: (C.IsAllegraBasedEra era) => Prism' (C.TxValidityLowerBound era, C.TxValidityUpperBound era) (SlotNo, SlotNo)
_TxValidityFiniteRange = prism' from to
 where
  from (l, u) = (C.TxValidityLowerBound C.allegraBasedEra l, C.TxValidityUpperBound C.shelleyBasedEra (Just u))
  to = \case
    (C.TxValidityLowerBound _ l, C.TxValidityUpperBound _ (Just u)) -> Just (l, u)
    _ -> Nothing

_Interval :: Iso' (Interval a) (LowerBound a, UpperBound a)
_Interval = iso from to
 where
  from Interval{ivFrom, ivTo} = (ivFrom, ivTo)
  to (ivFrom, ivTo) = Interval{ivFrom, ivTo}

_UpperBound :: Iso' (UpperBound a) (Extended a, Closure)
_UpperBound = iso from to
 where
  from (UpperBound a b) = (a, b)
  to = uncurry UpperBound

_LowerBound :: Iso' (LowerBound a) (Extended a, Closure)
_LowerBound = iso from to
 where
  from (LowerBound a b) = (a, b)
  to = uncurry LowerBound

_NegInf :: Prism' (Extended a) ()
_NegInf = prism' from to
 where
  from () = NegInf
  to = \case
    NegInf -> Just ()
    _ -> Nothing

_PosInf :: Prism' (Extended a) ()
_PosInf = prism' from to
 where
  from () = PosInf
  to = \case
    PosInf -> Just ()
    _ -> Nothing

_Finite :: Prism' (Extended a) a
_Finite = prism' from to
 where
  from = Finite
  to = \case
    Finite a -> Just a
    _ -> Nothing

{- | Access a finite interval with the two boundaries and closures. This is
  convenient if you want to change the length of the interval for example.
-}
_FiniteInterval :: Prism' (Interval a) ((a, a), (Closure, Closure))
_FiniteInterval = prism' from to
 where
  from ((l, u), (lc, uc)) = Interval (LowerBound (Finite l) lc) (UpperBound (Finite u) uc)
  to (Interval (LowerBound (Finite l) lc) (UpperBound (Finite u) uc)) = Just ((l, u), (lc, uc))
  to _ = Nothing
