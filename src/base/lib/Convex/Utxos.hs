{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- FIXME (koslambrou) Remove once we have the newtype for 'AnyCardanoEra TxOut'.
{-# OPTIONS_GHC -Wno-orphans #-}

module Convex.Utxos(
  -- * Utxo sets
  UtxoSet(..),
  fromUtxoTx,
  singleton,
  PrettyBalance(..),
  _UtxoSet,
  totalBalance,
  partition,
  onlyAda,
  onlyPubKey,
  onlyAddress,
  onlyCredential,
  onlyCredentials,
  onlyStakeCredential,
  removeUtxos,
  fromApiUtxo,
  toApiUtxo,
  txOutToLatestEra,
  utxoToLatestEra,
  selectUtxo,

  -- * Events based on transactions
  UtxoChangeEvent,
  AddUtxoEvent(..),
  RemoveUtxoEvent(..),
  extract,
  extract_,
  extractBabbageTxn,
  txId,

  -- * Changes to utxo sets
  UtxoChange(..),
  toUtxoChangeTx,
  fromEvent,
  PrettyUtxoChange(..),
  outputsAdded,
  outputsRemoved,
  null,
  apply,
  inv,
  describeChange,

  -- * Changes to addresses
  BalanceChanges(..),
  balanceChange,
  invBalanceChange,
  changeFor,
  changeForAddress
  ) where

import           Cardano.Api                   (AddressInEra, BabbageEra,
                                                Block (..), BlockInMode (..),
                                                HashableScriptData,
                                                PaymentCredential,
                                                StakeCredential, Tx (..), TxId,
                                                TxIn (..), TxIx (..), UTxO (..))
import qualified Cardano.Api                   as C
import           Cardano.Api.Shelley           (ExecutionUnits, TxBody (..))
import qualified Cardano.Api.Shelley           as CS
import qualified Cardano.Ledger.Alonzo.Scripts as Scripts
import           Cardano.Ledger.Alonzo.TxWits  (unRedeemers)
import qualified Cardano.Ledger.Babbage.TxBody as Babbage.TxBody
import qualified Cardano.Ledger.BaseTypes      as CT
import qualified Cardano.Ledger.Credential     as Shelley
import           Cardano.Ledger.Crypto         (StandardCrypto)
import qualified Cardano.Ledger.TxIn           as CT
import           Control.Lens                  (_2, makeLenses, makePrisms,
                                                over, preview, view)
import qualified Convex.CardanoApi.Lenses      as L
import           Data.Aeson                    (FromJSON, ToJSON,
                                                Value (String), object,
                                                parseJSON, toJSON, withObject,
                                                (.:), (.=))
import           Data.Bifunctor                (Bifunctor (..))
import           Data.DList                    (DList)
import qualified Data.DList                    as DList
import           Data.Kind                     (Constraint, Type)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (isJust, isNothing, listToMaybe,
                                                mapMaybe)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           GHC.Word                      (Word32)
import           Prelude                       hiding (null)
import           Prettyprinter                 (Doc, Pretty (..), hang, parens,
                                                viaShow, vsep, (<+>))
import qualified Prettyprinter

type AddressCredential = Shelley.PaymentCredential StandardCrypto

{-| A set of unspent transaction outputs
-}
newtype UtxoSet ctx a = UtxoSet{ _utxos :: Map C.TxIn (C.InAnyCardanoEra (C.TxOut ctx), a) }
  deriving stock (Functor)
  deriving newtype (Semigroup, Monoid)

instance Show a => Show (UtxoSet ctx a) where
  show (UtxoSet set1) = show set1

instance Eq a => Eq (UtxoSet ctx a) where
  (UtxoSet set1) == (UtxoSet set2) = set1 == set2

deriving instance FromJSON a => FromJSON (UtxoSet C.CtxTx a)
deriving instance FromJSON a => FromJSON (UtxoSet C.CtxUTxO a)
deriving instance ToJSON a => ToJSON (UtxoSet C.CtxTx a)
deriving instance ToJSON a => ToJSON (UtxoSet C.CtxUTxO a)

-- FIXME (koslambrou) Remove those orphan instances and use custom type instead such as TxOutInAnyEra
instance Show (C.InAnyCardanoEra (C.TxOut ctx)) where
  show (C.InAnyCardanoEra _ txOut) = show txOut

instance Eq (C.InAnyCardanoEra (C.TxOut ctx)) where
  (C.InAnyCardanoEra C.ByronEra txOutL) == (C.InAnyCardanoEra C.ByronEra txOutR) = txOutL == txOutR
  (C.InAnyCardanoEra C.ShelleyEra txOutL) == (C.InAnyCardanoEra C.ShelleyEra txOutR) = txOutL == txOutR
  (C.InAnyCardanoEra C.AllegraEra txOutL) == (C.InAnyCardanoEra C.AllegraEra txOutR) = txOutL == txOutR
  (C.InAnyCardanoEra C.MaryEra txOutL) == (C.InAnyCardanoEra C.MaryEra txOutR) = txOutL == txOutR
  (C.InAnyCardanoEra C.AlonzoEra txOutL) == (C.InAnyCardanoEra C.AlonzoEra txOutR) = txOutL == txOutR
  (C.InAnyCardanoEra C.BabbageEra txOutL) == (C.InAnyCardanoEra C.BabbageEra txOutR) = txOutL == txOutR
  (C.InAnyCardanoEra C.ConwayEra txOutL) == (C.InAnyCardanoEra C.ConwayEra txOutR) = txOutL == txOutR
  _ == _ = False

instance ToJSON (C.InAnyCardanoEra (C.TxOut ctx)) where
  toJSON (C.InAnyCardanoEra C.ByronEra txOut) =
    object
      [ "tag" .= String "ByronTxOut"
      , "txOut" .= toJSON txOut
      ]
  toJSON (C.InAnyCardanoEra C.ShelleyEra txOut) =
    object
      [ "tag" .= String "ShelleyTxOut"
      , "txOut" .= toJSON txOut
      ]
  toJSON (C.InAnyCardanoEra C.AllegraEra txOut) =
    object
      [ "tag" .= String "AllegraTxOut"
      , "txOut" .= toJSON txOut
      ]
  toJSON (C.InAnyCardanoEra C.MaryEra txOut) =
    object
      [ "tag" .= String "MaryTxOut"
      , "txOut" .= toJSON txOut
      ]
  toJSON (C.InAnyCardanoEra C.AlonzoEra txOut) =
    object
      [ "tag" .= String "AlonzoTxOut"
      , "txOut" .= toJSON txOut
      ]
  toJSON (C.InAnyCardanoEra C.BabbageEra txOut) =
    object
      [ "tag" .= String "BabbageTxOut"
      , "txOut" .= toJSON txOut
      ]
  toJSON (C.InAnyCardanoEra C.ConwayEra txOut) =
    object
      [ "tag" .= String "ConwayTxOut"
      , "txOut" .= toJSON txOut
      ]

type TxOutConstraints (k :: Type -> Constraint) ctx =
  ( k (CS.TxOut ctx CS.ShelleyEra)
  , k (CS.TxOut ctx CS.AllegraEra)
  , k (CS.TxOut ctx CS.MaryEra)
  , k (CS.TxOut ctx CS.AlonzoEra)
  , k (CS.TxOut ctx CS.BabbageEra)
  , k (CS.TxOut ctx CS.ConwayEra)
  )

-- FIXME (koslambou) Remove duplication with similar instance below
instance TxOutConstraints FromJSON ctx => FromJSON (C.InAnyCardanoEra (C.TxOut ctx)) where
  parseJSON = withObject "InAnyCardanoEra (C.TxOut C.CtxTx)" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "ShelleyTxOut" -> fmap (C.InAnyCardanoEra C.ShelleyEra) $ o .: "txOut"
      "AllegraTxOut" -> fmap (C.InAnyCardanoEra C.AllegraEra) $ o .: "txOut"
      "MaryTxOut" -> fmap (C.InAnyCardanoEra C.MaryEra) $ o .: "txOut"
      "AlonzoTxOut" -> fmap (C.InAnyCardanoEra C.AlonzoEra) $ o .: "txOut"
      "BabbageTxOut" -> fmap (C.InAnyCardanoEra C.BabbageEra) $ o .: "txOut"
      "ConwayTxOut" -> fmap (C.InAnyCardanoEra C.ConwayEra) $ o .: "txOut"
      _ -> fail "Expected tag to be ShelleyTxOut, AllegraTxOut, MaryTxOut, AlonzoTxOut, BabbageTxOut, ConwayTxOut"

-- deriving instance (FromJSON a, FromJSON (C.TxOut ctx C.BabbageEra)) => FromJSON (UtxoSet ctx a)
-- deriving instance (ToJSON a, ToJSON (C.TxOut ctx C.BabbageEra)) => ToJSON (UtxoSet ctx a)

{-| A utxo set with one element
-}
singleton :: TxIn -> (C.InAnyCardanoEra (C.TxOut ctx), a) -> UtxoSet ctx a
singleton txi = UtxoSet . Map.singleton txi

{-| Change the context of the outputs in this utxo set to 'CtxUTxO'
-}
fromUtxoTx :: UtxoSet C.CtxTx a -> UtxoSet C.CtxUTxO a
fromUtxoTx = UtxoSet . fmap (first fromTxOutInAnyEraTx) . _utxos

fromTxOutInAnyEraTx :: C.InAnyCardanoEra (C.TxOut C.CtxTx) -> C.InAnyCardanoEra (C.TxOut C.CtxUTxO)
fromTxOutInAnyEraTx (C.InAnyCardanoEra era txOut) = (C.InAnyCardanoEra era $ C.toCtxUTxOTxOut txOut)

makePrisms ''UtxoSet

{-| Convert a @cardano-api@ 'UTxO BabbageEra' to a utxo set
-}
fromApiUtxo :: C.IsCardanoEra era => a -> UTxO era -> UtxoSet C.CtxUTxO a
fromApiUtxo v (UTxO utxoSet) = UtxoSet (fmap (\x -> (C.InAnyCardanoEra C.cardanoEra x, v)) utxoSet)

{-| Convert a utxo set to a @cardano-api@ 'UTxO BabbageEra'
-}
toApiUtxo :: UtxoSet C.CtxUTxO a -> UTxO C.BabbageEra
toApiUtxo (UtxoSet utxos) =
  UTxO (fmap (toTxOut . fst) utxos)
 where
  toTxOut :: C.InAnyCardanoEra (C.TxOut C.CtxUTxO) -> C.TxOut C.CtxUTxO C.BabbageEra
  toTxOut (C.InAnyCardanoEra _era txOut) = utxoToLatestEra txOut

-- FIXME (koslambrou) Move to proper package
txOutToLatestEra :: C.TxOut C.CtxTx era -> C.TxOut C.CtxTx C.BabbageEra
txOutToLatestEra (C.TxOut addrInEra txOutValue txOutDatum ref) =
  C.TxOut
    (convertAddrToLatestEra addrInEra)
    (C.TxOutValueShelleyBased C.ShelleyBasedEraBabbage $
      C.toLedgerValue C.MaryEraOnwardsBabbage $ C.txOutValueToValue txOutValue)
    (convertDatumToLatestEra txOutDatum)
    (convertRefScriptToLatestEra ref)
 where
  convertAddrToLatestEra :: C.AddressInEra era -> C.AddressInEra C.BabbageEra
  convertAddrToLatestEra (C.AddressInEra C.ByronAddressInAnyEra addr) =
    C.AddressInEra C.ByronAddressInAnyEra addr
  convertAddrToLatestEra (C.AddressInEra (C.ShelleyAddressInEra _) addr) =
    C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) addr

  convertDatumToLatestEra :: C.TxOutDatum C.CtxTx era -> C.TxOutDatum C.CtxTx C.BabbageEra
  convertDatumToLatestEra C.TxOutDatumNone = C.TxOutDatumNone
  convertDatumToLatestEra (C.TxOutDatumHash _ h) = C.TxOutDatumHash C.AlonzoEraOnwardsBabbage h
  convertDatumToLatestEra (C.TxOutDatumInTx _ d) = C.TxOutDatumInTx C.AlonzoEraOnwardsBabbage d
  convertDatumToLatestEra (C.TxOutDatumInline _ d) = C.TxOutDatumInline C.BabbageEraOnwardsBabbage d

  convertRefScriptToLatestEra :: CS.ReferenceScript era -> CS.ReferenceScript CS.BabbageEra
  convertRefScriptToLatestEra CS.ReferenceScriptNone = CS.ReferenceScriptNone
  convertRefScriptToLatestEra (CS.ReferenceScript _ script) = CS.ReferenceScript CS.BabbageEraOnwardsBabbage script

-- FIXME (koslambrou) Move to proper package
utxoToLatestEra :: C.TxOut C.CtxUTxO era -> C.TxOut C.CtxUTxO C.BabbageEra
utxoToLatestEra (C.TxOut addrInEra txOutValue txOutDatum ref) =
  C.TxOut
    (convertAddrToLatestEra addrInEra)
    (C.TxOutValueShelleyBased C.ShelleyBasedEraBabbage $
      C.toLedgerValue C.MaryEraOnwardsBabbage $ C.txOutValueToValue txOutValue)
    (convertDatumToLatestEra txOutDatum)
    (convertRefScriptToLatestEra ref)
 where
  convertAddrToLatestEra :: C.AddressInEra era -> C.AddressInEra C.BabbageEra
  convertAddrToLatestEra (C.AddressInEra C.ByronAddressInAnyEra addr) =
    C.AddressInEra C.ByronAddressInAnyEra addr
  convertAddrToLatestEra (C.AddressInEra (C.ShelleyAddressInEra _) addr) =
    C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) addr

  convertDatumToLatestEra :: C.TxOutDatum C.CtxUTxO era -> C.TxOutDatum C.CtxUTxO C.BabbageEra
  convertDatumToLatestEra C.TxOutDatumNone = C.TxOutDatumNone
  convertDatumToLatestEra (C.TxOutDatumHash _ h) = C.TxOutDatumHash C.AlonzoEraOnwardsBabbage h
  convertDatumToLatestEra (C.TxOutDatumInline _ d) = C.TxOutDatumInline C.BabbageEraOnwardsBabbage d

  convertRefScriptToLatestEra :: CS.ReferenceScript era -> CS.ReferenceScript CS.BabbageEra
  convertRefScriptToLatestEra CS.ReferenceScriptNone = CS.ReferenceScriptNone
  convertRefScriptToLatestEra (CS.ReferenceScript _ script) = CS.ReferenceScript CS.BabbageEraOnwardsBabbage script


{-| Pick an unspent output from the 'UtxoSet', if there is one.
-}
selectUtxo :: UtxoSet ctx a -> Maybe (C.TxIn, (C.InAnyCardanoEra (C.TxOut ctx), a))
selectUtxo =
  -- sorting by key is pretty much a random order
  listToMaybe . Map.toAscList . _utxos

{-| Restrict the 'UtxoSet' to outputs that only have Ada values (no native assets)
-}
onlyAda :: UtxoSet ctx a -> UtxoSet ctx a
onlyAda =
  let txOutHasOnlyAda :: (C.InAnyCardanoEra (C.TxOut ctx), a) -> Bool
      txOutHasOnlyAda (C.InAnyCardanoEra _ txOut, _) =
        isJust $ C.valueToLovelace $ C.txOutValueToValue $ view (L._TxOut . _2) txOut
  in fst . partition txOutHasOnlyAda

{-| Partition the UtxoSet according to a predicate. The first UtxoSet contains all
utxos that satisfy the predicate, the second all utxos that fail the predicate.
-}
partition :: ((C.InAnyCardanoEra (C.TxOut ctx), a) -> Bool) -> UtxoSet ctx a -> (UtxoSet ctx a, UtxoSet ctx a)
partition p (UtxoSet s) =
  bimap UtxoSet UtxoSet (Map.partition p s)

{-| Restrict the 'UtxoSet' to outputs at the address
-}
onlyAddress :: C.AddressAny -> UtxoSet ctx a -> UtxoSet ctx a
onlyAddress expectedAddr =
  let txOutHasAddr :: (C.InAnyCardanoEra (C.TxOut ctx), a) -> Bool
      txOutHasAddr (C.InAnyCardanoEra _ (C.TxOut addrInEra _ _ _), _) =
        case addrInEra of
          C.AddressInEra C.ByronAddressInAnyEra addr -> expectedAddr == C.toAddressAny addr
          C.AddressInEra (C.ShelleyAddressInEra _) addr -> expectedAddr == C.toAddressAny addr
  in fst . partition txOutHasAddr

{-| Restrict the utxo set to outputs with the given payment credential
-}
onlyCredential :: C.PaymentCredential -> UtxoSet ctx a -> UtxoSet ctx a
onlyCredential c = onlyCredentials (Set.singleton c)

{-| Restrict the utxo set to outputs locked by one of the given payment credentials
-}
onlyCredentials :: Set (C.PaymentCredential) -> UtxoSet ctx a -> UtxoSet ctx a
onlyCredentials cs =
  let txOutHasOneOfCreds :: (C.InAnyCardanoEra (C.TxOut ctx), a) -> Bool
      txOutHasOneOfCreds (C.InAnyCardanoEra _ (C.TxOut (C.AddressInEra C.ByronAddressInAnyEra _) _ _ _), _) =
        False
      txOutHasOneOfCreds (C.InAnyCardanoEra _ (C.TxOut (C.AddressInEra (C.ShelleyAddressInEra _era) addr) _ _ _), _) =
        let (CS.ShelleyAddress _ (CS.fromShelleyPaymentCredential -> c) _) = addr
         in c `Set.member` cs
  in fst . partition txOutHasOneOfCreds

{-| Restrict the utxo set to outputs with the given stake credential
-}
onlyStakeCredential :: StakeCredential -> UtxoSet ctx a -> UtxoSet ctx a
onlyStakeCredential expectedStakeCredential =
  let txOutHasStakeCred :: (C.InAnyCardanoEra (C.TxOut ctx), a) -> Bool
      txOutHasStakeCred (C.InAnyCardanoEra _ (C.TxOut (C.AddressInEra C.ByronAddressInAnyEra _) _ _ _), _) =
        False
      txOutHasStakeCred (C.InAnyCardanoEra _ (C.TxOut (C.AddressInEra (C.ShelleyAddressInEra _era) addr) _ _ _), _) =
        let (CS.ShelleyAddress _ _ (CS.fromShelleyStakeReference -> stakeRef)) = addr
         in case stakeRef of
              C.StakeAddressByValue stakeCred -> expectedStakeCredential == stakeCred
              C.StakeAddressByPointer _ -> False
              C.NoStakeAddress -> False
  in fst . partition txOutHasStakeCred

{-| Restrict the 'UtxoSet' to public key outputs
-}
onlyPubKey :: UtxoSet ctx a -> UtxoSet ctx a
onlyPubKey =
  let txOutHasPubKeyAddr :: (C.InAnyCardanoEra (C.TxOut ctx), a) -> Bool
      txOutHasPubKeyAddr (C.InAnyCardanoEra _ (C.TxOut (C.AddressInEra C.ByronAddressInAnyEra _) _ _ _), _) =
        False
      txOutHasPubKeyAddr (C.InAnyCardanoEra _ (C.TxOut (C.AddressInEra (C.ShelleyAddressInEra _era) addr) _ _ _), _) =
        let (CS.ShelleyAddress _ (CS.fromShelleyPaymentCredential -> cred) _) = addr
         in case cred of
              C.PaymentCredentialByKey _    -> True
              C.PaymentCredentialByScript _ -> False
  in fst . partition txOutHasPubKeyAddr

{-| The combined 'Value' of all outputs in the set
-}
totalBalance :: UtxoSet ctx a -> C.Value
totalBalance =
  foldMap (\(C.InAnyCardanoEra _era (C.TxOut _ txOutValue _ _), _) -> C.txOutValueToValue txOutValue) . _utxos

{-| Delete some outputs from the 'UtxoSet'
-}
removeUtxos :: Set.Set C.TxIn -> UtxoSet ctx a -> UtxoSet ctx a
removeUtxos ins = over _UtxoSet (flip Map.withoutKeys ins)

{-| A change to the UTxO set, adding and/or removing UTxOs
-}
data UtxoChange ctx a =
  UtxoChange
    { _outputsAdded   :: !(Map C.TxIn (C.InAnyCardanoEra (C.TxOut ctx), a))
    , _outputsRemoved :: !(Map C.TxIn (C.InAnyCardanoEra (C.TxOut ctx), a))
    }

{-| Change the context of the outputs in this utxo change
-}
toUtxoChangeTx :: UtxoChange C.CtxTx a -> UtxoChange C.CtxUTxO a
toUtxoChangeTx (UtxoChange added removed) =
  UtxoChange (fmap (first fromTxOutInAnyEraTx) added) (fmap (first fromTxOutInAnyEraTx) removed)

makeLenses ''UtxoChange
-- TODO: change '<>' so that @x <> invert x == mempty@ and @invert x <> x == mempty@

instance Semigroup (UtxoChange ctx a) where
  l <> r =
    UtxoChange
      { _outputsAdded   = _outputsAdded l <> _outputsAdded r
      , _outputsRemoved = _outputsRemoved l <> _outputsRemoved r
      }

instance Monoid (UtxoChange ctx a) where
  mempty = UtxoChange mempty mempty

{-| Is this the empty 'UtxoChange'?
-}
null :: UtxoChange ctx a -> Bool
null UtxoChange{_outputsAdded, _outputsRemoved} = Map.null _outputsAdded && Map.null _outputsRemoved

{-| An event that caused the utxo set to change
-}
type UtxoChangeEvent a = Either (AddUtxoEvent a) (RemoveUtxoEvent a)

{-| A new tx out was added
-}
data AddUtxoEvent a =
  AddUtxoEvent
    { aueEvent :: !a
    , aueTxOut :: !(C.InAnyCardanoEra (C.TxOut C.CtxTx))
    , aueTxIn  :: !TxIn
    , aueTxId  :: !TxId
    , aueTx    :: C.Tx BabbageEra
    }

{-| A tx output was spent
-}
data RemoveUtxoEvent a =
  RemoveUtxoEvent
    { rueEvent    :: !a
    , rueTxOut    :: !(C.InAnyCardanoEra (C.TxOut C.CtxTx))
    , rueTxIn     :: !TxIn
    , rueTxId     :: !TxId
    -- ^ Id of the transaction that spent the output
    , rueTx       :: C.Tx BabbageEra
    -- ^ The transaction that spent the output
    , rueRedeemer :: Maybe (HashableScriptData, ExecutionUnits) -- fromAlonzoData
    }

{-| The 'UtxoChange' represented by the event.
-}
fromEvent :: UtxoChangeEvent a -> UtxoChange C.CtxTx a
fromEvent = \case
  Left AddUtxoEvent{aueEvent, aueTxOut, aueTxIn} ->
    let ch = Map.singleton aueTxIn (aueTxOut, aueEvent)
    in UtxoChange ch mempty
  Right RemoveUtxoEvent{rueEvent, rueTxOut, rueTxIn} ->
    let ch = Map.singleton rueTxIn (rueTxOut, rueEvent)
    in UtxoChange mempty ch

{-| ID of the transaction that caused the event
-}
txId :: UtxoChangeEvent a -> TxId
txId = either aueTxId rueTxId

{-| A type capturing the effect a 'UtxoChange' has on the total balance of each address that it touches
-}
newtype BalanceChanges = BalanceChanges{tbBalances :: Map PaymentCredential C.Value }
  deriving stock (Eq, Show)

prettyAda :: C.Quantity -> Doc ann
prettyAda (C.Quantity lvl) =
  let ada :: Double = fromIntegral lvl / 1_000_000
  in pretty ada

prettyPolicy :: C.PolicyId -> C.AssetName -> Doc ann
prettyPolicy p a =
  let ps = C.serialiseToRawBytesHexText p
      x = Text.take 4 ps
      md = Text.drop 52 ps
  in pretty x <> "..." <> pretty md <+> viaShow a

instance Pretty BalanceChanges where
  pretty (BalanceChanges mp) =
    let f (paymentCredential, vl) =
          hang 4 $ vsep $ viaShow paymentCredential : prettyValue vl
    in vsep (f <$> Map.toAscList mp)

prettyValue :: C.Value -> [Doc ann]
prettyValue vl =
  let k (C.AdaAssetId, l)             = "Ada" <+> prettyAda l
      k (C.AssetId p n, C.Quantity q) = prettyPolicy p n <+> pretty q
  in k <$> C.valueToList vl

invBalanceChange :: BalanceChanges -> BalanceChanges
invBalanceChange = BalanceChanges . Map.map C.negateValue . tbBalances

instance Semigroup BalanceChanges where
  (BalanceChanges l) <> (BalanceChanges r) =
    BalanceChanges (Map.unionWith (<>) l r)

instance Monoid BalanceChanges where
  mempty = BalanceChanges mempty

{-| The change in currency affected by the 'UtxoChange' on each address
-}
balanceChange :: UtxoChange ctx a -> BalanceChanges
balanceChange UtxoChange{_outputsAdded, _outputsRemoved} =
  let k :: (C.InAnyCardanoEra (C.TxOut ctx), a) -> Maybe (C.PaymentCredential, C.Value)
      k (C.InAnyCardanoEra _ (C.TxOut (C.AddressInEra C.ByronAddressInAnyEra _) _ _ _), _) = Nothing
      k (C.InAnyCardanoEra _ (C.TxOut (C.AddressInEra (C.ShelleyAddressInEra _era) addr) txOutValue _ _), _) =
        let (CS.ShelleyAddress _ (CS.fromShelleyPaymentCredential -> cred) _) = addr
         in Just (cred, C.txOutValueToValue txOutValue)
      tv = Map.fromListWith (<>) . mapMaybe (k . snd) . Map.toList

  in BalanceChanges (tv _outputsAdded <> fmap CS.negateValue (tv _outputsRemoved))

{-| Balance change for the payment credential of a particular address. Note
that this may include the change for addresses with the same payment
credential and different staking credentials.
-}
changeForAddress :: AddressInEra C.BabbageEra -> BalanceChanges -> C.Value
changeForAddress (fmap CS.fromShelleyPaymentCredential . preview (L._AddressInEra . L._Address . _2) -> Just cred) c =
  changeFor cred c
changeForAddress _ _ = mempty

{-| Change for a 'PaymentCredential'
-}
changeFor :: PaymentCredential -> BalanceChanges -> C.Value
changeFor cred (BalanceChanges c) = Map.findWithDefault mempty cred c

{-| Describe the UtxoChange
-}
describeChange :: UtxoChange ctx a -> Text
describeChange UtxoChange{_outputsAdded, _outputsRemoved} =
  let tshow = Text.pack . show
  in tshow (Map.size _outputsAdded) <> " outputs added, " <> tshow (Map.size _outputsRemoved) <> " outputs removed"

newtype PrettyUtxoChange ctx a = PrettyUtxoChange (UtxoChange ctx a)

instance Pretty a => Pretty (PrettyUtxoChange ctx a) where
  pretty (PrettyUtxoChange UtxoChange{_outputsAdded, _outputsRemoved}) =
    let b = foldMap (\((C.InAnyCardanoEra _ (C.TxOut _ txOutValue _ _)), _) -> C.txOutValueToValue txOutValue)
        bPlus = b _outputsAdded
        bMinus = C.negateValue (b _outputsRemoved)
    in Prettyprinter.hsep $
        [ pretty (Map.size _outputsAdded)
        , "outputs added"
        , pretty (Map.size _outputsRemoved), "outputs removed."]
        ++ (prettyValue (bPlus <> bMinus))

newtype PrettyBalance ctx  a = PrettyBalance (UtxoSet ctx a)

instance Pretty a => Pretty (PrettyBalance ctx a) where
  pretty (PrettyBalance bal) =
    let nOutputs = Map.size (_utxos bal)
    in hang 4 $ vsep
        $ ("Balance" <+> parens (pretty nOutputs <+> "outputs") <> ":")
        : prettyValue (totalBalance bal)

{-| Change the 'UtxoSet'
-}
apply :: UtxoSet ctx a -> UtxoChange ctx a -> UtxoSet ctx a
apply UtxoSet{_utxos} UtxoChange{_outputsAdded, _outputsRemoved} =
  UtxoSet $ (_utxos `Map.union` _outputsAdded) `Map.difference` _outputsRemoved

{-| Invert a 'UtxoChange' value
-}
inv :: UtxoChange ctx a -> UtxoChange ctx a
inv (UtxoChange added removed) = UtxoChange removed added

{-| Extract from a block the UTXO changes at the given address. Returns the
'UtxoChange' itself and a set of all transactions that affected the change.
-}
extract :: (C.TxIn -> C.InAnyCardanoEra (C.TxOut C.CtxTx) -> Maybe a) -> Maybe AddressCredential -> UtxoSet C.CtxTx a -> BlockInMode -> [UtxoChangeEvent a]
extract ex cred state = DList.toList . \case
  -- FIXME (koslambrou) why is this only extracting from babbage?
  BlockInMode C.BabbageEra block -> extractBabbage ex state cred block
  _                              -> mempty

{-| Extract from a block the UTXO changes at the given address
-}
extract_ :: AddressCredential -> UtxoSet C.CtxTx () -> BlockInMode -> UtxoChange C.CtxTx ()
extract_ a b = foldMap fromEvent . extract (\_ -> const $ Just ()) (Just a) b

{-| Extract from a transaction the UTXO changes at the given address}
 -}
extractBabbageTxn
  :: (C.TxIn -> C.InAnyCardanoEra (C.TxOut C.CtxTx) -> Maybe a)
  -> Maybe AddressCredential
  -> UtxoSet C.CtxTx a
  -> C.Tx BabbageEra
  -> [UtxoChangeEvent a]
extractBabbageTxn ex cred state = DList.toList . extractBabbageTxn' ex state cred

extractBabbage
  :: (C.TxIn -> C.InAnyCardanoEra (C.TxOut C.CtxTx) -> Maybe a)
  -> UtxoSet C.CtxTx a
  -> Maybe AddressCredential
  -> Block BabbageEra
  -> DList (UtxoChangeEvent a)
extractBabbage ex state cred (CS.Block _blockHeader txns) = foldMap (extractBabbageTxn' ex state cred) txns

extractBabbageTxn'
  :: forall a. (C.TxIn -> C.InAnyCardanoEra (C.TxOut C.CtxTx) -> Maybe a)
  -> UtxoSet C.CtxTx a
  -> Maybe AddressCredential
  -> C.Tx BabbageEra
  -> DList (UtxoChangeEvent a)
extractBabbageTxn' ex UtxoSet{_utxos} cred theTx@(Tx txBody _) =
  let ShelleyTxBody _ txBody' _scripts scriptData _auxiliaryData _ = txBody
      Babbage.TxBody.BabbageTxBody{Babbage.TxBody.btbInputs} = txBody'
      txid = C.getTxId txBody

      allOuts = fmap (C.InAnyCardanoEra C.BabbageEra) $ C.fromLedgerTxOuts C.ShelleyBasedEraBabbage txBody' scriptData

      txReds = case scriptData of
              C.TxBodyScriptData _ _ r -> unRedeemers r
              _                        -> mempty

      checkInput :: (Word32, TxIn) -> Maybe (TxIn, ((C.InAnyCardanoEra (C.TxOut C.CtxTx), a), Maybe (HashableScriptData, ExecutionUnits)))
      checkInput (idx, txIn) = fmap (txIn,) $ do
        o <- Map.lookup txIn _utxos
        let redeemer = fmap (bimap CS.fromAlonzoData CS.fromAlonzoExUnits) (Map.lookup (Scripts.AlonzoSpending $ Scripts.AsIx idx) txReds)
        pure (o, redeemer)

      checkOutput :: TxIx -> C.InAnyCardanoEra (C.TxOut C.CtxTx) -> Maybe (TxIn, (C.InAnyCardanoEra (C.TxOut C.CtxTx), a))
      checkOutput _ (C.InAnyCardanoEra _era (C.TxOut (C.AddressInEra C.ByronAddressInAnyEra _) _ _ _)) = Nothing
      checkOutput txIx_ (C.InAnyCardanoEra era txOut@(C.TxOut (C.AddressInEra (C.ShelleyAddressInEra _era) (CS.ShelleyAddress _ outputCred _)) _ _ _))
        | isNothing cred || Just outputCred == cred =
            let txi = TxIn txid txIx_
            in fmap (\a -> (txi, (C.InAnyCardanoEra era txOut, a))) (ex txi $ C.InAnyCardanoEra era txOut)
        | otherwise = Nothing

      mkI (aueTxIn, (aueTxOut, aueEvent)) = AddUtxoEvent{aueEvent, aueTxOut, aueTxIn, aueTxId = txid, aueTx = theTx}

      mkO (rueTxIn, ((rueTxOut, rueEvent), rueRedeemer)) = RemoveUtxoEvent{rueEvent, rueTxOut, rueTxIn, rueTxId = txid, rueTx = theTx, rueRedeemer}

      _outputsAdded =
        DList.fromList
        $ fmap (Left . mkI)
        $ mapMaybe (uncurry checkOutput)
        $ (zip (TxIx <$> [0..]) allOuts)

      _outputsRemoved =
        DList.fromList
        $ fmap (Right . mkO)
        $ mapMaybe checkInput
        $ zip [0..] -- for redeemer pointers
        $ fmap (uncurry TxIn . bimap CS.fromShelleyTxId txIx . (\(CT.TxIn i n) -> (i, n)))
        $ Set.toList btbInputs

  in _outputsAdded <> _outputsRemoved

txIx :: CT.TxIx -> TxIx
txIx (CT.TxIx i) = TxIx (fromIntegral i)
