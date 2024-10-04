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
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE TypeApplications     #-}

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
  selectUtxo,

  -- * Events based on transactions
  UtxoChangeEvent,
  AddUtxoEvent(..),
  RemoveUtxoEvent(..),
  extract,
  extract_,
  extractBabbageTxn,
  extractConwayTxn,
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
                                                Block (..), ConwayEra,
                                                HashableScriptData,
                                                PaymentCredential,
                                                StakeCredential, Tx (..), TxId,
                                                TxIn (..), TxIx (..), UTxO (..))
import qualified Cardano.Api                   as C
import           Cardano.Api.Shelley           (ExecutionUnits, TxBody (..))
import qualified Cardano.Api.Shelley           as CS
import qualified Cardano.Ledger.Alonzo.Scripts as Scripts
import           Cardano.Ledger.Alonzo.TxWits  (unRedeemers)
import qualified Cardano.Ledger.Alonzo.TxWits  as Alonzo.TxWits
import qualified Cardano.Ledger.Babbage.TxBody as Babbage.TxBody
import qualified Cardano.Ledger.BaseTypes      as CT
import qualified Cardano.Ledger.Conway.Scripts as Scripts.Conway
import qualified Cardano.Ledger.Conway.TxBody  as Conway.TxBody
import qualified Cardano.Ledger.Credential     as Shelley
import           Cardano.Ledger.Crypto         (StandardCrypto)
import qualified Cardano.Ledger.TxIn           as CT
import           Control.Lens                  (_2, makeLenses, makePrisms,
                                                over, preview, view)
import qualified Convex.CardanoApi.Lenses      as L
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Bifunctor                (Bifunctor (..))
import           Data.DList                    (DList)
import qualified Data.DList                    as DList
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
newtype UtxoSet ctx era a = UtxoSet{ _utxos :: Map C.TxIn (C.TxOut ctx era, a) }
  deriving stock (Functor)
  deriving newtype (Semigroup, Monoid, Show, Eq)

deriving instance (CS.IsShelleyBasedEra era, FromJSON a) => FromJSON (UtxoSet C.CtxTx era a)
deriving instance (CS.IsShelleyBasedEra era, FromJSON a) => FromJSON (UtxoSet C.CtxUTxO era a)
deriving instance (CS.IsCardanoEra era, ToJSON a) => ToJSON (UtxoSet C.CtxTx era a)
deriving instance (CS.IsCardanoEra era, ToJSON a) => ToJSON (UtxoSet C.CtxUTxO era a)

{-| A utxo set with one element
-}
singleton :: TxIn -> (C.TxOut ctx era, a) -> UtxoSet ctx era a
singleton txi = UtxoSet . Map.singleton txi

{-| Change the context of the outputs in this utxo set to 'CtxUTxO'
-}
fromUtxoTx :: UtxoSet C.CtxTx era a -> UtxoSet C.CtxUTxO era a
fromUtxoTx = UtxoSet . fmap (first C.toCtxUTxOTxOut) . _utxos

makePrisms ''UtxoSet

{-| Convert a @cardano-api@ 'UTxO BabbageEra' to a utxo set
-}
fromApiUtxo :: a -> UTxO era -> UtxoSet C.CtxUTxO era a
fromApiUtxo v (UTxO utxoSet) = UtxoSet (fmap (, v) utxoSet)

{-| Convert a utxo set to a @cardano-api@ 'UTxO BabbageEra'
-}
toApiUtxo :: UtxoSet C.CtxUTxO era a -> UTxO era
toApiUtxo (UtxoSet utxos) =
  UTxO (fmap fst utxos)

{-| Pick an unspent output from the 'UtxoSet', if there is one.
-}
selectUtxo :: UtxoSet ctx era a -> Maybe (C.TxIn, (C.TxOut ctx era, a))
selectUtxo =
  -- sorting by key is pretty much a random order
  listToMaybe . Map.toAscList . _utxos

{-| Restrict the 'UtxoSet' to outputs that only have Ada values (no native assets)
-}
onlyAda :: UtxoSet ctx era a -> UtxoSet ctx era a
onlyAda =
  let txOutHasOnlyAda :: (C.TxOut ctx era, a) -> Bool
      txOutHasOnlyAda (txOut, _) =
        isJust $ C.valueToLovelace $ C.txOutValueToValue $ view (L._TxOut . _2) txOut
  in fst . partition txOutHasOnlyAda

{-| Partition the UtxoSet according to a predicate. The first UtxoSet contains all
utxos that satisfy the predicate, the second all utxos that fail the predicate.
-}
partition :: ((C.TxOut ctx era, a) -> Bool) -> UtxoSet ctx era a -> (UtxoSet ctx era a, UtxoSet ctx era a)
partition p (UtxoSet s) =
  bimap UtxoSet UtxoSet (Map.partition p s)

{-| Restrict the 'UtxoSet' to outputs at the address
-}
onlyAddress :: C.AddressAny -> UtxoSet ctx era a -> UtxoSet ctx era a
onlyAddress expectedAddr =
  let txOutHasAddr :: (C.TxOut ctx era, a) -> Bool
      txOutHasAddr (C.TxOut addrInEra _ _ _, _) =
        case addrInEra of
          C.AddressInEra C.ByronAddressInAnyEra addr -> expectedAddr == C.toAddressAny addr
          C.AddressInEra (C.ShelleyAddressInEra _) addr -> expectedAddr == C.toAddressAny addr
  in fst . partition txOutHasAddr

{-| Restrict the utxo set to outputs with the given payment credential
-}
onlyCredential :: C.PaymentCredential -> UtxoSet ctx era a -> UtxoSet ctx era a
onlyCredential c = onlyCredentials (Set.singleton c)

{-| Restrict the utxo set to outputs locked by one of the given payment credentials
-}
onlyCredentials :: Set (C.PaymentCredential) -> UtxoSet ctx era a -> UtxoSet ctx era a
onlyCredentials cs =
  let txOutHasOneOfCreds :: (C.TxOut ctx era, a) -> Bool
      txOutHasOneOfCreds (C.TxOut (C.AddressInEra C.ByronAddressInAnyEra _) _ _ _, _) =
        False
      txOutHasOneOfCreds (C.TxOut (C.AddressInEra (C.ShelleyAddressInEra _era) addr) _ _ _, _) =
        let (CS.ShelleyAddress _ (CS.fromShelleyPaymentCredential -> c) _) = addr
         in c `Set.member` cs
  in fst . partition txOutHasOneOfCreds

{-| Restrict the utxo set to outputs with the given stake credential
-}
onlyStakeCredential :: StakeCredential -> UtxoSet ctx era a -> UtxoSet ctx era a
onlyStakeCredential expectedStakeCredential =
  let txOutHasStakeCred :: (C.TxOut ctx era, a) -> Bool
      txOutHasStakeCred (C.TxOut (C.AddressInEra C.ByronAddressInAnyEra _) _ _ _, _) =
        False
      txOutHasStakeCred (C.TxOut (C.AddressInEra (C.ShelleyAddressInEra _era) addr) _ _ _, _) =
        let (CS.ShelleyAddress _ _ (CS.fromShelleyStakeReference -> stakeRef)) = addr
         in case stakeRef of
              C.StakeAddressByValue stakeCred -> expectedStakeCredential == stakeCred
              C.StakeAddressByPointer _ -> False
              C.NoStakeAddress -> False
  in fst . partition txOutHasStakeCred

{-| Restrict the 'UtxoSet' to public key outputs
-}
onlyPubKey :: UtxoSet ctx era a -> UtxoSet ctx era a
onlyPubKey =
  let txOutHasPubKeyAddr :: (C.TxOut ctx era, a) -> Bool
      txOutHasPubKeyAddr (C.TxOut (C.AddressInEra C.ByronAddressInAnyEra _) _ _ _, _) =
        False
      txOutHasPubKeyAddr (C.TxOut (C.AddressInEra (C.ShelleyAddressInEra _era) addr) _ _ _, _) =
        let (CS.ShelleyAddress _ (CS.fromShelleyPaymentCredential -> cred) _) = addr
         in case cred of
              C.PaymentCredentialByKey _    -> True
              C.PaymentCredentialByScript _ -> False
  in fst . partition txOutHasPubKeyAddr

{-| The combined 'Value' of all outputs in the set
-}
totalBalance :: UtxoSet ctx era a -> C.Value
totalBalance =
  foldMap (\(C.TxOut _ txOutValue _ _, _) -> C.txOutValueToValue txOutValue) . _utxos

{-| Delete some outputs from the 'UtxoSet'
-}
removeUtxos :: Set.Set C.TxIn -> UtxoSet ctx era a -> UtxoSet ctx era a
removeUtxos ins = over _UtxoSet (flip Map.withoutKeys ins)

{-| A change to the UTxO set, adding and/or removing UTxOs
-}
data UtxoChange ctx era a =
  UtxoChange
    { _outputsAdded   :: !(Map C.TxIn (C.TxOut ctx era, a))
    , _outputsRemoved :: !(Map C.TxIn (C.TxOut ctx era, a))
    }

{-| Change the context of the outputs in this utxo change
-}
toUtxoChangeTx :: UtxoChange C.CtxTx era a -> UtxoChange C.CtxUTxO era a
toUtxoChangeTx (UtxoChange added removed) =
  UtxoChange (fmap (first C.toCtxUTxOTxOut) added) (fmap (first C.toCtxUTxOTxOut) removed)

makeLenses ''UtxoChange
-- TODO: change '<>' so that @x <> invert x == mempty@ and @invert x <> x == mempty@

instance Semigroup (UtxoChange ctx era a) where
  l <> r =
    UtxoChange
      { _outputsAdded   = _outputsAdded l <> _outputsAdded r
      , _outputsRemoved = _outputsRemoved l <> _outputsRemoved r
      }

instance Monoid (UtxoChange ctx era a) where
  mempty = UtxoChange mempty mempty

{-| Is this the empty 'UtxoChange'?
-}
null :: UtxoChange ctx era a -> Bool
null UtxoChange{_outputsAdded, _outputsRemoved} = Map.null _outputsAdded && Map.null _outputsRemoved

{-| An event that caused the utxo set to change
-}
type UtxoChangeEvent era a = Either (AddUtxoEvent era a) (RemoveUtxoEvent era a)

{-| A new tx out was added
-}
data AddUtxoEvent era a =
  AddUtxoEvent
    { aueEvent :: !a
    , aueTxOut :: !(C.TxOut C.CtxTx era)
    , aueTxIn  :: !TxIn
    , aueTxId  :: !TxId
    , aueTx    :: !(C.Tx era)
    }

{-| A tx output was spent
-}
data RemoveUtxoEvent era a =
  RemoveUtxoEvent
    { rueEvent    :: !a
    , rueTxOut    :: !(C.TxOut C.CtxTx era)
    , rueTxIn     :: !TxIn
    , rueTxId     :: !TxId
    -- ^ Id of the transaction that spent the output
    , rueTx       :: !(C.Tx era)
    -- ^ The transaction that spent the output
    , rueRedeemer :: Maybe (HashableScriptData, ExecutionUnits) -- fromAlonzoData
    }

{-| The 'UtxoChange' represented by the event.
-}
fromEvent :: UtxoChangeEvent era a -> UtxoChange C.CtxTx era a
fromEvent = \case
  Left AddUtxoEvent{aueEvent, aueTxOut, aueTxIn} ->
    let ch = Map.singleton aueTxIn (aueTxOut, aueEvent)
    in UtxoChange ch mempty
  Right RemoveUtxoEvent{rueEvent, rueTxOut, rueTxIn} ->
    let ch = Map.singleton rueTxIn (rueTxOut, rueEvent)
    in UtxoChange mempty ch

{-| ID of the transaction that caused the event
-}
txId :: UtxoChangeEvent era a -> TxId
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
balanceChange :: UtxoChange ctx era a -> BalanceChanges
balanceChange UtxoChange{_outputsAdded, _outputsRemoved} =
  let k :: (C.TxOut ctx era, a) -> Maybe (C.PaymentCredential, C.Value)
      k (C.TxOut (C.AddressInEra C.ByronAddressInAnyEra _) _ _ _, _) = Nothing
      k (C.TxOut (C.AddressInEra (C.ShelleyAddressInEra _era) addr) txOutValue _ _, _) =
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
describeChange :: UtxoChange ctx era a -> Text
describeChange UtxoChange{_outputsAdded, _outputsRemoved} =
  let tshow = Text.pack . show
  in tshow (Map.size _outputsAdded) <> " outputs added, " <> tshow (Map.size _outputsRemoved) <> " outputs removed"

newtype PrettyUtxoChange ctx era a = PrettyUtxoChange (UtxoChange ctx era a)

instance Pretty a => Pretty (PrettyUtxoChange ctx era a) where
  pretty (PrettyUtxoChange UtxoChange{_outputsAdded, _outputsRemoved}) =
    let b = foldMap (\((C.TxOut _ txOutValue _ _), _) -> C.txOutValueToValue txOutValue)
        bPlus = b _outputsAdded
        bMinus = C.negateValue (b _outputsRemoved)
    in Prettyprinter.hsep $
        [ pretty (Map.size _outputsAdded)
        , "outputs added"
        , pretty (Map.size _outputsRemoved), "outputs removed."]
        ++ prettyValue (bPlus <> bMinus)

newtype PrettyBalance ctx era a = PrettyBalance (UtxoSet ctx era a)

instance Pretty a => Pretty (PrettyBalance ctx era a) where
  pretty (PrettyBalance bal) =
    let nOutputs = Map.size (_utxos bal)
    in hang 4 $ vsep
        $ ("Balance" <+> parens (pretty nOutputs <+> "outputs") <> ":")
        : prettyValue (totalBalance bal)

{-| Change the 'UtxoSet'
-}
apply :: UtxoSet ctx era a -> UtxoChange ctx era a -> UtxoSet ctx era a
apply UtxoSet{_utxos} UtxoChange{_outputsAdded, _outputsRemoved} =
  UtxoSet $ (_utxos `Map.union` _outputsAdded) `Map.difference` _outputsRemoved

{-| Invert a 'UtxoChange' value
-}
inv :: UtxoChange ctx era a -> UtxoChange ctx era a
inv (UtxoChange added removed) = UtxoChange removed added

{-| Extract from a block the UTXO changes at the given address. Returns the
'UtxoChange' itself and a set of all transactions that affected the change.
-}
-- TODO make better polymorphic
extract :: forall era a. C.IsCardanoEra era => (C.TxIn -> C.TxOut C.CtxTx era -> Maybe a) -> Maybe AddressCredential -> UtxoSet C.CtxTx era a -> Block era -> [UtxoChangeEvent era a]
extract ex cred state block@(CS.Block _blockHeader txns) = DList.toList $ case C.cardanoEra @era of
  C.BabbageEra -> extractBabbage ex state cred block
  C.ConwayEra  -> foldMap (extractConwayTxn' ex state cred) txns
  _            -> mempty

{-| Extract from a block the UTXO changes at the given address
-}
extract_ :: C.IsCardanoEra era => AddressCredential -> UtxoSet C.CtxTx era () -> Block era -> UtxoChange C.CtxTx era ()
extract_ a b = foldMap fromEvent . extract (\_ -> const $ Just ()) (Just a) b


checkOutput :: (C.TxIn -> C.TxOut C.CtxTx era -> Maybe a) -> TxId -> Maybe AddressCredential -> TxIx -> C.TxOut C.CtxTx era -> Maybe (TxIn, (C.TxOut C.CtxTx era, a))
checkOutput _ex _txid _cred _ (C.TxOut (C.AddressInEra C.ByronAddressInAnyEra _) _ _ _) = Nothing
checkOutput ex txid cred txIx_ txOut@(C.TxOut (C.AddressInEra (C.ShelleyAddressInEra _era) (CS.ShelleyAddress _ outputCred _)) _ _ _)
  | isNothing cred || Just outputCred == cred =
      let txi = TxIn txid txIx_
      in fmap (\a -> (txi, (txOut, a))) (ex txi txOut)
  | otherwise = Nothing

mkI :: TxId -> Tx era -> (TxIn, (CS.TxOut CS.CtxTx era, a)) -> AddUtxoEvent era a
mkI txid tx (aueTxIn, (aueTxOut, aueEvent)) = AddUtxoEvent{aueEvent, aueTxOut, aueTxIn, aueTxId = txid, aueTx = tx}

mkO :: TxId -> Tx era -> (TxIn, ((CS.TxOut CS.CtxTx era, a), Maybe (HashableScriptData, ExecutionUnits))) -> RemoveUtxoEvent era a
mkO txid tx (rueTxIn, ((rueTxOut, rueEvent), rueRedeemer)) = RemoveUtxoEvent{rueEvent, rueTxOut, rueTxIn, rueTxId = txid, rueTx = tx, rueRedeemer}

{-| Extract from a conway-era transaction the UTXO changes at the given address
 -}
extractConwayTxn
  :: (C.TxIn -> C.TxOut C.CtxTx ConwayEra -> Maybe a)
  -> Maybe AddressCredential
  -> UtxoSet C.CtxTx ConwayEra a
  -> C.Tx ConwayEra
  -> [UtxoChangeEvent ConwayEra a]
extractConwayTxn ex cred state = DList.toList . extractConwayTxn' ex state cred

extractConwayTxn'
  :: forall a. (C.TxIn -> C.TxOut C.CtxTx ConwayEra -> Maybe a)
  -> UtxoSet C.CtxTx ConwayEra a
  -> Maybe AddressCredential
  -> C.Tx ConwayEra
  -> DList (UtxoChangeEvent ConwayEra a)
extractConwayTxn' ex UtxoSet{_utxos} cred theTx@(Tx txBody _) =
  let ShelleyTxBody _ txBody' _scripts scriptData _auxiliaryData _ = txBody
      Conway.TxBody.ConwayTxBody{Conway.TxBody.ctbSpendInputs} = txBody'
      txid = C.getTxId txBody

      allOuts = C.fromLedgerTxOuts C.shelleyBasedEra txBody' scriptData

      txReds = case scriptData of
              C.TxBodyScriptData _ _ r -> r
              _                        -> mempty

      checkInput :: (Word32, TxIn) -> Maybe (TxIn, ((C.TxOut C.CtxTx ConwayEra, a), Maybe (HashableScriptData, ExecutionUnits)))
      checkInput (idx, txIn) = fmap (txIn,) $ do
        o <- Map.lookup txIn _utxos
        let redeemer = fmap (bimap CS.fromAlonzoData CS.fromAlonzoExUnits) (Alonzo.TxWits.lookupRedeemer (Scripts.Conway.ConwaySpending $ Scripts.AsIx idx) txReds)
        pure (o, redeemer)

      _outputsAdded =
        DList.fromList
        $ fmap (Left . mkI txid theTx)
        $ mapMaybe (uncurry (checkOutput ex txid cred))
        $ zip (TxIx <$> [0..]) allOuts

      _outputsRemoved =
        DList.fromList
        $ fmap (Right . mkO txid theTx)
        $ mapMaybe checkInput
        $ zip [0..] -- for redeemer pointers
        $ fmap (uncurry TxIn . bimap CS.fromShelleyTxId txIx . (\(CT.TxIn i n) -> (i, n)))
        $ Set.toList ctbSpendInputs

  in _outputsAdded <> _outputsRemoved

extractBabbage
  :: (C.TxIn -> C.TxOut C.CtxTx BabbageEra -> Maybe a)
  -> UtxoSet C.CtxTx BabbageEra a
  -> Maybe AddressCredential
  -> Block BabbageEra
  -> DList (UtxoChangeEvent BabbageEra a)
extractBabbage ex state cred (CS.Block _blockHeader txns) = foldMap (extractBabbageTxn' ex state cred) txns

{-| Extract from a transaction the UTXO changes at the given address
 -}
extractBabbageTxn
  :: (C.TxIn -> C.TxOut C.CtxTx BabbageEra -> Maybe a)
  -> Maybe AddressCredential
  -> UtxoSet C.CtxTx BabbageEra a
  -> C.Tx BabbageEra
  -> [UtxoChangeEvent BabbageEra a]
extractBabbageTxn ex cred state = DList.toList . extractBabbageTxn' ex state cred

extractBabbageTxn'
  :: forall a. (C.TxIn -> C.TxOut C.CtxTx BabbageEra -> Maybe a)
  -> UtxoSet C.CtxTx BabbageEra a
  -> Maybe AddressCredential
  -> C.Tx BabbageEra
  -> DList (UtxoChangeEvent BabbageEra a)
extractBabbageTxn' ex UtxoSet{_utxos} cred theTx@(Tx txBody _) =
  let ShelleyTxBody _ txBody' _scripts scriptData _auxiliaryData _ = txBody
      Babbage.TxBody.BabbageTxBody{Babbage.TxBody.btbInputs} = txBody'
      txid = C.getTxId txBody

      allOuts = C.fromLedgerTxOuts C.shelleyBasedEra txBody' scriptData

      txReds = case scriptData of
              C.TxBodyScriptData _ _ r -> unRedeemers r
              _                        -> mempty

      checkInput :: (Word32, TxIn) -> Maybe (TxIn, ((C.TxOut C.CtxTx BabbageEra, a), Maybe (HashableScriptData, ExecutionUnits)))
      checkInput (idx, txIn) = fmap (txIn,) $ do
        o <- Map.lookup txIn _utxos
        let redeemer = fmap (bimap CS.fromAlonzoData CS.fromAlonzoExUnits) (Map.lookup (Scripts.AlonzoSpending $ Scripts.AsIx idx) txReds)
        pure (o, redeemer)

      _outputsAdded =
        DList.fromList
        $ fmap (Left . mkI txid theTx)
        $ mapMaybe (uncurry (checkOutput ex txid cred))
        $ zip (TxIx <$> [0..]) allOuts

      _outputsRemoved =
        DList.fromList
        $ fmap (Right . mkO txid theTx)
        $ mapMaybe checkInput
        $ zip [0..] -- for redeemer pointers
        $ fmap (uncurry TxIn . bimap CS.fromShelleyTxId txIx . (\(CT.TxIn i n) -> (i, n)))
        $ Set.toList btbInputs

  in _outputsAdded <> _outputsRemoved

txIx :: CT.TxIx -> TxIx
txIx (CT.TxIx i) = TxIx (fromIntegral i)
