{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
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
  extract_,
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
                                                TxIn (..), TxIx (..), UTxO (..),
                                                Value)
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
import           Control.Lens                  (_1, _2, _3, makeLenses,
                                                makePrisms, over, preview, view)
import qualified Convex.Lenses                 as L
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
newtype UtxoSet ctx a = UtxoSet{ _utxos :: Map C.TxIn (C.TxOut ctx C.BabbageEra, a) }
  deriving stock (Eq, Show, Functor)
  deriving newtype (Semigroup, Monoid)

deriving instance (FromJSON a, FromJSON (C.TxOut ctx C.BabbageEra)) => FromJSON (UtxoSet ctx a)
deriving instance (ToJSON a, ToJSON (C.TxOut ctx C.BabbageEra)) => ToJSON (UtxoSet ctx a)

{-| A utxo set with one element
-}
singleton :: TxIn -> (C.TxOut ctx C.BabbageEra, a) -> UtxoSet ctx a
singleton txi = UtxoSet . Map.singleton txi

{-| Change the context of the outputs in this utxo set to 'CtxUTxO'
-}
fromUtxoTx :: UtxoSet C.CtxTx a -> UtxoSet C.CtxUTxO a
fromUtxoTx = UtxoSet . fmap (first C.toCtxUTxOTxOut) . _utxos

makePrisms ''UtxoSet

{-| Convert a @cardano-api@ 'UTxO BabbageEra' to a utxo set
-}
fromApiUtxo :: UTxO BabbageEra -> UtxoSet C.CtxUTxO ()
fromApiUtxo (UTxO x) = UtxoSet (fmap (,()) x)

{-| Convert a utxo set to a @cardano-api@ 'UTxO BabbageEra'
-}
toApiUtxo :: UtxoSet C.CtxUTxO () -> UTxO BabbageEra
toApiUtxo (UtxoSet s) = UTxO (fmap fst s)

{-| Pick an unspent output from the 'UtxoSet', if there is one.
-}
selectUtxo :: UtxoSet ctx a -> Maybe (C.TxIn, (C.TxOut ctx C.BabbageEra, a))
selectUtxo =
  -- sorting by key is pretty much a random order
  listToMaybe . Map.toAscList . _utxos

{-| Restrict the 'UtxoSet' to outputs that only have Ada values (no native assets)
-}
onlyAda :: UtxoSet ctx a -> UtxoSet ctx a
onlyAda =
  let flt = isJust . C.valueToLovelace . view (_1 . L._TxOut . _2 . L._TxOutValue)
  in fst . partition flt

{-| Partition the UtxoSet according to a predicate. The first UtxoSet contains all
utxos that satisfy the predicate, the second all utxos that fail the predicate.
-}
partition :: ((C.TxOut ctx C.BabbageEra, a) -> Bool) -> UtxoSet ctx a -> (UtxoSet ctx a, UtxoSet ctx a)
partition p (UtxoSet s) =
  bimap UtxoSet UtxoSet (Map.partition p s)

{-| Restrict the 'UtxoSet' to outputs at the address
-}
onlyAddress :: AddressInEra BabbageEra -> UtxoSet ctx a -> UtxoSet ctx a
onlyAddress addr =
  let flt = (==) addr . view (_1 . L._TxOut . _1)
  in fst . partition flt

{-| Restrict the utxo set to outputs with the given payment credential
-}
onlyCredential :: PaymentCredential -> UtxoSet ctx a -> UtxoSet ctx a
onlyCredential c = onlyCredentials (Set.singleton c)

{-| Restrict the utxo set to outputs locked by one of the given payment credentials
-}
onlyCredentials :: Set (PaymentCredential) -> UtxoSet ctx a -> UtxoSet ctx a
onlyCredentials cs =
  let flt (fmap CS.fromShelleyPaymentCredential . preview (_1 . L._TxOut . _1 . L._ShelleyAddressInBabbageEra . _2) -> k) = case k of
        Just c -> c `Set.member` cs
        _      -> False
  in fst . partition flt

{-| Restrict the utxo set to outputs with the given stake credential
-}
onlyStakeCredential :: StakeCredential -> UtxoSet ctx a -> UtxoSet ctx a
onlyStakeCredential (C.StakeAddressByValue -> c) =
  let flt (fmap CS.fromShelleyStakeReference . preview (_1 . L._TxOut . _1 . L._ShelleyAddressInBabbageEra . _3) -> k) = k == Just c
  in fst . partition flt

{-| Restrict the 'UtxoSet' to public key outputs
-}
onlyPubKey :: UtxoSet ctx a -> UtxoSet ctx a
onlyPubKey =
  let flt = isJust . preview (_1 . L._TxOut . _1 . L._ShelleyAddressInBabbageEra . _2 . L._ShelleyPaymentCredentialByKey)
  in fst . partition flt

{-| The combined 'Value' of all outputs in the set
-}
totalBalance :: UtxoSet ctx a -> Value
totalBalance = foldMap (view (_1 . L._TxOut . _2 . L._TxOutValue)) . _utxos

{-| Delete some outputs from the 'UtxoSet'
-}
removeUtxos :: Set.Set C.TxIn -> UtxoSet ctx a -> UtxoSet ctx a
removeUtxos ins = over _UtxoSet (flip Map.withoutKeys ins)

{-| A change to the UTxO set, adding and/or removing UTxOs
-}
data UtxoChange ctx a =
  UtxoChange
    { _outputsAdded   :: !(Map C.TxIn (C.TxOut ctx C.BabbageEra, a))
    , _outputsRemoved :: !(Map C.TxIn (C.TxOut ctx C.BabbageEra, a))
    }

{-| Change the context of the outputs in this utxo change
-}
toUtxoChangeTx :: UtxoChange C.CtxTx a -> UtxoChange C.CtxUTxO a
toUtxoChangeTx (UtxoChange added removed) =
  UtxoChange (fmap (first C.toCtxUTxOTxOut) added) (fmap (first C.toCtxUTxOTxOut) removed)

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
    , aueTxOut :: !(C.TxOut C.CtxTx C.BabbageEra)
    , aueTxIn  :: !TxIn
    , aueTxId  :: !TxId
    , aueTx    :: C.Tx BabbageEra
    }

{-| A tx output was spent
-}
data RemoveUtxoEvent a =
  RemoveUtxoEvent
    { rueEvent    :: !a
    , rueTxOut    :: !(C.TxOut C.CtxTx C.BabbageEra)
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
newtype BalanceChanges = BalanceChanges{tbBalances :: Map PaymentCredential Value }
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
  let k (view (_1 . L._TxOut) -> (fmap CS.fromShelleyPaymentCredential . preview (L._AddressInEra . L._Address . _2) -> Just addr, view L._TxOutValue -> vl, _, _)) = Just (addr, vl)
      k _ = Nothing
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
    let b = foldMap (view (_1 . L._TxOut . _2 . L._TxOutValue))
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
extract :: (C.TxIn -> C.TxOut C.CtxTx C.BabbageEra -> Maybe a) -> Maybe AddressCredential -> UtxoSet C.CtxTx a -> BlockInMode -> [UtxoChangeEvent a]
extract ex cred state = DList.toList . \case
  BlockInMode C.BabbageEra block -> extractBabbage ex state cred block
  _                              -> mempty

{-| Extract from a block the UTXO changes at the given address
-}
extract_ :: AddressCredential -> UtxoSet C.CtxTx () -> BlockInMode -> UtxoChange C.CtxTx ()
extract_ a b = foldMap fromEvent . extract (\_ -> const $ Just ()) (Just a) b

{-| Extract from a transaction the UTXO changes at the given address}
 -}
extractBabbageTxn :: (C.TxIn -> C.TxOut C.CtxTx C.BabbageEra -> Maybe a) -> Maybe AddressCredential -> UtxoSet C.CtxTx a -> C.Tx BabbageEra -> [UtxoChangeEvent a]
extractBabbageTxn ex cred state = DList.toList . extractBabbageTxn' ex state cred

extractBabbage :: (C.TxIn -> C.TxOut C.CtxTx C.BabbageEra -> Maybe a) -> UtxoSet C.CtxTx a -> Maybe AddressCredential -> Block BabbageEra -> DList (UtxoChangeEvent a)
extractBabbage ex state cred (CS.Block _blockHeader txns) = foldMap (extractBabbageTxn' ex state cred) txns

extractBabbageTxn' :: forall a. (C.TxIn -> C.TxOut C.CtxTx C.BabbageEra -> Maybe a) -> UtxoSet C.CtxTx a -> Maybe AddressCredential -> C.Tx BabbageEra -> DList (UtxoChangeEvent a)
extractBabbageTxn' ex UtxoSet{_utxos} cred theTx@(Tx txBody _) =
  let ShelleyTxBody _ txBody' _scripts scriptData _auxiliaryData _ = txBody
      Babbage.TxBody.BabbageTxBody{Babbage.TxBody.btbInputs} = txBody'
      txid = C.getTxId txBody

      allOuts = C.fromLedgerTxOuts C.ShelleyBasedEraBabbage txBody' scriptData

      txReds = case scriptData of
              C.TxBodyScriptData _ _ r -> unRedeemers r
              _                        -> mempty

      checkInput :: (Word32, TxIn) -> Maybe (TxIn, ((C.TxOut C.CtxTx C.BabbageEra, a), Maybe (HashableScriptData, ExecutionUnits)))
      checkInput (idx, txIn) = fmap (txIn,) $ do
        o <- Map.lookup txIn _utxos
        let redeemer = fmap (bimap CS.fromAlonzoData CS.fromAlonzoExUnits) (Map.lookup (Scripts.AlonzoSpending $ Scripts.AsIx idx) txReds)
        pure (o, redeemer)

      checkOutput :: TxIx -> C.TxOut C.CtxTx C.BabbageEra -> Maybe (TxIn, (C.TxOut C.CtxTx C.BabbageEra, a))
      checkOutput txIx_ txOut
        | isNothing cred || preview (L._TxOut . _1 . L._AddressInEra . L._Address . _2) txOut == cred =
            let txi = TxIn txid txIx_
            in fmap  (\a -> (txi, (txOut, a))) (ex txi txOut)
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
