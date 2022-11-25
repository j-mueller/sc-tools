{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}
module Convex.Utxos(
  -- * Utxo sets
  UtxoSet(..),
  PrettyBalance(..),
  _UtxoSet,
  totalBalance,
  partition,
  onlyAda,
  onlyPubKey,
  onlyAddress,
  onlyCredential,
  removeUtxos,
  fromApiUtxo,
  selectUtxo,

  -- * Changes to utxo sets
  UtxoChange(..),
  PrettyUtxoChange(..),
  outputsAdded,
  outputsRemoved,
  null,
  apply,
  inv,
  extract,
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
                                                CardanoMode, EraInMode (..),
                                                PaymentCredential, Tx (..),
                                                TxIn (..), TxIx (..), UTxO (..),
                                                Value)
import qualified Cardano.Api                   as C
import           Cardano.Api.Shelley           (TxBody (..))
import qualified Cardano.Api.Shelley           as CS
import qualified Cardano.Ledger.Babbage.TxBody as Babbage.TxBody
import qualified Cardano.Ledger.BaseTypes      as CT
import qualified Cardano.Ledger.Credential     as Shelley
import           Cardano.Ledger.Crypto         (StandardCrypto)
import qualified Cardano.Ledger.TxIn           as CT
import           Control.Lens                  (_1, _2, makeLenses, makePrisms,
                                                over, preview, view)
import qualified Convex.Lenses                 as L
import           Data.Bifunctor                (Bifunctor (..))
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (isJust, listToMaybe, mapMaybe)
import qualified Data.Set                      as Set
import           Data.String                   (fromString)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Prelude                       hiding (null)
import           Prettyprinter                 (Doc, Pretty (..), hang, viaShow,
                                                vsep, (<+>))
import qualified Prettyprinter

type AddressCredential = Shelley.PaymentCredential StandardCrypto

{-| A set of unspent transaction outputs
-}
newtype UtxoSet = UtxoSet{ _utxos :: Map C.TxIn (C.TxOut C.CtxUTxO C.BabbageEra) }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

makePrisms ''UtxoSet

{-| Convert a @cardano-api@ 'UTxO BabbageEra' to a utxo set
-}
fromApiUtxo :: UTxO BabbageEra -> UtxoSet
fromApiUtxo (UTxO x) = UtxoSet x

{-| Pick an unspent output from the 'UtxoSet', if there is one.
-}
selectUtxo :: UtxoSet -> Maybe (C.TxIn, C.TxOut C.CtxUTxO C.BabbageEra)
selectUtxo =
  -- sorting by key is pretty much a random order
  listToMaybe . Map.toAscList . _utxos

{-| Restrict the 'UtxoSet' to outputs that only have Ada values (no native assets)
-}
onlyAda :: UtxoSet -> UtxoSet
onlyAda =
  let flt = isJust . C.valueToLovelace . view (L._TxOut . _2 . L._TxOutValue)
  in fst . partition flt

{-| Partition the UtxoSet according to a predicate. The first UtxoSet contains all
utxos that satisfy the predicate, the second all utxos that fail the predicate.
-}
partition :: (C.TxOut C.CtxUTxO C.BabbageEra -> Bool) -> UtxoSet -> (UtxoSet, UtxoSet)
partition p (UtxoSet s) =
  bimap UtxoSet UtxoSet (Map.partition p s)

{-| Restrict the 'UtxoSet' to outputs at the address
-}
onlyAddress :: AddressInEra BabbageEra -> UtxoSet -> UtxoSet
onlyAddress addr =
  let flt = (==) addr . view (L._TxOut . _1)
  in fst . partition flt

{-| Restrict the utxo set to outputs with the given payment credential
-}
onlyCredential :: PaymentCredential -> UtxoSet -> UtxoSet
onlyCredential c =
  let flt (fmap CS.fromShelleyPaymentCredential . preview (L._TxOut . _1 . L._ShelleyAddressInBabbageEra . _2) -> k) = k == Just c
  in fst . partition flt

{-| Restrict the 'UtxoSet' to public key outputs
-}
onlyPubKey :: UtxoSet -> UtxoSet
onlyPubKey =
  let flt = isJust . preview (L._TxOut . _1 . L._ShelleyAddressInBabbageEra . _2 . L._ShelleyPaymentCredentialByKey)
  in fst . partition flt

{-| The combined 'Value' of all outputs in the set
-}
totalBalance :: UtxoSet -> Value
totalBalance = foldMap (view (L._TxOut . _2 . L._TxOutValue)) . _utxos

{-| Delete some outputs from the 'UtxoSet'
-}
removeUtxos :: Set.Set C.TxIn -> UtxoSet -> UtxoSet
removeUtxos ins = over _UtxoSet (flip Map.withoutKeys ins)

{-| A change to the UTxO set, adding and/or removing UTxOs
-}
data UtxoChange =
  UtxoChange
    { _outputsAdded   :: Map C.TxIn (C.TxOut C.CtxUTxO C.BabbageEra)
    , _outputsRemoved :: Map C.TxIn (C.TxOut C.CtxUTxO C.BabbageEra)
    }

makeLenses ''UtxoChange
-- TODO: change '<>' so that @x <> invert x == mempty@ and @invert x <> x == mempty@

instance Semigroup UtxoChange where
  l <> r =
    UtxoChange
      { _outputsAdded   = _outputsAdded l <> _outputsAdded r
      , _outputsRemoved = _outputsRemoved l <> _outputsRemoved r
      }

instance Monoid UtxoChange where
  mempty = UtxoChange mempty mempty

{-| Is this the empty 'UtxoChange'?
-}
null :: UtxoChange -> Bool
null UtxoChange{_outputsAdded, _outputsRemoved} = Map.null _outputsAdded && Map.null _outputsRemoved

{-| A type capturing the effect a 'UtxoChange' has on the total balance of each address that it touches
-}
newtype BalanceChanges = BalanceChanges{tbBalances :: Map PaymentCredential Value }
  deriving stock (Eq, Show)

prettyAda :: C.Lovelace -> Doc ann
prettyAda (C.Lovelace lvl) =
  let ada :: Double = fromIntegral lvl / 1_000_000
  in pretty ada

prettyPolicy :: C.PolicyId -> C.AssetName -> Doc ann
prettyPolicy p a =
  let ps = show p
      x = take 5 ps
      md = drop 53 ps
  in fromString x <> "..." <> fromString md <+> viaShow a

instance Pretty BalanceChanges where
  pretty (BalanceChanges mp) =
    let f (paymentCredential, vl) =
          hang 4 $ vsep $ viaShow paymentCredential : prettyValue vl
    in vsep (f <$> Map.toAscList mp)

prettyValue :: C.Value -> [Doc ann]
prettyValue vl =
  let k (C.AdaAssetId, C.Quantity l)  = "Ada" <+> prettyAda (C.Lovelace l)
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
balanceChange :: UtxoChange -> BalanceChanges
balanceChange UtxoChange{_outputsAdded, _outputsRemoved} =
  let k (view L._TxOut -> (fmap CS.fromShelleyPaymentCredential . preview (L._AddressInEra . L._Address . _2) -> Just addr, view L._TxOutValue -> vl, _, _)) = Just (addr, vl)
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
describeChange :: UtxoChange -> Text
describeChange UtxoChange{_outputsAdded, _outputsRemoved} =
  let tshow = Text.pack . show
  in tshow (Map.size _outputsAdded) <> " outputs added, " <> tshow (Map.size _outputsRemoved) <> " outputs removed"

newtype PrettyUtxoChange = PrettyUtxoChange UtxoChange

instance Pretty PrettyUtxoChange where
  pretty (PrettyUtxoChange UtxoChange{_outputsAdded, _outputsRemoved}) =
    let b = foldMap (view (L._TxOut . _2 . L._TxOutValue))
        bPlus = b _outputsAdded
        bMinus = C.negateValue (b _outputsRemoved)
    in Prettyprinter.hsep $
        [ pretty (Map.size _outputsAdded)
        , "outputs added"
        , pretty (Map.size _outputsRemoved), "outputs removed."]
        ++ (prettyValue (bPlus <> bMinus))

newtype PrettyBalance = PrettyBalance UtxoSet

instance Pretty PrettyBalance where
  pretty (PrettyBalance bal) =
    let nOutputs = Map.size (_utxos bal)
    in hang 4 $ vsep
        $ (pretty nOutputs <+> "outputs")
        : prettyValue (totalBalance bal)

{-| Change the 'UtxoSet'
-}
apply :: UtxoSet -> UtxoChange -> UtxoSet
apply UtxoSet{_utxos} UtxoChange{_outputsAdded, _outputsRemoved} =
  UtxoSet $ (_utxos `Map.union` _outputsAdded) `Map.difference` _outputsRemoved

{-| Invert a 'UtxoChange' value
-}
inv :: UtxoChange -> UtxoChange
inv (UtxoChange added removed) = UtxoChange removed added

{-| Extract from a block the UTXO changes at the given address
-}
extract :: AddressCredential -> UtxoSet -> BlockInMode CardanoMode -> UtxoChange
extract cred state = \case
  BlockInMode block BabbageEraInCardanoMode -> extractBabbage state cred block
  _                                         -> mempty

extractBabbage :: UtxoSet -> AddressCredential -> Block BabbageEra -> UtxoChange
extractBabbage state cred (Block _blockHeader txns) = foldMap (extractBabbageTxn state cred) txns

extractBabbageTxn :: UtxoSet -> AddressCredential -> C.Tx BabbageEra -> UtxoChange
extractBabbageTxn UtxoSet{_utxos} cred (Tx txBody _) =
  let ShelleyTxBody _ txBody' _scripts scriptData _auxiliaryData _ = txBody
      Babbage.TxBody.TxBody{Babbage.TxBody.inputs} = txBody'
      txId = C.getTxId txBody

      allOuts = C.fromLedgerTxOuts C.ShelleyBasedEraBabbage txBody' scriptData

      checkInput :: TxIn -> Maybe (TxIn, C.TxOut C.CtxUTxO C.BabbageEra)
      checkInput txIn = fmap (txIn,) $ Map.lookup txIn _utxos

      checkOutput :: TxIx -> C.TxOut C.CtxTx C.BabbageEra -> Maybe (TxIn, C.TxOut C.CtxUTxO C.BabbageEra)
      checkOutput txIx_ txOut
        | preview (L._TxOut . _1 . L._AddressInEra . L._Address . _2) txOut == Just cred = Just (TxIn txId txIx_, C.toCtxUTxOTxOut txOut)
        | otherwise = Nothing

      _outputsAdded =
        Map.fromList
        $ mapMaybe (uncurry checkOutput)
        $ (zip (TxIx <$> [0..]) allOuts)

      _outputsRemoved =
        Map.fromList
        $ mapMaybe checkInput
        $ fmap (uncurry TxIn . bimap CS.fromShelleyTxId txIx . (\(CT.TxIn i n) -> (i, n)))
        $ Set.toList inputs
  in UtxoChange{_outputsAdded, _outputsRemoved}

txIx :: CT.TxIx -> TxIx
txIx (CT.TxIx i) = TxIx (fromIntegral i)
