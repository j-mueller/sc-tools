{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Primitive wallet
-}
module Convex.Wallet(
  Wallet(..),
  paymentCredential,
  address,
  addressInEra,
  privateKey,
  generateWallet,
  parse,
  -- * UTxOs and coin selection
  WalletUtxo(..),
  fromUtxo,
  fromUtxos,
  selectAdaInputsCovering,
  selectAnyInputsCovering,
  selectMixedInputsCovering,
  removeTxIns
) where

import           Cardano.Api     (Address, AddressInEra, IsShelleyBasedEra,
                                  NetworkId, PaymentCredential, PaymentKey,
                                  ShelleyAddr, SigningKey)
import qualified Cardano.Api     as C
import           Control.Lens    (_1, _2, to, view, (^.))
import qualified Convex.Lenses   as L
import           Data.Aeson      (FromJSON (..), ToJSON (..), object,
                                  withObject, (.:), (.=))
import           Data.Foldable   (fold)
import           Data.List       (find)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as Text

newtype Wallet = Wallet { getWallet :: SigningKey PaymentKey }

instance ToJSON Wallet where
  toJSON k = object ["private_key" .= privateKey k]

instance FromJSON Wallet where
  parseJSON = withObject "Wallet" $ \obj -> do
    x <- obj .: "private_key"
    case parse x of
      Right pk -> pure pk
      Left err -> fail $ "failed to parse 'private_key': " <> show err

instance Show Wallet where
  show = Text.unpack . privateKey

{-| The wallet's payment credential (public key)
-}
paymentCredential :: Wallet -> PaymentCredential
paymentCredential Wallet{getWallet} =
  let hsh = C.verificationKeyHash (C.getVerificationKey getWallet)
  in C.PaymentCredentialByKey hsh

{-| The address of the wallet
-}
address :: NetworkId -> Wallet -> Address ShelleyAddr
address networkId wallet =
  C.makeShelleyAddress networkId (paymentCredential wallet) C.NoStakeAddress

addressInEra :: IsShelleyBasedEra era => NetworkId -> Wallet -> AddressInEra era
addressInEra networkId wallet =
  C.makeShelleyAddressInEra networkId (paymentCredential wallet) C.NoStakeAddress

{-| The wallet's private key (serialised)
-}
privateKey :: Wallet -> Text
privateKey Wallet{getWallet} = C.serialiseToBech32 getWallet

generateWallet :: IO Wallet
generateWallet = Wallet <$> C.generateSigningKey C.AsPaymentKey

parse :: Text -> Either C.Bech32DecodeError Wallet
parse = fmap Wallet . C.deserialiseFromBech32 (C.AsSigningKey C.AsPaymentKey)

{-| The wallet's transaction outputs
-}
data WalletUtxo =
  WalletUtxo
    { wiAdaOnlyOutputs :: !(Map C.TxIn C.Lovelace)
    , wiMixedOutputs   :: !(Map C.TxIn (C.TxOut C.CtxUTxO C.BabbageEra))
    }

adaOnly :: C.TxIn -> C.Lovelace -> WalletUtxo
adaOnly i c = WalletUtxo (Map.singleton i c) mempty

mixed :: C.TxIn -> C.TxOut C.CtxUTxO C.BabbageEra -> WalletUtxo
mixed i o = WalletUtxo mempty (Map.singleton i o)

removeTxIns :: Set C.TxIn -> WalletUtxo -> WalletUtxo
removeTxIns txIns WalletUtxo{wiAdaOnlyOutputs, wiMixedOutputs} =
  WalletUtxo
    (Map.withoutKeys wiAdaOnlyOutputs txIns)
    (Map.withoutKeys wiMixedOutputs txIns)

instance Semigroup WalletUtxo where
  l <> r =
    WalletUtxo
      { wiAdaOnlyOutputs = Map.unionWith (<>) (wiAdaOnlyOutputs l) (wiAdaOnlyOutputs r)
      , wiMixedOutputs   = Map.union (wiMixedOutputs l) (wiMixedOutputs r)
      }

instance Monoid WalletUtxo where
  mempty = WalletUtxo mempty mempty

fromUtxo :: AddressInEra C.BabbageEra -> C.TxIn -> C.TxOut C.CtxUTxO C.BabbageEra -> Maybe WalletUtxo
fromUtxo addr txIn txOut
  | view (L._TxOut . _1) txOut == addr =
      case (txOut ^. L._TxOut . _2 . L._TxOutValue . to C.valueToLovelace) of
        Just l -> Just (adaOnly txIn l)
        _      -> Just (mixed txIn txOut)
  | otherwise = Nothing

fromUtxos :: NetworkId -> Wallet -> C.UTxO C.BabbageEra -> WalletUtxo
fromUtxos nw (addressInEra nw -> a) (C.UTxO mp) =
  fold
  $ mapMaybe (uncurry $ fromUtxo a)
  $ Map.toList mp

{-| Select Ada-only inputs controlled by the wallet that cover the given amount of lovelace
-}
selectAdaInputsCovering :: WalletUtxo -> C.Lovelace -> Maybe (C.Lovelace, [C.TxIn])
selectAdaInputsCovering WalletUtxo{wiAdaOnlyOutputs} target =
  selectInputsForAda target wiAdaOnlyOutputs

{-| Select Ada-only inputs controlled by the wallet that cover the given amount of lovelace
-}
selectAnyInputsCovering :: WalletUtxo -> C.Lovelace -> Maybe (C.Lovelace, [C.TxIn])
selectAnyInputsCovering WalletUtxo{wiAdaOnlyOutputs, wiMixedOutputs} target =
  let others = Map.map (\txo -> txo ^. L._TxOut . _2 . L._TxOutValue . to C.selectLovelace) wiMixedOutputs
  in selectInputsForAda target (wiAdaOnlyOutputs `Map.union` others)

selectInputsForAda :: C.Lovelace -> Map C.TxIn C.Lovelace -> Maybe (C.Lovelace, [C.TxIn])
selectInputsForAda (C.Lovelace target) =
  let append (C.Lovelace total_, txIns) (txIn, C.Lovelace coin_) = (C.Lovelace (total_ + coin_), txIn : txIns) in
  find (\(C.Lovelace c, _) -> c >= target)
  . scanl append (C.Lovelace 0, [])
  . Map.toAscList

{-| Select inputs controlled by the wallet that cover the given amount of non-Ada
assets.
-}
selectMixedInputsCovering :: WalletUtxo -> [(C.PolicyId, C.AssetName, C.Quantity)] -> Maybe (C.Value, [C.TxIn])
selectMixedInputsCovering _                          [] = Just (mempty, [])
selectMixedInputsCovering WalletUtxo{wiMixedOutputs} xs =
  let append (vl, txIns) (vl', txIn) = (vl <> vl', txIn : txIns)
      coversTarget (candidateVl, _txIns) =
        all (\(policyId, assetName, quantity) -> C.selectAsset candidateVl (C.AssetId policyId assetName) >= quantity) xs
      requiredAssets = foldMap (\(p, a, _) -> Set.singleton (p, a)) xs
      nonAdaAssets = \case
        C.AdaAssetId  -> Set.empty
        C.AssetId p n -> Set.singleton (p, n)
      relevantValue (txIn, view (L._TxOut . _2 . L._TxOutValue) -> txOutValue) =
        let providedAssets = foldMap (nonAdaAssets . fst) (C.valueToList txOutValue)
        in if Set.null (Set.intersection requiredAssets providedAssets)
          then Nothing
          else Just (txOutValue, txIn)
  in
    find coversTarget
    $ scanl append (mempty, mempty)
    $ mapMaybe relevantValue
    $ Map.toAscList wiMixedOutputs
