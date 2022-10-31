{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Some wallets
-}
module Convex.MockChain.Wallets(
  Wallet(..),
  paymentCredential,
  address,
  address',
  addressInEra,
  addressInEra',
  privateKey,
  generateWallet,
  -- * UTxOs and coin selection
  WalletUtxo(..),
  fromUtxo,
  fromUtxos,
  selectAdaInputsCovering,
  selectMixedInputsCovering,
  removeTxIns,
  -- * Mock wallets for testing
  mockWallets,
  initialUTxOs,
  w1,
  w2,
  w3,
  w5,
  w6,
  w7,
  w8,
  w9,
  w10
) where

import           Cardano.Api                (Address, AddressInEra,
                                             IsShelleyBasedEra, NetworkId,
                                             PaymentCredential, PaymentKey,
                                             ShelleyAddr, SigningKey)
import qualified Cardano.Api                as C
import           Cardano.Ledger.Shelley.API (Coin (..))
import           Control.Lens               (_1, _2, to, view, (^.))
import qualified Convex.Lenses              as L
import qualified Convex.MockChain.Defaults  as Defaults
import           Data.Aeson                 (FromJSON (..), ToJSON (..), object,
                                             withObject, (.:), (.=))
import           Data.Foldable              (fold)
import           Data.List                  (find)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (mapMaybe)
import           Data.Set                   (Set)
import qualified Data.Set as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

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
address' :: NetworkId -> Wallet -> Address ShelleyAddr
address' networkId wallet =
  C.makeShelleyAddress networkId (paymentCredential wallet) C.NoStakeAddress

{-| The address of the wallet with the default network ID
-}
address :: Wallet -> Address ShelleyAddr
address = address' Defaults.networkId

addressInEra' :: IsShelleyBasedEra era => NetworkId -> Wallet -> AddressInEra era
addressInEra' networkId wallet =
  C.makeShelleyAddressInEra networkId (paymentCredential wallet) C.NoStakeAddress

{-| Address of the wallet with the default network ID
-}
addressInEra :: IsShelleyBasedEra era => Wallet -> AddressInEra era
addressInEra = addressInEra' Defaults.networkId

{-| The wallet's private key (serialised)
-}
privateKey :: Wallet -> Text
privateKey Wallet{getWallet} = C.serialiseToBech32 getWallet

generateWallet :: IO Wallet
generateWallet = Wallet <$> C.generateSigningKey C.AsPaymentKey

parse :: Text -> Either C.Bech32DecodeError Wallet
parse = fmap Wallet . C.deserialiseFromBech32 (C.AsSigningKey C.AsPaymentKey)

mkWallet :: Text -> Wallet
mkWallet txt = case parse txt of
  Left err -> error $ "failed to parse '" <> Text.unpack txt <> "': " <> show err
  Right w  -> w

mockWallets :: [Wallet]
mockWallets = [w1, w2, w3, w4, w5, w6, w7, w8, w9, w10]

w1 :: Wallet
w1 = mkWallet "addr_sk1ts8uksf097vzjl8yl7x4xawhjfmlcs2tmw3wrqunkvxu2cjgfj7q4x5mwg"

w2 :: Wallet
w2 = mkWallet "addr_sk1jhyfdp39sps7y9g8chlrgme9z8w6t5nqytsv54sz6fgs94spv2aqcegj7s"

w3 :: Wallet
w3 = mkWallet "addr_sk1j3p5c67k8zgqfh76kevuxzm6ampdd3mnfg9dgnwjan2ryqns2sdsrvyz2s"

w4 :: Wallet
w4 = mkWallet "addr_sk1z2gwlmnlspuh8pmkkf3kx06qttt5pyzc3um4q9tyty7dnl2ldf6swznq5q"

w5 :: Wallet
w5 = mkWallet "addr_sk1hpnuvd3rh6mgczjwd6f5yrya0e8p3q0sns2sc3c8qzezhtsqlhasqzzl2l"

w6 :: Wallet
w6 = mkWallet "addr_sk10ua3537rdvk63hgzlfarf32t6ntw69hwec3mudqmn2zs3m8r7cgsj2zgzl"

w7 :: Wallet
w7 = mkWallet "addr_sk1j6rwy2kajtfh4adj0allvalle6memx9an3sn740n9f45d6rqpz8qj5kqat"

w8 :: Wallet
w8 = mkWallet "addr_sk1d0z0prgh90tfcq6zfyepz8wmedcvyr5had3emxdlnzt2x9w4e0dq5yudvu"

w9 :: Wallet
w9 = mkWallet "addr_sk1xc4ad4jph6fp0kx5edkj8wdd4kw5qxgyu0yf7y0gkqgexa53spqsvlgtyh"

w10 :: Wallet
w10 = mkWallet "addr_sk129sf8z7c4qvzm3lgw0m0scu6nm456lxjdjtwpywcleqwp23mfv3swq8t6h"

{-| 100 Ada for each wallet
-}
initialUTxOs :: [(Wallet, Coin)]
initialUTxOs =
  let c = Coin 100_000_000_000
  in fmap (\w -> (w, c)) mockWallets

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
fromUtxos nw (addressInEra' nw -> a) (C.UTxO mp) =
  fold
  $ mapMaybe (uncurry $ fromUtxo a)
  $ Map.toList mp

selectAdaInputsCovering :: WalletUtxo -> C.Lovelace -> Maybe (C.Lovelace, [C.TxIn])
selectAdaInputsCovering WalletUtxo{wiAdaOnlyOutputs} (C.Lovelace target) =
  let append (C.Lovelace total_, txIns) (txIn, C.Lovelace coin_) = (C.Lovelace (total_ + coin_), txIn : txIns) in
  find (\(C.Lovelace c, _) -> c >= target)
  $ scanl append (C.Lovelace 0, [])
  $ Map.toAscList wiAdaOnlyOutputs

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
