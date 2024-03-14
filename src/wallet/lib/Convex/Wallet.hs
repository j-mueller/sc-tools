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
  verificationKeyHash,
  shelleyPaymentCredential,
  address,
  addressInEra,
  privateKey,
  generateWallet,
  parse,
  signTx,
  addSignature,
  addSignatureExtended,
  -- * UTxOs and coin selection
  selectAdaInputsCovering,
  selectAnyInputsCovering,
  selectMixedInputsCovering
) where

import           Cardano.Api               (Address, AddressInEra,
                                            IsShelleyBasedEra, NetworkId,
                                            PaymentCredential,
                                            PaymentExtendedKey, PaymentKey,
                                            ShelleyAddr, SigningKey)
import qualified Cardano.Api               as C
import qualified Cardano.Ledger.Credential as Shelley
import           Cardano.Ledger.Crypto     (StandardCrypto)
import           Control.Lens              (_2, preview, view)
import qualified Convex.Lenses             as L
import           Convex.Utxos              (UtxoSet (..), onlyAda)
import           Data.Aeson                (FromJSON (..), ToJSON (..), object,
                                            withObject, (.:), (.=))
import           Data.Bifunctor            (Bifunctor (..))
import           Data.List                 (find, nub)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe, mapMaybe)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

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
paymentCredential = C.PaymentCredentialByKey . verificationKeyHash

{-| Verification key hash of the wallet
-}
verificationKeyHash :: Wallet -> C.Hash C.PaymentKey
verificationKeyHash = C.verificationKeyHash . C.getVerificationKey . getWallet

shelleyPaymentCredential :: Wallet -> Shelley.PaymentCredential StandardCrypto
shelleyPaymentCredential =
  fromMaybe (error "shelleyPaymentCredential: Expected ShelleyAddress in babbage era")
  . preview (L._AddressInEra . L._Address . _2)
  . addressInEra C.Mainnet

{-| Sign the transaction body with the signing key and attach the signature
to the transaction
-}
addSignature :: IsShelleyBasedEra era => SigningKey PaymentKey -> C.Tx era -> C.Tx era
addSignature (C.WitnessPaymentKey -> key) tx =
  let C.Tx body wits = tx
      wit = nub $ C.makeShelleyKeyWitness body key : wits
      stx = C.makeSignedTransaction wit body
  in stx

{-| Sign the transaction body with the extended signing key and attach the signature
to the transaction
-}
addSignatureExtended :: IsShelleyBasedEra era => SigningKey PaymentExtendedKey -> C.Tx era -> C.Tx era
addSignatureExtended (C.WitnessPaymentExtendedKey -> key) tx =
  let C.Tx body wits = tx
      wit = nub $ C.makeShelleyKeyWitness body key : wits
      stx = C.makeSignedTransaction wit body
  in stx

{-| Add the wallet's signature to the signatures of the transaction
-}
signTx :: IsShelleyBasedEra era => Wallet -> C.Tx era -> C.Tx era
signTx Wallet{getWallet} = addSignature getWallet

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

{-| Select Ada-only inputs that cover the given amount of lovelace
-}
selectAdaInputsCovering :: UtxoSet ctx a -> C.Lovelace -> Maybe (C.Lovelace, [C.TxIn])
selectAdaInputsCovering utxoSet target = selectAnyInputsCovering (onlyAda utxoSet) target

{-| Select Ada-only inputs that cover the given amount of lovelace
-}
selectAnyInputsCovering :: UtxoSet ctx a -> C.Lovelace -> Maybe (C.Lovelace, [C.TxIn])
selectAnyInputsCovering UtxoSet{_utxos} (C.Lovelace target) =
  let append (C.Lovelace total_, txIns) (txIn, C.selectLovelace . view (L._TxOut . _2 . L._TxOutValue) -> C.Lovelace coin_) = (C.Lovelace (total_ + coin_), txIn : txIns) in
  find (\(C.Lovelace c, _) -> c >= target)
  $ scanl append (C.Lovelace 0, [])
  $ fmap (second fst)
  $ Map.toAscList _utxos

{-| Select inputs that cover the given amount of non-Ada
assets.
-}
selectMixedInputsCovering :: UtxoSet ctx a -> [(C.AssetId, C.Quantity)] -> Maybe (C.Value, [C.TxIn])
selectMixedInputsCovering UtxoSet{_utxos} xs =
  let append (vl, txIns) (vl', txIn) = (vl <> vl', txIn : txIns)
      coversTarget (candidateVl, _txIns) =
        all (\(assetId, quantity) -> C.selectAsset candidateVl assetId >= quantity) xs
      requiredAssets = foldMap (\(a, _) -> Set.singleton a) xs
      relevantValue (txIn, view (L._TxOut . _2 . L._TxOutValue) -> txOutValue) =
        let providedAssets = foldMap (Set.singleton . fst) (C.valueToList txOutValue)
        in if Set.null (Set.intersection requiredAssets providedAssets)
          then Nothing
          else Just (txOutValue, txIn)
  in
    find coversTarget
    $ scanl append (mempty, mempty)
    $ mapMaybe relevantValue
    $ fmap (second fst)
    $ Map.toAscList _utxos
