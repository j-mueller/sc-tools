{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Primitive wallet
module Convex.Wallet (
  Wallet (..),
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
  selectMixedInputsCovering,
) where

import Cardano.Api (
  Address,
  AddressInEra,
  IsShelleyBasedEra,
  NetworkId,
  PaymentCredential,
  PaymentExtendedKey,
  PaymentKey,
  ShelleyAddr,
  SigningKey,
 )
import Cardano.Api qualified as C
import Cardano.Ledger.Credential qualified as Shelley
import Control.Lens (preview, _2)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Utxos (UtxoSet (..), onlyAda)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Bifunctor (Bifunctor (..))
import Data.List (find, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.IsList (IsList (toList))

newtype Wallet = Wallet {getWallet :: SigningKey PaymentKey}

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

-- | The wallet's payment credential (public key)
paymentCredential :: Wallet -> PaymentCredential
paymentCredential = C.PaymentCredentialByKey . verificationKeyHash

-- | Verification key hash of the wallet
verificationKeyHash :: Wallet -> C.Hash C.PaymentKey
verificationKeyHash = C.verificationKeyHash . C.getVerificationKey . getWallet

shelleyPaymentCredential :: Wallet -> Shelley.PaymentCredential
shelleyPaymentCredential =
  fromMaybe (error "shelleyPaymentCredential: Expected ShelleyAddress in Conway era")
    . preview (L._AddressInEra @C.ConwayEra . L._Address . _2)
    . addressInEra C.Mainnet

{- | Sign the transaction body with the signing key and attach the signature
to the transaction
-}
addSignature :: forall era. (IsShelleyBasedEra era) => SigningKey PaymentKey -> C.Tx era -> C.Tx era
addSignature (C.WitnessPaymentKey -> key) tx =
  let C.Tx body wits = tx
      wit = nub $ C.makeShelleyKeyWitness (C.shelleyBasedEra @era) body key : wits
      stx = C.makeSignedTransaction wit body
   in stx

{- | Sign the transaction body with the extended signing key and attach the signature
to the transaction
-}
addSignatureExtended :: forall era. (IsShelleyBasedEra era) => SigningKey PaymentExtendedKey -> C.Tx era -> C.Tx era
addSignatureExtended (C.WitnessPaymentExtendedKey -> key) tx =
  let C.Tx body wits = tx
      wit = nub $ C.makeShelleyKeyWitness (C.shelleyBasedEra @era) body key : wits
      stx = C.makeSignedTransaction wit body
   in stx

-- | Add the wallet's signature to the signatures of the transaction
signTx :: (IsShelleyBasedEra era) => Wallet -> C.Tx era -> C.Tx era
signTx Wallet{getWallet} = addSignature getWallet

-- | The address of the wallet
address :: NetworkId -> Wallet -> Address ShelleyAddr
address networkId wallet =
  C.makeShelleyAddress networkId (paymentCredential wallet) C.NoStakeAddress

addressInEra :: forall era. (IsShelleyBasedEra era) => NetworkId -> Wallet -> AddressInEra era
addressInEra networkId wallet =
  C.makeShelleyAddressInEra (C.shelleyBasedEra @era) networkId (paymentCredential wallet) C.NoStakeAddress

-- | The wallet's private key (serialised)
privateKey :: Wallet -> Text
privateKey Wallet{getWallet} = C.serialiseToBech32 getWallet

generateWallet :: IO Wallet
generateWallet = Wallet <$> C.generateSigningKey C.AsPaymentKey

parse :: Text -> Either C.Bech32DecodeError Wallet
parse = fmap Wallet . C.deserialiseFromBech32

-- | Select Ada-only inputs that cover the given amount of lovelace
selectAdaInputsCovering :: UtxoSet ctx a -> C.Quantity -> Maybe (C.Quantity, [C.TxIn])
selectAdaInputsCovering utxoSet = selectAnyInputsCovering (onlyAda utxoSet)

-- | Select Ada-only inputs that cover the given amount of lovelace
selectAnyInputsCovering :: UtxoSet ctx a -> C.Quantity -> Maybe (C.Quantity, [C.TxIn])
selectAnyInputsCovering UtxoSet{_utxos} (C.Quantity target) =
  let append (C.Quantity total_, txIns) (txIn, C.InAnyCardanoEra _ (C.TxOut _ (C.lovelaceToQuantity . C.selectLovelace . C.txOutValueToValue -> C.Quantity coin_) _ _)) =
        (C.Quantity (total_ + coin_), txIn : txIns)
   in find (\(C.Quantity c, _) -> c >= target) $
        scanl append (C.Quantity 0, []) $
          fmap (second fst) $
            Map.toAscList _utxos

{- | Select inputs that cover the given amount of non-Ada
assets.
-}
selectMixedInputsCovering :: UtxoSet ctx a -> [(C.AssetId, C.Quantity)] -> Maybe (C.Value, [C.TxIn])
selectMixedInputsCovering UtxoSet{_utxos} xs =
  let append (vl, txIns) (vl', txIn) = (vl <> vl', txIn : txIns)
      coversTarget (candidateVl, _txIns) =
        all (\(assetId, quantity) -> C.selectAsset candidateVl assetId >= quantity) xs
      requiredAssets = foldMap (\(a, _) -> Set.singleton a) xs
      relevantValue (txIn, C.InAnyCardanoEra _ (C.TxOut _ (C.txOutValueToValue -> value) _ _)) =
        let providedAssets = foldMap (Set.singleton . fst) (toList value)
         in if Set.null (Set.intersection requiredAssets providedAssets)
              then Nothing
              else Just (value, txIn)
   in find coversTarget $
        scanl append (mempty, mempty) $
          mapMaybe relevantValue $
            fmap (second fst) $
              Map.toAscList _utxos
