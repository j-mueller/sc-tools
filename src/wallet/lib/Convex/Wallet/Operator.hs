{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Managing the credentials used for batching |-}
module Convex.Wallet.Operator(
  -- * Operator
  PaymentExtendedKey(..),
  Signing,
  Verification,
  toVerification,
  verificationKey,
  signTx,
  Operator(..),
  operatorAddress,
  operatorPaymentCredential,
  operatorWalletID,
  operatorReturnOutput,
  generateOperator,

  -- * Configuration
  OperatorConfigSigning(..),
  parseOperatorConfigSigning,
  loadOperatorFiles,
  OperatorConfigVerification(..),
  parseOperatorConfigVerification,
  loadOperatorFilesVerification
) where

import           Cardano.Api         (BabbageEra, CtxTx, TxOut)
import qualified Cardano.Api         as C
import           Convex.Class        (MonadBlockchain (networkId))
import           Convex.Lenses       (emptyTxOut)
import           Convex.PlutusLedger (transPubKeyHash, transStakeKeyHash)
import           Convex.Utils        (readSigningKeyFromFile,
                                      readStakingKeyFromFile,
                                      readVerificationKeyFromFile)
import           Convex.Wallet       (addSignature, addSignatureExtended)
import           Options.Applicative (Parser, help, long, metavar, optional,
                                      strOption)
import           PlutusLedgerApi.V1  (PubKeyHash (..))

data Signing

data Verification

data PaymentExtendedKey k where
  PESigning :: C.SigningKey C.PaymentKey -> PaymentExtendedKey Signing
  PESigningEx :: C.SigningKey C.PaymentExtendedKey -> PaymentExtendedKey Signing
  PEVerification :: C.VerificationKey C.PaymentKey -> PaymentExtendedKey Verification

deriving stock instance Show (PaymentExtendedKey k)

verificationKey :: PaymentExtendedKey k -> C.VerificationKey C.PaymentKey
verificationKey = \case
  PESigningEx k    -> C.castVerificationKey $ C.getVerificationKey k
  PESigning k      -> C.getVerificationKey k
  PEVerification k -> k

toVerification :: PaymentExtendedKey Signing -> PaymentExtendedKey Verification
toVerification = PEVerification . verificationKey

signTx :: C.IsShelleyBasedEra era => PaymentExtendedKey Signing -> C.Tx era -> C.Tx era
signTx = \case
  PESigningEx k -> addSignatureExtended k
  PESigning   k -> addSignature k

{-| An entity that can match orders
-}
data Operator k =
  Operator
    { oPaymentKey :: PaymentExtendedKey k
    , oStakeKey   :: Maybe (C.VerificationKey C.StakeKey)
    }

deriving stock instance Show (PaymentExtendedKey k) => Show (Operator k)

instance Show (PaymentExtendedKey k) => Eq (Operator k) where
  l == r = show l == show r

{-| Address of the operator in a network
-}
operatorAddress :: C.NetworkId -> Operator k -> C.Address C.ShelleyAddr
operatorAddress networkId_ op =
  C.makeShelleyAddress
    networkId_
    (operatorPaymentCredential op)
    C.NoStakeAddress

{-| The operator's payment credential (public key)
-}
operatorPaymentCredential :: Operator k -> C.PaymentCredential
operatorPaymentCredential = C.PaymentCredentialByKey . C.verificationKeyHash . verificationKey . oPaymentKey

{-| Key hashes in Plutus format
-}
operatorWalletID :: Operator k -> (PubKeyHash, Maybe PubKeyHash)
operatorWalletID Operator{oPaymentKey, oStakeKey} =
  ( transPubKeyHash $ C.verificationKeyHash $ verificationKey oPaymentKey
  , fmap (transStakeKeyHash . C.verificationKeyHash) oStakeKey
  )

operatorReturnOutput :: MonadBlockchain m => Operator k -> m (TxOut CtxTx BabbageEra)
operatorReturnOutput op = do
  n <- networkId
  pure $ emptyTxOut $ C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) $ operatorAddress n op

{-| Key files for operating the oracle and stablecoin
-}
data OperatorConfigSigning =
  OperatorConfigSigning
    { ocSigningKeyFile           :: FilePath
    , ocStakeVerificationKeyFile :: Maybe FilePath
    }
    deriving stock (Eq, Show)

{-| Key files for operating the oracle and stablecoin
-}
data OperatorConfigVerification =
  OperatorConfigVerification
    { ocvPaymentKeyFile           :: FilePath
    , ocvStakeVerificationKeyFile :: Maybe FilePath
    }
    deriving stock (Eq, Show)

loadOperatorFiles :: OperatorConfigSigning -> IO (Operator Signing)
loadOperatorFiles OperatorConfigSigning{ocSigningKeyFile, ocStakeVerificationKeyFile} =
  Operator <$> fmap PESigning (readSigningKeyFromFile ocSigningKeyFile) <*> traverse readStakingKeyFromFile ocStakeVerificationKeyFile

loadOperatorFilesVerification :: OperatorConfigVerification -> IO (Operator Verification)
loadOperatorFilesVerification OperatorConfigVerification{ocvStakeVerificationKeyFile, ocvPaymentKeyFile} =
  Operator <$> fmap PEVerification (readVerificationKeyFromFile ocvPaymentKeyFile) <*> traverse readStakingKeyFromFile ocvStakeVerificationKeyFile

parseOperatorConfigSigning :: Parser OperatorConfigSigning
parseOperatorConfigSigning =
  OperatorConfigSigning
    <$> strOption (long "signing-key-file" <> metavar "FILE" <> help "The operator's signing key file.")
    <*> optional  (strOption (long "stake-verification-key-file" <> metavar "FILE" <> help "The operator's stake verification key file (optional)."))

parseOperatorConfigVerification :: Parser OperatorConfigVerification
parseOperatorConfigVerification =
  OperatorConfigVerification
    <$> strOption (long "verification-key-file"  <> help "Payment verification key of the operator")
    <*> optional  (strOption (long "stake-verification-key-file"  <> help "Stake verification key of the operator (optional)"))

generateOperator :: IO (Operator Signing)
generateOperator =
  Operator
    <$> fmap PESigning (C.generateSigningKey C.AsPaymentKey)
    <*> pure Nothing
