{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main(main) where

import qualified Cardano.Api.Shelley       as C
import           Control.Lens              ((&))
import           Control.Monad             (void)
import           Convex.BuildTx            (mintPlutusV1, payToAddress,
                                            payToPlutusV1, spendPlutusV1, assetValue)
import qualified Convex.CoinSelection      as CoinSelection
import           Convex.Lenses             (emptyTxBodyContent)
import           Convex.MockChain          (Mockchain, runMockchain0)
import           Convex.MockChain.Class    (MonadBlockchain (..),
                                            MonadBlockchainQuery)
import qualified Convex.MockChain.Defaults as Defaults
import           Convex.MockChain.Wallets  (Wallet)
import qualified Convex.MockChain.Wallets  as Wallet
import           Test.Tasty                (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit          (Assertion, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "unit tests"
  [ testGroup "payments"
    [ testCase "spending a public key output" spendPublicKeyOutput
    , testCase "making several payments" makeSeveralPayments
    ]
  , testGroup "scripts"
    [ testCase "paying to a plutus script" (mockchainSucceeds payToPlutusScript)
    , testCase "spending a plutus script output" (mockchainSucceeds (payToPlutusScript >>= spendPlutusScript))
    , testCase "minting a token" (mockchainSucceeds mintingPlutus)
    , testCase "making payments with tokens" (mockchainSucceeds (mintingPlutus >>= spendTokens))
    ]
  ]

spendPublicKeyOutput :: Assertion
spendPublicKeyOutput = mockchainSucceeds (Wallet.w2 `paymentTo` Wallet.w1)

makeSeveralPayments :: Assertion
makeSeveralPayments = mockchainSucceeds $ do
  void $ Wallet.w1 `paymentTo` Wallet.w2
  void $ Wallet.w2 `paymentTo` Wallet.w1
  void $ Wallet.w3 `paymentTo` Wallet.w1
  void $ Wallet.w1 `paymentTo` Wallet.w2

mockchainSucceeds :: Mockchain a -> Assertion
mockchainSucceeds action =
  case runMockchain0 Wallet.initialUTxOs action of
    Right _  -> pure ()
    Left err -> fail (show err)

txInscript :: C.PlutusScript C.PlutusScriptV1
txInscript = C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn

mintingScript :: C.PlutusScript C.PlutusScriptV1
mintingScript = C.examplePlutusScriptAlwaysSucceeds C.WitCtxMint

payToPlutusScript :: Mockchain C.TxIn
payToPlutusScript = do
  let tx = emptyTxBodyContent & payToPlutusV1 Defaults.networkId txInscript () (C.lovelaceToValue 10_000_000)
  i <- CoinSelection.balanceForWallet Defaults.nodeParams Wallet.w1 tx >>= sendTx
  pure (C.TxIn i (C.TxIx 0))

spendPlutusScript :: C.TxIn -> Mockchain C.TxId
spendPlutusScript ref = do
  let tx = emptyTxBodyContent & spendPlutusV1 ref txInscript () ()
  CoinSelection.balanceForWallet Defaults.nodeParams Wallet.w1 tx >>= sendTx

mintingPlutus :: Mockchain C.TxId
mintingPlutus = do
  void $ Wallet.w2 `paymentTo` Wallet.w1
  let tx = emptyTxBodyContent & mintPlutusV1 mintingScript () "assetName" 100
  CoinSelection.balanceForWallet Defaults.nodeParams Wallet.w1 tx >>= sendTx

spendTokens :: C.TxId -> Mockchain C.TxId
spendTokens _ = do
  _ <- nativeAssetPaymentTo 49 Wallet.w1 Wallet.w2
  _ <- nativeAssetPaymentTo 51 Wallet.w1 Wallet.w2
  _ <- nativeAssetPaymentTo 100 Wallet.w2 Wallet.w3
  nativeAssetPaymentTo 99 Wallet.w3 Wallet.w1

paymentTo :: (MonadBlockchain m, MonadBlockchainQuery m, MonadFail m) => Wallet -> Wallet -> m C.TxId
paymentTo wFrom wTo = do
  let tx = emptyTxBodyContent & payToAddress (Wallet.addressInEra wTo) (C.lovelaceToValue 10_000_000)
  CoinSelection.balanceForWallet Defaults.nodeParams wFrom tx >>= sendTx

nativeAssetPaymentTo :: (MonadBlockchain m, MonadBlockchainQuery m, MonadFail m) => C.Quantity -> Wallet -> Wallet -> m C.TxId
nativeAssetPaymentTo q wFrom wTo = do
  let vl = C.lovelaceToValue 3_000_000 <> assetValue (C.hashScript $ C.PlutusScript C.PlutusScriptV1 mintingScript) "assetName" q
      tx = emptyTxBodyContent & payToAddress (Wallet.addressInEra wTo) vl
  CoinSelection.balanceForWallet Defaults.nodeParams wFrom tx >>= sendTx
