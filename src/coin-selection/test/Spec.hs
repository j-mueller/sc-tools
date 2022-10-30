{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main(main) where

import qualified Cardano.Api.Shelley       as C
import           Control.Lens              ((&))
import           Control.Monad             (void)
import           Convex.BuildTx            (payToAddress, payToPlutusV1,
                                            spendPlutusV1)
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

paymentTo :: (MonadBlockchain m, MonadBlockchainQuery m, MonadFail m) => Wallet -> Wallet -> m C.TxId
paymentTo wFrom wTo = do
  let tx = emptyTxBodyContent & payToAddress (Wallet.addressInEra wTo) (C.lovelaceToValue 10_000_000)
  CoinSelection.balanceForWallet Defaults.nodeParams wFrom tx >>= sendTx

script :: C.PlutusScript C.PlutusScriptV1
script = C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn

payToPlutusScript :: Mockchain C.TxIn
payToPlutusScript = do
  let tx = emptyTxBodyContent & payToPlutusV1 Defaults.networkId script () (C.lovelaceToValue 10_000_000)
  i <- CoinSelection.balanceForWallet Defaults.nodeParams Wallet.w1 tx >>= sendTx
  pure (C.TxIn i (C.TxIx 0))

spendPlutusScript :: C.TxIn -> Mockchain C.TxId
spendPlutusScript ref = do
  let tx = emptyTxBodyContent & spendPlutusV1 ref script () ()
  CoinSelection.balanceForWallet Defaults.nodeParams Wallet.w1 tx >>= sendTx
