{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main(main) where

import qualified Cardano.Api.Shelley       as C
import           Control.Lens              (mapped, over, (&))
import           Control.Monad             (void)
import           Convex.BuildTx            (assetValue, mintPlutusV1,
                                            payToAddress, payToPlutusV1,
                                            setMinAdaDeposit, spendPlutusV1)
import           Convex.Class              (MonadBlockchain (..),
                                            MonadBlockchainQuery)
import qualified Convex.CoinSelection      as CoinSelection
import           Convex.Lenses             (emptyTx)
import qualified Convex.Lenses             as L
import           Convex.MockChain          (Mockchain, runMockchain0)
import qualified Convex.MockChain.Defaults as Defaults
import           Convex.Wallet             (Wallet)
import qualified Convex.Wallet             as Wallet
import qualified Convex.Wallet.MockWallet  as Wallet
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
  let tx = emptyTx & payToPlutusV1 Defaults.networkId txInscript () (C.lovelaceToValue 10_000_000)
  i <- CoinSelection.balanceForWallet Defaults.nodeParams Wallet.w1 tx >>= sendTx
  pure (C.TxIn i (C.TxIx 0))

spendPlutusScript :: C.TxIn -> Mockchain C.TxId
spendPlutusScript ref = do
  let tx = emptyTx & spendPlutusV1 ref txInscript () ()
  CoinSelection.balanceForWallet Defaults.nodeParams Wallet.w1 tx >>= sendTx

mintingPlutus :: Mockchain C.TxId
mintingPlutus = do
  void $ Wallet.w2 `paymentTo` Wallet.w1
  let tx = emptyTx & mintPlutusV1 mintingScript () "assetName" 100
  CoinSelection.balanceForWallet Defaults.nodeParams Wallet.w1 tx >>= sendTx

spendTokens :: C.TxId -> Mockchain C.TxId
spendTokens _ = do
  _ <- nativeAssetPaymentTo 49 Wallet.w1 Wallet.w2
  _ <- nativeAssetPaymentTo 51 Wallet.w1 Wallet.w2
  _ <- nativeAssetPaymentTo 100 Wallet.w2 Wallet.w3
  nativeAssetPaymentTo 99 Wallet.w3 Wallet.w1

paymentTo :: (MonadBlockchain m, MonadBlockchainQuery m, MonadFail m) => Wallet -> Wallet -> m C.TxId
paymentTo wFrom wTo = do
  let tx = emptyTx & payToAddress (Wallet.addressInEra Defaults.networkId wTo) (C.lovelaceToValue 10_000_000)
  CoinSelection.balanceForWallet Defaults.nodeParams wFrom tx >>= sendTx

nativeAssetPaymentTo :: (MonadBlockchain m, MonadBlockchainQuery m, MonadFail m) => C.Quantity -> Wallet -> Wallet -> m C.TxId
nativeAssetPaymentTo q wFrom wTo = do
  let vl = assetValue (C.hashScript $ C.PlutusScript C.PlutusScriptV1 mintingScript) "assetName" q
      tx = emptyTx
            & payToAddress (Wallet.addressInEra Defaults.networkId wTo) vl
            & over (L.txOuts . mapped) (setMinAdaDeposit Defaults.protocolParameters)
  -- create a public key output for the sender to make
  -- sure that the sender has enough Ada in ada-only inputs
  void $ wTo `paymentTo` wFrom
  CoinSelection.balanceForWallet Defaults.nodeParams wFrom tx >>= sendTx
