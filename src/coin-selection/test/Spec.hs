{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main(main) where

import qualified Cardano.Api.Shelley            as C
import           Control.Lens                   (mapped, over, (&))
import           Control.Monad                  (void)
import           Convex.BuildTx                 (addReference, assetValue, spendPlutusV1Ref,
                                                 mintPlutusV1, payToAddress,
                                                 payToPlutusV1,
                                                 payToPlutusV1Inline,
                                                 setMinAdaDeposit,
                                                 setMinAdaDepositAll,
                                                 spendPlutusV1)
import           Convex.Class                   (MonadBlockchain (..),
                                                 MonadMockchain)
import           Convex.Lenses                  (emptyTx)
import qualified Convex.Lenses                  as L
import           Convex.MockChain               (Mockchain)
import           Convex.MockChain.CoinSelection (balanceAndSubmit, paymentTo)
import qualified Convex.MockChain.Defaults      as Defaults
import           Convex.MockChain.Utils         (mockchainSucceeds)
import           Convex.Wallet                  (Wallet)
import qualified Convex.Wallet                  as Wallet
import qualified Convex.Wallet.MockWallet       as Wallet
import           Test.Tasty                     (TestTree, defaultMain,
                                                 testGroup)
import           Test.Tasty.HUnit               (Assertion, testCase)

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
    , testCase "using a reference script" (mockchainSucceeds (payToPlutusScript >>= spendPlutusScriptReference))
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

txInscript :: C.PlutusScript C.PlutusScriptV1
txInscript = C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn

mintingScript :: C.PlutusScript C.PlutusScriptV1
mintingScript = C.examplePlutusScriptAlwaysSucceeds C.WitCtxMint

payToPlutusScript :: Mockchain C.TxIn
payToPlutusScript = do
  let tx = emptyTx & payToPlutusV1 Defaults.networkId txInscript () (C.lovelaceToValue 10_000_000)
  i <- balanceAndSubmit Wallet.w1 tx
  pure (C.TxIn i (C.TxIx 0))

spendPlutusScript :: C.TxIn -> Mockchain C.TxId
spendPlutusScript ref = do
  let tx = emptyTx & spendPlutusV1 ref txInscript () ()
  balanceAndSubmit Wallet.w1 tx

putReferenceScript :: Wallet -> Mockchain C.TxIn
putReferenceScript wallet = do
  let tx = emptyTx
            & payToPlutusV1Inline (Wallet.addressInEra Defaults.networkId wallet) txInscript (C.lovelaceToValue 1_000_000)
            & setMinAdaDepositAll Defaults.protocolParameters
  txId <- balanceAndSubmit wallet tx
  pure (C.TxIn txId (C.TxIx 0))

spendPlutusScriptReference :: C.TxIn -> Mockchain C.TxId
spendPlutusScriptReference txIn = do
  refTxIn <- putReferenceScript Wallet.w1
  let tx = emptyTx
            & spendPlutusV1Ref txIn refTxIn (Just $ C.hashScript $ C.PlutusScript C.PlutusScriptV1 txInscript) () ()
            -- & addReference refTxIn
  balanceAndSubmit Wallet.w1 tx

mintingPlutus :: Mockchain C.TxId
mintingPlutus = do
  void $ Wallet.w2 `paymentTo` Wallet.w1
  let tx = emptyTx & mintPlutusV1 mintingScript () "assetName" 100
  balanceAndSubmit Wallet.w1 tx

spendTokens :: C.TxId -> Mockchain C.TxId
spendTokens _ = do
  _ <- nativeAssetPaymentTo 49 Wallet.w1 Wallet.w2
  _ <- nativeAssetPaymentTo 51 Wallet.w1 Wallet.w2
  _ <- nativeAssetPaymentTo 100 Wallet.w2 Wallet.w3
  nativeAssetPaymentTo 99 Wallet.w3 Wallet.w1

nativeAssetPaymentTo :: (MonadBlockchain m, MonadMockchain m, MonadFail m) => C.Quantity -> Wallet -> Wallet -> m C.TxId
nativeAssetPaymentTo q wFrom wTo = do
  let vl = assetValue (C.hashScript $ C.PlutusScript C.PlutusScriptV1 mintingScript) "assetName" q
      tx = emptyTx
            & payToAddress (Wallet.addressInEra Defaults.networkId wTo) vl
            & over (L.txOuts . mapped) (setMinAdaDeposit Defaults.protocolParameters)
  -- create a public key output for the sender to make
  -- sure that the sender has enough Ada in ada-only inputs
  void $ wTo `paymentTo` wFrom
  balanceAndSubmit wFrom tx
