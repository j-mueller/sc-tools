{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-| Unit tests for UnAda
-}
module UnAda.Test.UnitTest (
tests
) where

import qualified Cardano.Api.Shelley            as C
import           Control.Lens                   (mapped, over)
import           Control.Monad                  (void)
import           Convex.BuildTx                 (payToAddress, setMinAdaDeposit)
import           Convex.Class                   (MonadBlockchain (..),
                                                 MonadMockchain)
import           Convex.Lenses                  (emptyTx)
import qualified Convex.Lenses                  as L
import           Convex.MockChain.CoinSelection (balanceAndSubmit, paymentTo)
import qualified Convex.MockChain.Defaults      as Defaults
import           Convex.MockChain.Utils         (mockchainSucceeds)
import           Convex.Wallet                  (Wallet)
import qualified Convex.Wallet                  as Wallet
import qualified Convex.Wallet.MockWallet       as Wallet
import           Data.Function                  ((&))
import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.HUnit               (Assertion, testCase)
import           UnAda.OffChain.Transaction     (burnUnAda, findUnAdaOutputs,
                                                 mintUnAda)
import           UnAda.OffChain.Value           (unLovelaceValue)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "mint some un-Ada" canMintUnAda
  ]

canMintUnAda :: Assertion
canMintUnAda = mockchainSucceeds $ do
  let tx = emptyTx & mintUnAda Defaults.networkId 10_000_000
  _ <- Wallet.w2 `paymentTo` Wallet.w1
  mintingTx <- balanceAndSubmit Wallet.w1 tx
  _ <- unAdaPaymentTo 5_000_000 Wallet.w1 Wallet.w2

  case findUnAdaOutputs mintingTx of
    [(txi, (txo, _))] -> do
      let tx' = emptyTx & burnUnAda Defaults.networkId txi txo 3_000_000
      fmap findUnAdaOutputs (balanceAndSubmit Wallet.w1 tx') >>= \xs ->
        if length xs == 1 then pure () else fail "1: Expected exactly one unAda output"
    _ -> fail "0: Expected exactly one unAda output"

unAdaPaymentTo :: (MonadBlockchain m, MonadMockchain m, MonadFail m) => C.Quantity -> Wallet -> Wallet -> m (C.Tx C.BabbageEra)
unAdaPaymentTo q wFrom wTo = do
  let vl = unLovelaceValue q
      tx = emptyTx
            & payToAddress (Wallet.addressInEra Defaults.networkId wTo) vl
            & over (L.txOuts . mapped) (setMinAdaDeposit Defaults.protocolParameters)
  -- create a public key output for the sender to make
  -- sure that the sender has enough Ada in ada-only inputs
  void $ wTo `paymentTo` wFrom
  balanceAndSubmit wFrom tx
