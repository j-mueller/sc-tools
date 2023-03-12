{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-| Unit tests for UnAda
-}
module UnAda.Test.UnitTest (
tests
) where

import           Convex.Lenses                  (emptyTx)
import           Convex.MockChain.CoinSelection (balanceAndSubmit, paymentTo)
import qualified Convex.MockChain.Defaults      as Defaults
import           Convex.MockChain.Utils         (mockchainSucceeds)
import qualified Convex.Wallet.MockWallet       as Wallet
import           Data.Function                  ((&))
import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.HUnit               (Assertion, testCase)
import           UnAda.OffChain.Transaction     (mintUnAda)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "mint some un-Ada" canMintUnAda
  ]

canMintUnAda :: Assertion
canMintUnAda = mockchainSucceeds $ do
  let tx = emptyTx & mintUnAda Defaults.networkId 10_000_000
  _ <- Wallet.w2 `paymentTo` Wallet.w1
  balanceAndSubmit Wallet.w1 tx
