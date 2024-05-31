{-# LANGUAGE TypeOperators #-}
{-| Testing plutarch scripts in the emulator
-}
module Main where

import qualified Cardano.Api.Shelley            as C
import           Control.Monad                  (void)
import           Convex.BuildTx                 (execBuildTx', payToPlutusV2,
                                                 spendPlutusV2)
import           Convex.Class                   (MonadMockchain)
import           Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import qualified Convex.MockChain.Defaults      as Defaults
import           Convex.MockChain.Utils         (mockchainSucceeds)
import           Convex.Plutarch                (plutarchScriptToCapiScript)
import           Convex.Utils                   (failOnError)
import qualified Convex.Wallet.MockWallet       as Wallet
import qualified Data.Text                      as Text
import           Plutarch                       (compile,
                                                Config(NoTracing),
                                                )
import           Plutarch.Prelude               (PData, PUnit (..), Term,
                                                 pconstant, plam, type (:-->))
import           Test.Tasty                     (TestTree, defaultMain,
                                                 testGroup)
import           Test.Tasty.HUnit               (testCase)

main :: IO ()
main = defaultMain tests

alwaysSucceedsP :: Term s (PData :--> PData :--> PData :--> PUnit)
alwaysSucceedsP = plam $ \_datm _redm _ctx -> pconstant ()

tests :: TestTree
tests = testGroup "plutarch"
  [ testCase "run the always-succeeds script" (mockchainSucceeds alwaysSucceeds)
  ]

alwaysSucceeds :: (MonadFail m, MonadMockchain m) => m ()
alwaysSucceeds = failOnError $ do
  k <- either (fail . Text.unpack) (pure . plutarchScriptToCapiScript) (compile NoTracing alwaysSucceedsP)
  ref <- fmap (C.getTxId . C.getTxBody) $ tryBalanceAndSubmit mempty Wallet.w1 $ execBuildTx' $ do
          payToPlutusV2 Defaults.networkId k () C.NoStakeAddress (C.lovelaceToValue 10_000_000)
  void $ tryBalanceAndSubmit mempty Wallet.w1 $ execBuildTx' (spendPlutusV2 (C.TxIn ref (C.TxIx 0)) k () ())
