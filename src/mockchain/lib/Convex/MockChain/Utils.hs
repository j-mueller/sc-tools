{-# LANGUAGE LambdaCase #-}
module Convex.MockChain.Utils(
  mockchainSucceeds,
  mockchainFails
  ) where

import           Convex.MockChain         (Mockchain, MockchainError,
                                           MockchainIO, runMockchain0,
                                           runMockchain0IO)
import qualified Convex.Wallet.MockWallet as Wallet
import           Test.HUnit               (Assertion)

{-| Run the 'Mockchain' action and fail if there is an error
-}
mockchainSucceeds :: MockchainIO a -> Assertion
mockchainSucceeds action =
  runMockchain0IO Wallet.initialUTxOs action >>= \case
    Right{}  -> pure ()
    Left err -> fail (show err)

{-| Run the 'Mockchain' action, fail if it succeeds, and handle the error
  appropriately.
|-}
mockchainFails :: Mockchain a -> (MockchainError -> Assertion) -> Assertion
mockchainFails action handleError =
  case runMockchain0 Wallet.initialUTxOs action of
    Right _  -> fail ("mockchainFails: Did not fail")
    Left err -> handleError err
