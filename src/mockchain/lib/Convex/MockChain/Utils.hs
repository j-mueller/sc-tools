{-# LANGUAGE LambdaCase #-}
module Convex.MockChain.Utils(
  mockchainSucceeds,
  mockchainSucceedsWith,
  mockchainFails,
  mockchainFailsWith
  ) where

import           Convex.MockChain          (MockchainError, MockchainIO,
                                            runMockchain0IOWith)
import qualified Convex.MockChain.Defaults as Defaults
import           Convex.NodeParams         (NodeParams)
import qualified Convex.Wallet.MockWallet  as Wallet
import           Test.HUnit                (Assertion)

{-| Run the 'Mockchain' action and fail if there is an error
-}
mockchainSucceeds :: MockchainIO a -> Assertion
mockchainSucceeds = mockchainSucceedsWith Defaults.nodeParams

{-| Run the 'Mockchain' action with the given node parameters and fail if there is an error
-}
mockchainSucceedsWith :: NodeParams -> MockchainIO a -> Assertion
mockchainSucceedsWith params action =
   runMockchain0IOWith Wallet.initialUTxOs params action >>= \case
    Right{}  -> pure ()
    Left err -> fail (show err)

{-| Run the 'Mockchain' action, fail if it succeeds, and handle the error
  appropriately.
-}
mockchainFails :: MockchainIO a -> (MockchainError -> Assertion) -> Assertion
mockchainFails action =
  mockchainFailsWith Defaults.nodeParams action

{-| Run the 'Mockchain' action with the given node parameters, fail if it
    succeeds, and handle the error appropriately.
-}
mockchainFailsWith :: NodeParams -> MockchainIO a -> (MockchainError -> Assertion) -> Assertion
mockchainFailsWith params action handleError =
  runMockchain0IOWith Wallet.initialUTxOs params action >>= \case
    Right _  -> fail ("mockchainFailsWith: Did not fail")
    Left err -> handleError err
