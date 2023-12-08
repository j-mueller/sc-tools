{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Utility functions for using the mockchain types in @hunit@ or @QuickCheck@ tests
module Convex.MockChain.Utils(
  -- * Useful mockchain actions

  -- * Running mockchain actions in HUnit tests
  mockchainSucceeds,
  mockchainSucceedsWith,
  mockchainFails,
  mockchainFailsWith,

  -- * Running mockchain actions in QuickCheck tests
  runMockchainProp,
  runMockchainPropWith
  ) where

import           Convex.MockChain          (InitialUTXOs, MockChainState (..),
                                            MockchainError, MockchainIO,
                                            MockchainT, initialStateFor,
                                            runMockchain, runMockchain0IOWith)
import qualified Convex.MockChain.Defaults as Defaults
import           Convex.NodeParams         (NodeParams)
import qualified Convex.Wallet.MockWallet  as Wallet
import           Data.Functor.Identity     (Identity (..))
import           Test.HUnit                (Assertion)
import           Test.QuickCheck           (Property, Testable, counterexample,
                                            property)
import           Test.QuickCheck.Monadic   (PropertyM (..), monadic)

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

{-| Run the 'Mockchain' action as a QuickCheck property, considering all 'MockchainError'
as test failures.
-}
runMockchainPropWith ::
  forall a. (Testable a)
  => NodeParams
  -- ^ Node parameters to use for the mockchain
  -> InitialUTXOs
  -- ^ Initial distribution
  -> PropertyM (MockchainT Identity) a
  -- ^ The mockchain action to run
  -> Property
runMockchainPropWith nodeParams utxos =
  let iState = initialStateFor nodeParams utxos
      resultToProp :: Either MockchainError (Property, MockChainState) -> Property
      resultToProp (Right (prop, _)) = prop
      resultToProp (Left err)        = counterexample (show err) $ property False
  in monadic (\a -> resultToProp $ runMockchain a nodeParams iState)

{-| Run the 'Mockchain' action as a QuickCheck property, using the default node params
    and initial distribution, and considering all 'MockchainError's as test failures.
-}
runMockchainProp :: forall a. (Testable a) => PropertyM (MockchainT Identity) a -> Property
runMockchainProp = runMockchainPropWith Defaults.nodeParams Wallet.initialUTxOs
