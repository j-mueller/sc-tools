{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
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
  runMockchainPropWith,
  runTestableErr
  ) where

import qualified Cardano.Api               as C
import           Control.Exception         (SomeException, try)
import           Control.Monad.Except      (ExceptT, runExceptT)
import           Convex.MockChain          (InitialUTXOs, MockchainIO,
                                            MockchainT, initialStateFor,
                                            runMockchain, runMockchain0IOWith)
import qualified Convex.MockChain.Defaults as Defaults
import           Convex.NodeParams         (NodeParams)
import qualified Convex.Wallet.MockWallet  as Wallet
import           Data.Functor.Identity     (Identity)
import           Test.HUnit                (Assertion)
import           Test.QuickCheck           (Property, Testable (..),
                                            counterexample)
import           Test.QuickCheck.Monadic   (PropertyM (..), monadic)

{-| Run the 'Mockchain' action and fail if there is an error
-}
mockchainSucceeds :: MockchainIO C.ConwayEra a -> Assertion
mockchainSucceeds = mockchainSucceedsWith Defaults.nodeParams

{-| Run the 'Mockchain' action with the given node parameters and fail if there is an error
-}
mockchainSucceedsWith :: C.IsShelleyBasedEra era => NodeParams era -> MockchainIO era a -> Assertion
mockchainSucceedsWith params action =
   try @SomeException (runMockchain0IOWith Wallet.initialUTxOs params action) >>= \case
    Right{}  -> pure ()
    Left err -> fail (show err)

{-| Run the 'Mockchain' action, fail if it succeeds, and handle the error
  appropriately.
-}
mockchainFails :: MockchainIO C.ConwayEra a -> (SomeException -> Assertion) -> Assertion
mockchainFails =
  mockchainFailsWith Defaults.nodeParams

{-| Run the 'Mockchain' action with the given node parameters, fail if it
    succeeds, and handle the error appropriately.
-}
mockchainFailsWith :: C.IsShelleyBasedEra era => NodeParams era -> MockchainIO era a -> (SomeException -> Assertion) -> Assertion
mockchainFailsWith params action handleError =
  try @SomeException (runMockchain0IOWith Wallet.initialUTxOs params action) >>= \case
    Right _  -> fail "mockchainFailsWith: Did not fail"
    Left err -> handleError err

{-| Run the 'Mockchain' action as a QuickCheck property, considering all 'MockchainError'
as test failures.
-}
runMockchainPropWith ::
  forall era a. (Testable a, C.IsShelleyBasedEra era)
  => NodeParams era
  -- ^ Node parameters to use for the mockchain
  -> InitialUTXOs
  -- ^ Initial distribution
  -> PropertyM (MockchainT era Identity) a
  -- ^ The mockchain action to run
  -> Property
runMockchainPropWith nodeParams utxos =
  let iState = initialStateFor nodeParams utxos
  in monadic (\a -> fst $ runMockchain a nodeParams iState)

{-| Run the 'Mockchain' action as a QuickCheck property, using the default node params
    and initial distribution, and considering all 'MockchainError's as test failures.
-}
runMockchainProp :: forall a. (Testable a) => PropertyM (MockchainT C.ConwayEra Identity) a -> Property
runMockchainProp = runMockchainPropWith Defaults.nodeParams Wallet.initialUTxOs

{-| 'Either' with a 'Testable' instance for the 'Left' case
-}
newtype TestableErr e a = TestableErr (Either e a)

instance (Show e, Testable a) => Testable (TestableErr e a) where
  property (TestableErr v) = case v of
    Left err -> counterexample (show err) False
    Right k  -> property k

{-| Run the 'Mockchain' action as a QuickCheck property, using the default node params
    and initial distribution, and considering all 'MockchainError's as test failures.
-}
runTestableErr :: forall e m a. Functor m => ExceptT e m a -> m (TestableErr e a)
runTestableErr = fmap TestableErr . runExceptT
