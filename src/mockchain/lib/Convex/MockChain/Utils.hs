module Convex.MockChain.Utils(
  mockchainSucceeds
  ) where

import           Convex.MockChain         (Mockchain, runMockchain0)
import qualified Convex.Wallet.MockWallet as Wallet
import           Test.HUnit               (Assertion)

mockchainSucceeds :: Mockchain a -> Assertion
mockchainSucceeds action =
  case runMockchain0 Wallet.initialUTxOs action of
    Right _  -> pure ()
    Left err -> fail (show err)
