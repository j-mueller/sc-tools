-- | Tests for the blockfrost backend
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Unit qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "blockfrost"
    [ Unit.tests
    ]
