{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import UnAda.Test.UnitTest qualified as UnitTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "unAda tests"
    [ UnitTest.tests
    ]
