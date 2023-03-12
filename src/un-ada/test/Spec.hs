{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main(main) where

import           Test.Tasty          (TestTree, defaultMain, testGroup)
import qualified UnAda.Test.UnitTest as UnitTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "unAda tests"
  [ UnitTest.tests

  ]
