{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Unit tests for blockfrost backend
-}
module Unit(tests) where

import           Blockfrost.Types.Shared.Amount (Amount (..))
import qualified Cardano.Api                    as C
import           Convex.Blockfrost.Types        (toAssetId, toPolicyId,
                                                 toTxHash)
import qualified Money
import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.HUnit               (assertEqual, testCase)

tests :: TestTree
tests = testGroup "unit"
  [ testGroup "conversions"
      [ testCase "policyId"
          $ assertEqual "policy ID should match"
              "476039a0949cf0b22f6a800f56780184c44533887ca6e821007840c3"
              (toPolicyId "476039a0949cf0b22f6a800f56780184c44533887ca6e821007840c3")
      , testCase "txId"
          $ assertEqual "tx ID should match"
              "8788591983aa73981fc92d6cddbbe643959f5a784e84b8bee0db15823f575a5b"
              (toTxHash "8788591983aa73981fc92d6cddbbe643959f5a784e84b8bee0db15823f575a5b")
      , testCase "amount"
          $ assertEqual "asset ID should match"
              (toAssetId $ AssetAmount $ Money.toSomeDiscrete (12 :: Money.Discrete' "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e" '(1, 1)))
              (C.AssetId "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a7" "6e7574636f696e", 12)
      ]
  ]
