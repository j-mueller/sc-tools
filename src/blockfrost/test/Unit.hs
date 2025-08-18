{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Unit tests for blockfrost backend
module Unit (tests) where

import Blockfrost.Types.Shared.Amount (Amount (..))
import Cardano.Api qualified as C
import Convex.Blockfrost.Types (
  toAddress,
  toAssetId,
  toPolicyId,
  toStakeAddress,
  toTxHash,
 )
import Convex.Utils.String (unsafeAssetName, unsafePolicyId, unsafeTxId)
import Data.Maybe (isJust)
import Money qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (
  assertBool,
  assertEqual,
  testCase,
 )

tests :: TestTree
tests =
  testGroup
    "unit"
    [ testGroup
        "conversions"
        [ testCase "policyId" $
            assertEqual
              "policy ID should match"
              (Right $ unsafePolicyId "476039a0949cf0b22f6a800f56780184c44533887ca6e821007840c3")
              (toPolicyId "476039a0949cf0b22f6a800f56780184c44533887ca6e821007840c3")
        , testCase "txId" $
            assertEqual
              "tx ID should match"
              (Right $ unsafeTxId "8788591983aa73981fc92d6cddbbe643959f5a784e84b8bee0db15823f575a5b")
              (toTxHash "8788591983aa73981fc92d6cddbbe643959f5a784e84b8bee0db15823f575a5b")
        , testCase "amount" $
            assertEqual
              "asset ID should match"
              (toAssetId $ AssetAmount $ Money.toSomeDiscrete (12 :: Money.Discrete' "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e" '(1, 1)))
              -- Note the difference in the asset names ('nutcoin' vs '6e7574636f696e')
              -- This is because the 'IsString' instance of AssetName uses the UTF8 encoding of the string (which doesn't make sense for script hash token names)
              -- whereas blockfrost gives us the hex encoded bytestring
              (Right (C.AssetId (unsafePolicyId "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a7") (unsafeAssetName "nutcoin"), 12))
        , testCase "address" $
            deserialiseAddress "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz"
        , testCase "stake address" $
            deserialiseStakeAddress "stake1ux3g2c9dx2nhhehyrezyxpkstartcqmu9hk63qgfkccw5rqttygt7"
        ]
    ]

deserialiseAddress =
  assertBool "should be able to deserialise address"
    . isJust
    . toAddress @C.ConwayEra

deserialiseStakeAddress =
  assertBool "should be able to deserialise stake address"
    . isJust
    . toStakeAddress
