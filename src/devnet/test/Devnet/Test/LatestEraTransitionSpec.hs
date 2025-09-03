{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Devnet.Test.LatestEraTransitionSpec (
  tests,
) where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Control.Lens ((&), (.~))
import Convex.BuildTx (
  execBuildTx,
  mintPlutus,
 )
import Convex.Class (MonadMockchain)
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils (
  mockchainFails,
  mockchainFailsWith,
  mockchainSucceeds,
 )
import Convex.NodeParams qualified as Params
import Convex.Utils (failOnError)
import Convex.Utils.String (unsafeAssetName)
import Convex.Wallet.MockWallet qualified as Wallet
import Devnet.Test.LatestEraTransitionSpec.PV2 qualified as LatestEraTransitionSpec.PV2
import Devnet.Test.LatestEraTransitionSpec.PV3 qualified as LatestEraTransitionSpec.PV3
import PlutusTx.Builtins qualified as BI
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests = do
  testGroup
    "Latest era and protocol version tests"
    [ testCase "usingReadBitInPlutusV2AndProtVer9ShouldFailSpec" usingReadBitInPlutusV2AndProtVer9ShouldFailSpec
    , testCase "usingWriteBitInPlutusV2AndProtVer9ShouldFailSpec" usingWriteBitInPlutusV2AndProtVer9ShouldFailSpec
    , testCase "usingReadBitInPlutusV3AndProtVer9ShouldFailSpec" usingReadBitInPlutusV3AndProtVer9ShouldFailSpec
    , testCase "usingWriteBitInPlutusV3AndProtVer9ShouldFailSpec" usingWriteBitInPlutusV3AndProtVer9ShouldFailSpec
    , testCase "usingReadBitInPlutusV2AndLatestProtVerShouldPassSpec" usingReadBitInPlutusV2AndLatestProtVerShouldPassSpec
    , testCase "usingWriteBitInPlutusV2AndLatestProtVerShouldPassSpec" usingWriteBitInPlutusV2AndLatestProtVerShouldPassSpec
    , testCase "usingReadBitInPlutusV3AndLatestProtVerShouldPassSpec" usingReadBitInPlutusV3AndLatestProtVerShouldPassSpec
    , testCase "usingWriteBitInPlutusV3AndLatestProtVerShouldPassSpec" usingWriteBitInPlutusV3AndLatestProtVerShouldPassSpec
    ]

pv9NodeParams :: Params.NodeParams C.ConwayEra
pv9NodeParams =
  Defaults.nodeParams
    & Params.ledgerProtocolParameters . Params.protocolParameters . L.ppProtocolVersionL .~ L.ProtVer (toEnum 9) 0

usingReadBitInPlutusV2AndProtVer9ShouldFailSpec :: IO ()
usingReadBitInPlutusV2AndProtVer9ShouldFailSpec = do
  mockchainFailsWith pv9NodeParams (mintTokenScriptTest (LatestEraTransitionSpec.PV2.readBitTestMintingPolicyScriptPV2 $ BI.mkB "0xF4")) (const $ pure ())

usingWriteBitInPlutusV2AndProtVer9ShouldFailSpec :: IO ()
usingWriteBitInPlutusV2AndProtVer9ShouldFailSpec = do
  mockchainFailsWith pv9NodeParams (mintTokenScriptTest (LatestEraTransitionSpec.PV2.writeBitTestMintingPolicyScriptPV2 $ BI.mkB "0xFF")) (const $ pure ())

usingReadBitInPlutusV3AndProtVer9ShouldFailSpec :: IO ()
usingReadBitInPlutusV3AndProtVer9ShouldFailSpec = do
  mockchainFailsWith pv9NodeParams (mintTokenScriptTest (LatestEraTransitionSpec.PV3.readBitTestMintingPolicyScriptPV3 $ BI.mkB "0xF4")) (const $ pure ())

usingWriteBitInPlutusV3AndProtVer9ShouldFailSpec :: IO ()
usingWriteBitInPlutusV3AndProtVer9ShouldFailSpec = do
  mockchainFailsWith pv9NodeParams (mintTokenScriptTest (LatestEraTransitionSpec.PV3.writeBitTestMintingPolicyScriptPV3 $ BI.mkB "0xFF")) (const $ pure ())

usingReadBitInPlutusV2AndLatestProtVerShouldPassSpec :: IO ()
usingReadBitInPlutusV2AndLatestProtVerShouldPassSpec = do
  mockchainFails (mintTokenScriptTest (LatestEraTransitionSpec.PV2.readBitTestMintingPolicyScriptPV2 $ BI.mkB "0xF4")) (const $ pure ())

usingWriteBitInPlutusV2AndLatestProtVerShouldPassSpec :: IO ()
usingWriteBitInPlutusV2AndLatestProtVerShouldPassSpec = do
  mockchainFails (mintTokenScriptTest (LatestEraTransitionSpec.PV2.writeBitTestMintingPolicyScriptPV2 $ BI.mkB "0xFF")) (const $ pure ())

usingReadBitInPlutusV3AndLatestProtVerShouldPassSpec :: IO ()
usingReadBitInPlutusV3AndLatestProtVerShouldPassSpec = do
  mockchainSucceeds (mintTokenScriptTest (LatestEraTransitionSpec.PV3.readBitTestMintingPolicyScriptPV3 $ BI.mkB "0xF4"))

usingWriteBitInPlutusV3AndLatestProtVerShouldPassSpec :: IO ()
usingWriteBitInPlutusV3AndLatestProtVerShouldPassSpec = do
  mockchainSucceeds (mintTokenScriptTest (LatestEraTransitionSpec.PV3.writeBitTestMintingPolicyScriptPV3 $ BI.mkB "0xFF"))

mintTokenScriptTest
  :: ( MonadMockchain era m
     , C.IsPlutusScriptLanguage lang
     , C.HasScriptLanguageInEra lang era
     , C.IsBabbageBasedEra era
     , MonadFail m
     )
  => C.PlutusScript lang
  -> m (C.Tx era)
mintTokenScriptTest script = do
  let txb =
        execBuildTx $
          mintPlutus script () (unsafeAssetName "C0FFEE") 1
  failOnError $ tryBalanceAndSubmit mempty Wallet.w1 txb TrailingChange []
