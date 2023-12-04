{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}
module Main(main) where

import qualified Cardano.Api.Shelley            as C
import           Cardano.Ledger.Alonzo.Rules    (AlonzoUtxoPredFailure (..))
import           Cardano.Ledger.Alonzo.Scripts  (AlonzoScript (..),
                                                 transProtocolVersion,
                                                 validScript)
import           Cardano.Ledger.Babbage.Rules   (BabbageUtxoPredFailure (..),
                                                 BabbageUtxowPredFailure (..))
import           Cardano.Ledger.Language        (Language (..))
import           Cardano.Ledger.Shelley.API     (ApplyTxError (..))
import           Cardano.Ledger.Shelley.Rules   (ShelleyLedgerPredFailure (..))
import           Control.Lens                   (_3, _4, view, (&), (.~))
import           Control.Monad                  (void, when)
import           Control.Monad.Except           (MonadError, runExcept,
                                                 runExceptT)
import           Control.Monad.State.Strict     (execStateT, modify)
import           Control.Monad.Trans.Class      (MonadTrans (..))
import           Convex.BuildTx                 (BuildTxT, addRequiredSignature,
                                                 assetValue, execBuildTx',
                                                 execBuildTxT, mintPlutusV1,
                                                 payToAddress,
                                                 payToAddressTxOut,
                                                 payToPlutusV1, payToPlutusV2,
                                                 payToPlutusV2Inline,
                                                 prependTxOut,
                                                 setMinAdaDepositAll,
                                                 spendPlutusV1, spendPlutusV2,
                                                 spendPlutusV2Ref)
import qualified Convex.BuildTx                 as BuildTx
import           Convex.Class                   (MonadBlockchain (..),
                                                 MonadMockchain (resolveDatumHash))
import           Convex.CoinSelection           (BalanceTxError, keyWitnesses,
                                                 publicKeyCredential)
import qualified Convex.Lenses                  as L
import           Convex.MockChain               (MockchainError (..),
                                                 ValidationError (..))
import           Convex.MockChain.CoinSelection (balanceAndSubmit,
                                                 payToOperator', paymentTo)
import qualified Convex.MockChain.Defaults      as Defaults
import qualified Convex.MockChain.Gen           as Gen
import           Convex.MockChain.Utils         (mockchainFails,
                                                 mockchainSucceeds,
                                                 mockchainSucceedsWith,
                                                 runMockchainProp)
import           Convex.NodeParams              (maxTxSize, protocolParameters)
import           Convex.Query                   (balancePaymentCredentials)
import           Convex.Utils                   (failOnError)
import           Convex.Wallet                  (Wallet)
import qualified Convex.Wallet                  as Wallet
import qualified Convex.Wallet.MockWallet       as Wallet
import           Convex.Wallet.Operator         (oPaymentKey,
                                                 operatorPaymentCredential,
                                                 operatorReturnOutput,
                                                 signTxOperator,
                                                 verificationKey)
import           Data.Foldable                  (traverse_)
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import qualified PlutusLedgerApi.V2             as PV2
import qualified Scripts
import qualified Test.QuickCheck.Gen            as Gen
import           Test.Tasty                     (TestTree, defaultMain,
                                                 testGroup)
import           Test.Tasty.HUnit               (Assertion, assertBool,
                                                 testCase)
import qualified Test.Tasty.QuickCheck          as QC
import           Test.Tasty.QuickCheck          (Property, classify,
                                                 testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "unit tests"
  [ testGroup "payments"
    [ testCase "spending a public key output" spendPublicKeyOutput
    , testCase "making several payments" makeSeveralPayments
    , testProperty "balance transactions with many addresses" balanceMultiAddress
    ]
  , testGroup "scripts"
    [ testCase "paying to a plutus script" (mockchainSucceeds $ failOnError payToPlutusScript)
    , testCase "spending a plutus script output" (mockchainSucceeds $ failOnError (payToPlutusScript >>= spendPlutusScript))
    , testCase "spending a plutus script (V2) output" (mockchainSucceeds $ failOnError (payToPlutusV2Script >>= spendPlutusV2Script))
    , testCase "well-formed scripts" wellFormedScripts
    , testCase "creating a reference script output" (mockchainSucceeds $ failOnError $ putReferenceScript Wallet.w1)
    , testCase "using a reference script" (mockchainSucceeds $ failOnError (payToPlutusV2Script >>= spendPlutusScriptReference))
    , testCase "minting a token" (mockchainSucceeds $ failOnError mintingPlutus)
    , testCase "making payments with tokens" (mockchainSucceeds $ failOnError (mintingPlutus >>= spendTokens))
    , testCase "making payments with tokens (2)" (mockchainSucceeds $ failOnError (mintingPlutus >>= spendTokens2))
    ]
  , testGroup "mockchain"
    [ testCase "resolveDatumHash" (mockchainSucceeds $ failOnError checkResolveDatumHash)
    , testCase "large transactions" largeTransactionTest
    ]
  ]

spendPublicKeyOutput :: Assertion
spendPublicKeyOutput = mockchainSucceeds $ failOnError (Wallet.w2 `paymentTo` Wallet.w1)

wellFormedScripts :: Assertion
wellFormedScripts = do
  let protVer = Defaults.protVer Defaults.nodeParams
      s = Cardano.Ledger.Alonzo.Scripts.PlutusScript PlutusV2 Scripts.v2SpendingScriptSerialised
  either (fail . show) pure (runExcept (PV2.assertScriptWellFormed (transProtocolVersion protVer) Scripts.v2SpendingScriptSerialised))
  assertBool "validScript" (validScript protVer s)

makeSeveralPayments :: Assertion
makeSeveralPayments = mockchainSucceeds $ failOnError $ do
  void $ Wallet.w1 `paymentTo` Wallet.w2
  void $ Wallet.w2 `paymentTo` Wallet.w1
  void $ Wallet.w3 `paymentTo` Wallet.w1
  void $ Wallet.w1 `paymentTo` Wallet.w2

txInscript :: C.PlutusScript C.PlutusScriptV1
txInscript = C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn

mintingScript :: C.PlutusScript C.PlutusScriptV1
mintingScript = C.examplePlutusScriptAlwaysSucceeds C.WitCtxMint

payToPlutusScript :: (MonadFail m, MonadError BalanceTxError m, MonadMockchain m) => m C.TxIn
payToPlutusScript = do
  let tx = execBuildTx' (payToPlutusV1 Defaults.networkId txInscript () C.NoStakeAddress (C.lovelaceToValue 10_000_000))
  i <- C.getTxId . C.getTxBody <$> balanceAndSubmit mempty Wallet.w1 tx
  pure (C.TxIn i (C.TxIx 0))

payToPlutusV2Script :: (MonadFail m, MonadMockchain m, MonadError BalanceTxError m) => m C.TxIn
payToPlutusV2Script = do
  let tx = execBuildTx' (payToPlutusV2 Defaults.networkId Scripts.v2SpendingScript () C.NoStakeAddress (C.lovelaceToValue 10_000_000))
  i <- C.getTxId . C.getTxBody <$> balanceAndSubmit mempty Wallet.w1 tx
  pure (C.TxIn i (C.TxIx 0))

spendPlutusScript :: (MonadFail m, MonadMockchain m, MonadError BalanceTxError m) => C.TxIn -> m C.TxId
spendPlutusScript ref = do
  let tx = execBuildTx' (spendPlutusV1 ref txInscript () ())
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty Wallet.w1 tx

spendPlutusV2Script :: (MonadFail m, MonadMockchain m, MonadError BalanceTxError m) => C.TxIn -> m C.TxId
spendPlutusV2Script ref = do
  let tx = execBuildTx' (spendPlutusV2 ref Scripts.v2SpendingScript () ())
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty Wallet.w1 tx

putReferenceScript :: (MonadFail m, MonadMockchain m, MonadError BalanceTxError m) => Wallet -> m C.TxIn
putReferenceScript wallet = do
  let hsh = C.hashScript (C.PlutusScript C.PlutusScriptV2 Scripts.v2SpendingScript)
      addr = C.makeShelleyAddressInEra Defaults.networkId (C.PaymentCredentialByScript hsh) C.NoStakeAddress
      tx = execBuildTx' $
            payToPlutusV2Inline addr Scripts.v2SpendingScript (C.lovelaceToValue 10_000_000)
            >> setMinAdaDepositAll Defaults.bundledProtocolParameters
  txId <- C.getTxId . C.getTxBody <$> balanceAndSubmit mempty wallet tx
  let outRef = C.TxIn txId (C.TxIx 0)
  C.UTxO utxo <- utxoByTxIn (Set.singleton outRef)
  case Map.lookup outRef utxo of
    Nothing  -> fail "Utxo not found"
    Just out -> case view (L._TxOut . _4) out of
      C.ReferenceScript{} -> pure ()
      _                   -> fail "No reference script found"
  pure outRef

spendPlutusScriptReference :: (MonadFail m, MonadMockchain m, MonadError BalanceTxError m) =>  C.TxIn -> m C.TxId
spendPlutusScriptReference txIn = do
  refTxIn <- putReferenceScript Wallet.w1
  let tx = execBuildTx' (spendPlutusV2Ref txIn refTxIn (Just $ C.hashScript (C.PlutusScript C.PlutusScriptV2 Scripts.v2SpendingScript)) () ())
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty Wallet.w1 tx

mintingPlutus :: (MonadFail m, MonadMockchain m, MonadError BalanceTxError m) => m C.TxId
mintingPlutus = do
  void $ Wallet.w2 `paymentTo` Wallet.w1
  let tx = execBuildTx' (mintPlutusV1 mintingScript () "assetName" 100)
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty Wallet.w1 tx

spendTokens :: (MonadFail m, MonadMockchain m, MonadError BalanceTxError m) => C.TxId -> m C.TxId
spendTokens _ = do
  _ <- nativeAssetPaymentTo 49 Wallet.w1 Wallet.w2
  _ <- nativeAssetPaymentTo 51 Wallet.w1 Wallet.w2
  _ <- nativeAssetPaymentTo 100 Wallet.w2 Wallet.w3
  nativeAssetPaymentTo 99 Wallet.w3 Wallet.w1

spendTokens2 :: (MonadFail m, MonadMockchain m, MonadError BalanceTxError m) => C.TxId -> m C.TxId
spendTokens2 txi = do
  let q  = 98
      wTo = Wallet.w2
      wFrom = Wallet.w1
      vl = assetValue (C.hashScript $ C.PlutusScript C.PlutusScriptV1 mintingScript) "assetName" q
      tx = execBuildTx' $ do
            payToAddress (Wallet.addressInEra Defaults.networkId wTo) vl
            BuildTx.spendPublicKeyOutput (C.TxIn txi (C.TxIx 0))
            mintPlutusV1 mintingScript () "assetName" (-2)
            setMinAdaDepositAll Defaults.bundledProtocolParameters
  void $ wTo `paymentTo` wFrom
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty wFrom tx

nativeAssetPaymentTo :: (MonadMockchain m, MonadFail m, MonadError BalanceTxError m) => C.Quantity -> Wallet -> Wallet -> m C.TxId
nativeAssetPaymentTo q wFrom wTo = do
  let vl = assetValue (C.hashScript $ C.PlutusScript C.PlutusScriptV1 mintingScript) "assetName" q
      tx = execBuildTx' $
            payToAddress (Wallet.addressInEra Defaults.networkId wTo) vl
            >> setMinAdaDepositAll Defaults.bundledProtocolParameters
  -- create a public key output for the sender to make
  -- sure that the sender has enough Ada in ada-only inputs
  void $ wTo `paymentTo` wFrom
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty wFrom tx

checkResolveDatumHash :: (MonadMockchain m, MonadFail m, MonadError BalanceTxError m) => m ()
checkResolveDatumHash = do
  let addr = Wallet.addressInEra Defaults.networkId Wallet.w1
      assertDatumPresent vl = resolveDatumHash (C.hashScriptDataBytes vl) >>= \case
        Nothing -> fail "Expected datum"
        Just x
          | x == C.getScriptData vl -> pure ()
          | otherwise -> fail $ "Expected " <> show (C.getScriptData vl) <> ", found " <> show x


  -- 1. resolve an inline datum
  let datum1 = C.unsafeHashableScriptData  (C.ScriptDataConstructor 5 [])
      dat = C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra datum1
      txOut = payToAddressTxOut addr mempty
                & L._TxOut . _3 .~ dat

  _ <- execBuildTxWallet Wallet.w1 (prependTxOut txOut)

  assertDatumPresent datum1

  -- 2. resolve a datum that was provided as a "TxOutDatumInTx" (this is what 'payToPlutusV1' does)
  let d2 = (11 :: Integer, 12 :: Integer)
      datum2 = C.unsafeHashableScriptData $ C.fromPlutusData $ PV2.toData d2
  _ <- execBuildTxWallet Wallet.w1 (payToPlutusV1 Defaults.networkId txInscript d2 C.NoStakeAddress mempty)
  assertDatumPresent datum2

  -- 3. resolve a datum that was provided by a redeeming transaction
  let d3 = 500 :: Integer
      datum3 = C.unsafeHashableScriptData $ C.fromPlutusData $ PV2.toData d3
      txo =
        C.TxOut
          (C.makeShelleyAddressInEra Defaults.networkId (C.PaymentCredentialByScript (C.hashScript (C.PlutusScript C.PlutusScriptV1 txInscript))) C.NoStakeAddress)
          (C.TxOutValue C.MultiAssetInBabbageEra mempty)
          (C.TxOutDatumHash C.ScriptDataInBabbageEra (C.hashScriptDataBytes datum3))
          C.ReferenceScriptNone
  txId <- execBuildTxWallet Wallet.w1 (prependTxOut txo)
  _ <- execBuildTxWallet Wallet.w1 (spendPlutusV1 (C.TxIn txId (C.TxIx 0)) txInscript d3 ())
  assertDatumPresent datum3


{-| Build a transaction, then balance and sign it with the wallet, then
  submit it to the mockchain.
-}
execBuildTxWallet :: (MonadMockchain m, MonadError BalanceTxError m) => Wallet -> BuildTxT m a -> m C.TxId
execBuildTxWallet wallet action = do
  tx <- execBuildTxT (action >> setMinAdaDepositAll Defaults.bundledProtocolParameters)
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty wallet (tx L.emptyTx)

-- | Balance a transaction using a list of operators
--   Check that the fees are calculated correctly to spend outputs from different addresses
--   and to consider the required signatures
balanceMultiAddress :: Property
balanceMultiAddress = do
  let gen = (,) <$> Gen.operator <*> fmap (take 20) (Gen.listOf Gen.operator)
  QC.forAll gen $ \(op, operators) ->
    QC.forAll (Gen.chooseInteger (5_000_000, 100_000_00 * fromIntegral (1 + length operators))) $ \(C.Lovelace -> nAmount) ->
      QC.forAll (Gen.sublistOf (op:operators)) $ \requiredSignatures ->
        classify (null operators) "1 operator"
          $ classify (length operators > 0 && length operators <= 9) "2-9 operators"
          $ classify (length operators > 9) "10+ operators"
          $ classify (length requiredSignatures == 0) "0 required signatures"
          $ classify (length requiredSignatures > 0 && length requiredSignatures <= 9) "1-9 required signatures"
          $ classify (length requiredSignatures > 9) "10+ required signatures"
          $ runMockchainProp $ lift $ failOnError $ do
              -- send Ada to each operator
              traverse_ (payToOperator' mempty (C.lovelaceToValue $ 2_500_000 + nAmount) Wallet.w2) (op:operators)

              -- send the entire amount back to Wallet.w2
              walletAddr <- Wallet.addressInEra <$> networkId <*> pure Wallet.w2
              protParams <- queryProtocolParameters
              let tx = execBuildTx' $ do
                        payToAddress walletAddr $ C.lovelaceToValue nAmount
                        traverse_ addRequiredSignature (fmap (C.verificationKeyHash . verificationKey . oPaymentKey) requiredSignatures)
                        setMinAdaDepositAll protParams

              -- balance the tx using all of the operators' addressses
              balancedTx <- runExceptT (balancePaymentCredentials mempty (operatorPaymentCredential op) (operatorPaymentCredential <$> operators) Nothing tx) >>= either (fail . show) pure
              txInputs <- let C.Tx (C.TxBody txBody) _ = balancedTx in keyWitnesses txBody
              let (Set.fromList -> extraWits) = let C.Tx (C.TxBody txBody) _ = balancedTx in view (L.txExtraKeyWits . L._TxExtraKeyWitnesses) txBody
              -- add the required operators' signatures
              finalTx <- flip execStateT balancedTx $ flip traverse_ (op:operators) $ \o -> do
                Just pkh <- publicKeyCredential <$> operatorReturnOutput o
                when (pkh `Set.member` txInputs || (C.verificationKeyHash . verificationKey $ oPaymentKey o) `Set.member` extraWits) (modify (signTxOperator o))
              void (sendTx finalTx)

largeTransactionTest :: Assertion
largeTransactionTest = do
  let largeDatum :: [Integer] = replicate 10_000 33
      largeDatumTx = execBuildTxWallet Wallet.w1 (payToPlutusV1 Defaults.networkId txInscript largeDatum C.NoStakeAddress mempty)

  -- tx fails with default parameters
  mockchainFails (failOnError largeDatumTx) $ \case
    MockchainValidationFailed (ApplyTxFailure (ApplyTxError [UtxowFailure (UtxoFailure (AlonzoInBabbageUtxoPredFailure (MaxTxSizeUTxO 20311 16384)))])) -> pure ()
    err -> fail $ "Unexpected failure: " <> show err

  -- the tx should succeed after setting the max tx size to exactly 20311 (see the error message in the test above)
  let protParams = Defaults.protocolParameters & maxTxSize .~ 20311
      params' = Defaults.nodeParams & protocolParameters .~ (either (error. show) id (C.bundleProtocolParams C.BabbageEra protParams))
  mockchainSucceedsWith params' (failOnError largeDatumTx)
