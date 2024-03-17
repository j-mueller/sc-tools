{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}
module Main(main) where

import qualified Cardano.Api.Shelley            as C
import           Cardano.Ledger.Shelley.API     (ApplyTxError (..))
import           Control.Lens                   (_3, _4, view, (&), (.~))
import           Control.Monad                  (replicateM, void, when)
import           Control.Monad.Except           (MonadError, runExceptT)
import           Control.Monad.IO.Class         (MonadIO (..))
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
                                                 spendPlutusV1,
                                                 spendPlutusV2Ref)
import qualified Convex.BuildTx                 as BuildTx
import           Convex.Class                   (MonadBlockchain (..),
                                                 MonadMockchain (resolveDatumHash),
                                                 getUtxo, setUtxo)
import           Convex.CoinSelection           (BalanceTxError, keyWitnesses,
                                                 publicKeyCredential)
import qualified Convex.Lenses                  as L
import           Convex.MockChain               (MockchainError (..),
                                                 ValidationError (..),
                                                 fromLedgerUTxO)
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
import qualified Convex.Utxos                   as Utxos
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
import qualified Plutus.V1.Ledger.Api           as PV1
import qualified Scripts
import qualified Test.QuickCheck.Gen            as Gen
import           Test.Tasty                     (TestTree, defaultMain,
                                                 testGroup)
import           Test.Tasty.HUnit               (Assertion, testCase)
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
    , testCase "build a transaction without Ada-only inputs" buildTxMixedInputs
    ]
  , testGroup "scripts"
    [ testCase "paying to a plutus script" (mockchainSucceeds $ failOnError payToPlutusScript)
    , testCase "spending a plutus script output" (mockchainSucceeds $ failOnError (payToPlutusScript >>= spendPlutusScript))
    , testCase "creating a reference script output" (mockchainSucceeds $ failOnError $ putReferenceScript Wallet.w1)
    , testCase "using a reference script" (mockchainSucceeds $ failOnError $  (payToPlutusV2Script >>= spendPlutusScriptReference))
    , testCase "minting a token" (mockchainSucceeds $ failOnError mintingPlutus)
    , testCase "making payments with tokens" (mockchainSucceeds $ failOnError (mintingPlutus >>= spendTokens))
    , testCase "making payments with tokens (2)" (mockchainSucceeds $ failOnError (mintingPlutus >>= spendTokens2))
    , testCase "spending a singleton output" (mockchainSucceeds $ failOnError (mintingPlutus >>= spendSingletonOutput))
    , testCase "spend an output locked by the matching index script" (mockchainSucceeds $ failOnError matchingIndex)
    ]
  , testGroup "mockchain"
    [ testCase "resolveDatumHash" (mockchainSucceeds $ failOnError checkResolveDatumHash)
    , testCase "large transactions" largeTransactionTest
    ]
  ]

spendPublicKeyOutput :: Assertion
spendPublicKeyOutput = mockchainSucceeds $ failOnError (Wallet.w2 `paymentTo` Wallet.w1)

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

putReferenceScript :: (MonadFail m, MonadMockchain m, MonadError BalanceTxError m) => Wallet -> m C.TxIn
putReferenceScript wallet = do
  let hsh = C.hashScript (C.PlutusScript C.PlutusScriptV2 Scripts.v2SpendingScript)
      addr = C.makeShelleyAddressInEra Defaults.networkId (C.PaymentCredentialByScript hsh) C.NoStakeAddress
  let tx = execBuildTx' $
            payToPlutusV2Inline addr Scripts.v2SpendingScript (C.lovelaceToValue 10_000_000)
            >> setMinAdaDepositAll Defaults.ledgerProtocolParameters
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

spendTokens :: (MonadMockchain m, MonadError BalanceTxError m) => C.TxId -> m C.TxId
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
            setMinAdaDepositAll Defaults.ledgerProtocolParameters
  void $ wTo `paymentTo` wFrom
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty wFrom tx

-- | Put all of the Wallet 2's funds into a single UTxO with mixed assets
--   Then make a transaction that splits this output into two
spendSingletonOutput :: (MonadFail m, MonadMockchain m, MonadError BalanceTxError m) => C.TxId -> m ()
spendSingletonOutput txi = do
  void (nativeAssetPaymentTo 49 Wallet.w1 Wallet.w2 >> Wallet.w1 `paymentTo` Wallet.w2)
  utxoSet <- Utxos.fromApiUtxo . fromLedgerUTxO C.ShelleyBasedEraBabbage <$> getUtxo
  let k = Utxos.onlyCredential (Wallet.paymentCredential Wallet.w2) utxoSet
  let totalVal = Utxos.totalBalance k
      newOut = C.TxOut (Wallet.addressInEra Defaults.networkId Wallet.w2) (C.TxOutValue C.MultiAssetInBabbageEra totalVal) C.TxOutDatumNone C.ReferenceScriptNone
      utxoSetMinusW2 = Utxos.removeUtxos (Map.keysSet $ Utxos._utxos k) utxoSet
      utxoSetPlusSingleOutput = utxoSetMinusW2 <> Utxos.singleton (C.TxIn txi $ C.TxIx 1000) (newOut, ())

  setUtxo (C.toLedgerUTxO C.ShelleyBasedEraBabbage $ Utxos.toApiUtxo utxoSetPlusSingleOutput)
  -- check that wallet 2 only
  void $ nativeAssetPaymentTo 49 Wallet.w1 Wallet.w2

nativeAssetPaymentTo :: (MonadBlockchain m, MonadMockchain m, MonadError BalanceTxError m) => C.Quantity -> Wallet -> Wallet -> m C.TxId
nativeAssetPaymentTo q wFrom wTo = do
  let vl = assetValue (C.hashScript $ C.PlutusScript C.PlutusScriptV1 mintingScript) "assetName" q
      tx = execBuildTx' $
            payToAddress (Wallet.addressInEra Defaults.networkId wTo) vl
            >> setMinAdaDepositAll Defaults.ledgerProtocolParameters
  -- create a public key output for the sender to make
  -- sure that the sender has enough Ada in ada-only inputs
  void $ wTo `paymentTo` wFrom
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty wFrom tx

checkResolveDatumHash :: (MonadMockchain m, MonadFail m, MonadError BalanceTxError m) => m ()
checkResolveDatumHash = do
  let addr = Wallet.addressInEra Defaults.networkId Wallet.w1
      assertDatumPresent vl = resolveDatumHash (C.hashScriptData vl) >>= \case
        Nothing -> fail "Expected datum"
        Just x
          | x == vl -> pure ()
          | otherwise -> fail $ "Expected " <> show vl <> ", found " <> show x


  -- 1. resolve an inline datum
  let datum1 = C.ScriptDataConstructor 5 []
      dat = C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra datum1
      txOut = payToAddressTxOut addr mempty
                & L._TxOut . _3 .~ dat

  _ <- execBuildTxWallet Wallet.w1 (prependTxOut txOut)

  assertDatumPresent datum1

  -- 2. resolve a datum that was provided as a "TxOutDatumInTx" (this is what 'payToPlutusV1' does)
  let d2 = (11 :: Integer, 12 :: Integer)
      datum2 = C.fromPlutusData $ PV1.toData d2
  _ <- execBuildTxWallet Wallet.w1 (payToPlutusV1 Defaults.networkId txInscript d2 C.NoStakeAddress mempty)
  assertDatumPresent datum2

  -- 3. resolve a datum that was provided by a redeeming transaction
  let d3 = 500 :: Integer
      datum3 = C.fromPlutusData $ PV1.toData d3
      txo =
        C.TxOut
          (C.makeShelleyAddressInEra Defaults.networkId (C.PaymentCredentialByScript (C.hashScript (C.PlutusScript C.PlutusScriptV1 txInscript))) C.NoStakeAddress)
          (C.TxOutValue C.MultiAssetInBabbageEra mempty)
          (C.TxOutDatumHash C.ScriptDataInBabbageEra (C.hashScriptData datum3))
          C.ReferenceScriptNone
  txId <- execBuildTxWallet Wallet.w1 (prependTxOut txo)
  _ <- execBuildTxWallet Wallet.w1 (spendPlutusV1 (C.TxIn txId (C.TxIx 0)) txInscript d3 ())
  assertDatumPresent datum3


{-| Build a transaction, then balance and sign it with the wallet, then
  submit it to the mockchain.
-}
execBuildTxWallet :: (MonadMockchain m, MonadError BalanceTxError m) => Wallet -> BuildTxT m a -> m C.TxId
execBuildTxWallet wallet action = do
  tx <- execBuildTxT (action >> setMinAdaDepositAll Defaults.ledgerProtocolParameters)
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty wallet (BuildTx.buildTx tx)

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
              let tx = execBuildTx' $ do
                        payToAddress walletAddr $ C.lovelaceToValue nAmount
                        traverse_ addRequiredSignature (fmap (C.verificationKeyHash . verificationKey . oPaymentKey) requiredSignatures)
                        setMinAdaDepositAll Defaults.ledgerProtocolParameters

              -- balance the tx using all of the operators' addressses
              balancedTx <- runExceptT (balancePaymentCredentials mempty (operatorPaymentCredential op) (operatorPaymentCredential <$> operators) Nothing tx) >>= either (fail . show) pure
              txInputs <- let C.Tx (C.TxBody txBody) _ = balancedTx in keyWitnesses txBody
              let (Set.fromList -> extraWits) = let C.Tx (C.TxBody txBody) _ = balancedTx in view (L.txExtraKeyWits . L._TxExtraKeyWitnesses) txBody
              -- add the required operators' signatures
              finalTx <- flip execStateT balancedTx $ flip traverse_ (op:operators) $ \o -> do
                Just pkh <- publicKeyCredential <$> operatorReturnOutput o
                when (pkh `Set.member` txInputs || (C.verificationKeyHash . verificationKey $ oPaymentKey o) `Set.member` extraWits) (modify (signTxOperator o))
              void (sendTx finalTx)

buildTxMixedInputs :: Assertion
buildTxMixedInputs = mockchainSucceeds $ failOnError $ do
  testWallet <- liftIO Wallet.generateWallet
  -- configure the UTxO set to that the new wallet has two outputs, each with 40 native tokens and 10 Ada.
  utxoSet <- Utxos.fromApiUtxo . fromLedgerUTxO C.ShelleyBasedEraBabbage <$> getUtxo
  let utxoVal = assetValue (C.hashScript $ C.PlutusScript C.PlutusScriptV1 mintingScript) "assetName" 40 <> C.lovelaceToValue 10_000_000
      newUTxO = C.TxOut (Wallet.addressInEra Defaults.networkId testWallet) (C.TxOutValue C.MultiAssetInBabbageEra utxoVal) C.TxOutDatumNone C.ReferenceScriptNone
      txi :: C.TxId = "771dfef6ad6f1fc51eb399c07ff89257b06ba9822aec8f83d89012f04eb738f2"
  setUtxo
    $ C.toLedgerUTxO C.ShelleyBasedEraBabbage
    $ Utxos.toApiUtxo
    $ utxoSet
      <> Utxos.singleton (C.TxIn txi $ C.TxIx 1000) (newUTxO, ())
      <> Utxos.singleton (C.TxIn txi $ C.TxIx 1001) (newUTxO, ())

  -- pay 'utxoVal' to wallet 1.
  -- this requires both outputs to be included in the final transaction
  -- so that there is enough Ada for the transaction fees.
  void
    $ balanceAndSubmit mempty testWallet
    $ BuildTx.buildTx
    $ BuildTx.execBuildTx
    $ payToAddress (Wallet.addressInEra Defaults.networkId Wallet.w1) utxoVal


largeTransactionTest :: Assertion
largeTransactionTest = do
  let largeDatum :: [Integer] = replicate 10_000 33
      largeDatumTx = execBuildTxWallet Wallet.w1 (payToPlutusV1 Defaults.networkId txInscript largeDatum C.NoStakeAddress mempty)

  -- tx fails with default parameters
  mockchainFails (failOnError largeDatumTx) $ \case
    MockchainValidationFailed (ApplyTxFailure (ApplyTxError [_])) -> pure ()
    err -> fail $ "Unexpected failure: " <> show err

  -- the tx should succeed after increasing the max. tx size
  let requiredTxSize = 20311 -- see the error message from the test above
      params' = Defaults.nodeParams & protocolParameters . maxTxSize .~ requiredTxSize
  mockchainSucceedsWith params' (failOnError largeDatumTx)

matchingIndex :: (MonadMockchain m, MonadError BalanceTxError m) => m ()
matchingIndex = do
  let txBody = execBuildTx' (payToPlutusV2 Defaults.networkId Scripts.matchingIndexScript () C.NoStakeAddress (C.lovelaceToValue 10_000_000))
      tx     = C.TxIn <$> (C.getTxId . C.getTxBody <$> balanceAndSubmit mempty Wallet.w1 txBody) <*> pure (C.TxIx 0)

  -- create three separate tx outputs that are locked by the matching index script
  inputs <- replicateM 3 tx

  -- Spend the outputs in a single transaction
  void (balanceAndSubmit mempty Wallet.w1 $ execBuildTx' $ traverse_ Scripts.spendMatchingIndex inputs)
