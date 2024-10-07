{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}
module Main(main) where

import qualified Cardano.Api                    as C
import qualified Cardano.Api.Ledger             as C
import qualified Cardano.Api.Shelley            as C
import qualified Cardano.Ledger.Api             as Ledger
import qualified Cardano.Ledger.Conway.Rules    as Rules
import           Cardano.Ledger.Shelley.API     (ApplyTxError (..))
import           Control.Lens                   (_3, _4, view, (&), (.~))
import           Control.Monad                  (replicateM, void, when)
import           Control.Monad.Except           (MonadError, runExceptT)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.State.Strict     (execStateT, modify)
import           Convex.BuildTx                 (BuildTxT, addRequiredSignature,
                                                 assetValue, execBuildTx,
                                                 execBuildTxT, mintPlutus,
                                                 payToAddress,
                                                 payToAddressTxOut,
                                                 payToScriptDatumHash,
                                                 prependTxOut,
                                                 setMinAdaDepositAll,
                                                 spendPlutus, spendPlutusRef)
import qualified Convex.BuildTx                 as BuildTx
import qualified Convex.CardanoApi.Lenses       as L
import           Convex.Class                   (MonadBlockchain (..),
                                                 MonadDatumQuery (queryDatumFromHash),
                                                 MonadMockchain, getUtxo,
                                                 setReward, setUtxo, singleUTxO)
import           Convex.CoinSelection           (BalanceTxError,
                                                 ChangeOutputPosition (TrailingChange),
                                                 keyWitnesses,
                                                 publicKeyCredential)
import           Convex.MockChain               (ValidationError (..),
                                                 failedTransactions,
                                                 fromLedgerUTxO,
                                                 runMockchain0IOWith)
import           Convex.MockChain.CoinSelection (balanceAndSubmit,
                                                 payToOperator', paymentTo,
                                                 tryBalanceAndSubmit)
import qualified Convex.MockChain.Defaults      as Defaults
import qualified Convex.MockChain.Gen           as Gen
import           Convex.MockChain.Staking       (registerPool)
import           Convex.MockChain.Utils         (mockchainSucceeds,
                                                 runMockchainPropErr)
import           Convex.NodeParams              (ledgerProtocolParameters,
                                                 protocolParameters)
import           Convex.Query                   (balancePaymentCredentials)
import           Convex.Utils                   (failOnError, inBabbage,
                                                 inConway)
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
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import qualified PlutusLedgerApi.V2             as PV2
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
    , testCase "spending a plutus script (V2) output" (mockchainSucceeds $ failOnError (payToPlutusV2Script >>= spendPlutusV2Script))
    , testCase "creating a reference script output" (mockchainSucceeds $ failOnError $ putReferenceScript Wallet.w1)
    , testCase "using a reference script" (mockchainSucceeds $ failOnError (payToPlutusV2Script >>= spendPlutusScriptReference))
    , testCase "minting a token" (mockchainSucceeds $ failOnError mintingPlutus)
    , testCase "making payments with tokens" (mockchainSucceeds $ failOnError (mintingPlutus >>= spendTokens))
    , testCase "making payments with tokens (2)" (mockchainSucceeds $ failOnError (mintingPlutus >>= spendTokens2))
    , testCase "spending a singleton output" (mockchainSucceeds $ failOnError (mintingPlutus >>= spendSingletonOutput))
    , testCase "spend an output locked by the matching index script" (mockchainSucceeds $ failOnError matchingIndex)
    , testCase "mint a token with the matching index minting policy" (mockchainSucceeds $ failOnError matchingIndexMP)
    ]
  , testGroup "mockchain"
    [ testCase "queryDatumFromHash" (mockchainSucceeds $ failOnError checkResolveDatumHash)
    , testCase "large transactions" largeTransactionTest
    ]
  , testGroup "staking"
    [ testCase "register a staking credential" (mockchainSucceeds $ failOnError registerStakingCredential)
    , testCase "zero withdrawal" (mockchainSucceeds $ failOnError $ registerStakingCredential >> withdrawZero)
    , testCase "register a stake pool" (mockchainSucceeds $ failOnError $ registerPool Wallet.w1)
    , testCase "query stake addresses" (mockchainSucceeds $ failOnError queryStakeAddressesTest)
    , testCase "withdrawal" (mockchainSucceeds $ failOnError withdrawalTest)
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

plutusScript :: C.IsPlutusScriptLanguage lang => C.PlutusScript lang -> C.Script lang
plutusScript = C.PlutusScript C.plutusScriptVersion

payToPlutusScript :: forall era m. (MonadFail m, MonadError (BalanceTxError era) m, MonadMockchain era m, C.IsBabbageBasedEra era) => m C.TxIn
payToPlutusScript = inBabbage @era $ do
  let tx = execBuildTx (payToScriptDatumHash Defaults.networkId (plutusScript txInscript) () C.NoStakeAddress (C.lovelaceToValue 10_000_000))
  i <- C.getTxId . C.getTxBody <$> tryBalanceAndSubmit mempty Wallet.w1 tx TrailingChange []
  pure (C.TxIn i (C.TxIx 0))

payToPlutusV2Script :: forall era m. (MonadFail m, MonadMockchain era m, MonadError (BalanceTxError era) m, C.IsBabbageBasedEra era) => m C.TxIn
payToPlutusV2Script = inBabbage @era $ do
  let tx = execBuildTx (payToScriptDatumHash Defaults.networkId (plutusScript Scripts.v2SpendingScript) () C.NoStakeAddress (C.lovelaceToValue 10_000_000))
  i <- C.getTxId . C.getTxBody <$> tryBalanceAndSubmit mempty Wallet.w1 tx TrailingChange []
  pure (C.TxIn i (C.TxIx 0))

spendPlutusScript :: forall era m. (MonadFail m, MonadMockchain era m, MonadError (BalanceTxError era) m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV1 era) => C.TxIn -> m C.TxId
spendPlutusScript ref = inBabbage @era $ do
  let tx = execBuildTx (spendPlutus ref txInscript () ())
  C.getTxId . C.getTxBody <$> tryBalanceAndSubmit mempty Wallet.w1 tx TrailingChange []

spendPlutusV2Script :: forall era m. (MonadFail m, MonadMockchain era m, MonadError (BalanceTxError era) m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV2 era) => C.TxIn -> m C.TxId
spendPlutusV2Script ref = inBabbage @era $ do
  let tx = execBuildTx (spendPlutus ref Scripts.v2SpendingScript () ())
  C.getTxId . C.getTxBody <$> tryBalanceAndSubmit mempty Wallet.w1 tx TrailingChange []

putReferenceScript :: forall m. (MonadFail m, MonadMockchain C.ConwayEra m, MonadError (BalanceTxError C.ConwayEra) m) => Wallet -> m C.TxIn
putReferenceScript wallet = do
  let addr = Wallet.addressInEra Defaults.networkId wallet
      tx = execBuildTx $
            BuildTx.createRefScriptDatumHash addr (plutusScript Scripts.v2SpendingScript) () (C.lovelaceToValue 10_000_000)
            >> setMinAdaDepositAll Defaults.bundledProtocolParameters
  txId <- C.getTxId . C.getTxBody <$> tryBalanceAndSubmit mempty wallet tx TrailingChange []
  let outRef = C.TxIn txId (C.TxIx 0)
  C.UTxO utxo <- utxoByTxIn (Set.singleton outRef)
  case Map.lookup outRef utxo of
    Nothing  -> fail "Utxo not found"
    Just out -> case view (L._TxOut . _4) out of
      C.ReferenceScript{} -> pure ()
      _                   -> fail "No reference script found"
  pure outRef

spendPlutusScriptReference :: (MonadFail m, MonadMockchain C.ConwayEra m, MonadError (BalanceTxError C.ConwayEra) m) =>  C.TxIn -> m C.TxId
spendPlutusScriptReference txIn = do
  refTxIn <- putReferenceScript Wallet.w1
  let tx = execBuildTx (spendPlutusRef txIn refTxIn C.PlutusScriptV2 () ())
  C.getTxId . C.getTxBody <$> tryBalanceAndSubmit mempty Wallet.w1 tx TrailingChange []

mintingPlutus :: forall era m. (MonadFail m, MonadMockchain era m, MonadError (BalanceTxError era) m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV1 era) => m C.TxId
mintingPlutus = inBabbage @era $ do
  void $ Wallet.w2 `paymentTo` Wallet.w1
  let tx = execBuildTx (mintPlutus mintingScript () "assetName" 100)
  C.getTxId . C.getTxBody <$> tryBalanceAndSubmit mempty Wallet.w1 tx TrailingChange []

spendTokens :: (MonadFail m, MonadMockchain C.ConwayEra m, MonadError (BalanceTxError C.ConwayEra) m) => C.TxId -> m C.TxId
spendTokens _ = do
  _ <- nativeAssetPaymentTo 49 Wallet.w1 Wallet.w2
  _ <- nativeAssetPaymentTo 51 Wallet.w1 Wallet.w2
  _ <- nativeAssetPaymentTo 100 Wallet.w2 Wallet.w3
  nativeAssetPaymentTo 99 Wallet.w3 Wallet.w1

spendTokens2 :: (MonadFail m, MonadMockchain C.ConwayEra m, MonadError (BalanceTxError C.ConwayEra) m) => C.TxId -> m C.TxId
spendTokens2 txi = do
  let q  = 98
      wTo = Wallet.w2
      wFrom = Wallet.w1
      vl = assetValue (C.hashScript $ C.PlutusScript C.PlutusScriptV1 mintingScript) "assetName" q
      tx = execBuildTx $ do
            payToAddress (Wallet.addressInEra Defaults.networkId wTo) vl
            BuildTx.spendPublicKeyOutput (C.TxIn txi (C.TxIx 0))
            mintPlutus mintingScript () "assetName" (-2)
            setMinAdaDepositAll Defaults.bundledProtocolParameters
  void $ wTo `paymentTo` wFrom
  C.getTxId . C.getTxBody <$> tryBalanceAndSubmit mempty wFrom tx TrailingChange []

-- | Put all of the Wallet 2's funds into a single UTxO with mixed assets
--   Then make a transaction that splits this output into two
spendSingletonOutput :: (MonadFail m, MonadMockchain C.ConwayEra m, MonadError (BalanceTxError C.ConwayEra) m) => C.TxId -> m ()
spendSingletonOutput txi = do
  void (nativeAssetPaymentTo 49 Wallet.w1 Wallet.w2 >> Wallet.w1 `paymentTo` Wallet.w2)
  utxoSet <- Utxos.fromApiUtxo () . fromLedgerUTxO C.shelleyBasedEra <$> getUtxo
  let k = Utxos.onlyCredential (Wallet.paymentCredential Wallet.w2) utxoSet
  let totalVal = Utxos.totalBalance k
      newOut = C.TxOut (Wallet.addressInEra Defaults.networkId Wallet.w2) (C.TxOutValueShelleyBased C.ShelleyBasedEraConway $ C.toMaryValue totalVal) C.TxOutDatumNone C.ReferenceScriptNone
      utxoSetMinusW2 = Utxos.removeUtxos (Map.keysSet $ Utxos._utxos k) utxoSet
      utxoSetPlusSingleOutput = utxoSetMinusW2 <> Utxos.singleton (C.TxIn txi $ C.TxIx 1000) (newOut, ())

  setUtxo (C.toLedgerUTxO C.shelleyBasedEra $ Utxos.toApiUtxo utxoSetPlusSingleOutput)
  -- check that wallet 2 only
  void $ nativeAssetPaymentTo 49 Wallet.w1 Wallet.w2

nativeAssetPaymentTo :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m) => C.Quantity -> Wallet -> Wallet -> m C.TxId
nativeAssetPaymentTo q wFrom wTo = do
  let vl = assetValue (C.hashScript $ C.PlutusScript C.PlutusScriptV1 mintingScript) "assetName" q
      tx = execBuildTx $
            payToAddress (Wallet.addressInEra Defaults.networkId wTo) vl
            >> setMinAdaDepositAll Defaults.bundledProtocolParameters
  -- create a public key output for the sender to make
  -- sure that the sender has enough Ada in ada-only inputs
  void $ wTo `paymentTo` wFrom
  C.getTxId . C.getTxBody <$> tryBalanceAndSubmit mempty wFrom tx TrailingChange []

checkResolveDatumHash :: (MonadMockchain C.ConwayEra m, MonadDatumQuery m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m) => m ()
checkResolveDatumHash = do
  let addr = Wallet.addressInEra Defaults.networkId Wallet.w1
      assertDatumPresent vl = queryDatumFromHash (C.hashScriptDataBytes vl) >>= \case
        Nothing -> fail "Expected datum"
        Just x
          | x == vl -> pure ()
          | otherwise -> fail $ "Expected " <> show (C.getScriptData vl) <> ", found " <> show x


  -- 1. resolve an inline datum
  let datum1 = C.unsafeHashableScriptData  (C.ScriptDataConstructor 5 [])
      dat = C.TxOutDatumInline C.babbageBasedEra datum1
      txOut = payToAddressTxOut addr mempty
                & L._TxOut . _3 .~ dat

  _ <- tryExecBuildTxWallet Wallet.w1 (prependTxOut txOut)

  assertDatumPresent datum1

  -- 2. resolve a datum that was provided as a "TxOutDatumInTx" (this is what 'payToScriptDatumHash' does)
  let d2 = (11 :: Integer, 12 :: Integer)
      datum2 = C.unsafeHashableScriptData $ C.fromPlutusData $ PV2.toData d2
  _ <- tryExecBuildTxWallet Wallet.w1 (payToScriptDatumHash Defaults.networkId (plutusScript txInscript) d2 C.NoStakeAddress mempty)
  assertDatumPresent datum2

  -- 3. resolve a datum that was provided by a redeeming transaction
  let d3 = 500 :: Integer
      datum3 = C.unsafeHashableScriptData $ C.fromPlutusData $ PV2.toData d3
      txo =
        C.TxOut
          (C.makeShelleyAddressInEra C.shelleyBasedEra Defaults.networkId (C.PaymentCredentialByScript (C.hashScript (C.PlutusScript C.PlutusScriptV1 txInscript))) C.NoStakeAddress)
          (C.TxOutValueShelleyBased C.shelleyBasedEra mempty)
          (C.TxOutDatumHash (C.babbageEraOnwardsToAlonzoEraOnwards C.babbageBasedEra) (C.hashScriptDataBytes datum3))
          C.ReferenceScriptNone
  txId <- tryExecBuildTxWallet Wallet.w1 (prependTxOut txo)
  _ <- tryExecBuildTxWallet Wallet.w1 (spendPlutus (C.TxIn txId (C.TxIx 0)) txInscript d3 ())
  assertDatumPresent datum3

{-| Build a transaction, then balance and sign it with the wallet, then
  submit it to the mockchain.
-}
execBuildTxWallet :: (MonadMockchain C.ConwayEra m, MonadError (BalanceTxError C.ConwayEra) m) => Wallet -> BuildTxT C.ConwayEra m a -> m (Either (ValidationError C.ConwayEra) C.TxId)
execBuildTxWallet wallet action = do
  tx <- execBuildTxT (action >> setMinAdaDepositAll Defaults.bundledProtocolParameters)
  fmap (C.getTxId . C.getTxBody) <$> balanceAndSubmit mempty wallet tx TrailingChange []

{-| Build a transaction, then balance and sign it with the wallet, then
  submit it to the mockchain. Fail if 'balanceAndSubmit' is not successful.
-}
tryExecBuildTxWallet :: (MonadMockchain C.ConwayEra m, MonadError (BalanceTxError C.ConwayEra) m, MonadFail m) => Wallet -> BuildTxT C.ConwayEra m a -> m C.TxId
tryExecBuildTxWallet wallet action = execBuildTxWallet wallet action >>= either (fail . show) pure

-- | Balance a transaction using a list of operators
--   Check that the fees are calculated correctly to spend outputs from different addresses
--   and to consider the required signatures
balanceMultiAddress :: Property
balanceMultiAddress = do
  let gen = (,) <$> Gen.operator <*> fmap (take 20) (Gen.listOf Gen.operator)
  QC.forAll gen $ \(op, operators) ->
    QC.forAll (Gen.chooseInteger (7_500_000, 100_000_00 * fromIntegral (1 + length operators))) $ \(C.Quantity -> nAmount) ->
      QC.forAll (Gen.sublistOf (op:operators)) $ \requiredSignatures ->
        classify (null operators) "1 operator"
          $ classify (not (null operators) && length operators <= 9) "2-9 operators"
          $ classify (length operators > 9) "10+ operators"
          $ classify (null requiredSignatures) "0 required signatures"
          $ classify (not (null requiredSignatures) && length requiredSignatures <= 9) "1-9 required signatures"
          $ classify (length requiredSignatures > 9) "10+ required signatures"
          $ runMockchainPropErr @(BalanceTxError C.ConwayEra) $ do
              -- send Ada to each operator
              traverse_ (payToOperator' mempty (C.lovelaceToValue $ 7_500_000 + C.quantityToLovelace nAmount) Wallet.w2) (op:operators)

              -- send the entire amount back to Wallet.w2
              walletAddr <- Wallet.addressInEra <$> queryNetworkId <*> pure Wallet.w2
              protParams <- queryProtocolParameters
              let tx = execBuildTx $ do
                        payToAddress walletAddr $ C.lovelaceToValue $ C.quantityToLovelace nAmount
                        traverse_ addRequiredSignature (fmap (C.verificationKeyHash . verificationKey . oPaymentKey) requiredSignatures)
                        setMinAdaDepositAll protParams

              -- balance the tx using all of the operators' addressses
              balancedTx <- runExceptT (balancePaymentCredentials mempty (operatorPaymentCredential op) (operatorPaymentCredential <$> operators) Nothing tx TrailingChange) >>= either (fail . show) pure
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
  utxoSet <- Utxos.fromApiUtxo () . fromLedgerUTxO C.ShelleyBasedEraConway <$> getUtxo
  let utxoVal = assetValue (C.hashScript $ C.PlutusScript C.PlutusScriptV1 mintingScript) "assetName" 40 <> C.lovelaceToValue 10_000_000
      newUTxO = C.TxOut (Wallet.addressInEra Defaults.networkId testWallet) (C.TxOutValueShelleyBased C.ShelleyBasedEraConway $ C.toMaryValue utxoVal) C.TxOutDatumNone C.ReferenceScriptNone
      txi :: C.TxId = "771dfef6ad6f1fc51eb399c07ff89257b06ba9822aec8f83d89012f04eb738f2"
  setUtxo
    $ C.toLedgerUTxO C.ShelleyBasedEraConway
    $ Utxos.toApiUtxo
    $ utxoSet
      <> Utxos.singleton (C.TxIn txi $ C.TxIx 1000) (newUTxO, ())
      <> Utxos.singleton (C.TxIn txi $ C.TxIx 1001) (newUTxO, ())

  -- pay 'utxoVal' to wallet 1.
  -- this requires both outputs to be included in the final transaction
  -- so that there is enough Ada for the transaction fees.
  void
    $ balanceAndSubmit mempty testWallet
      (BuildTx.execBuildTx (payToAddress (Wallet.addressInEra Defaults.networkId Wallet.w1) utxoVal)) TrailingChange []


largeTransactionTest :: Assertion
largeTransactionTest = do
  let largeDatum :: [Integer] = replicate 10_000 33
      largeDatumTx = execBuildTxWallet Wallet.w1 (payToScriptDatumHash Defaults.networkId (plutusScript txInscript) largeDatum C.NoStakeAddress mempty)

  -- tx fails with default parameters
  runMockchain0IOWith Wallet.initialUTxOs Defaults.nodeParams (failOnError largeDatumTx) >>= \case
    (_, view failedTransactions -> [(_, err)]) -> case err of
      ApplyTxFailure (ApplyTxError (Rules.ConwayUtxowFailure (Rules.UtxoFailure (Rules.MaxTxSizeUTxO 20313 16384)):|[])) -> pure ()
      _ -> fail $ "Unexpected failure. Expected 'MaxTxSizeUTxO', found " <> show err
    (_, length . view failedTransactions -> numFailed) -> fail $ "Expected one failed transaction, found " <> show numFailed

  -- the tx should succeed after setting the max tx size to exactly 20304 (see the error message in the test above)
  let params' = Defaults.nodeParams & ledgerProtocolParameters . protocolParameters . Ledger.ppMaxTxSizeL .~ 20313
  runMockchain0IOWith Wallet.initialUTxOs params' (failOnError largeDatumTx) >>= \case
    (Right{}, view failedTransactions -> []) -> pure ()
    (_, length . view failedTransactions -> numFailed) -> fail $ "Expected success with 0 failed transactions, found " <> show numFailed

matchingIndex :: forall era m. (MonadMockchain era m, MonadError (BalanceTxError era) m, MonadFail m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era) => m ()
matchingIndex = inBabbage @era $ do
  let txBody = execBuildTx (BuildTx.payToScriptDatumHash Defaults.networkId (plutusScript Scripts.matchingIndexValidatorScript) () C.NoStakeAddress (C.lovelaceToValue 10_000_000))
      tx     = C.TxIn . C.getTxId . C.getTxBody <$> tryBalanceAndSubmit mempty Wallet.w1 txBody TrailingChange [] <*> pure (C.TxIx 0)

  -- create three separate tx outputs that are locked by the matching index script
  inputs <- replicateM 3 tx

  -- Spend the outputs in a single transaction
  void (tryBalanceAndSubmit mempty Wallet.w1 (execBuildTx $ traverse_ Scripts.spendMatchingIndex inputs) TrailingChange [])

stakingCredential :: C.StakeCredential
stakingCredential = C.StakeCredentialByScript $ C.hashScript (C.PlutusScript C.PlutusScriptV2 Scripts.v2StakingScript)

registerStakingCredential :: forall era m. (MonadMockchain era m, MonadError (BalanceTxError era) m, MonadFail m, C.IsConwayBasedEra era) => m C.TxIn
registerStakingCredential = inConway @era $ do
  let txBody = execBuildTx (BuildTx.addStakeCredentialCertificate stakingCredential)
  C.TxIn . C.getTxId . C.getTxBody <$> tryBalanceAndSubmit mempty Wallet.w1 txBody TrailingChange [] <*> pure (C.TxIx 0)

withdrawZero :: forall era m. (MonadIO m, MonadMockchain era m, MonadError (BalanceTxError era) m, MonadFail m, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV2 era) => m ()
withdrawZero = inBabbage @era $ do
  txBody <- execBuildTxT (BuildTx.addWithdrawZeroPlutusV2InTransaction Defaults.networkId Scripts.v2StakingScript ())
  txI <- C.TxIn . C.getTxId . C.getTxBody <$> tryBalanceAndSubmit mempty Wallet.w1 txBody TrailingChange [] <*> pure (C.TxIx 0)
  singleUTxO txI >>= \case
    Nothing -> fail "txI not found"
    Just{} -> pure ()

matchingIndexMP :: forall m. (MonadMockchain C.ConwayEra m, MonadError (BalanceTxError C.ConwayEra) m, MonadFail m) => m ()
matchingIndexMP = do
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV3 Scripts.matchingIndexMPScript)
      policyId = C.PolicyId sh
      runTx assetName = Scripts.mintMatchingIndex policyId assetName 100
  void $ tryBalanceAndSubmit mempty Wallet.w1 (execBuildTx $ traverse_ runTx ["assetName1", "assetName2", "assetName3"]) TrailingChange []

queryStakeAddressesTest :: forall m. (MonadIO m, MonadMockchain C.ConwayEra m, MonadError (BalanceTxError C.ConwayEra) m, MonadFail m) => m ()
queryStakeAddressesTest = do
  poolId <- registerPool Wallet.w1
  stakeKey <- C.generateSigningKey C.AsStakeKey
  let
    withdrawalAmount = 10_000_000

    stakeHash =
      C.verificationKeyHash . C.getVerificationKey $ stakeKey

    stakeCred = C.StakeCredentialByKey stakeHash

    stakeCert =
      C.makeStakeAddressRegistrationCertificate
      . C.StakeAddrRegistrationConway C.ConwayEraOnwardsConway 0
      $ stakeCred

    delegationCert =
      C.makeStakeAddressDelegationCertificate
      $ C.StakeDelegationRequirementsConwayOnwards C.ConwayEraOnwardsConway stakeCred (C.DelegStake $ C.unStakePoolKeyHash poolId)

    stakeCertTx = BuildTx.execBuildTx $ do
      BuildTx.addCertificate stakeCert

    delegCertTx = BuildTx.execBuildTx $ do
      BuildTx.addCertificate delegationCert

  -- activate stake
  void $ tryBalanceAndSubmit mempty Wallet.w2 stakeCertTx TrailingChange [C.WitnessStakeKey stakeKey]
  -- delegate to pool
  void $ tryBalanceAndSubmit mempty Wallet.w2 delegCertTx TrailingChange [C.WitnessStakeKey stakeKey]

  -- modify the ledger state
  setReward stakeCred (C.quantityToLovelace withdrawalAmount)

  (rewards, delegations) <- queryStakeAddresses (Set.fromList [stakeCred]) Defaults.networkId

  when (length rewards /= 1) $ fail "Expected 1 reward"
  when (length delegations /= 1) $ fail "Expected 1 delegation"

withdrawalTest :: forall m. (MonadIO m, MonadMockchain C.ConwayEra m, MonadError (BalanceTxError C.ConwayEra) m, MonadFail m) => m ()
withdrawalTest = do
  poolId <- registerPool Wallet.w1
  stakeKey <- C.generateSigningKey C.AsStakeKey
  let
    withdrawalAmount = 10_000_000

    stakeHash =
      C.verificationKeyHash . C.getVerificationKey $ stakeKey

    stakeCred = C.StakeCredentialByKey stakeHash

    stakeCert =
      C.makeStakeAddressRegistrationCertificate
      . C.StakeAddrRegistrationConway C.ConwayEraOnwardsConway 0
      $ stakeCred
    stakeAddress = C.makeStakeAddress Defaults.networkId stakeCred

    delegationCert =
      C.makeStakeAddressDelegationCertificate
      $ C.StakeDelegationRequirementsConwayOnwards C.ConwayEraOnwardsConway stakeCred (C.DelegStake $ C.unStakePoolKeyHash poolId)

    stakeCertTx = BuildTx.execBuildTx $ do
      BuildTx.addCertificate stakeCert

    delegCertTx = BuildTx.execBuildTx $ do
      BuildTx.addCertificate delegationCert

    withdrawalTx = execBuildTx $ do
      BuildTx.addWithdrawal stakeAddress withdrawalAmount (C.KeyWitness C.KeyWitnessForStakeAddr)

  -- activate stake
  void $ tryBalanceAndSubmit mempty Wallet.w2 stakeCertTx TrailingChange [C.WitnessStakeKey stakeKey]
  -- delegate to pool
  void $ tryBalanceAndSubmit mempty Wallet.w2 delegCertTx TrailingChange [C.WitnessStakeKey stakeKey]

  -- modify the ledger state
  setReward stakeCred (C.quantityToLovelace withdrawalAmount)

  -- withdraw rewards
  void $ tryBalanceAndSubmit mempty Wallet.w2 withdrawalTx TrailingChange [C.WitnessStakeKey stakeKey]
