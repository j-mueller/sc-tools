{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}
module Main(main) where

import qualified Cardano.Api.Shelley            as C
import           Control.Lens                   (_3, _4, view, (&), (.~))
import           Control.Monad                  (void, when)
import           Control.Monad.Except           (runExceptT)
import           Control.Monad.State.Strict     (execStateT, modify)
import           Control.Monad.Trans.Class      (MonadTrans (..))
import           Convex.BuildTx                 (BuildTxT, assetValue,
                                                 execBuildTx', execBuildTxT,
                                                 mintPlutusV1, payToAddress,
                                                 payToAddressTxOut,
                                                 payToPlutusV1, payToPlutusV2,
                                                 payToPlutusV2Inline,
                                                 prependTxOut,
                                                 setMinAdaDepositAll,
                                                 spendPlutusV1,
                                                 spendPlutusV2Ref)
import           Convex.Class                   (MonadBlockchain (..),
                                                 MonadMockchain (resolveDatumHash))
import           Convex.CoinSelection           (keyWitnesses,
                                                 publicKeyCredential)
import qualified Convex.Lenses                  as L
import           Convex.MockChain.CoinSelection (balanceAndSubmit,
                                                 payToOperator', paymentTo)
import qualified Convex.MockChain.Defaults      as Defaults
import qualified Convex.MockChain.Gen           as Gen
import           Convex.MockChain.Utils         (mockchainSucceeds,
                                                 runMockchainProp)
import           Convex.Query                   (balancePaymentCredentials)
import           Convex.Wallet                  (Wallet)
import qualified Convex.Wallet                  as Wallet
import qualified Convex.Wallet.MockWallet       as Wallet
import           Convex.Wallet.Operator         (operatorPaymentCredential,
                                                 operatorReturnOutput,
                                                 signTxOperator)
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
    ]
  , testGroup "scripts"
    [ testCase "paying to a plutus script" (mockchainSucceeds payToPlutusScript)
    , testCase "spending a plutus script output" (mockchainSucceeds (payToPlutusScript >>= spendPlutusScript))
    , testCase "creating a reference script output" (mockchainSucceeds $ putReferenceScript Wallet.w1)
    , testCase "using a reference script" (mockchainSucceeds (payToPlutusV2Script >>= spendPlutusScriptReference))
    , testCase "minting a token" (mockchainSucceeds mintingPlutus)
    , testCase "making payments with tokens" (mockchainSucceeds (mintingPlutus >>= spendTokens))
    ]
  , testGroup "mockchain"
    [ testCase "resolveDatumHash" (mockchainSucceeds checkResolveDatumHash)
    ]
  ]

spendPublicKeyOutput :: Assertion
spendPublicKeyOutput = mockchainSucceeds (Wallet.w2 `paymentTo` Wallet.w1)

makeSeveralPayments :: Assertion
makeSeveralPayments = mockchainSucceeds $ do
  void $ Wallet.w1 `paymentTo` Wallet.w2
  void $ Wallet.w2 `paymentTo` Wallet.w1
  void $ Wallet.w3 `paymentTo` Wallet.w1
  void $ Wallet.w1 `paymentTo` Wallet.w2

txInscript :: C.PlutusScript C.PlutusScriptV1
txInscript = C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn

mintingScript :: C.PlutusScript C.PlutusScriptV1
mintingScript = C.examplePlutusScriptAlwaysSucceeds C.WitCtxMint

payToPlutusScript :: (MonadFail m, MonadMockchain m) => m C.TxIn
payToPlutusScript = do
  let tx = execBuildTx' (payToPlutusV1 Defaults.networkId txInscript () C.NoStakeAddress (C.lovelaceToValue 10_000_000))
  i <- C.getTxId . C.getTxBody <$> balanceAndSubmit mempty Wallet.w1 tx
  pure (C.TxIn i (C.TxIx 0))

payToPlutusV2Script :: (MonadFail m, MonadMockchain m) => m C.TxIn
payToPlutusV2Script = do
  let tx = execBuildTx' (payToPlutusV2 Defaults.networkId Scripts.v2SpendingScript () C.NoStakeAddress (C.lovelaceToValue 10_000_000))
  i <- C.getTxId . C.getTxBody <$> balanceAndSubmit mempty Wallet.w1 tx
  pure (C.TxIn i (C.TxIx 0))

spendPlutusScript :: (MonadFail m, MonadMockchain m) => C.TxIn -> m C.TxId
spendPlutusScript ref = do
  let tx = execBuildTx' (spendPlutusV1 ref txInscript () ())
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty Wallet.w1 tx

putReferenceScript :: (MonadFail m, MonadMockchain m) => Wallet -> m C.TxIn
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

spendPlutusScriptReference :: (MonadFail m, MonadMockchain m) =>  C.TxIn -> m C.TxId
spendPlutusScriptReference txIn = do
  refTxIn <- putReferenceScript Wallet.w1
  let tx = execBuildTx' (spendPlutusV2Ref txIn refTxIn (Just $ C.hashScript (C.PlutusScript C.PlutusScriptV2 Scripts.v2SpendingScript)) () ())
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty Wallet.w1 tx

mintingPlutus :: (MonadFail m, MonadMockchain m) => m C.TxId
mintingPlutus = do
  void $ Wallet.w2 `paymentTo` Wallet.w1
  let tx = execBuildTx' (mintPlutusV1 mintingScript () "assetName" 100)
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty Wallet.w1 tx

spendTokens :: (MonadFail m, MonadMockchain m) => C.TxId -> m C.TxId
spendTokens _ = do
  _ <- nativeAssetPaymentTo 49 Wallet.w1 Wallet.w2
  _ <- nativeAssetPaymentTo 51 Wallet.w1 Wallet.w2
  _ <- nativeAssetPaymentTo 100 Wallet.w2 Wallet.w3
  nativeAssetPaymentTo 99 Wallet.w3 Wallet.w1

nativeAssetPaymentTo :: (MonadBlockchain m, MonadMockchain m, MonadFail m) => C.Quantity -> Wallet -> Wallet -> m C.TxId
nativeAssetPaymentTo q wFrom wTo = do
  let vl = assetValue (C.hashScript $ C.PlutusScript C.PlutusScriptV1 mintingScript) "assetName" q
      tx = execBuildTx' $
            payToAddress (Wallet.addressInEra Defaults.networkId wTo) vl
            >> setMinAdaDepositAll Defaults.ledgerProtocolParameters
  -- create a public key output for the sender to make
  -- sure that the sender has enough Ada in ada-only inputs
  void $ wTo `paymentTo` wFrom
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty wFrom tx

checkResolveDatumHash :: (MonadMockchain m, MonadFail m) => m ()
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
execBuildTxWallet :: (MonadMockchain m, MonadFail m) => Wallet -> BuildTxT m a -> m C.TxId
execBuildTxWallet wallet action = do
  tx <- execBuildTxT (action >> setMinAdaDepositAll Defaults.ledgerProtocolParameters)
  C.getTxId . C.getTxBody <$> balanceAndSubmit mempty wallet (tx L.emptyTx)

-- | Balance a transaction using
-- a list of operators
balanceMultiAddress :: Property
balanceMultiAddress = do
  let gen = (,) <$> Gen.operator <*> fmap (take 20) (Gen.listOf Gen.operator)
  QC.forAll gen $ \(op, operators) ->
    QC.forAll (Gen.chooseInteger (5_000_000, 100_000_00 * fromIntegral (1 + length operators))) $ \(C.Lovelace -> nAmount) ->
      classify (null operators) "1 operator"
        $ classify (length operators > 0 && length operators <= 9) "2-10 operators"
        $ classify (length operators > 9) "10+ operators"
        $ runMockchainProp $ lift $ do
            -- send Ada to each operator
            traverse_ (payToOperator' mempty (C.lovelaceToValue $ 2_500_000 + nAmount) Wallet.w2) (op:operators)

            -- send the entire amount back to Wallet.w2
            walletAddr <- Wallet.addressInEra <$> networkId <*> pure Wallet.w2
            let tx = execBuildTx' $ do
                      payToAddress walletAddr $ C.lovelaceToValue nAmount
                      setMinAdaDepositAll Defaults.ledgerProtocolParameters

            -- balance the tx using all of the operators' addressses
            balancedTx <- runExceptT (balancePaymentCredentials mempty (operatorPaymentCredential op) (operatorPaymentCredential <$> operators) Nothing tx) >>= either (fail . show) pure
            txInputs <- let C.Tx (C.TxBody txBody) _ = balancedTx in keyWitnesses txBody
            -- add the required operators' signatures
            finalTx <- flip execStateT balancedTx $ flip traverse_ (op:operators) $ \o -> do
              Just pkh <- publicKeyCredential <$> operatorReturnOutput o
              when (pkh `Set.member` txInputs) (modify (signTxOperator o))
            void (sendTx finalTx)
