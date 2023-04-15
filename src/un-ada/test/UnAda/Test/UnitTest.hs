{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Unit tests for UnAda
-}
module UnAda.Test.UnitTest (
tests
) where

import qualified Cardano.Api.Shelley            as C
import           Control.Lens                   (_2, _Just, at, element, mapped,
                                                 over)
import           Control.Monad                  (void)
import           Convex.BuildTx                 (payToAddress, setMinAdaDeposit)
import           Convex.Class                   (MonadBlockchain (..),
                                                 MonadMockchain)
import           Convex.Lenses                  (emptyTx)
import qualified Convex.Lenses                  as L
import           Convex.MockChain               (Mockchain, MockchainError (..))
import           Convex.MockChain.CoinSelection (balanceAndSubmit, paymentTo)
import qualified Convex.MockChain.Defaults      as Defaults
import           Convex.MockChain.Utils         (mockchainFails,
                                                 mockchainSucceeds)
import           Convex.Wallet                  (Wallet)
import qualified Convex.Wallet                  as Wallet
import qualified Convex.Wallet.MockWallet       as Wallet
import           Data.Function                  ((&))
import           Data.List                      (isPrefixOf)
import           Data.String                    (IsString (..))
import           Plutus.V1.Ledger.Interval      (Extended (..), LowerBound (..),
                                                 interval)
import           Plutus.V1.Ledger.Time          (POSIXTime (..), POSIXTimeRange)
import           PlutusCore.Data                (Data (..))
import qualified PlutusTx
import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.HUnit               (Assertion, testCase,
                                                 testCaseSteps)
import           UnAda.OffChain.Transaction     (burnUnAda, findUnAdaOutputs,
                                                 mintUnAda)
import           UnAda.OffChain.Value           (unLovelaceValue)
import           UnAda.OnChain.Types            (BuiltinData (FiniteExtended, FinitePOSIXTimeRange, ItvlBound, UnAdaStateBuiltin),
                                                 UnAdaState (..))

tests :: TestTree
tests = testGroup "unit tests"
  [ testGroup "Happy path"
    [testCase "mint some un-Ada" canMintUnAda
    , testCase "burn some un-Ada" canBurnUnAda
    ]
  , testGroup "attacks"
    (testAttack <$> [NotEnoughAda, TooMuchAda])
  , testGroup "ToData / FromData"
      [ testCaseSteps "toData UnAdaState" toDataUnAdaState
      , testCaseSteps "builtin pattern UnAdaState" builtinPatternUnAdaState
      , testCaseSteps "builtin pattern Extended.Finite" builtinPatternExtendedFinite
      , testCaseSteps "builtin pattern LowerBound" builtinPatternLowerBound
      , testCaseSteps "builtin pattern POSIXTimeRange" builtinPatternRange
      ]
  ]

testAttack :: Attack -> TestTree
testAttack att =
  testCase (fromString $ attackName att) (mockchainFails (attack att) failWithTxBodyScriptExecutionError)

canMintUnAda :: Assertion
canMintUnAda = mockchainSucceeds mintSomeUnAda

mintSomeUnAda :: (MonadFail m, MonadMockchain m) => m (C.TxIn, (C.TxOut C.CtxTx C.BabbageEra, UnAdaState))
mintSomeUnAda = do
  let tx = emptyTx & mintUnAda Defaults.networkId 1 10_000_000
  _ <- Wallet.w2 `paymentTo` Wallet.w1
  mintingTx <- balanceAndSubmit Wallet.w1 tx
  _ <- unAdaPaymentTo 5_000_000 Wallet.w1 Wallet.w2

  getUnAdaOutput mintingTx

canBurnUnAda :: Assertion
canBurnUnAda = mockchainSucceeds $ do
  (txi, (txo, st)) <- mintSomeUnAda

  let tx' = emptyTx & burnUnAda Defaults.networkId 0 txi txo st 5_000_000
  _ <- Wallet.w3 `paymentTo` Wallet.w1
  _ <- Wallet.w2 `paymentTo` Wallet.w1
  balanceAndSubmit Wallet.w1 tx' >>= getUnAdaOutput

unAdaPaymentTo :: (MonadBlockchain m, MonadMockchain m, MonadFail m) => C.Quantity -> Wallet -> Wallet -> m (C.Tx C.BabbageEra)
unAdaPaymentTo q wFrom wTo = do
  let vl = unLovelaceValue q
      tx = emptyTx
            & payToAddress (Wallet.addressInEra Defaults.networkId wTo) vl
            & over (L.txOuts . mapped) (setMinAdaDeposit Defaults.ledgerProtocolParameters)
  -- create a public key output for the sender to make
  -- sure that the sender has enough Ada in ada-only inputs
  void $ wTo `paymentTo` wFrom
  balanceAndSubmit wFrom tx

{-| Get exactly 1 un-Ada output from the transaction. Fails if there are 0 or more than one
un-Ada outputs.
-}
getUnAdaOutput :: MonadFail m => C.Tx C.BabbageEra -> m (C.TxIn, (C.TxOut C.CtxTx C.BabbageEra, UnAdaState))
getUnAdaOutput tx = case findUnAdaOutputs tx of
  []  -> fail "getUnAdaOutput: Found no outputs locked by the unAda validator, expected 1"
  [k] -> pure k
  xs  -> fail $ "getUnAdaOutput: Found " <> show (length xs) <> " outputs locked by the unAda validator, expected 1"

testState :: UnAdaState
testState = UnAdaState{spendAfter = POSIXTime 1000, mps = "aabbccddeeff"}

toDataUnAdaState :: (String -> IO ()) -> Assertion
toDataUnAdaState step = do
  case PlutusTx.toData testState of
    Constr 0
      [ I 1000
      , B _
      ] -> pure ()
    x -> do
      step (show x)
      fail "unexpected format"

builtinPatternUnAdaState :: (String -> IO ()) -> Assertion
builtinPatternUnAdaState step = do
  case PlutusTx.toBuiltinData testState of
    UnAdaStateBuiltin 1000 _mpsB -> pure ()
    x -> do
      step (show x)
      fail "unexpected format"

builtinPatternExtendedFinite :: (String -> IO ()) -> Assertion
builtinPatternExtendedFinite step = do
  case PlutusTx.toBuiltinData (Finite (POSIXTime 0)) of
    FiniteExtended (PlutusTx.toData -> I 0) -> pure ()
    x -> do
      step (show x)
      fail "unexpected format"

builtinPatternLowerBound :: (String -> IO ()) -> Assertion
builtinPatternLowerBound step = do
  case PlutusTx.toBuiltinData (LowerBound (Finite (POSIXTime 0)) True) of
    ItvlBound (FiniteExtended (PlutusTx.toData -> I 0)) _ -> pure ()
    x -> do
      step (show x)
      fail "unexpected format"

testRange :: POSIXTimeRange
testRange = interval 0 100

builtinPatternRange :: (String -> IO ()) -> Assertion
builtinPatternRange step = do
  case PlutusTx.toBuiltinData testRange of
    FinitePOSIXTimeRange (ItvlBound (FiniteExtended (PlutusTx.toData -> I 0)) _) _ -> pure ()
    x -> do
      step (show x)
      fail "unexpected format"

data Attack =
  NotEnoughAda
  | TooMuchAda

attackName :: Attack -> String
attackName = \case
  NotEnoughAda -> "not enough Ada"
  TooMuchAda   -> "too much Ada"

modTx :: Attack -> C.TxBodyContent v C.BabbageEra -> C.TxBodyContent v C.BabbageEra
modTx = \case
  NotEnoughAda ->
    over (L.txOuts . element 0 . L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId . _Just) (\n -> n - 1_000)
  TooMuchAda ->
    over (L.txOuts . element 0 . L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId . _Just) (\n -> n + 1_000)

attack :: Attack -> Mockchain ()
attack att = do
  let tx = modTx att (mintUnAda Defaults.networkId 1 10_000_000 emptyTx)
  _ <- Wallet.w2 `paymentTo` Wallet.w1
  void (balanceAndSubmit Wallet.w1 tx)

failWithTxBodyScriptExecutionError :: MockchainError -> Assertion
failWithTxBodyScriptExecutionError = \case
  FailWith err
    | "BalancingError (TxBodyScriptExecutionError" `isPrefixOf` err -> pure ()
    | otherwise -> fail ("Wrong error: " <> err)
  MockchainValidationFailed _err ->
    fail "Unexpected MockchainValidationFailed"
