{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{- | Module: Cardano.Djed.Test.Utils.Cases
Description: Construct nominal and attack test cases for unit tests

This module exposes a flexible way to generate test cases that go
through a pre-processing -> pre-condition -> execution -> post-condition
pipeline.

The intention is to provide an easier interface to re-using predicates and pre-processors
between many tests and between different _kinds_ of tests, including nominal and attack
cases, as well as unit and property testing.

This module is currently targeted primarily at tests run on the CEK machine.
In the future, it may be expanded to be polymorphic in terms of arbitrary
"computation". The current computation type is essentially hard-coded to

  (Maybe Datum, Redeemer, ScriptContext, Script) -> (Either EvalError Script, ExBudget, Logs)

but this could be expanded so that other types of tests (emulator, integration, serialization,
golden) could be run as well.
-}
module Convex.TestUtils (
  -- * Polymorphic Types and Functions

  -- ** PreProcessing
  PreProcessor,

  -- *** Construction
  mkPreProcessor,

  -- *** Elimination
  preProcess,

  -- ** PreCondition Checking
  PreCondition,

  -- *** Construction
  mkPreCondition,

  -- *** Elimination
  checkPreConditions,

  -- ** PostCondition Checking
  PostCondition,

  -- *** Construction
  mkPostCondition,

  -- *** Elimination
  checkPostConditions,

  -- ** Test Cases
  PipelinedTestCase (..),
  PipelinedTestErrors (..),
  pipelinedUnitCase,

  -- * TxFCEKMachinery

  -- ** Generic TxF TestCase
  TxFCEKCase,
  TxFCEKInput (..),
  TxFCEKOutput (..),
  mkTxFCEKCase,
  txfCEKUnitCase,

  -- *** Nominal Tests
  nominalPostCondition,
  nominalCaseBasic,

  -- *** Attack Tests
  attackCaseRegexPostCondition,
  attackCaseBasicRegex,
) where

import Control.Arrow (Kleisli (Kleisli), runKleisli, (>>>))
import Data.Foldable (fold, foldl')
import Data.List (intercalate)
import Data.Monoid (First (First), getFirst)
import Data.Text (Text)
import Plutarch (Script)
import Plutarch.Evaluate (EvalError, evalScriptHuge)
import Plutarch.Extra.Script (applyArguments)
import PlutusCore.Data qualified as PLC
import PlutusLedgerApi.V2 (Datum, ExBudget, Redeemer, ScriptContext, toData)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.RE.TDFA.Text (RE, countMatches, reSource, (*=~))
import Data.Kind (Type)
--------------------------------------------------------------------------------

{- | PreProcessing -- could be used for normalization or attacks
TODO: rewwrite as Kleisli?
-}
newtype PreProcessor err input
  = PreProcessor
  { preProcessor ::
      Kleisli
        (Either err)
        input
        input
  }

mkPreProcessor :: (input -> Either err input) -> PreProcessor err input
mkPreProcessor = PreProcessor . Kleisli

{- | Given a list of preprocessors, apply them in order from left to right.
Returns the first error encountered, if any.
-}
preProcess ::
  [PreProcessor err input] ->
  input ->
  Either err input
preProcess pps = runKleisli $ foldl' (>>>) (Kleisli pure) (fmap preProcessor pps)

--------------------------------------------------------------------------------

{- | Checks for after the preprocessing is run. Could be used to check for
balancing, etc

TODO: rewrite as Kleisli?
-}
data PreCondition err input
  = PreCondition
  { preCondition :: input -> First err
  }

mkPreCondition :: (input -> Maybe err) -> PreCondition err input
mkPreCondition f = PreCondition (First . f)

-- | Check a list of pre-conditions, returning the first error encountered
checkPreConditions ::
  [PreCondition err input] ->
  input ->
  Maybe err
checkPreConditions pcs args =
  getFirst (foldMap ((\x -> (x $ args)) . preCondition) pcs)

--------------------------------------------------------------------------------

{- | Represents a paritial post-condition predicate on the result of
a call to `evalScript :: Script -> (Either EvalError Script, ExBudget, [Text]) `
TOOO: Rewrite as Kleisli?

TODO: If its possible to turn budgeting off, we should. These scripts
will be run with some logging output, and they wouldn't in production; thus
costing is not actually representative of anything useful.
-}
data PostCondition err output = PostCondition
  {postCondition :: output -> First err}

mkPostCondition :: (output -> Maybe err) -> PostCondition err output
mkPostCondition f = PostCondition (First . f)

{- | Check a list of post-conditions from left to right, returning the first
error encountered
-}
checkPostConditions ::
  [PostCondition err output] ->
  output ->
  Maybe err
checkPostConditions pcs args =
  getFirst (foldMap ((\x -> (x $ args)) . postCondition) pcs)

--------------------------------------------------------------------------------

{- | A type representing a named test case for a transaction family.
NOTE: if the `sc-tools` work ends up panning out, the `args` and `script` arguments
might get combined to a (Maybe Datum, Redeemer, ScriptContext, Script)
-}
data PipelinedTestCase errPP errPreC errPostC input output = PipelinedTestCase
  { name :: String
  -- ^ Name of the test case
  , preProcessors :: [PreProcessor errPP input]
  -- ^ Pre-processors (normalizers or attacks); applied in order from left to right
  , preConditions :: [PreCondition errPreC input]
  -- ^ Pre-condition Checking. Run after pre-processing
  , input :: input
  -- ^ the input to the computation
  , computation :: input -> output
  -- ^ the computation to run
  , postConditions :: [PostCondition errPostC output]
  -- ^ Post conditions are checked after script execution
  }

-- | A sum type to collect the errors of a pipelined test case
data PipelinedTestErrors errPP errPreC errPostC
  = PipelinedPreProcessorError errPP
  | PipelinedPreConditionError errPreC
  | PipelinedPostConditionError errPostC

{- | Behavior of this function:

- 1.) All pre-processors are run in order from left to right on the arguments.
  If any pre-processor returns a Left, then the resulting test case fails with
  the appropriate error message.
- 2.) All pre-condition checks are run on the pre-processed arguments.
  If any pre-condition checks return Nothing, then the resulting test case fails
  with the appropriate error meesage.
- 3.) The pre-processed arguments are fed to the script.
- 4.) The results of (3) are checked against each post-condition.
  If any of the post-condition checks fail, the test case fails with the appropriate
  error message
- 5.) If none of the above checks cause a failure, the test succeeds.
-}
pipelinedUnitCase ::
  (Show errPP, Show errPreC, Show errPostC) =>
  PipelinedTestCase errPP errPreC errPostC input output ->
  TestTree
pipelinedUnitCase
  PipelinedTestCase
    { name = name'
    , preProcessors = preProcessors'
    , preConditions = preConditions'
    , input = input'
    , computation = computation'
    , postConditions = postConditions'
    } =
    testCase name' $ do
      -- Run preprocessing
      case preProcess preProcessors' input' of
        Left err -> assertFailure $ show err
        Right preProcessedArgs ->
          -- Run pre-condition  checks
          case checkPreConditions preConditions' preProcessedArgs of
            Just err' -> assertFailure $ show err'
            Nothing ->
              -- Run computation
              let
                evaluationResult =
                  computation' preProcessedArgs
               in
                -- Run post condition checks
                case checkPostConditions postConditions' evaluationResult of
                  Just err'' -> assertFailure $ show err''
                  Nothing -> pure ()

--------------------------------------------------------------------------------

-- * TxF CEK Machinery

{- | The arguments needed to run a YTxP-style transaction family (single script)
on the CEK Machine. Includes a datum (for validators only), a redeemer, script context,
and the script itself.
-}
data TxFCEKInput = TxFCEKInput
  { cekDatum :: Maybe Datum
  , cekRedeemer :: Redeemer
  , cekScriptContext :: ScriptContext
  , cekScript :: Script
  }
  deriving stock (Eq, Show)

-- | The data produced by the CEK machine when run against a TxFCEKInput.
data TxFCEKOutput = TxFCEKOutput
  { cekResult :: Either EvalError Script
  , cekExBudget :: ExBudget
  , cekLogs :: [Text]
  }
  deriving stock (Eq, Show)

-- | A sum type of the errors for a TxFCEKCase
newtype TxFCEKCase errPP errPreC errPostC
  = TxFCEKCase
      ( PipelinedTestCase
          errPP
          errPreC
          errPostC
          TxFCEKInput
          TxFCEKOutput
      )

-- | Create a TxFCEKCase by filling in the computation via running the script.
mkTxFCEKCase ::
  forall (errPP :: Type) (errPreC :: Type) (errPostC :: Type).
  (Show errPP, Show errPreC, Show errPostC) =>
  -- | Name of the test case
  String ->
  -- | List of pre-processors over something like (Maybe Datum, Redeemer, ScriptContext, Script)
  [PreProcessor errPP TxFCEKInput] ->
  -- | List of pre-condition checks over something like (Maybe Datum, Redeemer, ScriptContext, Script)
  [PreCondition errPreC TxFCEKInput] ->
  -- | Something like (Maybe Datum, Redeemer, ScriptContext, Script)
  TxFCEKInput ->
  -- | Post-conditions over something like (Either EvalError Script, ExBudget, [Text])
  [PostCondition errPostC TxFCEKOutput] ->
  TxFCEKCase errPP errPreC errPostC
mkTxFCEKCase name' pps preCs input' postCs =
  TxFCEKCase $
    PipelinedTestCase
      { name = name'
      , preProcessors = pps
      , preConditions = preCs
      , input = input'
      , computation = comp
      , postConditions = postCs
      }
  where
    comp :: TxFCEKInput -> TxFCEKOutput
    comp (TxFCEKInput md r sc script) =
      let
        dataArgs = mkDataArgs $ (md, r, sc)
        (res, budget, logs) = evalScriptHuge . applyArguments script $ dataArgs
       in
        TxFCEKOutput res budget logs

txfCEKUnitCase ::
  forall (errPP :: Type) (errPreC :: Type) (errPostC :: Type).
  (Show errPP, Show errPreC, Show errPostC) =>
  TxFCEKCase errPP errPreC errPostC ->
  TestTree
txfCEKUnitCase (TxFCEKCase pipelinedCase) = pipelinedUnitCase pipelinedCase

--------------------------------------------------------------------------------
-- Nominal Case

{- | A post condition for checking whether the script execution succeeded or failed
You must supply a way to turn a generic evaluation error into your domain-specifc
error type
-}
nominalPostCondition ::
  ((EvalError, [Text]) -> err) -> PostCondition err TxFCEKOutput
nominalPostCondition toErr =
  mkPostCondition $ \(TxFCEKOutput eitherErrOrScript _ logs) ->
    case eitherErrOrScript of
      Left evalErr -> Just $ toErr (evalErr, logs)
      Right _ -> Nothing

{- | A basic nominal case unit test.
Only checks if the script succeeds; does not do pre-processing, pre-condition checking,
or other post-condition checks.

Throws a generic "String" error (not domain-specific)
-}
nominalCaseBasic ::
  -- | Name of the test case
  String ->
  -- | Datum to apply to the script. Set this to Nothing unless you are
  -- testing a validator
  Maybe Datum ->
  -- | Redeemer
  Redeemer ->
  -- | Nominal context to apply Nominal
  ScriptContext ->
  -- | Nominal to apply
  Script ->
  TxFCEKCase String String String
nominalCaseBasic name' maybeDatum redeemer nominalCtx script =
  let
    errorPrinter :: ((EvalError, [Text]) -> String)
    errorPrinter (err, logs) =
      "Script failed.\nEvaluation error: \n"
        <> show err
        <> "\n Trace Log: \n"
        <> intercalate "\n" (show <$> logs)
   in
    mkTxFCEKCase
      name'
      []
      []
      (TxFCEKInput maybeDatum redeemer nominalCtx script)
      [nominalPostCondition errorPrinter]

--------------------------------------------------------------------------------

{- | The attackCaseRegex-style functions will fail with
  - A (Nothing, [Text]) if the script succeeds. [Text] in the tuple contains the logs.
  - A (Just Int, [Text]) if the script fails, but the regex does not match exactly once.
    The int contains the number of matches
-}
attackCaseRegexPostCondition ::
  -- | A way to go from the number of matches (if unequal to 1) and the logs to
  -- a domain-specific error
  ((Maybe Int, [Text]) -> err) ->
  -- | The regular expression to match against. It must match exactly once
  -- for the post-condition check to pass
  RE ->
  PostCondition err TxFCEKOutput
attackCaseRegexPostCondition toErr expectedFailureRE =
  mkPostCondition $ \(TxFCEKOutput eitherErrorOrScript _ logs) ->
    case eitherErrorOrScript of
      Left _evalError ->
        let
          numMatches = countMatches (fold logs *=~ expectedFailureRE)
         in
          if numMatches == 1
            then Nothing
            else Just $ toErr (Just numMatches, logs)
      Right _ ->
        Just $ toErr (Nothing, logs)

{- | Generate an "attack case" test tree, given a name, an expected failure condition,
the arguments to the script, an attack, and the script itself.

The expected failure condition is matched as a regex. It must match the logs exactly once.
-}
attackCaseBasicRegex ::
  (Show errPP) =>
  -- | Name of the test case
  String ->
  -- | Expected Failure String Match; TODO: This can be improved. Maybe use
  -- a discriminated error sum type with a injective string mapping?
  RE ->
  -- | Datum to apply to the script. Set this to Nothing unless you are
  -- testing a validator
  Maybe Datum ->
  -- | Redeemer
  Redeemer ->
  -- | Nominal context to apply attack
  ScriptContext ->
  -- | The script to execute
  Script ->
  -- | Attack to apply
  PreProcessor errPP TxFCEKInput ->
  TxFCEKCase errPP String String
attackCaseBasicRegex name' expectedFailureRE maybeDatum redeemer nominalCtx script' attack =
  let
    errorPrinter (Nothing, logs) =
      "Script Succeeded, but failure was expected. Logs: \n"
        <> intercalate "\n    " (show <$> logs)
    errorPrinter (Just numMatches, logs) =
      "The script execution against the attack case failed, but without "
        <> "the expected error. The regex\n    "
        <> reSource expectedFailureRE
        <> "\nshould have matched the logs exactly once, but it matched "
        <> show numMatches
        <> " times."
        <> "\nLogs:\n    "
        <> intercalate "\n    " (show <$> logs)
   in
    mkTxFCEKCase
      name'
      [attack]
      []
      (TxFCEKInput maybeDatum redeemer nominalCtx script')
      [attackCaseRegexPostCondition errorPrinter expectedFailureRE]

--------------------------------------------------------------------------------
-- Helpers

-- | Turn a scripts arguments into the appropriate list-of-data representation
mkDataArgs :: (Maybe Datum, Redeemer, ScriptContext) -> [PLC.Data]
mkDataArgs (Nothing, redeemer, ctx) = [toData redeemer, toData ctx]
mkDataArgs (Just datum, redeemer, ctx) = [toData datum, toData redeemer, toData ctx]
