{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
-- | Scripts used for testing
module Scripts(
  v2SpendingScript,
  matchingIndexScript,
  spendMatchingIndex
) where

import           Cardano.Api           (TxIn)
import qualified Cardano.Api.Shelley   as C
import           Convex.BuildTx        (MonadBuildTx)
import qualified Convex.BuildTx        as BuildTx
import           Convex.Scripts        (compiledCodeToScript, toScriptData)
import           PlutusTx              (CompiledCode)
import qualified PlutusTx
import           PlutusTx.Builtins     (BuiltinData)
import qualified Scripts.MatchingIndex as MatchingIndex

type UntypedValidator = BuiltinData -> BuiltinData -> BuiltinData -> ()

unappliedRewardFeeValidator :: PlutusTx.CompiledCode UntypedValidator
unappliedRewardFeeValidator = $$(PlutusTx.compile [|| \_ _ _ -> () ||])

-- | Validator that always succeeds
v2SpendingScript :: C.PlutusScript C.PlutusScriptV2
v2SpendingScript = compiledCodeToScript unappliedRewardFeeValidator

matchingIndexCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
matchingIndexCompiled =  $$(PlutusTx.compile [|| \d r c -> MatchingIndex.validator d r c ||])

{-| Script that passes if the input's index (in the list of transaction inputs)
  matches the number passed as the redeemer
-}
matchingIndexScript :: C.PlutusScript C.PlutusScriptV2
matchingIndexScript = compiledCodeToScript matchingIndexCompiled

{-| Spend an output locked by 'matchingIndexScript', setting
the redeemer to the index of the input in the final transaction
-}
spendMatchingIndex :: MonadBuildTx m => TxIn -> m ()
spendMatchingIndex txi =
  let witness lkp =
        C.ScriptWitness C.ScriptWitnessForSpending
        $ C.PlutusScriptWitness
            C.PlutusScriptV2InBabbage
            C.PlutusScriptV2
            (C.PScript matchingIndexScript)
            (C.ScriptDatumForTxIn $ toScriptData ())
            (toScriptData $ fromIntegral @Int @Integer $ BuildTx.findIndexSpending txi lkp)
            (C.ExecutionUnits 0 0)
  in BuildTx.setScriptsValid >> BuildTx.addInput txi witness
