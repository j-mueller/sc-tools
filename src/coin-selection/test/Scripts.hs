{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-} -- 1.1.0.0 will be enabled in conway
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
-- | Scripts used for testing
module Scripts(
  v2SpendingScriptSerialised,
  v2SpendingScript,
  v2StakingScript,

  -- * Matching index scripts
  matchingIndexScript,
  spendMatchingIndex
) where

import           Cardano.Api                   (TxIn)
import qualified Cardano.Api.Shelley           as C
import           Convex.BuildTx                (MonadBuildTx)
import qualified Convex.BuildTx                as BuildTx
import           Convex.Scripts                (compiledCodeToScript,
                                                toHashableScriptData)
import           PlutusLedgerApi.Common        (SerialisedScript)
import           PlutusLedgerApi.Test.Examples (alwaysSucceedingNAryFunction)
import           PlutusTx                      (BuiltinData, CompiledCode)
import qualified PlutusTx
import qualified Scripts.MatchingIndex         as MatchingIndex

v2SpendingScript :: C.PlutusScript C.PlutusScriptV2
v2SpendingScript = C.PlutusScriptSerialised $ alwaysSucceedingNAryFunction 3

v2SpendingScriptSerialised :: SerialisedScript
v2SpendingScriptSerialised = alwaysSucceedingNAryFunction 3

v2StakingScript :: C.PlutusScript C.PlutusScriptV2
v2StakingScript = C.PlutusScriptSerialised $ alwaysSucceedingNAryFunction 2

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
            (C.ScriptDatumForTxIn $ toHashableScriptData ())
            (toHashableScriptData $ fromIntegral @Int @Integer $ BuildTx.findIndexSpending txi lkp)
            (C.ExecutionUnits 0 0)
  in BuildTx.setScriptsValid >> BuildTx.addInput txi witness
