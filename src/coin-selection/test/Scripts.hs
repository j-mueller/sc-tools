{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
-- | Scripts used for testing
module Scripts(
  v2SpendingScript,
  matchingIndexValidatorScript,
  matchingIndexMPScript,
  spendMatchingIndex,
  mintMatchingIndex
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

matchingIndexValidatorCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
matchingIndexValidatorCompiled =  $$(PlutusTx.compile [|| \d r c -> MatchingIndex.validator d r c ||])

matchingIndexMPCompiled :: CompiledCode (BuiltinData -> BuiltinData -> ())
matchingIndexMPCompiled = $$(PlutusTx.compile [|| \r c -> MatchingIndex.mintingPolicy r c ||])

{-| Script that passes if the input's index (in the list of transaction inputs)
  matches the number passed as the redeemer
-}
matchingIndexValidatorScript :: C.PlutusScript C.PlutusScriptV2
matchingIndexValidatorScript = compiledCodeToScript matchingIndexValidatorCompiled

matchingIndexMPScript :: C.PlutusScript C.PlutusScriptV2
matchingIndexMPScript = compiledCodeToScript matchingIndexMPCompiled

{-| Spend an output locked by 'matchingIndexValidatorScript', setting
the redeemer to the index of the input in the final transaction
-}
spendMatchingIndex :: MonadBuildTx m => TxIn -> m ()
spendMatchingIndex txi =
  let witness txBody =
        C.ScriptWitness C.ScriptWitnessForSpending
        $ C.PlutusScriptWitness
            C.PlutusScriptV2InBabbage
            C.PlutusScriptV2
            (C.PScript matchingIndexValidatorScript)
            (C.ScriptDatumForTxIn $ toScriptData ())
            (toScriptData $ fromIntegral @Int @Integer $ BuildTx.findIndexSpending txi txBody)
            (C.ExecutionUnits 0 0)
  in BuildTx.setScriptsValid >> BuildTx.addInputWithTxBody txi witness

{-| Mint a token from the 'matchingIndexMPScript', setting
the redeemer to the index of its currency symbol in the final transaction mint
-}
mintMatchingIndex :: MonadBuildTx m => C.PolicyId -> C.AssetName -> C.Quantity -> m ()
mintMatchingIndex policy assetName quantity =
  let witness txBody =
        C.PlutusScriptWitness
          C.PlutusScriptV2InBabbage
          C.PlutusScriptV2
          (C.PScript matchingIndexMPScript)
          (C.NoScriptDatumForMint)
          (toScriptData $ fromIntegral @Int @Integer $ BuildTx.findIndexMinted policy txBody)
          (C.ExecutionUnits 0 0)
  in BuildTx.setScriptsValid >> BuildTx.addMintWithTxBody policy assetName quantity witness
