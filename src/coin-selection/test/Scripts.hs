{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fplugin-opt PlutusTx.Plugin:target-version=1.1.0.0 #-} -- 1.1.0.0 will be enabled in conway
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Scripts used for testing
module Scripts(
  v2SpendingScriptSerialised,
  v2SpendingScript,
  v2StakingScript,
  matchingIndexValidatorScript,
  matchingIndexMPScript,
  spendMatchingIndex,
  mintMatchingIndex
) where

import           Cardano.Api                   (TxIn)
import qualified Cardano.Api.Shelley           as C
import           Convex.BuildTx                (MonadBuildTx)
import qualified Convex.BuildTx                as BuildTx
import           Convex.PlutusTx               (compiledCodeToScript)
import           Convex.Scripts                (toHashableScriptData)
import           PlutusLedgerApi.Common        (SerialisedScript)
import           PlutusLedgerApi.Test.Examples (alwaysSucceedingNAryFunction)
import           PlutusTx                      (BuiltinData, CompiledCode)
import qualified PlutusTx
import           PlutusTx.Prelude              (BuiltinUnit)
import qualified Scripts.MatchingIndex         as MatchingIndex

v2SpendingScript :: C.PlutusScript C.PlutusScriptV2
v2SpendingScript = C.PlutusScriptSerialised $ alwaysSucceedingNAryFunction 3

v2SpendingScriptSerialised :: SerialisedScript
v2SpendingScriptSerialised = alwaysSucceedingNAryFunction 3

v2StakingScript :: C.PlutusScript C.PlutusScriptV2
v2StakingScript = C.PlutusScriptSerialised $ alwaysSucceedingNAryFunction 2

matchingIndexValidatorCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
matchingIndexValidatorCompiled =  $$(PlutusTx.compile [|| \c -> MatchingIndex.validator c ||])

matchingIndexMPCompiled :: CompiledCode (BuiltinData -> BuiltinUnit)
matchingIndexMPCompiled = $$(PlutusTx.compile [|| \c -> MatchingIndex.mintingPolicy c ||])

{-| Script that passes if the input's index (in the list of transaction inputs)
  matches the number passed as the redeemer
-}
matchingIndexValidatorScript :: C.PlutusScript C.PlutusScriptV3
matchingIndexValidatorScript = compiledCodeToScript matchingIndexValidatorCompiled

matchingIndexMPScript :: C.PlutusScript C.PlutusScriptV3
matchingIndexMPScript = compiledCodeToScript matchingIndexMPCompiled

{-| Spend an output locked by 'matchingIndexValidatorScript', setting
the redeemer to the index of the input in the final transaction
-}
spendMatchingIndex :: forall era m. (C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era) => MonadBuildTx era m => TxIn -> m ()
spendMatchingIndex txi =
  let witness txBody = C.ScriptWitness C.ScriptWitnessForSpending $ BuildTx.buildScriptWitness
        matchingIndexValidatorScript
        (C.ScriptDatumForTxIn $ Just $ toHashableScriptData ())
        (fromIntegral @Int @Integer $ BuildTx.findIndexSpending txi txBody)
  in BuildTx.setScriptsValid >> BuildTx.addInputWithTxBody txi witness

{-| Mint a token from the 'matchingIndexMPScript', setting
the redeemer to the index of its currency symbol in the final transaction mint
-}
mintMatchingIndex :: forall era m. (C.IsMaryBasedEra era, C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra C.PlutusScriptV3 era) => MonadBuildTx era m => C.PolicyId -> C.AssetName -> C.Quantity -> m ()
mintMatchingIndex policy assetName quantity =
  let witness txBody = BuildTx.buildScriptWitness
        matchingIndexMPScript
        C.NoScriptDatumForMint
        (fromIntegral @Int @Integer $ BuildTx.findIndexMinted policy txBody)
  in BuildTx.setScriptsValid >> BuildTx.addMintWithTxBody policy assetName quantity witness
