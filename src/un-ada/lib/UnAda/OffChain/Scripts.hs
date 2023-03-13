{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module UnAda.OffChain.Scripts(
  Scripts(..),
  validatorScript,
  mintingPolicyScript,
  scripts,
  unAdaAssetId,
  assetName,
  unAdaPaymentCredential
) where

import           Cardano.Api.Shelley         (AssetId (..), AssetName,
                                              PaymentCredential, PlutusScriptV2,
                                              PolicyId, Script)
import qualified Cardano.Api.Shelley         as C
import           Convex.Scripts              (compiledCodeToScript)
import           Plutus.V1.Ledger.Scripts    (ValidatorHash)
import           Plutus.V2.Ledger.Contexts   (ScriptContext)
import           PlutusTx                    (CompiledCode)
import qualified PlutusTx
import           PlutusTx.Prelude
import           UnAda.OnChain.MintingPolicy (mintingPolicy)
import           UnAda.OnChain.Types         (UnAdaRedeemer, UnAdaState)
import           UnAda.OnChain.Validator     (unAdaValidator)

validatorScriptCompiled :: CompiledCode (UnAdaState -> UnAdaRedeemer -> ScriptContext -> ())
validatorScriptCompiled =
  $$(PlutusTx.compile [|| \d r c ->
                          check $ unAdaValidator d r c ||])

{-| The UnAda validator script
-}
validatorScript :: C.PlutusScript PlutusScriptV2
validatorScript = compiledCodeToScript validatorScriptCompiled

mintingPolicyCompiled :: ValidatorHash -> CompiledCode (() -> ScriptContext -> ())
mintingPolicyCompiled hsh_ = $$(PlutusTx.compile [|| \hsh r c ->
                            check $ mintingPolicy hsh r c ||])
  `PlutusTx.applyCode` PlutusTx.liftCode hsh_

unAdaPaymentCredential :: PaymentCredential
unAdaPaymentCredential = C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV2 validatorScript

{-| The UnAda mps script
-}
mintingPolicyScript :: C.PlutusScript C.PlutusScriptV2
mintingPolicyScript = compiledCodeToScript validatorScriptCompiled

data Scripts =
  Scripts
    { sValidator     :: !(Script PlutusScriptV2)
    , sMintingPolicy :: !(Script PlutusScriptV2)
    , sCredential    :: !(PaymentCredential)
    , sAssetId       :: !(PolicyId, AssetName)
    }

assetName :: AssetName
assetName = "unAda"

{-| The script and hashes for UnAda
-}
scripts :: Scripts
scripts =
  Scripts
    { sValidator = C.PlutusScript C.PlutusScriptV2 validatorScript
    , sMintingPolicy = C.PlutusScript C.PlutusScriptV2 mintingPolicyScript
    , sCredential = C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV2 validatorScript
    , sAssetId = (C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV2 mintingPolicyScript, assetName)
    }

unAdaAssetId :: AssetId
unAdaAssetId = uncurry AssetId $ sAssetId scripts
