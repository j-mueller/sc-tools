{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module UnAda.OffChain.Scripts(
  Scripts(..),
  validatorScript,
  mintingPolicyScript,
  mintingPolicyId,
  mintingPolicyHash,
  scripts,
  unAdaAssetId,
  assetName,
  unAdaPaymentCredential
) where

import           Cardano.Api.Shelley         (AssetId (..), AssetName,
                                              PaymentCredential, PlutusScriptV2,
                                              PolicyId, Script)
import qualified Cardano.Api.Shelley         as C
import           Convex.PlutusLedger         (transPolicyId)
import           Convex.Scripts              (compiledCodeToScript)
import qualified Plutus.V1.Ledger.Api        as PV1
import           Plutus.V1.Ledger.Scripts    (ValidatorHash)
import           PlutusTx                    (CompiledCode)
import qualified PlutusTx
import           PlutusTx.Prelude
import           UnAda.OnChain.MintingPolicy (mintingPolicy)
import           UnAda.OnChain.Validator     (unAdaValidator)

validatorScriptCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorScriptCompiled =
  $$(PlutusTx.compile [|| \d r c -> unAdaValidator d r c ||])

{-| The UnAda validator script
-}
validatorScript :: C.PlutusScript PlutusScriptV2
validatorScript = compiledCodeToScript validatorScriptCompiled

mintingPolicyCompiled :: ValidatorHash -> CompiledCode (BuiltinData -> BuiltinData -> ())
mintingPolicyCompiled hsh_ = $$(PlutusTx.compile [|| \hsh r c ->
                            check $ mintingPolicy hsh r c ||])
  `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData hsh_)

unAdaPaymentCredential :: PaymentCredential
unAdaPaymentCredential = C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV2 validatorScript

{-| The UnAda mps script
-}
mintingPolicyScript :: C.PlutusScript C.PlutusScriptV2
mintingPolicyScript = compiledCodeToScript validatorScriptCompiled

mintingPolicyId :: PolicyId
mintingPolicyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV2 mintingPolicyScript

mintingPolicyHash :: PV1.MintingPolicyHash
mintingPolicyHash = transPolicyId mintingPolicyId

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
    , sAssetId = (mintingPolicyId, assetName)
    }

unAdaAssetId :: AssetId
unAdaAssetId = uncurry AssetId $ sAssetId scripts
