{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UnAda.OffChain.Scripts (
  Scripts (..),
  validatorScript,
  mintingPolicyScript,
  mintingPolicyId,
  mintingPolicyHash,
  scripts,
  unAdaAssetId,
  assetName,
  unAdaPaymentCredential,
) where

import Cardano.Api.Shelley (
  AssetId (..),
  AssetName,
  PaymentCredential,
  PlutusScriptV2,
  PolicyId,
  Script,
 )
import Cardano.Api.Shelley qualified as C
import Convex.PlutusLedger.V1 (
  toMaryAssetName,
  transAssetName,
  transPolicyId,
 )
import Convex.Scripts (compiledCodeToScript)
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import PlutusTx (CompiledCode)
import PlutusTx qualified
import PlutusTx.Prelude
import UnAda.OnChain.MintingPolicy (unAdaMintingPolicy)
import UnAda.OnChain.Validator (unAdaValidator)

assetName :: AssetName
assetName = "unAda"

unAdaAssetId :: AssetId
unAdaAssetId = uncurry AssetId $ sAssetId scripts

validatorScriptCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorScriptCompiled =
  $$(PlutusTx.compile [||\tn d r c -> unAdaValidator tn d r c||])
    `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ transAssetName $ toMaryAssetName assetName)

-- | The UnAda validator script
validatorScript :: C.PlutusScript PlutusScriptV2
validatorScript = compiledCodeToScript validatorScriptCompiled

mintingPolicyCompiled :: ValidatorHash -> CompiledCode (BuiltinData -> BuiltinData -> ())
mintingPolicyCompiled hsh_ =
  $$(PlutusTx.compile [||\validatorHash assetName r c -> unAdaMintingPolicy validatorHash assetName r c||])
    `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData hsh_)
    `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ transAssetName $ toMaryAssetName assetName)

unAdaPaymentCredential :: PaymentCredential
unAdaPaymentCredential = C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV2 validatorScript

-- | The UnAda mps script
mintingPolicyScript :: C.PlutusScript C.PlutusScriptV2
mintingPolicyScript = compiledCodeToScript validatorScriptCompiled

mintingPolicyId :: PolicyId
mintingPolicyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV2 mintingPolicyScript

mintingPolicyHash :: PV1.MintingPolicyHash
mintingPolicyHash = transPolicyId mintingPolicyId

data Scripts
  = Scripts
  { sValidator :: !(Script PlutusScriptV2)
  , sMintingPolicy :: !(Script PlutusScriptV2)
  , sCredential :: !(PaymentCredential)
  , sAssetId :: !(PolicyId, AssetName)
  }

-- | The script and hashes for UnAda
scripts :: Scripts
scripts =
  Scripts
    { sValidator = C.PlutusScript C.PlutusScriptV2 validatorScript
    , sMintingPolicy = C.PlutusScript C.PlutusScriptV2 mintingPolicyScript
    , sCredential = C.PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV2 validatorScript
    , sAssetId = (mintingPolicyId, assetName)
    }
