{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Scripts used for testing
module Scripts(
  v2SpendingScript
) where

import qualified Cardano.Api.Shelley as C
import           Convex.Scripts      (compiledCodeToScript)
import qualified PlutusTx
import           PlutusTx.Builtins   (BuiltinData)

type UntypedValidator = BuiltinData -> BuiltinData -> BuiltinData -> ()

unappliedRewardFeeValidator :: PlutusTx.CompiledCode UntypedValidator
unappliedRewardFeeValidator = $$(PlutusTx.compile [|| \_ _ _ -> () ||])

-- | Validator that always succeeds
v2SpendingScript :: C.PlutusScript C.PlutusScriptV2
v2SpendingScript = compiledCodeToScript unappliedRewardFeeValidator
