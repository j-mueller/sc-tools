{-# OPTIONS_GHC -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-} -- 1.1.0.0 will be enabled in conway
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Scripts used for testing
module Scripts(
  v2SpendingScriptSerialised,
  v2SpendingScript
) where

import qualified Cardano.Api.Shelley           as C
import           PlutusLedgerApi.Common        (SerialisedScript)
import           PlutusLedgerApi.Test.Examples (alwaysSucceedingNAryFunction)

v2SpendingScript :: C.PlutusScript C.PlutusScriptV2
v2SpendingScript = C.PlutusScriptSerialised $ alwaysSucceedingNAryFunction 3

v2SpendingScriptSerialised :: SerialisedScript
v2SpendingScriptSerialised = alwaysSucceedingNAryFunction 3
