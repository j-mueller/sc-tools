{-# LANGUAGE TypeApplications #-}
{-| Functions for dealing with @plutus-tx@ scripts
-}
module Convex.PlutusTx(
  compiledCodeToScript
) where

import           Cardano.Api            (PlutusScript)
import qualified Cardano.Api.Shelley    as C
import           PlutusLedgerApi.Common (serialiseCompiledCode)
import           PlutusTx.Code          (CompiledCode)

{-| Get the 'PlutusScript' of a 'CompiledCode'
-}
compiledCodeToScript :: CompiledCode a -> PlutusScript lang
compiledCodeToScript = C.PlutusScriptSerialised . serialiseCompiledCode
