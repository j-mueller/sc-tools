{-# LANGUAGE TypeApplications #-}

-- | Functions for dealing with @plutus-tx@ scripts
module Convex.PlutusTx (
  compiledCodeToScript,
) where

import Cardano.Api (PlutusScript)
import Cardano.Api.Shelley qualified as C
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Code (CompiledCode)

-- | Get the 'PlutusScript' of a 'CompiledCode'
compiledCodeToScript :: CompiledCode a -> PlutusScript lang
compiledCodeToScript = C.PlutusScriptSerialised . serialiseCompiledCode
