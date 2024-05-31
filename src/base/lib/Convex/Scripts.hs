{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}
{-| Functions for dealing with scripts and datums
-}
module Convex.Scripts(
  compiledCodeToScript,
  fromHashableScriptData,
  toHashableScriptData,
) where

import           Cardano.Api                      (PlutusScript)
import qualified Cardano.Api.Shelley              as C
import           Cardano.Ledger.Plutus.Data       (Data (..))
import           Ouroboros.Consensus.Shelley.Eras (StandardBabbage)
import           PlutusLedgerApi.Common           (serialiseCompiledCode)
import qualified PlutusLedgerApi.V1               as PV1
import           PlutusTx.Code                    (CompiledCode)

{-| Get the 'PlutusScript' of a 'CompiledCode'
-}
compiledCodeToScript :: CompiledCode a -> PlutusScript lang
compiledCodeToScript = C.PlutusScriptSerialised . serialiseCompiledCode

fromHashableScriptData :: PV1.FromData a => C.HashableScriptData -> Maybe a
fromHashableScriptData (C.toPlutusData . C.getScriptData -> d) = PV1.fromData d

toHashableScriptData :: PV1.ToData a => a -> C.HashableScriptData
toHashableScriptData = C.fromAlonzoData @StandardBabbage . Data . PV1.toData
