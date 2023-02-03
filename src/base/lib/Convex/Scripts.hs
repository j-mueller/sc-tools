{-# LANGUAGE ViewPatterns #-}
{-| Functions for dealing with scripts and datums
-}
module Convex.Scripts(
  compiledCodeToScript,

  -- * FromData / ToData
  fromScriptData,
  toScriptData,

  -- * Translating between ledger and plutus representations

) where

import           Cardano.Api              (PlutusScript)
import qualified Cardano.Api.Shelley      as C
import           Codec.Serialise          (serialise)
import           Data.ByteString.Lazy     (toStrict)
import           Data.ByteString.Short    (toShort)
import qualified Plutus.V1.Ledger.Api     as PV1
import           Plutus.V1.Ledger.Scripts (fromCompiledCode)
import           PlutusTx.Code            (CompiledCode)

{-| Get the 'PlutusScript' of a 'CompiledCode'
-}
compiledCodeToScript :: CompiledCode a -> PlutusScript lang
compiledCodeToScript = C.PlutusScriptSerialised . toShort . toStrict . serialise . fromCompiledCode

fromScriptData :: PV1.FromData a => C.ScriptData -> Maybe a
fromScriptData (C.toPlutusData -> d) = PV1.fromData d

toScriptData :: PV1.ToData a => a -> C.ScriptData
toScriptData = C.fromPlutusData . PV1.toData
