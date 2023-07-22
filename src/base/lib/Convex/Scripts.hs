{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}
{-| Functions for dealing with scripts and datums
-}
module Convex.Scripts(
  compiledCodeToScript,

  -- * FromData / ToData
  fromScriptData,
  toScriptData,
  toHashableScriptData,

  -- * Translating between ledger and plutus representations

) where

import           Cardano.Api                        (PlutusScript)
import qualified Cardano.Api.Shelley                as C
import           Cardano.Ledger.Alonzo.Scripts.Data (Data (..))
import           Codec.Serialise                    (serialise)
import           Data.ByteString.Lazy               (toStrict)
import           Data.ByteString.Short              (toShort)
import           Ouroboros.Consensus.Shelley.Eras   (StandardBabbage)
import           PlutusLedgerApi.Common             (serialiseCompiledCode)
import qualified PlutusLedgerApi.V1                 as PV1
import           PlutusTx.Code                      (CompiledCode)

{-| Get the 'PlutusScript' of a 'CompiledCode'
-}
compiledCodeToScript :: CompiledCode a -> PlutusScript lang
compiledCodeToScript = C.PlutusScriptSerialised . toShort . toStrict . serialise . serialiseCompiledCode

fromScriptData :: PV1.FromData a => C.ScriptData -> Maybe a
fromScriptData (C.toPlutusData -> d) = PV1.fromData d

toScriptData :: PV1.ToData a => a -> C.ScriptData
toScriptData = C.fromPlutusData . PV1.toData

toHashableScriptData :: PV1.ToData a => a -> C.HashableScriptData
toHashableScriptData = C.fromAlonzoData @StandardBabbage . Data . PV1.toData
