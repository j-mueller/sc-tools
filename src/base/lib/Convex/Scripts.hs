{-# LANGUAGE ViewPatterns #-}
{-| Functions for dealing with scripts and datums
-}
module Convex.Scripts(
  compiledCodeToScript,

  -- * FromData / ToData
  fromScriptData,
  toScriptData,

  -- * Translating between ledger and plutus representations

  -- ** Script hashes
  transScriptHash,

  -- ** Asset names
  transAssetName,
  unTransAssetName
) where

import           Cardano.Api               (PlutusScript)
import qualified Cardano.Api.Shelley       as C
import qualified Cardano.Ledger.Mary.Value as Mary (AssetName (..))
import           Codec.Serialise           (serialise)
import           Data.ByteString.Lazy      (toStrict)
import           Data.ByteString.Short     (fromShort, toShort)
import qualified Plutus.V1.Ledger.Api      as PV1
import           Plutus.V1.Ledger.Scripts  (fromCompiledCode)
import           PlutusTx.Code             (CompiledCode)

{-| Get the 'PlutusScript' of a 'CompiledCode'
-}
compiledCodeToScript :: CompiledCode a -> PlutusScript lang
compiledCodeToScript = C.PlutusScriptSerialised . toShort . toStrict . serialise . fromCompiledCode

transScriptHash :: C.ScriptHash -> PV1.ValidatorHash
transScriptHash h = PV1.ValidatorHash (PV1.toBuiltin (C.serialiseToRawBytes h)) -- TODO: is serialiseToRawBytes the correct thing to do here?

transAssetName :: Mary.AssetName -> PV1.TokenName
transAssetName (Mary.AssetName bs) = PV1.TokenName (PV1.toBuiltin (fromShort bs))

unTransAssetName :: PV1.TokenName -> C.AssetName
unTransAssetName (PV1.TokenName bs) = C.AssetName $ PV1.fromBuiltin bs

fromScriptData :: PV1.FromData a => C.ScriptData -> Maybe a
fromScriptData (C.toPlutusData -> d) = PV1.fromData d

toScriptData :: PV1.ToData a => a -> C.ScriptData
toScriptData = C.fromPlutusData . PV1.toData
