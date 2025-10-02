-- | Unsafe (partial) functions for parsing asset names and related values
module Convex.Utils.String (
  unsafePolicyId,
  unsafeTxId,
  unsafeAssetName,
  unsafeDatumHash,
  unsafeScriptHash,
  unsafeDeserialiseRawBytesHex,
) where

import Cardano.Api qualified as C
import Cardano.Api.Parser.Text qualified as Parser
import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.Text qualified as Text

-- | Parse a `C.PolicyId`, calling 'error' on failure
unsafePolicyId :: Text.Text -> C.PolicyId
unsafePolicyId = either error id . Parser.runParser C.parsePolicyId

-- | Parse a `C.TxId`, calling 'error' on failure
unsafeTxId :: Text.Text -> C.TxId
unsafeTxId = either error id . Parser.runParser C.parseTxId

-- | Parse a `C.AssetName`, calling 'error' on failure
unsafeAssetName :: Text.Text -> C.AssetName
unsafeAssetName = either error id . Parser.runParser C.parseAssetName

-- | Parse a `C.Hash C.ScriptData`, calling 'error' on failure
unsafeDatumHash :: Text.Text -> C.Hash C.ScriptData
unsafeDatumHash = either (error . (<>) "Failed to parse datum hash: ") id . Parser.runParser C.parseScriptDataHash

-- | Parse a `C.ScriptHash`, calling 'error' on failure
unsafeScriptHash :: Text.Text -> C.ScriptHash
unsafeScriptHash = either (error . (<>) "Failed to parse script hash: ") id . Parser.runParser C.parseScriptHash

-- | Deserialise from a bytestring in hex encoding, calling 'error' on failure
unsafeDeserialiseRawBytesHex :: (C.SerialiseAsRawBytes a) => ByteString -> a
unsafeDeserialiseRawBytesHex = fromRight (error "cant deserialize") . C.deserialiseFromRawBytesHex
