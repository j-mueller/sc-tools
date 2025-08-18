-- | Unsafe (partial) functions for parsing asset names and related values
module Convex.Utils.String (
  unsafePolicyId,
  unsafeTxId,
  unsafeAssetName,
) where

import Cardano.Api qualified as C
import Cardano.Api.Parser.Text qualified as Parser
import Data.Text qualified as Text

-- | Parse a `C.PolicyId`, calling 'error' on failure
unsafePolicyId :: String -> C.PolicyId
unsafePolicyId = either error id . Parser.runParser C.parsePolicyId . Text.pack

-- | Parse a `C.TxId`, calling 'error' on failure
unsafeTxId :: String -> C.TxId
unsafeTxId = either error id . Parser.runParser C.parseTxId . Text.pack

-- | Parse a `C.AssetName`, calling 'error' on failure
unsafeAssetName :: String -> C.AssetName
unsafeAssetName = either error id . Parser.runParser C.parseAssetName . Text.pack
