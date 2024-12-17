{-# LANGUAGE OverloadedStrings #-}
{-| Standard names for fields in structured logging output
-}
module Convex.TxMod.Logging(
  txId,
  txFile,
  dotGraphFile
) where

import           Blammo.Logging.Simple ((.=))
import           Cardano.Api           (TxId)
import           Data.Aeson.Types      (KeyValue)

txId :: KeyValue e kv => TxId -> kv
txId i = "tx_id" .= i

txFile :: KeyValue e kv => FilePath -> kv
txFile i = "tx_file_path" .= i

dotGraphFile :: KeyValue e kv => FilePath -> kv
dotGraphFile i = "dot_graph_file_path" .= i
