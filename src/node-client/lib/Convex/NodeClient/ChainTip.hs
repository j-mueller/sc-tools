{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | ChainTip type with ToJSON / FromJSON instances
module Convex.NodeClient.ChainTip (
  JSONChainTip (..),
  blockHeaderTip,
  JSONChainPoint (..),
  blockHeaderPoint,
  JSONBlockNo (..),

  -- * Etc.
  chainPointText,
) where

import Cardano.Api (
  BlockHeader (..),
  BlockNo (..),
  ChainPoint (..),
  ChainTip (..),
  Hash,
  chainTipToChainPoint,
  deserialiseFromRawBytesHex,
  proxyToAsType,
  serialiseToRawBytesHexText,
 )
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson qualified as Aeson
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Ouroboros.Consensus.Block qualified as Consensus

newtype JSONChainTip = JSONChainTip {unJSONChainTip :: ChainTip}
  deriving newtype (Eq, Show, ToJSON)

blockHeaderTip :: BlockHeader -> ChainTip
blockHeaderTip (BlockHeader slotNo blockHash blockNo) =
  ChainTip slotNo blockHash blockNo

instance FromJSON JSONChainTip where
  parseJSON Aeson.Null = pure (JSONChainTip ChainTipAtGenesis)
  parseJSON y =
    withObject
      "JSONChainTip"
      ( \obj ->
          fmap JSONChainTip $
            ChainTip
              <$> obj .: "slot"
              <*> (obj .: "hash" >>= \(t :: Text) -> case deserialiseFromRawBytesHex (proxyToAsType $ Proxy @(Hash BlockHeader)) (Text.encodeUtf8 t) of Left err -> fail (show err); Right x -> pure x)
              <*> fmap Consensus.BlockNo (obj .: "block")
      )
      y

instance Semigroup JSONChainTip where
  l <> JSONChainTip ChainTipAtGenesis = l
  _ <> r = r

instance Ord JSONChainTip where
  JSONChainTip ChainTipAtGenesis <= _ = True
  _ <= JSONChainTip ChainTipAtGenesis = False
  JSONChainTip (ChainTip _ _ lb) <= JSONChainTip (ChainTip _ _ rb) = lb <= rb

instance Monoid JSONChainTip where
  mempty = JSONChainTip ChainTipAtGenesis

newtype JSONChainPoint = JSONChainPoint ChainPoint
  deriving newtype (Eq, Show)

instance ToJSON JSONChainPoint where
  toJSON (JSONChainPoint jp) = case jp of
    ChainPointAtGenesis -> toJSON ("ChainPointAtGenesis" :: String)
    ChainPoint s h -> object ["slot" .= s, "block_header" .= h]

instance FromJSON JSONChainPoint where
  parseJSON (Aeson.String "ChainPointAtGenesis") = pure (JSONChainPoint ChainPointAtGenesis)
  parseJSON x =
    withObject
      "JSONChainPoint"
      ( \obj ->
          fmap JSONChainPoint (ChainPoint <$> obj .: "slot" <*> obj .: "block_header")
      )
      x

instance Semigroup JSONChainPoint where
  l <> JSONChainPoint ChainPointAtGenesis = l
  _ <> r = r

instance Monoid JSONChainPoint where
  mempty = JSONChainPoint ChainPointAtGenesis

instance Ord JSONChainPoint where
  JSONChainPoint ChainPointAtGenesis <= _ = True
  _ <= JSONChainPoint ChainPointAtGenesis = False
  JSONChainPoint (ChainPoint ls _) <= JSONChainPoint (ChainPoint rs _) = ls <= rs

newtype JSONBlockNo = JSONBlockNo {unJSONBlockNo :: BlockNo}
  deriving newtype (Eq, Show)

instance ToJSON JSONBlockNo where
  toJSON (JSONBlockNo (BlockNo n)) = toJSON n

instance FromJSON JSONBlockNo where
  parseJSON x = (JSONBlockNo . BlockNo) <$> parseJSON x

blockHeaderPoint :: BlockHeader -> ChainPoint
blockHeaderPoint = chainTipToChainPoint . blockHeaderTip

chainPointText :: ChainPoint -> Text
chainPointText = \case
  ChainPointAtGenesis -> "Genesis"
  ChainPoint (Consensus.SlotNo slot) blockHeader ->
    serialiseToRawBytesHexText blockHeader <> ":" <> Text.pack (show slot)
