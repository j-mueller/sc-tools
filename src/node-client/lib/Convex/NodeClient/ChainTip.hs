{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-| ChainTip type with ToJSON / FromJSON instances
-}
module Convex.NodeClient.ChainTip(
  JSONChainTip(..),
  blockHeaderTip,
  JSONChainPoint(..),
  blockHeaderPoint
) where

import           Cardano.Api               (BlockHeader (..), ChainPoint (..),
                                            ChainTip (..), Hash,
                                            chainTipToChainPoint,
                                            deserialiseFromRawBytesHex,
                                            proxyToAsType)
import           Data.Aeson                (FromJSON (..), ToJSON (..), object,
                                            withObject, (.:), (.=))
import qualified Data.Aeson                as Aeson
import           Data.Proxy                (Proxy (..))
import           Data.Text                 (Text)
import qualified Data.Text.Encoding        as Text
import qualified Ouroboros.Consensus.Block as Consensus

newtype JSONChainTip = JSONChainTip{ unJSONChainTip :: ChainTip }
  deriving newtype (Eq, Show, ToJSON)

blockHeaderTip :: BlockHeader -> ChainTip
blockHeaderTip (BlockHeader slotNo blockHash blockNo) =
  ChainTip slotNo blockHash blockNo

instance FromJSON JSONChainTip where
  parseJSON Aeson.Null = pure (JSONChainTip ChainTipAtGenesis)
  parseJSON y          = withObject "JSONChainTip" (\obj ->
    fmap JSONChainTip $ ChainTip
      <$> obj .: "slot"
      <*> (obj .: "hash" >>= \(t :: Text) -> case deserialiseFromRawBytesHex (proxyToAsType $ Proxy @(Hash BlockHeader)) (Text.encodeUtf8 t) of { Left err -> fail (show err); Right x -> pure x})
      <*> fmap Consensus.BlockNo (obj .: "block")) y

newtype JSONChainPoint = JSONChainPoint ChainPoint
  deriving newtype (Eq, Show)

instance ToJSON JSONChainPoint where
  toJSON (JSONChainPoint jp) = case jp of
    ChainPointAtGenesis -> toJSON ("ChainPointAtGenesis" :: String)
    ChainPoint s h      -> object ["slot" .= s, "block_header" .= h]

instance FromJSON JSONChainPoint where
  parseJSON (Aeson.String "ChainPointAtGenesis") = pure (JSONChainPoint ChainPointAtGenesis)
  parseJSON x = withObject "JSONChainPoint" (\obj ->
    fmap JSONChainPoint (ChainPoint <$> obj .: "slot" <*> obj .: "block_header")) x

blockHeaderPoint :: BlockHeader -> ChainPoint
blockHeaderPoint = chainTipToChainPoint . blockHeaderTip
