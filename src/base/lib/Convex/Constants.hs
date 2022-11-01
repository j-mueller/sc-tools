{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-| Some useful constants related to the cardano network.
-}
module Convex.Constants(
  alonzoMainnet,
  recent
  ) where

import           Cardano.Api (AsType (AsHash), BlockHeader, ChainPoint (..))
import qualified Cardano.Api as CAPI
import           Data.Proxy  (Proxy (..))

-- | Start of the Alonzo era on mainnet.
-- https://explorer.cardano.org/en/block?id=8959c0323b94cc670afe44222ab8b4e72cfcad3b5ab665f334bbe642dc6e9ef4
alonzoMainnet :: ChainPoint
alonzoMainnet = either (error . (<>) "alonzoMainnet: parse failed: " . show) id $ do
  hsh <- CAPI.deserialiseFromRawBytesHex (AsHash (CAPI.proxyToAsType (Proxy :: Proxy BlockHeader))) "8959c0323b94cc670afe44222ab8b4e72cfcad3b5ab665f334bbe642dc6e9ef4"
  return $ ChainPoint 39_916_975 hsh

-- | A recent block (1 November 2022, 5.42pm)
-- https://explorer.cardano.org/en/block?id=22e8b5455623ff8e75a79499bada55c3b0b259db33ddf4b94efaac858a40c4ed
recent :: ChainPoint
recent = either (error . (<>) "recent: parse failed: " . show) id $ do
  hsh <- CAPI.deserialiseFromRawBytesHex (AsHash (CAPI.proxyToAsType (Proxy :: Proxy BlockHeader))) "22e8b5455623ff8e75a79499bada55c3b0b259db33ddf4b94efaac858a40c4ed"
  return $ ChainPoint 75_758_280 hsh
