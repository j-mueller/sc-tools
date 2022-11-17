{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Convex.TradingBot.LPPoolEvent(
  LPPoolEvent(..),
  OrderbookEvent(..),
  extract
) where

import           Cardano.Api                (AssetName, Lovelace, PolicyId,
                                             Quantity, ScriptHash)
import qualified Cardano.Api.Shelley        as C
import qualified Convex.Muesli.LP.Constants as Muesli
import qualified Convex.Muesli.LP.Types     as Muesli

data LPPoolEvent =
  LPPoolEvent
    { lpePolicyId          :: !PolicyId
    , lpeAssetName         :: !AssetName
    , lpeLovelace          :: !Lovelace
    , lpeNativeTokenAmount :: !Quantity
    }

data OrderbookEvent =
  OrderbookEvent

extract :: C.TxOut C.CtxTx C.BabbageEra -> ScriptHash -> Maybe (Either LPPoolEvent OrderbookEvent)
extract out sh = case Muesli.getPoolScript out sh of
  Just (Right (Muesli.adaPair -> Just (lpePolicyId, lpeAssetName), Just (lpeLovelace, lpeNativeTokenAmount))) ->
    Just (Left LPPoolEvent{lpePolicyId, lpeAssetName, lpeLovelace, lpeNativeTokenAmount})
  _ | sh == orderbookScriptHash -> Just (Right OrderbookEvent)
    | otherwise -> Nothing

orderbookScriptHash :: C.ScriptHash
orderbookScriptHash = "00fb107bfbd51b3a5638867d3688e986ba38ff34fb738f5bd42b20d5"
