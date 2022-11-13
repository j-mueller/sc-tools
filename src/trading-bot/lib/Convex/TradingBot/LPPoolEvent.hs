{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns   #-}
module Convex.TradingBot.LPPoolEvent(
  LPPoolEvent(..),
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

extract :: C.TxOut C.CtxTx C.BabbageEra -> ScriptHash -> Maybe LPPoolEvent
extract out sh = case Muesli.getPoolScript out sh of
  Just (Right (Muesli.adaPair -> Just (lpePolicyId, lpeAssetName), Just (lpeLovelace, lpeNativeTokenAmount))) ->
    Just LPPoolEvent{lpePolicyId, lpeAssetName, lpeLovelace, lpeNativeTokenAmount}
  _ -> Nothing
