{-# LANGUAGE NamedFieldPuns #-}
{-| Lenses for @cardano-api@ transactions
-}
module Convex.Lenses(
  txOuts,
  txMintValue,
  _TxMintNone,
  _TxMintValue,
  _Value,
  _TxOut,
  _TxOutValue
) where

import           Cardano.Api         (AddressInEra, AssetId, BabbageEra,
                                      BuildTxWith, CtxTx,
                                      MultiAssetSupportedInEra, PolicyId,
                                      Quantity (..), ScriptWitness, TxMintValue,
                                      TxOut, TxOutDatum, TxOutValue, Value,
                                      WitCtxMint)
import qualified Cardano.Api         as C
import           Cardano.Api.Shelley (ReferenceScript)
import           Control.Lens        (Iso', Lens', Prism', iso, lens, prism')
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map

-- Lenses for working with cardano-api transactions
txOuts :: Lens' (C.TxBodyContent v BabbageEra) [TxOut CtxTx BabbageEra]
txOuts = lens get set_ where
  get = C.txOuts
  set_ body txOuts' = body{C.txOuts=txOuts'}

txMintValue :: Lens' (C.TxBodyContent v BabbageEra) (TxMintValue v BabbageEra)
txMintValue = lens get set_ where
  get = C.txMintValue
  set_ body txMintValue' = body{C.txMintValue=txMintValue'}

_TxMintNone :: Prism' (TxMintValue build era) ()
_TxMintNone = prism' from to where
  from () = C.TxMintNone
  to C.TxMintValue{} = Nothing
  to C.TxMintNone    = Just ()

_TxMintValue :: Prism' (TxMintValue build era) (MultiAssetSupportedInEra era, Value, BuildTxWith build (Map PolicyId (ScriptWitness WitCtxMint era)))
_TxMintValue = prism' from to where
  from (mv, vl, btx) = C.TxMintValue mv vl btx
  to (C.TxMintValue mv vl btx) = Just (mv, vl, btx)
  to _                         = Nothing

_Value :: Iso' Value (Map AssetId Quantity)
_Value = iso from to where
  -- the 'Value' constructor is not exposed so we have to take the long way around
  from = Map.fromList . C.valueToList
  to = C.valueFromList . Map.toList

_TxOut :: Iso' (TxOut ctx era) (AddressInEra era, TxOutValue era, TxOutDatum ctx era, ReferenceScript era)
_TxOut = iso from to where
  from (C.TxOut addr vl dt rs) = (addr, vl, dt, rs)
  to (addr, vl, dt, rs) = C.TxOut addr vl dt rs

_TxOutValue :: Iso' (TxOutValue BabbageEra) Value
_TxOutValue = iso from to where
  from = C.txOutValueToValue
  to = C.TxOutValue C.MultiAssetInBabbageEra
