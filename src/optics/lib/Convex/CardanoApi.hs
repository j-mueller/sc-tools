module Convex.CardanoApi
  ( txBodyScriptDataDatums
  , txBodyScriptDataRedeemers
  ) where

import qualified Cardano.Api.Shelley          as C
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger
import           Data.Bifunctor               (bimap)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map

txBodyScriptDataDatums :: C.TxBodyScriptData era -> Map (C.Hash C.ScriptData) C.HashableScriptData
txBodyScriptDataDatums C.TxBodyNoScriptData = mempty
txBodyScriptDataDatums (C.TxBodyScriptData _ (Ledger.TxDats' dats) _) =
  Map.fromList $ fmap (bimap C.ScriptDataHash C.fromAlonzoData) $ Map.toList dats

txBodyScriptDataRedeemers :: C.TxBodyScriptData era -> Map C.ScriptWitnessIndex C.HashableScriptData
txBodyScriptDataRedeemers C.TxBodyNoScriptData = mempty
txBodyScriptDataRedeemers (C.TxBodyScriptData aeo _ (Ledger.Redeemers reds)) =
  Map.fromList $ fmap (\(purpose, (red, _)) -> (C.toScriptIndex aeo purpose, C.fromAlonzoData red)) $ Map.toList reds

