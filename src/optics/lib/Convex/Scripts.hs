{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for dealing with scripts and datums
module Convex.Scripts (
  -- * FromData / ToData
  fromScriptData,
  toScriptData,

  -- * Hashable script data
  toHashableScriptData,
  fromHashableScriptData,
) where

import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Plutus.Data (Data (..))
import Ouroboros.Consensus.Shelley.Eras (BabbageEra)
import PlutusLedgerApi.V1 qualified as PV1

fromScriptData :: (PV1.FromData a) => C.ScriptData -> Maybe a
fromScriptData (C.toPlutusData -> d) = PV1.fromData d

toScriptData :: (PV1.ToData a) => a -> C.ScriptData
toScriptData = C.fromPlutusData . PV1.toData

fromHashableScriptData :: (PV1.FromData a) => C.HashableScriptData -> Maybe a
fromHashableScriptData (C.toPlutusData . C.getScriptData -> d) = PV1.fromData d

toHashableScriptData :: (PV1.ToData a) => a -> C.HashableScriptData
toHashableScriptData = C.fromAlonzoData @BabbageEra . Data . PV1.toData
