{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}
{-| Functions for dealing with scripts and datums
-}
module Convex.Scripts(
  -- * FromData / ToData
  fromScriptData,
  toScriptData,

) where

import qualified Cardano.Api.Shelley              as C
import qualified PlutusLedgerApi.V1               as PV1

fromScriptData :: PV1.FromData a => C.ScriptData -> Maybe a
fromScriptData (C.toPlutusData -> d) = PV1.fromData d

toScriptData :: PV1.ToData a => a -> C.ScriptData
toScriptData = C.fromPlutusData . PV1.toData
