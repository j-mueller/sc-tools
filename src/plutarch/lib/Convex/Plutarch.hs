{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-| Plutarch / cardano-api interop
-}
module Convex.Plutarch(
  CardanoApiScriptError(..),
  plutarchScriptToCapiScript,
  wrapValidator,
) where

import qualified Cardano.Api.Shelley    as C
import           Data.Text              (Text)
import           Plutarch               (pcon, popaque, (#))
import           Plutarch.LedgerApi     (PScriptContext)
import           Plutarch.Internal      (punsafeCoerce)
import           Plutarch.Prelude       (PBool, PData, PIsData, POpaque,
                                         PUnit (..), Term, pfromData, pif, plam,
                                         ptraceInfoError, type (:-->))
import           Plutarch.Script        (Script (..))
import           PlutusLedgerApi.Common (serialiseUPLC)

data CardanoApiScriptError =
    PlutarchScriptError Text
    deriving Show

wrapValidator ::
    (PIsData dt, PIsData rdmr) =>
    Term s (dt :--> rdmr :--> PScriptContext :--> PBool) ->
    Term s (PData :--> PData :--> PScriptContext :--> POpaque)
wrapValidator validator = plam $ \datum redeemer ctx ->
    let dt = pfromData $ punsafeCoerce datum
        rdmr = pfromData $ punsafeCoerce redeemer
        result = validator # dt # rdmr # ctx
     in popaque $ pif result (pcon PUnit) (ptraceInfoError "Validator reduced to False")

plutarchScriptToCapiScript :: Script -> C.PlutusScript C.PlutusScriptV2
plutarchScriptToCapiScript (Script k) = C.PlutusScriptSerialised $ serialiseUPLC k
