{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
{-| UnAda validator
-}
module UnAda.OnChain.Validator(
  unAdaValidator
) where

import qualified Plutus.V1.Ledger.Interval as Interval
import           PlutusTx.IsData.Class     (unsafeFromBuiltinData)
import           PlutusTx.Prelude
import           UnAda.OnChain.Types       (BuiltinData (ScriptContext, TxInfoV2, UnAdaStateBuiltin, txInfo),
                                            TxInfoRest8 (TxInfoPartTwo),
                                            validRange)

{-# INLINABLE unAdaValidator #-}
unAdaValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
unAdaValidator (UnAdaStateBuiltin spendAfter _mps) _ ScriptContext{txInfo} =
  case txInfo of
    TxInfoV2 _ _ _ _ TxInfoPartTwo{validRange} ->
      let itvl = unsafeFromBuiltinData validRange
      in if spendAfter `Interval.before` itvl
          then ()
          else traceError "spendAfter must be before validity interval" ()
  -- in spendAfter `Interval.before` txInfoValidRange || not (spendAfter `Interval.before` txInfoValidRange)
