{-# LANGUAGE NoImplicitPrelude #-}
{-| UnAda validator
-}
module UnAda.OnChain.Validator(
  unAdaValidator
) where

import           Plutus.V2.Ledger.Contexts (ScriptContext)
import           PlutusTx.Prelude
import           UnAda.OnChain.Types       (UnAdaRedeemer, UnAdaState)

{-# INLINABLE unAdaValidator #-}
unAdaValidator :: UnAdaState -> UnAdaRedeemer -> ScriptContext -> Bool
unAdaValidator (_spendAfter, _mps) _ _context = True
  -- let ScriptContext txInfo _ = context
  --     vs = C.valueSpent txInfo
  --     TxInfo{txInfoValidRange} = txInfo
  -- in spendAfter `Interval.before` txInfoValidRange

