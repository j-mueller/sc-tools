{-# LANGUAGE NoImplicitPrelude #-}
{-| UnAda validator
-}
module UnAda.OnChain.Validator(
  unAdaValidator
) where

import qualified Plutus.V1.Ledger.Interval as Interval
import           Plutus.V2.Ledger.Contexts (ScriptContext (..), TxInfo (..))
import           PlutusTx.Prelude
import           UnAda.OnChain.Types       (UnAdaRedeemer, UnAdaState)

{-# INLINABLE unAdaValidator #-}
unAdaValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
unAdaValidator _ _ _context = ()
  -- let ScriptContext txInfo _ = context
  --     -- vs = C.valueSpent txInfo
  --     TxInfo{txInfoValidRange} = txInfo
  -- in spendAfter `Interval.before` txInfoValidRange || not (spendAfter `Interval.before` txInfoValidRange)

