{-# LANGUAGE NoImplicitPrelude #-}
module UnAda.OnChain.Types(
  UnAdaState,
  UnAdaRedeemer
) where

import           Plutus.V1.Ledger.Time  (POSIXTime)
import           Plutus.V1.Ledger.Value (CurrencySymbol)

{-| The time after which the output can be spent, and the
minting policy
-}
type UnAdaState = (POSIXTime, CurrencySymbol)

{-| Always use the same redeemer
-}
type UnAdaRedeemer = ()
