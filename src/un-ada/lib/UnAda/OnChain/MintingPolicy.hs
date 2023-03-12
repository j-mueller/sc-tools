{-# LANGUAGE NoImplicitPrelude #-}
module UnAda.OnChain.MintingPolicy (
  mintingPolicy
) where

import           Plutus.V1.Ledger.Scripts  (ValidatorHash)
import           Plutus.V2.Ledger.Contexts (ScriptContext)
import           PlutusTx.Prelude

{-# INLINABLE mintingPolicy #-}
mintingPolicy :: ValidatorHash -> () -> ScriptContext -> Bool
mintingPolicy _ _ _ = True
