{-# LANGUAGE NoImplicitPrelude #-}
module UnAda.OnChain.MintingPolicy (
  mintingPolicy
) where

import           PlutusTx.Prelude

{-# INLINABLE mintingPolicy #-}
mintingPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> Bool
mintingPolicy _ _ _ = True
