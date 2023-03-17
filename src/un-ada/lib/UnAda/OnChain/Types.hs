{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns       #-}
module UnAda.OnChain.Types(
  UnAdaState(..),
  BuiltinData(UnAdaStateBuiltin),
  UnAdaRedeemer
) where

import           GHC.Generics           (Generic)
import           Plutus.V1.Ledger.Time  (POSIXTime)
import           Plutus.V1.Ledger.Value (CurrencySymbol)
import           PlutusTx               (BuiltinData)
import qualified PlutusTx
import           PlutusTx.Builtins      (unsafeDataAsB, unsafeDataAsConstr,
                                         unsafeDataAsI)
import           PlutusTx.Prelude       (BuiltinByteString, Integer)

{-| The time after which the output can be spent, and the
minting policy
-}
data UnAdaState =
  UnAdaState
    { spendAfter :: POSIXTime
    , mps        :: CurrencySymbol
    }
    deriving stock Generic

PlutusTx.makeIsDataIndexed ''UnAdaState [('UnAdaState, 0)]

-- pattern Head x <- x:xs
pattern UnAdaStateBuiltin :: Integer -> BuiltinByteString -> BuiltinData
pattern UnAdaStateBuiltin spendAfterB mpsB <- (match -> (spendAfterB, mpsB))

{-# INLINEABLE match #-}
match :: BuiltinData -> (Integer, BuiltinByteString)
match dt =
  let (_, [unsafeDataAsI -> i, unsafeDataAsB -> b]) = unsafeDataAsConstr dt
  in (i, b)

{-| Always use the same redeemer
-}
type UnAdaRedeemer = ()
