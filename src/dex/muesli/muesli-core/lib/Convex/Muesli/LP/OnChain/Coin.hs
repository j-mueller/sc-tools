{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Convex.Muesli.LP.OnChain.Coin
  ( assetClassValue,
    unitValue,
    isUnity,
    assetClassValueOf,
    -- adaCoin,
    symbolOf,
    tokenNameOf
  )
where

-- import           Ledger.Ada             (adaSymbol, adaToken)
import           Plutus.V1.Ledger.Value (valueOf)
import qualified Plutus.V2.Ledger.Api   as V2
import qualified PlutusTx.AssocMap      as Map
import           PlutusTx.Prelude

{-# INLINEABLE unitValue #-}
unitValue :: (V2.CurrencySymbol, V2.TokenName) -> V2.Value
unitValue (curr, tok) = V2.singleton curr tok 1

{-# INLINEABLE isUnity #-}
isUnity :: V2.Value -> (V2.CurrencySymbol, V2.TokenName) -> Bool
isUnity v (curr, tok) = Map.lookup curr (V2.getValue v) == Just (Map.fromList [(tok, 1)])

-- {-# INLINEABLE adaCoin #-}
-- adaCoin :: (V2.CurrencySymbol, V2.TokenName)
-- adaCoin = (adaSymbol, adaToken)

{-# INLINEABLE symbolOf #-}
symbolOf :: (V2.CurrencySymbol, V2.TokenName) -> V2.CurrencySymbol
symbolOf (cs, _) = cs

{-# INLINEABLE tokenNameOf #-}
tokenNameOf :: (V2.CurrencySymbol, V2.TokenName) -> V2.TokenName
tokenNameOf (_, tn) = tn

{-# INLINABLE assetClassValue #-}
-- | A 'Value' containing the given amount of the asset class.
assetClassValue :: (V2.CurrencySymbol, V2.TokenName) -> Integer -> V2.Value
assetClassValue (c, t) = V2.singleton c t

{-# INLINABLE assetClassValueOf #-}
-- | Get the quantity of the given "AssetClass" class in the 'Value'.
assetClassValueOf :: V2.Value -> (V2.CurrencySymbol, V2.TokenName) -> Integer
assetClassValueOf v (c, t) = valueOf v c t
