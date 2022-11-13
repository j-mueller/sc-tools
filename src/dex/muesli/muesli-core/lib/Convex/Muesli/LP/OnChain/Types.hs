{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-specialise #-}
module Convex.Muesli.LP.OnChain.Types(
  PoolDatum(..)
) where

import qualified Plutus.V2.Ledger.Api as V2
import qualified PlutusTx
import           PlutusTx.Prelude     (Eq, Integer, (&&), (==))
import qualified Prelude              as Haskell

data PoolDatum = PoolDatum
  { pdCoinA          :: (V2.CurrencySymbol, V2.TokenName),
    pdCoinB          :: (V2.CurrencySymbol, V2.TokenName),
    pdTotalLiquidity :: Integer,
    pdSwapFee        :: Integer
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''PoolDatum [('PoolDatum, 0)]

instance Eq PoolDatum where
  {-# INLINEABLE (==) #-}
  x == y =
    pdCoinA x == pdCoinA y
      && pdCoinB x == pdCoinB y
      && pdTotalLiquidity x == pdTotalLiquidity y
