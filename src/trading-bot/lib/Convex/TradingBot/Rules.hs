{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Convex.TradingBot.Rules(
  Signal(..),
  Rule,
  movingAverage,
  MomentumRule(..),
  mkRule
) where

import           Control.DeepSeq          (NFData)
import           Convex.Measures          (getMean)
import           Convex.TradingBot.Prices (LPPrices, lastDay, lastWeek, stats)
import qualified Convex.TradingBot.Prices as Prices
import           Data.Maybe               (fromMaybe)
import           GHC.Generics             (Generic)

data Signal = Buy | Sell | Hold
  deriving (Eq, Ord, Show)

type Rule = LPPrices -> Signal

movingAverage :: Rule
movingAverage = mkRule (MomentumRule 1.05 0.95)

data MomentumRule =
  MomentumRule
    { rdBuy  :: !Double
    , rdSell :: !Double
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass NFData

mkRule :: MomentumRule -> Rule
mkRule MomentumRule{rdBuy, rdSell} prices = fromMaybe Hold $ do
  pmWeek <- Prices.price (stats $ fst $ lastWeek prices) >>= getMean . Prices.pmPrice
  pmDay <- Prices.price (stats $ fst $ lastDay prices) >>= getMean . Prices.pmPrice
  if (pmWeek > pmDay * rdBuy)
    then pure Buy
    else if pmWeek < pmDay * rdSell
      then pure Sell
      else pure Hold
