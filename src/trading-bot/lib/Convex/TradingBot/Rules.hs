module Convex.TradingBot.Rules(
  Signal(..),
  Rule,
  movingAverage
) where

import           Convex.Measures          (getMean)
import           Convex.TradingBot.Prices (LPPrices, lastDay, lastWeek, stats)
import qualified Convex.TradingBot.Prices as Prices
import           Data.Maybe               (fromMaybe)

data Signal = Buy | Sell | Hold
  deriving (Eq, Ord, Show)

type Rule = LPPrices -> Signal

movingAverage :: Rule
movingAverage prices = fromMaybe Hold $ do
  pmWeek <- Prices.price (stats $ fst $ lastWeek prices) >>= getMean . Prices.pmPrice
  pmDay <- Prices.price (stats $ fst $ lastDay prices) >>= getMean . Prices.pmPrice
  if (pmWeek > pmDay * 1.05)
    then pure Buy
    else if pmWeek < pmDay * 0.95
      then pure Sell
      else pure Hold
