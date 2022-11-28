{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Convex.TradingBot.Rules(
  Signal(..),
  Rule,
  TwoMovingAveragesRule(..),
  defaultMomentum,
  mkRule,
  MovingAveragesRule(..),
  mkMovingAveragesRule,
  defaultMARule
) where

import           Control.DeepSeq          (NFData)
import           Control.Monad            (guard)
import           Convex.Measures          (getMean)
import           Convex.TradingBot.Prices (LPPrices, splitLastNSlots, stats)
import qualified Convex.TradingBot.Prices as Prices
import           Data.Maybe               (fromMaybe)
import           GHC.Generics             (Generic)

data Signal = Buy | Sell | Hold
  deriving (Eq, Ord, Show)

type Rule = LPPrices -> Signal

defaultMomentum :: TwoMovingAveragesRule
defaultMomentum = TwoMovingAveragesRule 1.15 0.8 (60 * 60 * 24 * 7) (60 * 60 * 24)

data TwoMovingAveragesRule =
  TwoMovingAveragesRule
    { rdBuy           :: !Double -- ^ Threshold for buy signal
    , rdSell          :: !Double -- ^ Threshold for sell signal
    , rdLookbackBig   :: !Int -- ^ Size of the "large" lookback window in seconds
    , rdLookbackSmall :: !Int -- ^ Size of the "small" lookback window in seconds
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass NFData

mkRule :: TwoMovingAveragesRule -> Rule
mkRule TwoMovingAveragesRule{rdBuy, rdSell, rdLookbackBig, rdLookbackSmall} prices = fromMaybe Hold $ do
  pmWeek <- Prices.price (stats $ fst $ splitLastNSlots rdLookbackBig prices) >>= getMean . Prices.pmPrice
  pmDay <- Prices.price (stats $ fst $ splitLastNSlots rdLookbackSmall prices) >>= getMean . Prices.pmPrice
  if (pmWeek > pmDay * rdBuy)
    then pure Buy
    else if pmWeek < pmDay * rdSell
      then pure Sell
      else pure Hold

data MovingAveragesRule =
  MovingAveragesRule
    { maPeriod    :: !Int -- ^ Size of the lookback window in seconds
    , maThreshold :: !Double
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass NFData

defaultMARule :: MovingAveragesRule
defaultMARule = MovingAveragesRule (60 * 60 * 24 * 7) 0.01

mkMovingAveragesRule :: MovingAveragesRule -> Rule
mkMovingAveragesRule MovingAveragesRule{maPeriod, maThreshold} prices = fromMaybe Hold $ do
  let getValue x = Prices.price (stats $ fst $ splitLastNSlots maPeriod x) >>= getMean . Prices.pmPrice
  current  <- getValue prices
  previous <- getValue (snd $ splitLastNSlots 1 prices)
  guard (previous /= 0)
  let r = current / previous
  if r > maThreshold
    then pure Buy
    else
      if r < negate maThreshold
      then pure Sell
  else pure Hold



