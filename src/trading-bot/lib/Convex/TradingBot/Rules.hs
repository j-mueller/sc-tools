{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Convex.TradingBot.Rules(
  Signal(..),
  Rule,
  MomentumRule(..),
  defaultMomentum,
  mkRule
) where

import           Control.DeepSeq          (NFData)
import           Convex.Measures          (getMean)
import           Convex.TradingBot.Prices (LPPrices, splitLastNSlots, stats)
import qualified Convex.TradingBot.Prices as Prices
import           Data.Maybe               (fromMaybe)
import           GHC.Generics             (Generic)

data Signal = Buy | Sell | Hold
  deriving (Eq, Ord, Show)

type Rule = LPPrices -> Signal

defaultMomentum :: MomentumRule
defaultMomentum = MomentumRule 1.15 0.8 (60 * 60 * 24 * 7) (60 * 60 * 24)

data MomentumRule =
  MomentumRule
    { rdBuy           :: !Double -- ^ Threshold for buy signal
    , rdSell          :: !Double -- ^ Threshold for sell signal
    , rdLookbackBig   :: !Int -- ^ Size of the "large" lookback window in seconds
    , rdLookbackSmall :: !Int -- ^ Size of the "small" lookback window in seconds
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass NFData

mkRule :: MomentumRule -> Rule
mkRule MomentumRule{rdBuy, rdSell, rdLookbackBig, rdLookbackSmall} prices = fromMaybe Hold $ do
  pmWeek <- Prices.price (stats $ fst $ splitLastNSlots rdLookbackBig prices) >>= getMean . Prices.pmPrice
  pmDay <- Prices.price (stats $ fst $ splitLastNSlots rdLookbackSmall prices) >>= getMean . Prices.pmPrice
  if (pmWeek > pmDay * rdBuy)
    then pure Buy
    else if pmWeek < pmDay * rdSell
      then pure Sell
      else pure Hold
