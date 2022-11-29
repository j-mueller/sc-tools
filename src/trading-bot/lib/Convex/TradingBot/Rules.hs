{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
module Convex.TradingBot.Rules(
  Signal(..),
  Rule,
  SearchableRule(..),

  -- * Concrete rules

  -- ** Two moving averages
  TwoMovingAverages(..),
  twoMovingAverages,

  -- * One moving average over two adjacent time periods
  MovingAverages(..),
  movingAverages
) where

import           Control.DeepSeq               (NFData)
import           Control.Monad                 (guard)
import           Control.Monad.Primitive       (PrimMonad, PrimState)
import           Convex.Measures               (getMean)
import           Convex.TradingBot.Prices      (LPPrices, splitLastNSlots,
                                                stats)
import qualified Convex.TradingBot.Prices      as Prices
import           Data.Maybe                    (fromMaybe)
import           GHC.Generics                  (Generic)
import qualified System.Random.MWC.Probability as P
import           System.Random.MWC.Probability (Gen)

type Rule = LPPrices -> Signal

{-| Rules whose parameter space can be searched by a random walk
-}
class SearchableRule r where
  {-| Apply the rule to a set of historic prices to obtain a signal
  -}
  signal :: r -> Rule
  neighbours :: PrimMonad m => Gen (PrimState m) -> r -> m r

data Signal = Buy | Sell | Hold
  deriving (Eq, Ord, Show)

twoMovingAverages :: TwoMovingAverages
twoMovingAverages = TwoMovingAverages 1.15 0.8 (60 * 60 * 24 * 7) (60 * 60 * 24)

instance SearchableRule TwoMovingAverages where
  signal TwoMovingAverages{rdBuy, rdSell, rdLookbackBig, rdLookbackSmall} prices = fromMaybe Hold $ do
    pmWeek <- Prices.price (stats $ fst $ splitLastNSlots rdLookbackBig prices) >>= getMean . Prices.pmPrice
    pmDay <- Prices.price (stats $ fst $ splitLastNSlots rdLookbackSmall prices) >>= getMean . Prices.pmPrice
    if (pmWeek > pmDay * rdBuy)
      then pure Buy
      else if pmWeek < pmDay * rdSell
        then pure Sell
        else pure Hold

  neighbours gen TwoMovingAverages{rdBuy, rdSell, rdLookbackBig, rdLookbackSmall} = do
    let sv = 0.01
    buy' <- max 0.1 <$> P.sample (P.normal rdBuy sv) gen
    sell' <- max 0.1 <$> P.sample (P.normal rdSell sv) gen
    pure
      $ TwoMovingAverages
          { rdBuy  = max buy' sell'
          , rdSell = min buy' sell'
          , rdLookbackBig
          , rdLookbackSmall
          }

data TwoMovingAverages =
  TwoMovingAverages
    { rdBuy           :: !Double -- ^ Threshold for buy signal
    , rdSell          :: !Double -- ^ Threshold for sell signal
    , rdLookbackBig   :: !Int -- ^ Size of the "large" lookback window in seconds
    , rdLookbackSmall :: !Int -- ^ Size of the "small" lookback window in seconds
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass NFData

data MovingAverages =
  MovingAverages
    { maPeriod        :: !Int -- ^ Size of the lookback window in seconds
    , maBuyThreshold  :: !Double
    , maSellThreshold :: !Double
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass NFData

instance SearchableRule MovingAverages where
  signal MovingAverages{maPeriod, maBuyThreshold, maSellThreshold} prices = fromMaybe Hold $ do
    let getValue x = Prices.price (stats $ fst $ splitLastNSlots maPeriod x) >>= getMean . Prices.pmPrice
    current  <- getValue prices
    previous <- getValue (snd $ splitLastNSlots 1 prices)
    guard (previous /= 0)
    let r = current / previous
    if r > 1 + maBuyThreshold
      then pure Buy
      else
        if r < 1 - maSellThreshold
        then pure Sell
    else pure Hold

  neighbours gen MovingAverages{maPeriod, maBuyThreshold, maSellThreshold} = do
    period' <- max (60 * 60) . round <$> P.sample (P.normal (fromIntegral maPeriod) 10_000) gen
    threshold' <- abs <$> P.sample (P.normal maBuyThreshold 0.1) gen
    threshold'' <- abs <$> P.sample (P.normal maSellThreshold 0.1) gen
    pure
      $ MovingAverages
          { maPeriod = period'
          , maBuyThreshold = threshold'
          , maSellThreshold = threshold''
          }

movingAverages :: MovingAverages
movingAverages = MovingAverages (60 * 60 * 24 * 7) 0.01 0.01
