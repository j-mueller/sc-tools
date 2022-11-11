{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-| Measure that we need for calculating prices
-}
module Convex.Measures(
  -- * Measures

  -- ** Mean
  Mean(..),
  getMean,
  countOne,
  countMany,

  -- ** Exponential weighted mean
  ExponentialWeightedMean(..),
  countExponentialWeightedMean,
  getExponentialWeightedMean,
  emptyEwm,

  -- ** Variance
  Variance(..),
  emptyVariance,
  observe,
  getVariance
  ) where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Maybe      (fromMaybe)
import           GHC.Generics    (Generic)
import           Numeric.Natural (Natural)

{-| Arithmetic mean
-}
data Mean = Mean
  { mCount :: !Natural
  , mSum   :: !Integer
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup Mean where
  l <> r = Mean{mCount = mCount l + mCount r, mSum = mSum l + mSum r}

instance Monoid Mean where
  mempty = Mean 0 0

-- | Count one observation
countOne :: Integer -> Mean
countOne = countMany 1

-- | Count many observations of the same value (ie. count the value with a higher weight)
countMany :: Natural -> Integer -> Mean
countMany = Mean

-- | Get the mean if at least one value has been observed
getMean :: Mean -> Maybe Double
getMean Mean{mCount, mSum} | mCount > 0 = Just (fromIntegral mSum / fromIntegral mCount)
                           | otherwise  = Nothing

data ExponentialWeightedMean =
  ExponentialWeightedMean
    { ewmCount :: !Double
    , ewmSum   :: !Double
    }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyEwm :: ExponentialWeightedMean
emptyEwm = ExponentialWeightedMean 0 0

-- | Add an observation of the 'Mean' to the exp. weighted mean,
--   discounting the past observations by a factor.
--   A good factor is 0.9 or 0.95
countExponentialWeightedMean :: Double -> ExponentialWeightedMean -> Mean -> ExponentialWeightedMean
countExponentialWeightedMean factor ExponentialWeightedMean{ewmCount = oldCount, ewmSum = oldSum } Mean{mCount, mSum} =
  ExponentialWeightedMean
    { ewmCount = fromIntegral mCount + factor * oldCount
    , ewmSum   = fromIntegral mSum   + factor * oldSum
    }

-- | Get the exp. weighted mean if at least one value has been observed
getExponentialWeightedMean :: ExponentialWeightedMean -> Maybe Double
getExponentialWeightedMean ExponentialWeightedMean{ewmCount, ewmSum} | ewmCount /= 0 = Just (ewmSum / ewmCount)
                                                                     | otherwise = Nothing

data Variance =
  Variance
    { vMean :: !Mean
    , vN    :: !Natural
    , vM2   :: !Double
    }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyVariance :: Variance
emptyVariance = Variance{vMean = mempty, vN = 0, vM2 = 0 }

observe :: Variance -> Mean -> Variance
observe Variance{vMean, vN, vM2} observation =
  -- Adapted from https://hackage.haskell.org/package/foldl-1.4.12/docs/src/Control.Foldl.html#variance
  let
    n' = vN + 1
    mean' = vMean <> observation
    delta = fromMaybe 0 (getMean observation) - fromMaybe 0 (getMean vMean)
    m2' = vM2 + delta * delta * fromIntegral vN / (fromIntegral n')
  in Variance{vMean = mean', vN = n', vM2 = m2'}

getVariance :: Variance -> Maybe Double
getVariance Variance{vN, vM2} | vN > 0 = Just (vM2 / fromIntegral vN)
                              | otherwise = Nothing
