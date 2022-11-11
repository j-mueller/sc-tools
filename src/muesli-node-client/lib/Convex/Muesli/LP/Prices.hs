{-# LANGUAGE NamedFieldPuns #-}
{-| Keeping track of asset prices
-}
module Convex.Muesli.LP.Prices(
  PriceMeasure(..),
  observeLP,

  -- * Statistics over time
  LPStats,
  prepend,
  PricesAt(..),
  emptyStats,
  splitLastNSlots,
  lastHour,
  lastDay,
  lastWeek,
  stats,
  toList
) where

import           Cardano.Api       (AssetName, BlockNo, Lovelace (..), PolicyId,
                                    Quantity (..), SlotNo)
import           Convex.Measures   (Mean, countMany)
import           Data.FingerTree   (FingerTree, Measured (..), split, (<|))
import qualified Data.Foldable     as F
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           Data.Maybe.Strict (StrictMaybe (SJust, SNothing))

data PriceMeasure =
  PriceMeasure
    { pmPrice  :: !Mean -- ^ Price per unit in Lovelace
    , pmVolume :: !Lovelace -- ^ Traded volume in Lovelace
    }

instance Semigroup PriceMeasure where
  l <> r =
    PriceMeasure
      { pmPrice  = pmPrice l <> pmPrice r
      , pmVolume = pmVolume l + pmVolume r
      }

instance Monoid PriceMeasure where
  mempty = PriceMeasure mempty mempty

{-| Measure the price and volume, given the old and the new contents of the LP
-}
observeLP ::
  (Lovelace, Quantity) ->
  (Lovelace, Quantity) ->
  PriceMeasure
observeLP (Lovelace oldLvl, _) (Lovelace newLvl, Quantity newQ) =
  let lvlDiff = abs (oldLvl - newLvl)
  in PriceMeasure
      { pmPrice  = countMany (fromIntegral newQ) newLvl
      , pmVolume = Lovelace lvlDiff
      }

data AssetPrices =
  AssetPrices
    { apByAsset   :: !(Map (PolicyId, AssetName) PriceMeasure)
    , apSlotRange :: !(StrictMaybe (SlotNo, SlotNo))
    }

instance Semigroup AssetPrices where
  l <> r =
    AssetPrices
      { apByAsset = Map.unionWith (<>) (apByAsset l) (apByAsset r)
      , apSlotRange =
          case (apSlotRange l, apSlotRange r) of
            (SNothing, x) -> x
            (x, SNothing) -> x
            (SJust (fromL, toL), SJust (fromR, toR)) -> SJust (min fromL fromR, max toL toR)
      }

instance Monoid AssetPrices where
  mempty = AssetPrices mempty SNothing

{-| Asset prices observed at a point in time
-}
data PricesAt =
  PricesAt
    { _blockNo :: BlockNo
    , _slotNo  :: SlotNo
    , _stats   :: AssetPrices
    }

instance Measured AssetPrices PricesAt where
  measure = _stats

newtype LPStats = LPStats{unLPStats :: FingerTree AssetPrices PricesAt }

prepend :: PricesAt -> LPStats -> LPStats
prepend s LPStats{unLPStats} = LPStats $ s <| unLPStats

emptyStats :: LPStats
emptyStats = LPStats mempty

splitLastNSlots :: Int -> LPStats -> (LPStats, LPStats)
splitLastNSlots n (LPStats s) =
  case (apSlotRange $ measure s) of
    SNothing -> (LPStats mempty, LPStats s)
    SJust (_, maxSlot) ->
      let cutoffPoint :: SlotNo = maxSlot - (fromIntegral n)
          f AssetPrices{apSlotRange = SNothing}            = True
          f AssetPrices{apSlotRange = SJust (minSlot', _)} = minSlot' < cutoffPoint
          (this, that) = split f s
      in (LPStats this, LPStats that)

-- | Split a 'MuesliStats' value into one with the values for the last hour
--   and one with the rest
lastHour :: LPStats -> (LPStats, LPStats)
lastHour = splitLastNSlots (60 * 60)

-- | Split a 'LPStats' value into one with the values for the last day
--   and one with the rest
lastDay :: LPStats -> (LPStats, LPStats)
lastDay = splitLastNSlots (60 * 60 * 24)

-- | Split a 'LPStats' value into one with the values for the last week
--   and one with the rest
lastWeek :: LPStats -> (LPStats, LPStats)
lastWeek = splitLastNSlots (60 * 60 * 24 * 7)

-- | The raw stats
stats :: LPStats -> AssetPrices
stats = measure . unLPStats

-- | A list with all stats that were recorded
toList :: LPStats -> [PricesAt]
toList = F.toList . unLPStats
