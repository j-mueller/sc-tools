{-# LANGUAGE NamedFieldPuns #-}
{-| Keeping track of asset prices
-}
module Convex.TradingBot.Prices(
  PriceMeasure(..),
  observeLP,
  showPriceMeasure,
  AssetPrices(..),

  -- * Statistics over time
  LPPrices,
  prepend,
  PricesAt(..),
  pricesAt,
  empty,
  null,
  splitLastNSlots,
  lastHour,
  lastDay,
  lastWeek,
  stats,
  toList
) where

import           Cardano.Api       (BlockNo, Lovelace (..), Quantity (..),
                                    SlotNo)
import           Convex.Measures   (Mean, countMany, getMean)
import           Data.FingerTree   (FingerTree, Measured (..), split, (<|))
import qualified Data.FingerTree   as FT
import qualified Data.Foldable     as F
import           Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import           Prelude           hiding (null)

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

showPriceMeasure :: PriceMeasure -> String
showPriceMeasure PriceMeasure{pmPrice, pmVolume} =
  let m = maybe "N/A" show (getMean pmPrice)
  in unwords ["Price:", m, "Volume:", show pmVolume]

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
    { apPrice     :: !PriceMeasure
    , apSlotRange :: !(StrictMaybe (SlotNo, SlotNo))
    }

instance Semigroup AssetPrices where
  l <> r =
    AssetPrices
      { apPrice = apPrice l <> apPrice r
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

pricesAt :: BlockNo -> SlotNo -> (Lovelace, Quantity) -> (Lovelace, Quantity) -> PricesAt
pricesAt _blockNo _slotNo old new =
  PricesAt
    { _blockNo
    , _slotNo
    , _stats = AssetPrices{apPrice = observeLP old new, apSlotRange = SJust (_slotNo, _slotNo)}
    }

newtype LPPrices = LPPrices{unLPPrices :: FingerTree AssetPrices PricesAt }

prepend :: PricesAt -> LPPrices -> LPPrices
prepend s LPPrices{unLPPrices} = LPPrices $ s <| unLPPrices

empty :: LPPrices
empty = LPPrices mempty

null :: LPPrices -> Bool
null LPPrices{unLPPrices} = FT.null unLPPrices

splitLastNSlots :: Int -> LPPrices -> (LPPrices, LPPrices)
splitLastNSlots n (LPPrices s) =
  case (apSlotRange $ measure s) of
    SNothing -> (LPPrices mempty, LPPrices s)
    SJust (_, maxSlot) ->
      let cutoffPoint :: SlotNo = maxSlot - (fromIntegral n)
          f AssetPrices{apSlotRange = SNothing}            = True
          f AssetPrices{apSlotRange = SJust (minSlot', _)} = minSlot' < cutoffPoint
          (this, that) = split f s
      in (LPPrices this, LPPrices that)

-- | Split a 'MuesliStats' value into one with the values for the last hour
--   and one with the rest
lastHour :: LPPrices -> (LPPrices, LPPrices)
lastHour = splitLastNSlots (60 * 60)

-- | Split a 'LPPrices' value into one with the values for the last day
--   and one with the rest
lastDay :: LPPrices -> (LPPrices, LPPrices)
lastDay = splitLastNSlots (60 * 60 * 24)

-- | Split a 'LPPrices' value into one with the values for the last week
--   and one with the rest
lastWeek :: LPPrices -> (LPPrices, LPPrices)
lastWeek = splitLastNSlots (60 * 60 * 24 * 7)

-- | The raw stats
stats :: LPPrices -> AssetPrices
stats = measure . unLPPrices

-- | A list with all stats that were recorded
toList :: LPPrices -> [PricesAt]
toList = F.toList . unLPPrices
