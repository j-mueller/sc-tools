{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
module Convex.TradingBot.Portfolio(
  -- * Positions
  Position(..),
  lastPrice,
  stopLoss,
  limit,
  setLimits,

  -- * Portfolios
  Portfolio(..),
  printPortfolioInfo,
  emptyPortfolio,
  distribution,
  tradeCount,
  buyOrder,
  updatePrice,

  -- * Config
  PortfolioConfig(..),
  defaultPortfolioConfig,

  -- * Executing trades
  MonadTrade(..),

  -- ** Simulated portfolios
  SimulatedPortfolioT,
  runSimulatedPortfolioT,
  execSimulatedPortfolioT,

  -- ** Real portfolios

  -- * Etc.
  closePositionMsg,
  availableFunds
) where

import           Cardano.Api                (AssetId (..), AssetName,
                                             Lovelace (..), PolicyId,
                                             Quantity (..), Value)
import qualified Cardano.Api                as C
import           Control.Lens               (Lens', _1, _2, _Just, at,
                                             makeLenses, makeLensesFor, over,
                                             set, use, view, (&), (+=), (.=),
                                             (.~))
import           Control.Monad              (guard, when)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (MonadState, StateT, get, put,
                                             runState, runStateT)
import           Convex.MonadLog            (MonadLog, logInfoS)
import           Convex.Muesli.LP.Types     (BuyOrder (..), Price, scale, unitsOf, unitPrice, valueOf,
                                             SellOrder (..))
import           Data.Foldable              (fold, traverse_)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe, isJust, isNothing)
import           Data.Ratio                 ((%))

type PricePoint = (PolicyId, AssetName, Quantity, Lovelace)

data PortfolioConfig =
  PortfolioConfig
    { pfMaxPositionSize :: !Rational -- ^ Max position size as a share of total AUM. Default: 0.10
    , pfDefaultStopLoss :: !Rational -- ^ Default stop loss on positions (as a fraction of the initial price. Default: 0.75)
    , pfDefaultLimit    :: !Rational -- ^ Default limit (Take profit) on positions as a multipe of the initial price. Default: 1.25
    , pfMinPositionSize :: !Lovelace -- ^ Minimum position size in Ada
    }

defaultPortfolioConfig :: PortfolioConfig
defaultPortfolioConfig =
  PortfolioConfig
    { pfMaxPositionSize = 0.1
    , pfDefaultStopLoss = 0.85
    , pfDefaultLimit    = 1.35
    , pfMinPositionSize = Lovelace 10_000_000
    }

{-| Position in our portfolio. Note that this does not include the
size of the position (this information is obtained from the wallet
in form of a 'Value' value)
-}
data Position =
  Position
    { pLastPrice :: !Price -- ^ last market price per unit of currency
    , pStopLoss  :: Maybe Price -- ^ stop (price per unit of currency)
    , pLimit     :: Maybe Price
    }
    deriving (Eq, Ord, Show)

makeLensesFor
  [ ("pLastPrice", "lastPrice")
  , ("pStopLoss", "stopLoss")
  , ("pLimit", "limit")
  ]
  ''Position

initPosition :: Price -> Position
initPosition p = Position p Nothing Nothing

setLimits :: PortfolioConfig -> Position -> Position
setLimits PortfolioConfig{pfDefaultLimit, pfDefaultStopLoss} position@Position{pLastPrice} =
  position
    & set stopLoss (Just $ pfDefaultStopLoss `scale` pLastPrice)
    & set limit (Just $ pfDefaultLimit `scale` pLastPrice)

-- | Whether the stop loss of the position has been triggered
triggerStopLoss :: Position -> Bool
triggerStopLoss Position{pStopLoss, pLastPrice} =
  maybe False (\sl -> sl > pLastPrice) pStopLoss

-- | Whether the limit of the position has been triggered
triggerLimit :: Position -> Bool
triggerLimit Position{pLimit, pLastPrice} =
  maybe False (\l -> l < pLastPrice) pLimit

{-| Simulated portfolio for testing
-}
data Portfolio = Portfolio
  { _positions  :: !(Map (PolicyId, AssetName) Position)
  , _tradeCount :: !Int
  }

makeLenses ''Portfolio

-- | Create a buy order if it conforms with the portfolio configuration
buyOrder :: PortfolioConfig -> Value -> Portfolio -> PolicyId -> AssetName -> Maybe BuyOrder
buyOrder PortfolioConfig{pfMaxPositionSize, pfMinPositionSize} vl portfolio p a = do
  let Lovelace totalAum          = aum vl portfolio
      Lovelace currentAllocation = allocatedTo vl portfolio p a
      maxAllocation = pfMaxPositionSize * fromIntegral totalAum
      newPositionSize = Lovelace (floor (maxAllocation - fromIntegral currentAllocation))
  guard (newPositionSize > 0)
  guard (newPositionSize > pfMinPositionSize)
  Position{pLastPrice} <- view (position p a) portfolio
  guard (pLastPrice > 0)
  let units = unitsOf newPositionSize pLastPrice
  pure
    BuyOrder
      { buyCurrency = (p, a)
      , buyQuantity = units
      , buyPrice = pLastPrice
      }

-- TODO: don't need PricePoint in sellOrder

sellOrder :: Value -> Portfolio -> PricePoint -> SellOrder
sellOrder vl _portfolio (p, a, q, l) =
  let currentSize = C.selectAsset vl (AssetId p a)
      price = unitPrice q l
      -- pos = view (position p a) portfolio
  in SellOrder{sellCurrency = (p, a), sellQuantity = currentSize, sellPrice = price }

-- | Update the portfolio with the price point
updatePrice :: PortfolioConfig -> Value -> Portfolio -> PricePoint -> (Maybe SellOrder, Portfolio)
updatePrice config vl portfolio (p, a, q, l) = flip runState portfolio $ do
  let currentSize = C.selectAsset vl (AssetId p a)
      price = unitPrice q l
  pos <- use (position p a)
  case pos of
    -- first time we are seeing this currency, make a new position.
    Nothing -> do
      let pos' = initPosition price
          pos'' = if currentSize > 0 then (setLimits config pos') else pos'
      position p a .= Just pos''
      pure Nothing
    -- update the position
    Just pos_
      | currentSize > 0 && isJust (pStopLoss pos_) -> do
          let pos' = updatePosition q l pos_
          position p a .= Just pos'
          if (triggerStopLoss pos' || triggerLimit pos')
            then pure $ Just $ SellOrder{sellCurrency = (p, a), sellQuantity = currentSize, sellPrice = price }
            else pure Nothing
      | currentSize > 0 || isNothing (pStopLoss pos_) -> do
          let pos' = updatePosition q l (setLimits config pos_)
          position p a .= Just pos'
          pure Nothing
      | otherwise -> do
          let pos' = initPosition price
          position p a .= Just pos'
          pure Nothing

{-| Initialise the portfolio with the amount of Ada available for purchases
-}
emptyPortfolio :: Portfolio
emptyPortfolio = Portfolio{_positions = mempty, _tradeCount = 0}

{-| Distribution of non-Ada assets, priced in Ada
-}
distribution ::
  Value -- ^ Sum of all assets in the portfolio
  -> Portfolio -- ^ List of positions and prices
  -> Map AssetId Lovelace
distribution value Portfolio{_positions} =
  let getPrice assetId = maybe 0 pLastPrice (Map.lookup assetId _positions)
      k (AssetId p a, quantity) = (AssetId p a, valueOf quantity (getPrice (p, a)))
      k (AdaAssetId, Quantity quantity) = (AdaAssetId, Lovelace quantity)
  in Map.fromList
  $ fmap k (C.valueToList value)

-- | Total "assets under management"
aum :: Value -> Portfolio -> Lovelace
aum vl pf = fold (distribution vl pf)

{-| Lovelace amount allocated to the native asset
-}
allocatedTo :: Value -> Portfolio -> PolicyId -> AssetName -> Lovelace
allocatedTo vl Portfolio{_positions} p a = fromMaybe 0 $ do
  Position{pLastPrice} <- Map.lookup (p, a) _positions
  let q = C.selectAsset vl (AssetId p a)
  pure (valueOf q pLastPrice)

updatePosition :: Quantity -> Lovelace -> Position -> Position
updatePosition q l p =
  let oldPrice = view lastPrice p
      newPrice = unitPrice q l
      diff   = max 0 (newPrice - oldPrice)
  in p
        & lastPrice .~ newPrice
        -- update the stop loss (trailing stop loss)
        & over (stopLoss . _Just) (+ diff)

position :: PolicyId -> AssetName -> Lens' Portfolio (Maybe Position)
position p a = positions . at (p, a)

formatAda :: Lovelace -> String
formatAda (Lovelace v) = show (round @Double @Integer (fromIntegral v / (1_000_000 :: Double)))

closePositionMsg :: AssetName -> Lovelace -> Lovelace -> String
closePositionMsg pAssetName (Lovelace marketPrice) (Lovelace purchasePrice) =
  let percChange = 100 * (marketPrice - purchasePrice) % purchasePrice
      mvAda = formatAda (Lovelace marketPrice)
  in "SELL "
      <> show pAssetName
      <> " for "
      <> mvAda
      <> " Ada (" <> show (round @Rational @Integer percChange) <> "%)"

printPortfolioInfo :: (MonadLog m) => Value -> Portfolio -> m ()
printPortfolioInfo vl p@Portfolio{_positions, _tradeCount}= do
  let lvl = aum vl p
      numPos = pred (length $ C.valueToList vl)
  logInfoS $ "Portfolio value: " <> formatAda lvl <> " with " <> show numPos <> " native assets and " <> formatAda (C.selectLovelace vl) <> " Ada in cash. Made " <> show _tradeCount <> " trades."

{-| How much Ada we can spend on a particular asset, considering
* the amount already invested in this asset
* the available cash
* the configuration in 'PortfolioConfig'
-}
availableFunds :: PortfolioConfig -> Value -> Portfolio -> PolicyId -> AssetName -> Lovelace
availableFunds PortfolioConfig{pfMaxPositionSize} vl portfolio p a =
  let Lovelace totalLvl = aum vl portfolio
      Lovelace alloc    = allocatedTo vl portfolio p a
      Lovelace cash     = C.selectLovelace vl
      maxAvailable = pfMaxPositionSize * fromIntegral totalLvl
      remaining    = min cash (floor (max 0 (maxAvailable - fromIntegral alloc)))
  in  Lovelace remaining

type Confidence = Double

class Monad m => MonadTrade m where
  update :: PricePoint -> m () -- ^ Update the positions with new pricing data
  buy    :: Confidence -> PricePoint -> m ()
  sell   :: Confidence -> PricePoint -> m ()

newtype SimulatedPortfolioT m a = SimulatedPortfolioT{ unSimulatedPortfolioT :: ReaderT PortfolioConfig (StateT (Portfolio, Value) m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLog)

runSimulatedPortfolioT :: PortfolioConfig -> (Portfolio, Value) -> SimulatedPortfolioT m a -> m (a, (Portfolio, Value))
runSimulatedPortfolioT config portfolio SimulatedPortfolioT{unSimulatedPortfolioT} =
  runStateT (runReaderT unSimulatedPortfolioT config ) portfolio

execSimulatedPortfolioT :: Functor m => PortfolioConfig -> (Portfolio, Value) -> SimulatedPortfolioT m a -> m (Portfolio, Value)
execSimulatedPortfolioT config portfolio = fmap snd <$> runSimulatedPortfolioT config portfolio

instance Monad m => MonadTrade (SimulatedPortfolioT m) where
  update pricePoint = SimulatedPortfolioT $ do
    (p, v) <- get
    c <- ask
    let (sellOrder', p') = updatePrice c v p pricePoint
    put (p', v)
    traverse_ applySellOrder sellOrder'

  sell _confidence pricePoint = SimulatedPortfolioT $ do
    (p, v) <- get
    applySellOrder (sellOrder v p pricePoint)

  buy _confidence (p, a, _, _) = SimulatedPortfolioT $ do
    (portfolio, v) <- get
    c <- ask
    let order = buyOrder c v portfolio p a
    traverse_ applyBuyOrder order


applySellOrder :: MonadState (Portfolio, Value) m => SellOrder -> m ()
applySellOrder SellOrder{sellCurrency, sellQuantity, sellPrice} = do
  oldV <- use _2
  let newV = oldV <> C.lovelaceToValue (valueOf sellQuantity sellPrice) <> C.negateValue (C.valueFromList [(uncurry C.AssetId sellCurrency, sellQuantity)])
  when (all (\q -> q > 0) $ fmap snd $ C.valueToList newV) $ do
    _2 .= newV
    _1 . tradeCount += 1

applyBuyOrder :: MonadState (Portfolio, Value) m => BuyOrder -> m ()
applyBuyOrder BuyOrder{buyCurrency, buyQuantity, buyPrice} = do
  oldV <- use _2
  let newV = oldV <> C.negateValue (C.lovelaceToValue (valueOf buyQuantity buyPrice)) <> (C.valueFromList [(uncurry C.AssetId buyCurrency, buyQuantity)])
  when (all (\q -> q > 0) $ fmap snd $ C.valueToList newV) $ do
    _2 .= newV
    _1 . tradeCount += 1
