{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
module Convex.TradingBot.Portfolio(
  MonadTrade(..),

  -- * Portfolios
  Position(..),
  Portfolio(..),
  printPortfolioInfo,
  emptyPortfolio,
  distribution,
  tradeCount,
  PortfolioConfig(..),
  defaultPortfolioConfig,

  -- ** Simulation / Backtesting
  SimulatedPortfolioT,
  runSimulatedPortfolioT,
  execSimulatedPortfolioT
) where

import           Cardano.Api                (AssetId (..), AssetName,
                                             Lovelace (..), PolicyId,
                                             Quantity (..))
import           Control.Lens               (Lens', _Just, anon, at, makeLenses,
                                             makeLensesFor, over, traversed,
                                             use, view, (%=), (&), (+=), (-=),
                                             (.=), (.~))
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader       (MonadReader (..), ReaderT,
                                             runReaderT)
import           Control.Monad.State.Strict (MonadState (..), StateT, gets,
                                             runStateT)
import           Convex.MonadLog            (MonadLog, logInfoS, logWarnS)
import           Data.Foldable              (toList)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Ratio                 ((%))
import           Data.Sequence              (Seq, (|>))
import qualified Data.Sequence              as Seq

type Confidence = Rational
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

{-| Buying and selling native tokens
-}
class Monad m => MonadTrade m where
  update :: PricePoint -> m () -- ^ Update the positions with new pricing data
  buy    :: Confidence -> PricePoint -> m ()
  sell   :: Confidence -> PricePoint -> m ()

data Position =
  Position
    { pPolicyId      :: !PolicyId
    , pAssetName     :: !AssetName
    , pQuantity      :: !Quantity
    , pPurchasePrice :: !Lovelace
    , pLastPrice     :: !Lovelace -- ^ last market price
    , pStopLoss      :: Maybe Lovelace
    , pLimit         :: Maybe Lovelace
    }

makeLensesFor
  [ ("pLastPrice", "lastPrice")
  , ("pStopLoss", "stopLoss")
  ]
  ''Position

triggerStopLoss :: Position -> Bool
triggerStopLoss Position{pStopLoss, pLastPrice} =
  maybe False (\sl -> sl > pLastPrice) pStopLoss

triggerLimit :: Position -> Bool
triggerLimit Position{pLimit, pLastPrice} =
  maybe False (\l -> l < pLastPrice) pLimit

{-| Simulated portfolio for testing
-}
data Portfolio = Portfolio
  { _positions  :: !(Map (PolicyId, AssetName) (Seq Position))
  , _ada        :: !Lovelace
  , _tradeCount :: !Int
  }

makeLenses ''Portfolio

{-| Initialise the portfolio with the amount of Ada available for purchases
-}
emptyPortfolio :: Lovelace -> Portfolio
emptyPortfolio _ada = Portfolio{_positions = mempty, _ada, _tradeCount = 0}

{-| Distribution of assets priced in Ada
-}
distribution :: Portfolio -> Map AssetId Lovelace
distribution Portfolio{_positions, _ada} =
  let k ((pol, as), positions_) = ((AssetId pol as), (foldMap pLastPrice positions_))
  in Map.fromList $ (AdaAssetId, _ada) : (k <$> Map.toList _positions)

aum :: Portfolio -> Lovelace
aum Portfolio{_positions, _ada} = _ada + foldMap (foldMap pLastPrice) _positions

{-| Lovelace amount allocated to the native asset
-}
allocatedTo :: PolicyId -> AssetName -> Portfolio -> Lovelace
allocatedTo p a = maybe 0 (foldMap pLastPrice) . Map.lookup (p, a) . _positions

{-| Price of one unit of the native token in Ada
-}
unitPrice :: Quantity -> Lovelace -> Rational
unitPrice (Quantity q) (Lovelace l) = l % q

marketValue :: Rational -> Quantity -> Lovelace
marketValue price (Quantity q) =
  Lovelace $ floor $ fromIntegral q * price

markToMarket :: Quantity -> Lovelace -> Position -> Position
markToMarket q l p =
  let oldVal = view lastPrice p
      newVal = marketValue (unitPrice q l) (pQuantity p)
      diff   = max 0 (newVal - oldVal)
  in p
        & lastPrice .~ newVal
        -- update the stop loss (trailing stop loss)
        & over (stopLoss . _Just) (+ diff)

-- TODO: Wallet trade,
-- * pending positions:
--     - submitting an order returns a pair of TxIns. The first item is currently in the UTxO set. The second item is not in the UTxO set. When the second UTxO appears in the set, the order is complete. When the first UTxO disappears, the order is cancelled.
--     - watch the wallet's UTxOs until the TxIn appears, then mark the order as completed or cancelled.

-- TODO: Separate market pricing info from position (so that we don't need to change the position entries in the map)

newtype SimulatedPortfolioT m a = SimulatedPortfolioT{ unSimulatedPortfolioT :: ReaderT PortfolioConfig (StateT Portfolio m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLog)

instance MonadLog m => MonadTrade (SimulatedPortfolioT m) where
  update (policyId, assetName, q, l) = do
    shouldSell <- SimulatedPortfolioT $ do
      position policyId assetName . traversed %= markToMarket q l
      stopLosses <- filter triggerStopLoss . toList <$> use (position policyId assetName)
      limits <- filter triggerLimit . toList <$> use (position policyId assetName)
      when (not $ null stopLosses) $ logInfoS $ "  Stop loss triggered for " <> show assetName
      when (not $ null limits) $ logInfoS $ "  Limit triggered for " <> show assetName
      return $ not $ null stopLosses && null limits
    when shouldSell $ sell 1 (policyId, assetName, q, l)

  buy confidence (pPolicyId, pAssetName, q, l) = SimulatedPortfolioT $ do
    PortfolioConfig{pfDefaultLimit, pfDefaultStopLoss, pfMinPositionSize} <- ask
    Lovelace maxPositionSize <- availableFunds pPolicyId pAssetName
    let -- size of the position in Lovelace
        actualPositionSize = confidence * fromIntegral maxPositionSize

        unitsPurchased = actualPositionSize / unitPrice q l

        purchasePrice = unitsPurchased * unitPrice q l

        pStopLoss = Just (Lovelace $ round $ pfDefaultStopLoss * purchasePrice)

        pLimit = Just (Lovelace $ round $ pfDefaultLimit * purchasePrice)

        newPosition =
          Position
            { pPolicyId
            , pAssetName
            , pQuantity      = Quantity (floor unitsPurchased)
            , pPurchasePrice = Lovelace (round purchasePrice)
            , pLastPrice     = Lovelace (round purchasePrice)
            , pStopLoss
            , pLimit
            }

    Lovelace available <- use ada
    if (available >= round purchasePrice)
      then do
        let p = Lovelace (round purchasePrice)
        when (p >= pfMinPositionSize) $ do
          position pPolicyId pAssetName %= (|> newPosition)
          ada -= p
          tradeCount += 1
          logInfoS $ "BUY: " <> show (round @_ @Integer unitsPurchased) <> " " <> show pAssetName <> " for " <> formatAda p
      else do
        -- shouldn't happen!
        logWarnS "WARNING: purchase price too high"

  sell _ (policyId, assetName, q, l) = SimulatedPortfolioT $ do
    pos <- use (position policyId assetName)
    unless (Seq.null pos) $ do
      let currentPrice = unitPrice q l
          marketVal     = foldMap (marketValue currentPrice . pQuantity) pos
          purchasePrice = foldMap pPurchasePrice pos

      ada += marketVal
      logInfoS (closePositionMsg assetName marketVal purchasePrice)
      position policyId assetName .= Seq.empty
      tradeCount += 1
      get >>= printPortfolioInfo

position :: PolicyId -> AssetName -> Lens' Portfolio (Seq Position)
position p a = positions . at (p, a) . anon Seq.empty Seq.null

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

printPortfolioInfo :: (MonadLog m) => Portfolio -> m ()
printPortfolioInfo p@Portfolio{_positions, _ada, _tradeCount}= do
  let lvl = aum p
      numPos = Map.size _positions
  logInfoS $ "Portfolio value: " <> formatAda lvl <> " with " <> show numPos <> " native assets and " <> formatAda _ada <> " Ada in cash. Made " <> show _tradeCount <> " trades."

{-| How much Ada we can spend on a particular asset, considering
* the amount already invested in this asset
* the available cash
* the configuration in 'PortfolioConfig'
-}
availableFunds :: (MonadReader PortfolioConfig m, MonadState Portfolio m) => PolicyId -> AssetName -> m Lovelace
availableFunds p a = do
  PortfolioConfig{pfMaxPositionSize} <- ask
  Lovelace totalLvl <- gets aum
  Lovelace alloc    <- gets (allocatedTo p a)
  Lovelace cash     <- use ada
  let maxAvailable = pfMaxPositionSize * fromIntegral totalLvl
      remaining    = min cash (floor (max 0 (maxAvailable - fromIntegral alloc)))
  pure (Lovelace remaining)

runSimulatedPortfolioT :: PortfolioConfig -> Portfolio -> SimulatedPortfolioT m a -> m (a, Portfolio)
runSimulatedPortfolioT config portfolio SimulatedPortfolioT{unSimulatedPortfolioT} =
  runStateT (runReaderT unSimulatedPortfolioT config ) portfolio

execSimulatedPortfolioT :: Functor m => PortfolioConfig -> Portfolio -> SimulatedPortfolioT m a -> m Portfolio
execSimulatedPortfolioT config portfolio = fmap snd <$> runSimulatedPortfolioT config portfolio
