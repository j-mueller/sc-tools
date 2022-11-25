{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Using simulated annealing to find a good set of trading rules
-}
module Convex.TradingBot.Annealing(
  runBacktestNode,
  optimiseRules,
  runAnnealing
) where

import           Annealing                                 (AnnealingState (..),
                                                            AssessedCandidate (..),
                                                            annealing)
import           Cardano.Api                               (AssetName, Lovelace,
                                                            PolicyId, Quantity)
import qualified Cardano.Api                               as C
import qualified Control.Concurrent.STM                    as STM
import           Control.Exception                         (bracket)
import           Control.Lens                              (Lens', anon, at,
                                                            makeLenses, use,
                                                            (%=))
import           Control.Monad                             (when)
import           Control.Monad.IO.Class                    (MonadIO (..))
import           Control.Monad.Primitive                   (PrimState)
import           Control.Monad.State.Strict                (evalStateT)
import           Control.Monad.Trans.Class                 (lift)
import           Control.Monad.Trans.Except                (ExceptT (..),
                                                            runExceptT)
import           Convex.MonadLog                           (MonadLog,
                                                            MonadLogKatipT (..),
                                                            logInfoS, logWarnS)
import           Convex.NodeClient.Types                   (runNodeClient)
import qualified Convex.TradingBot.NodeClient              as NC
import           Convex.TradingBot.NodeClient.PricesClient (PriceEventRow (..))
import           Convex.TradingBot.Portfolio               (Portfolio,
                                                            defaultPortfolioConfig)
import qualified Convex.TradingBot.Portfolio               as Portfolio
import           Convex.TradingBot.Prices                  (LPPrices)
import qualified Convex.TradingBot.Prices                  as Prices
import           Convex.TradingBot.Rules                   (MomentumRule (..),
                                                            Rule,
                                                            defaultMomentum)
import qualified Convex.TradingBot.Rules                   as Rules
import           Convex.Wallet.Cli.Config                  (Config (..),
                                                            ConfigMode (..))
import           Data.Foldable                             (traverse_)
import           Data.Map.Strict                           (Map)
import qualified Data.Map.Strict                           as Map
import           Data.Maybe                                (fromMaybe)
import qualified Data.Text                                 as Text
import qualified Katip                                     as K
import qualified Streaming
import           Streaming                                 (Of, Stream)
import           Streaming.Cassava                         (CsvParseException,
                                                            decodeByNameWith,
                                                            defaultDecodeOptions)
import qualified Streaming.Prelude                         as S
import           Streaming.With                            (withBinaryFileContents)
import           System.Exit                               (exitFailure)
import           System.IO                                 (stdout)
import qualified System.Random.MWC.Probability             as P
import           System.Random.MWC.Probability             (Gen)

data Prices =
  Prices
    { _priceHistory :: !(Map (PolicyId, AssetName) LPPrices)
    , _lastPrices   :: !(Map (PolicyId, AssetName) (Lovelace, Quantity))
    }

makeLenses ''Prices

runBacktestNode :: (MonadLog m, MonadIO m) => Rule -> K.LogEnv -> Config 'Typed -> m (Portfolio, C.Value)
runBacktestNode rule logEnv Config{cardanoNodeConfigFile, cardanoNodeSocket} = do
  logInfoS "Starting backtest"
  tv <- liftIO STM.newEmptyTMVarIO
  result <- liftIO $ do
    let makeLogEnv = do
          backtestingWorkerScribe <- K.mkHandleScribe (K.ColorLog True) stdout (K.permitItem K.NoticeS) K.V2
          K.registerScribe "stdout-backtesting-worker" backtestingWorkerScribe K.defaultScribeSettings logEnv
    bracket makeLogEnv K.closeScribes $ \le ->
      let client C.LocalNodeConnectInfo{C.localNodeNetworkId} env = do
            pure (NC.backtestingClient rule tv le "backtesting" localNodeNetworkId env)
      in runExceptT (runNodeClient cardanoNodeConfigFile cardanoNodeSocket client)
  case result of
    Left err -> do
      logWarnS (Text.unpack $ C.renderInitialLedgerStateError err)
      liftIO exitFailure
    Right () -> do
      logInfoS "Backtesting finished"
      liftIO (STM.atomically (STM.takeTMVar tv))

initialValue :: MomentumRule
initialValue = defaultMomentum

runBacktest :: FilePath -> MomentumRule -> (ExceptT CsvParseException IO) (Portfolio, C.Value)
runBacktest fp rule =
  let options = defaultDecodeOptions
  in withBinaryFileContents fp
      $ evalPriceEvents rule
      . decodeByNameWith options

pricesFor :: (PolicyId, AssetName) -> Lens' Prices LPPrices
pricesFor k = priceHistory . at k . anon Prices.empty Prices.null

evalPriceEvents :: Monad m => MomentumRule -> Stream (Of PriceEventRow) m () -> m (Portfolio, C.Value)
evalPriceEvents (Rules.mkRule -> rule) stream = do
  let portf = (Portfolio.emptyPortfolio, C.lovelaceToValue 3_000_000_000)
      init_ :: Prices
      init_ = Prices mempty mempty
  flip evalStateT init_ $ Portfolio.execSimulatedPortfolioT defaultPortfolioConfig portf $ do
    flip S.mapM_ (Streaming.hoist (lift . lift) stream) $ \PriceEventRow{peBlockNo, peSlot, pePolicyId, peAssetName, peQuantity, peLovelace} -> do
      let k = (pePolicyId, peAssetName)
          newPrice = (peLovelace, peQuantity)
      oldPrices <- use lastPrices
      let oldPrice = Map.findWithDefault (0, 0) k oldPrices
          pAt = Prices.pricesAt peBlockNo peSlot oldPrice newPrice
          p = fromMaybe mempty $ Prices.price $ Prices._stats pAt
      when (Prices.pmVolume p > 0) $ do
        pricesFor k %= Prices.prepend pAt
        newPrices' <- use (pricesFor k)
        let pricePoint = (pePolicyId, peAssetName, peQuantity, peLovelace)
        Portfolio.update pricePoint
        case rule newPrices' of
          Rules.Buy  -> Portfolio.buy 1.0 pricePoint
          Rules.Sell -> Portfolio.sell 1.0 pricePoint
          _          -> pure ()

{-| Evaluate the rule against the price events in the CSV file
-}
evaluateRule :: FilePath -> K.LogEnv -> MomentumRule -> IO (Portfolio, C.Value)
evaluateRule csvFile logEnv rule = K.runKatipContextT logEnv () "test-rule" $ runMonadLogKatipT $ do
  liftIO $ runExceptT (runBacktest csvFile rule) >>= either (error . show) pure

fitness :: (Portfolio, C.Value) -> Double
fitness (portfolio, vl) =
  let C.Lovelace totalVal = Portfolio.aum vl portfolio
  in if (totalVal /= 0)
      then (1 / fromIntegral totalVal)
      else 1

{-| Randomly change the values of the rule
-}
search :: Gen (PrimState IO) -> MomentumRule -> IO MomentumRule
search gen MomentumRule{rdBuy, rdSell, rdLookbackBig, rdLookbackSmall} = do
  let sv = 0.01
  buy' <- max 0.1 <$> P.sample (P.normal rdBuy sv) gen
  sell' <- max 0.1 <$> P.sample (P.normal rdSell sv) gen
  pure
    $ MomentumRule
        { rdBuy  = max buy' sell'
        , rdSell = min buy' sell'
        , rdLookbackBig
        , rdLookbackSmall
        }

optimiseRules :: MonadIO m => FilePath -> K.LogEnv -> m (Stream (Of (AnnealingState (AssessedCandidate MomentumRule (Portfolio, C.Value)))) IO ())
optimiseRules fp logEnv = do
  gen <- liftIO P.createSystemRandom
  let temp i = 1 + (50 / fromIntegral i)
  initial <- liftIO (evaluateRule fp logEnv initialValue)
  return $
    annealing
      gen
      temp
      (evaluateRule fp logEnv)
      fitness
      (AssessedCandidate initialValue initial (fitness initial))
      (search gen)

runAnnealing :: (MonadIO m, MonadLog m) => K.LogEnv -> FilePath -> m ()
runAnnealing logEnv filePath = do
  logInfoS "Starting annealing"
  stream <- optimiseRules filePath logEnv
  result <- liftIO (S.last_ (S.take 500 stream))
  flip traverse_ result $ \AnnealingState{asBestCandidate, asCurrentCandidate} -> do
    let p AssessedCandidate{acCandidate, acResult=(pf, v)} = do
          logInfoS (show acCandidate)
          Portfolio.printPortfolioInfo v pf
    logInfoS $ "Best result:"
    p asBestCandidate

    logInfoS $ "Current result:"
    p asCurrentCandidate
