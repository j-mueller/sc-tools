{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
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
import           Control.Monad                             (void, when)
import           Control.Monad.IO.Class                    (MonadIO (..))
import           Control.Monad.Primitive                   (PrimMonad,
                                                            PrimState)
import           Control.Monad.State.Strict                (evalStateT)
import           Control.Monad.Trans.Class                 (lift)
import           Control.Monad.Trans.Except                (ExceptT (..),
                                                            runExceptT)
import           Convex.MonadLog                           (MonadLog,
                                                            MonadLogIgnoreT (..),
                                                            MonadLogKatipT (..),
                                                            logInfoS, logWarnS)
import           Convex.NodeClient.Types                   (runNodeClient)
import qualified Convex.TradingBot.NodeClient              as NC
import           Convex.TradingBot.NodeClient.PricesClient (PriceEventRow (..))
import           Convex.TradingBot.Portfolio               (Portfolio,
                                                            PortfolioConfig (..))
import qualified Convex.TradingBot.Portfolio               as Portfolio
import           Convex.TradingBot.Prices                  (LPPrices)
import qualified Convex.TradingBot.Prices                  as Prices
import           Convex.TradingBot.Rules                   (Rule,
                                                            SearchableRule (..))
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
import           Streaming.With                            (MonadMask,
                                                            withBinaryFileContents)
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

runBacktest :: (MonadIO m, MonadMask m, MonadLog m, SearchableRule r) => PortfolioConfig -> Lovelace -> FilePath -> r -> (ExceptT CsvParseException m) (Portfolio, C.Value)
runBacktest config initialAda fp rule =
  let options = defaultDecodeOptions
  in withBinaryFileContents fp
      $ evalPriceEvents config initialAda rule
      . decodeByNameWith options

pricesFor :: (PolicyId, AssetName) -> Lens' Prices LPPrices
pricesFor k = priceHistory . at k . anon Prices.empty Prices.null

evalPriceEvents :: (SearchableRule r, Monad m, MonadLog m) => PortfolioConfig -> Lovelace -> r -> Stream (Of PriceEventRow) m () -> m (Portfolio, C.Value)
evalPriceEvents config initialAda (signal -> rule) stream = do
  let portf = (Portfolio.emptyPortfolio, C.lovelaceToValue initialAda)
      init_ :: Prices
      init_ = Prices mempty mempty
  flip evalStateT init_ $ Portfolio.execSimulatedPortfolioT config portf $ Portfolio.runLoggingPortfolioT $ do
    flip S.mapM_ (Streaming.hoist (lift . lift . lift) stream) $ \PriceEventRow{peBlockNo, peSlot, pePolicyId, peAssetName, peQuantity, peLovelace} -> do
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
        void (Portfolio.update pricePoint)
        case rule newPrices' of
          Rules.Buy  -> void (Portfolio.buy 1.0 pricePoint)
          Rules.Sell -> void (Portfolio.sell 1.0 pricePoint)
          _          -> pure ()

{-| Evaluate the rule against the price events in the CSV file
-}
evaluateRuleL :: SearchableRule r => Lovelace -> FilePath -> K.LogEnv -> r -> PortfolioConfig -> IO (Portfolio, C.Value)
evaluateRuleL initialAda csvFile logEnv rule config = K.runKatipContextT logEnv () "test-rule" $ runMonadLogKatipT $ do
  runExceptT (runBacktest config initialAda csvFile rule) >>= either (error . show) pure

{-| Evaluate the rule against the price events in the CSV file
-}
evaluateRuleNL :: SearchableRule r => Lovelace -> FilePath -> r -> PortfolioConfig -> IO (Portfolio, C.Value)
evaluateRuleNL initialAda csvFile rule config = runMonadLogIgnoreT $ do
  runExceptT (runBacktest config initialAda csvFile rule) >>= either (error . show) pure

fitness :: (Portfolio, C.Value) -> Double
fitness (portfolio, vl) =
  let C.Lovelace totalVal = Portfolio.aum vl portfolio
  in if (totalVal /= 0)
      then (1 / fromIntegral totalVal)
      else 1

optimiseRules :: forall r m.
  (SearchableRule r, MonadIO m) =>
  r ->
  FilePath ->
  K.LogEnv ->
  m ( Stream (Of (AnnealingState (AssessedCandidate (r, PortfolioConfig) (Portfolio, C.Value)))) IO ()
    , AssessedCandidate (r, PortfolioConfig) (Portfolio, C.Value)
    , Lovelace
    )
optimiseRules i fp logEnv = do
  gen <- liftIO P.createSystemRandom
  let initialAda = 3_000_000_000
      config = Portfolio.defaultPortfolioConfig
  initial <- liftIO (evaluateRuleL initialAda fp logEnv i config)
  let temp j = 1 + (1 / fromIntegral j)
      cand = AssessedCandidate (i, config) initial (fitness initial)
      stream =
        annealing
          gen
          temp
          (uncurry (evaluateRuleNL initialAda fp))
          fitness
          cand
          (\(r, cfg) -> (,) <$> neighbours gen r <*> searchPortfolioConfig gen cfg)
  return (stream, cand, initialAda)

runAnnealing :: (MonadIO m, MonadLog m) => K.LogEnv -> FilePath -> m ()
runAnnealing logEnv filePath = do
  logInfoS "Starting annealing"
  let r = Rules.twoMovingAverages
  -- let r = Rules.movingAverages
  (stream, initialRule, C.Lovelace initialAda) <- optimiseRules r filePath logEnv
  result <- liftIO (S.last_ (S.take 2_000 stream))
  flip traverse_ result $ \AnnealingState{asBestCandidate, asCurrentCandidate} -> do
    let p (AssessedCandidate{acCandidate, acResult=(pf, v)}) = do
          logInfoS (show acCandidate)
          let C.Lovelace currentVal = Portfolio.aum v pf
              diff = currentVal - initialAda
              percent = (100 :: Double) * (fromIntegral diff / fromIntegral initialAda)
          Portfolio.printPortfolioInfo v pf
          logInfoS $ "Value change: " <> show percent <> "%"
    logInfoS "Initial value:"
    p initialRule

    logInfoS $ "Best result:"
    p asBestCandidate

    logInfoS $ "Current result:"
    p asCurrentCandidate

searchPortfolioConfig :: (PrimMonad m) => Gen (PrimState m) -> PortfolioConfig -> m PortfolioConfig
searchPortfolioConfig gen cfg = do
  let sv = 0.05
  stopLoss <- max 0.1 . (\i -> toRational (1000 * i) / 1000) <$> P.sample (P.normal (fromRational $ pfDefaultStopLoss cfg) sv) gen
  defLimit <- max 0.1 . (\i -> toRational (1000 * i) / 1000) <$> P.sample (P.normal (fromRational $ pfDefaultLimit cfg) sv) gen
  pure cfg{pfDefaultStopLoss = stopLoss, pfDefaultLimit = defLimit}
