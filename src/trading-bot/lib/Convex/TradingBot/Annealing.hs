{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Using simulated annealing to find a good set of trading rules
-}
module Convex.TradingBot.Annealing(
  runBacktest,
  optimiseRules,
  runAnnealing
) where

import           Annealing                     (AnnealingState (..),
                                                AssessedCandidate (..),
                                                annealing)
import qualified Cardano.Api                   as C
import qualified Control.Concurrent.STM        as STM
import           Control.Exception             (bracket)
import           Control.Monad.Primitive       (PrimState)
import           Control.Monad.Trans.Except    (runExceptT)
import           Convex.MonadLog               (MonadLog, MonadLogKatipT (..),
                                                logInfoS, logWarnS)
import           Convex.NodeClient.Types       (runNodeClient)
import qualified Convex.TradingBot.NodeClient  as NC
import           Convex.TradingBot.Portfolio   (Portfolio)
import qualified Convex.TradingBot.Portfolio   as Portfolio
import           Convex.TradingBot.Rules       (MomentumRule (..), Rule)
import qualified Convex.TradingBot.Rules       as Rules
import           Convex.Wallet.Cli.Config      (Config (..), ConfigMode (..))
import           Data.Foldable                 (traverse_)
import qualified Data.Text                     as Text
import qualified Katip                         as K
import           Streaming
import qualified Streaming.Prelude             as S
import           System.Exit                   (exitFailure)
import           System.IO                     (stdout)
import qualified System.Random.MWC.Probability as P
import           System.Random.MWC.Probability (Gen)

runBacktest :: (MonadLog m, MonadIO m) => Rule -> K.LogEnv -> Config 'Typed -> m (Portfolio, C.Value)
runBacktest rule logEnv Config{cardanoNodeConfigFile, cardanoNodeSocket} = do
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
initialValue = MomentumRule 1.05 0.95

{-|
-}
testRule :: K.LogEnv -> Config 'Typed -> MomentumRule -> IO Double
testRule logEnv config (Rules.mkRule -> rule) = K.runKatipContextT logEnv () "test-rule" $ runMonadLogKatipT $ do
  (portfolio, vl) <- runBacktest rule logEnv config
  let C.Lovelace totalVal = Portfolio.aum vl portfolio
  if (totalVal /= 0)
    then pure (1 / fromIntegral totalVal)
    else pure 1

{-| Randomly change the values of the rule
-}
search :: Gen (PrimState IO) -> MomentumRule -> IO MomentumRule
search gen MomentumRule{rdBuy, rdSell} = do
  let diff = rdBuy - rdSell
  buy' <- max 0.1 <$> P.sample (P.normal rdBuy diff) gen
  sell' <- max 0.1 <$> P.sample (P.normal rdSell diff) gen
  pure $ MomentumRule (max buy' sell') (min buy' sell')

optimiseRules :: MonadIO m => K.LogEnv -> Config 'Typed -> m (Stream (Of (AnnealingState (AssessedCandidate MomentumRule))) IO ())
optimiseRules logEnv config = do
  gen <- liftIO P.createSystemRandom
  let temp i = 1 + (1 / fromIntegral i)
  initial <- liftIO (testRule logEnv config initialValue)
  return $
    annealing
      gen
      temp
      (testRule logEnv config)
      (AssessedCandidate initialValue initial)
      (search gen)

runAnnealing :: (MonadIO m, MonadLog m) => K.LogEnv -> Config 'Typed -> m ()
runAnnealing logEnv config = do
  stream <- optimiseRules logEnv config
  result <- liftIO (S.last_ (S.take 10 stream))
  flip traverse_ result $ \AnnealingState{asBestCandidate, asCurrentCandidate} -> do
    let p AssessedCandidate{acFitness, acCandidate} = show acCandidate <> " (" <> show acFitness <> ")"
    logInfoS $ "Best result:    " <> p asBestCandidate
    logInfoS $ "Current result: " <> p asCurrentCandidate
