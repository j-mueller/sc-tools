{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Convex.TradingBot.Cli (runMain) where

import qualified Cardano.Api                   as C
import qualified Control.Concurrent.STM        as STM
import           Control.Exception             (bracket)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Trans.Except    (runExceptT)
import           Convex.MonadLog               (MonadLog, MonadLogKatipT (..),
                                                logInfoS, logWarnS)
import           Convex.NodeClient.Types       (runNodeClient)
import           Convex.TradingBot.Cli.Command (CliCommand (..), commandParser)
import           Convex.TradingBot.Cli.Config  (Config (..), ConfigMode (..))
import qualified Convex.TradingBot.Cli.Config  as Config
import qualified Convex.TradingBot.NodeClient  as NC
import           Convex.TradingBot.Portfolio   (Portfolio, printPortfolioInfo)
import qualified Data.Text                     as Text
import qualified Katip                         as K
import           Options.Applicative           (customExecParser, disambiguate,
                                                helper, idm, info, prefs,
                                                showHelpOnEmpty,
                                                showHelpOnError)
import           System.Exit                   (exitFailure)
import           System.IO                     (stdout)

runMain :: IO ()
runMain = do
  mainScribe <- K.mkHandleScribe (K.ColorLog True) stdout (K.permitItem K.InfoS) K.V2
  initLogEnv <- K.initLogEnv "trading-bot" "cli"
  let makeLogEnv = K.registerScribe "stdout-main" mainScribe K.defaultScribeSettings initLogEnv
  bracket makeLogEnv K.closeScribes $ \le -> K.runKatipContextT le () "main" $ runMonadLogKatipT $ do
    command <- liftIO (customExecParser
                        (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
                        (info (helper <*> commandParser) idm))
    case command of
      StartMatcher config ->
        getConfig config >>= runBacktest initLogEnv >>= printPortfolioInfo

runBacktest :: (MonadLog m, MonadIO m) => K.LogEnv -> Config 'Typed -> m Portfolio
runBacktest logEnv Config{cardanoNodeConfigFile, cardanoNodeSocket} = do
  logInfoS "Starting backtest"
  tv <- liftIO (STM.newEmptyTMVarIO)
  result <- liftIO $ do
    let makeLogEnv = do
          backtestingWorkerScribe <- K.mkHandleScribe (K.ColorLog True) stdout (K.permitItem K.NoticeS) K.V2
          K.registerScribe "stdout-backtesting-worker" backtestingWorkerScribe K.defaultScribeSettings logEnv
    bracket makeLogEnv K.closeScribes $ \le ->
      let client C.LocalNodeConnectInfo{C.localNodeNetworkId} env = do
            pure (NC.backtestingClient tv le "backtesting" localNodeNetworkId env)
      in runExceptT (runNodeClient cardanoNodeConfigFile cardanoNodeSocket client)
  case result of
    Left err -> do
      logWarnS (Text.unpack $ C.renderInitialLedgerStateError err)
      liftIO exitFailure
    Right () -> do
      logInfoS "Backtesting finished"
      liftIO (STM.atomically (STM.takeTMVar tv))

getConfig :: MonadIO m => Config 'Str -> m (Config 'Typed)
getConfig c = case Config.mkTyped c of
  Left err -> liftIO $ do
    putStrLn (show err)
    exitFailure
  Right k -> pure k
