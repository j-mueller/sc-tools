{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Convex.TradingBot.Cli (runMain) where

import qualified Cardano.Api                   as C
import qualified Control.Concurrent.STM        as STM
import           Control.Exception             (bracket)
import           Control.Monad.Except          (MonadError (..))
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Trans.Except    (runExceptT)
import           Convex.MonadLog               (MonadLog, MonadLogKatipT (..),
                                                logInfoS, logWarnS)
import           Convex.NodeClient.Types       (loadConnectInfo, runNodeClient)
import           Convex.TradingBot.Cli.Command (CliCommand (..), commandParser)
import           Convex.TradingBot.Cli.Config  (Order (..))
import qualified Convex.TradingBot.NodeClient  as NC
import           Convex.TradingBot.Portfolio   (Portfolio, printPortfolioInfo)
import           Convex.Wallet.Cli.Config      (Config (..), ConfigMode (..),
                                                ParseFields (..))
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
    result <- runExceptT $ do
      case command of
        StartMatcher config ->
          mkTyped config >>= runBacktest initLogEnv >>= uncurry (flip printPortfolioInfo)
        Buy config order   ->
          (,) <$> mkTyped config <*> mkTyped order >>= uncurry (executeBuyOrder le)
        Sell config order   ->
          (,) <$> mkTyped config <*> mkTyped order >>= uncurry (executeSellOrder le)
    case result of
      Left err -> do
        logWarnS "Error in runMain"
        logWarnS (Text.unpack $ C.renderInitialLedgerStateError err)
      Right () -> pure ()

runBacktest :: (MonadLog m, MonadIO m) => K.LogEnv -> Config 'Typed -> m (Portfolio, C.Value)
runBacktest logEnv Config{cardanoNodeConfigFile, cardanoNodeSocket} = do
  logInfoS "Starting backtest"
  tv <- liftIO STM.newEmptyTMVarIO
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

executeBuyOrder ::  (MonadLog m, MonadError C.InitialLedgerStateError m, MonadIO m) => K.LogEnv -> Config 'Typed -> Order 'Typed -> m ()
executeBuyOrder logEnv Config{cardanoNodeConfigFile, cardanoNodeSocket, wallet} order = do
  (info_, _) <- loadConnectInfo cardanoNodeConfigFile cardanoNodeSocket
  let client _ env = do
        pure (NC.buyOrderClient info_ logEnv "order" wallet order env)
  logInfoS "Processing BUY order"
  result <- liftIO $ runExceptT (runNodeClient cardanoNodeConfigFile cardanoNodeSocket client)
  case result of
    Left err -> throwError err
    Right () -> pure ()

executeSellOrder ::  (MonadLog m, MonadError C.InitialLedgerStateError m, MonadIO m) => K.LogEnv -> Config 'Typed -> Order 'Typed -> m ()
executeSellOrder logEnv Config{cardanoNodeConfigFile, cardanoNodeSocket, wallet} order = do
  (info_, _) <- loadConnectInfo cardanoNodeConfigFile cardanoNodeSocket
  let client _ env = do
        pure (NC.sellOrderClient info_ logEnv "order" wallet order env)
  logInfoS "Processing SELL order"
  result <- liftIO $ runExceptT (runNodeClient cardanoNodeConfigFile cardanoNodeSocket client)
  case result of
    Left err -> throwError err
    Right () -> pure ()

mkTyped :: (ParseFields c, MonadIO m, MonadLog m) => c 'Str -> m (c 'Typed)
mkTyped c = case parseFields c of
  Left err -> do
    logWarnS (show err)
    liftIO exitFailure
  Right k -> pure k
