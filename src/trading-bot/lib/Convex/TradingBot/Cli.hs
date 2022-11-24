{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Convex.TradingBot.Cli (runMain) where

import qualified Cardano.Api                               as C
import           Control.Exception                         (bracket)
import           Control.Monad.Except                      (MonadError (..))
import           Control.Monad.IO.Class                    (MonadIO (..))
import           Control.Monad.Trans.Except                (runExceptT)
import           Convex.MonadLog                           (MonadLog,
                                                            MonadLogKatipT (..),
                                                            logInfoS, logWarnS)
import           Convex.NodeClient.Types                   (loadConnectInfo,
                                                            runNodeClient)
import           Convex.TradingBot.Annealing               (runAnnealing,
                                                            runBacktestNode)
import           Convex.TradingBot.Cli.Command             (CliCommand (..),
                                                            commandParser)
import           Convex.TradingBot.Cli.Config              (Order (..))
import qualified Convex.TradingBot.NodeClient              as NC
import           Convex.TradingBot.NodeClient.PricesClient (PriceEventRow)
import           Convex.TradingBot.Portfolio               (printPortfolioInfo)
import qualified Convex.TradingBot.Rules                   as Rules
import           Convex.Wallet.Cli.Config                  (Config (..),
                                                            ConfigMode (..),
                                                            ParseFields (..))
import           Data.CSV.Export                           (defaultCSVConfig,
                                                            writeCSVHeader)
import           Data.Proxy                                (Proxy (..))
import qualified Data.Text                                 as Text
import qualified Katip                                     as K
import           Options.Applicative                       (customExecParser,
                                                            disambiguate,
                                                            helper, idm, info,
                                                            prefs,
                                                            showHelpOnEmpty,
                                                            showHelpOnError)
import           System.Exit                               (exitFailure)
import qualified System.IO                                 as IO
import           System.IO                                 (stdout)

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
          mkTyped config >>= runBacktestNode Rules.movingAverage initLogEnv >>= uncurry (flip printPortfolioInfo)
        Buy config order   ->
          (,) <$> mkTyped config <*> mkTyped order >>= uncurry (executeBuyOrder le)
        Sell config order   ->
          (,) <$> mkTyped config <*> mkTyped order >>= uncurry (executeSellOrder le)
        Optimise fp -> runAnnealing le fp
        ExportPrices config outFile ->
          mkTyped config >>= runExport le outFile
    case result of
      Left err -> do
        logWarnS "Error in runMain"
        logWarnS (Text.unpack $ C.renderInitialLedgerStateError err)
      Right () -> pure ()

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

runExport :: (MonadError C.InitialLedgerStateError m, MonadIO m) => K.LogEnv -> FilePath -> Config 'Typed -> m ()
runExport logEnv fp Config{cardanoNodeConfigFile, cardanoNodeSocket} = do
  (info_, _) <- loadConnectInfo cardanoNodeConfigFile cardanoNodeSocket
  let client handle _ env = do
        pure (NC.pricesClient handle (C.localNodeNetworkId info_) logEnv "prices" env)
  result <- liftIO $ do
    IO.withFile fp IO.WriteMode $ \handle -> do
      writeCSVHeader handle defaultCSVConfig (Proxy @PriceEventRow)
      runExceptT (runNodeClient cardanoNodeConfigFile cardanoNodeSocket (client handle))
  case result of
    Left err -> throwError err
    Right () -> pure ()

mkTyped :: (ParseFields c, MonadIO m, MonadLog m) => c 'Str -> m (c 'Typed)
mkTyped c = case parseFields c of
  Left err -> do
    logWarnS (show err)
    liftIO exitFailure
  Right k -> pure k
