{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Convex.TradingBot.Cli (runMain) where

import qualified Cardano.Api                   as C
import           Control.Exception             (bracket)
import           Control.Monad.Except          (MonadError (..))
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Trans.Except    (runExceptT)
import           Convex.NodeClient.Types       (runNodeClient)
import           Convex.TradingBot.Cli.Command (CliCommand (..), commandParser)
import           Convex.TradingBot.Cli.Config  (Config (..), ConfigMode (..))
import qualified Convex.TradingBot.Cli.Config  as Config
import qualified Convex.TradingBot.NodeClient  as NC
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
  handleScribe <- K.mkHandleScribe (K.ColorLog True) stdout (K.permitItem K.InfoS) K.V2
  let makeLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings =<< K.initLogEnv "trading-bot" "cli"
  bracket makeLogEnv K.closeScribes $ \le -> do
    command <- customExecParser
                (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
                (info (helper <*> commandParser) idm)
    result <- runExceptT $ do
      case command of
        StartMatcher config -> do
          let initialNamespace = "main"
          getConfig config >>= startMatcher le initialNamespace
    case result of
      Left err -> do
        putStrLn "Error in runMain"
        putStrLn (Text.unpack $ C.renderInitialLedgerStateError err)
      Right () -> pure ()

startMatcher :: (MonadError C.InitialLedgerStateError m, MonadIO m) => K.LogEnv -> K.Namespace -> Config 'Typed -> m ()
startMatcher logEnv ns Config{cardanoNodeConfigFile, cardanoNodeSocket} = do
  let client C.LocalNodeConnectInfo{C.localNodeNetworkId} env = do
        pure (NC.muesliClient logEnv ns localNodeNetworkId env)
  result <- liftIO $ runExceptT (runNodeClient cardanoNodeConfigFile cardanoNodeSocket client)
  case result of
    Left err -> throwError err
    Right () -> pure ()

getConfig :: MonadIO m => Config 'Str -> m (Config 'Typed)
getConfig c = case Config.mkTyped c of
  Left err -> liftIO $ do
    putStrLn (show err)
    exitFailure
  Right k -> pure k
