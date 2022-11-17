{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-| CLI interface for a wallet
-}
module Convex.Wallet.Cli(
  runMain
  ) where

import qualified Cardano.Api                as C
import           Control.Exception          (bracket)
import           Control.Monad              (void)
import           Control.Monad.Except       (MonadError (..))
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Except (runExceptT)
import           Convex.MonadLog            (MonadLog, MonadLogKatipT (..),
                                             logInfo, logInfoS, logWarnS)
import           Convex.NodeClient.Types    (loadConnectInfo, runNodeClient)
import qualified Convex.Wallet              as Wallet
import           Convex.Wallet.Cli.Command  (CliCommand (..), commandParser)
import           Convex.Wallet.Cli.Config   (Config (..), ConfigMode (..),
                                             ParseFields (..))
import qualified Convex.Wallet.Cli.Config   as Config
import qualified Convex.Wallet.NodeClient   as NC
import qualified Data.Text                  as Text
import qualified Katip                      as K
import           Options.Applicative        (customExecParser, disambiguate,
                                             helper, idm, info, prefs,
                                             showHelpOnEmpty, showHelpOnError)
import           System.Exit                (exitFailure)
import           System.IO                  (stdout)

runMain :: IO ()
runMain = do
  mainScribe <- K.mkHandleScribe (K.ColorLog True) stdout (K.permitItem K.InfoS) K.V2
  initLogEnv <- K.initLogEnv "wallet" "cli"
  let makeLogEnv = K.registerScribe "stdout-main" mainScribe K.defaultScribeSettings initLogEnv
  bracket makeLogEnv K.closeScribes $ \le -> K.runKatipContextT le () "main" $ runMonadLogKatipT $ do
    command <- liftIO (customExecParser
                        (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
                        (info (helper <*> commandParser) idm))
    result <- runExceptT $ do
      case command of
        GenerateWallet     -> generateWallet
        RunWallet config   -> mkTyped config >>= runWallet le
        ShowAddress config -> mkTyped config >>= void . showAddress
    case result of
      Left err -> do
        logWarnS "Error in runMain"
        logWarnS (Text.unpack $ C.renderInitialLedgerStateError err)
      Right () -> pure ()

generateWallet :: (MonadIO m, MonadLog m) => m ()
generateWallet = do
  logInfoS "Generating wallet key..."
  key <- liftIO Wallet.generateWallet
  logInfo (Wallet.privateKey key)

mkTyped :: (ParseFields c, MonadIO m, MonadLog m) => c 'Str -> m (c 'Typed)
mkTyped c = case Config.parseFields c of
  Left err -> do
    logWarnS (show err)
    liftIO exitFailure
  Right k -> pure k

showAddress :: (MonadLog m, MonadError C.InitialLedgerStateError m, MonadIO m) => Config 'Typed -> m (C.LocalNodeConnectInfo C.CardanoMode)
showAddress Config{wallet, cardanoNodeConfigFile, cardanoNodeSocket} = do
  logInfoS "Wallet key:"
  logInfo (Wallet.privateKey wallet)
  (info_@C.LocalNodeConnectInfo{C.localNodeNetworkId}, _) <- loadConnectInfo cardanoNodeConfigFile cardanoNodeSocket
  logInfoS "Wallet address: "
  logInfo (C.serialiseToBech32 $ Wallet.address localNodeNetworkId wallet)
  pure info_

runWallet :: (MonadLog m, MonadError C.InitialLedgerStateError m, MonadIO m) => K.LogEnv -> Config 'Typed -> m ()
runWallet logEnv c@Config{cardanoNodeConfigFile, cardanoNodeSocket, wallet} = do
  void (showAddress c)
  let client _ env = do
        pure (NC.balanceClient logEnv "wallet" wallet env)
  result <- liftIO $ runExceptT (runNodeClient cardanoNodeConfigFile cardanoNodeSocket client)
  case result of
    Left err -> throwError err
    Right () -> pure ()
