{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-| CLI interface for a wallet
-}
module Convex.Wallet.Cli(
  runMain
  ) where

import qualified Cardano.Api                as C
import           Control.Monad.Except       (MonadError (..))
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Except (runExceptT)
import           Convex.NodeClient.Types    (loadConnectInfo, runNodeClient)
import qualified Convex.Wallet              as Wallet
import           Convex.Wallet.Cli.Command  (CliCommand (..), commandParser)
import           Convex.Wallet.Cli.Config   (Config (..), ConfigMode (..))
import qualified Convex.Wallet.Cli.Config   as Config
import qualified Convex.Wallet.NodeClient   as NC
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Options.Applicative        (customExecParser, disambiguate,
                                             helper, idm, info, prefs,
                                             showHelpOnEmpty, showHelpOnError)
import           System.Exit                (exitFailure)

runMain :: IO ()
runMain = do
  command <- customExecParser
              (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
              (info (helper <*> commandParser) idm)
  result <- runExceptT $ do
    case command of
      GenerateWallet     -> generateWallet
      RunWallet config   -> getConfig config >>= runWallet
      ShowAddress config -> getConfig config >>= showAddress
  case result of
    Left err -> do
      putStrLn "Error in runMain"
      putStrLn (Text.unpack $ C.renderInitialLedgerStateError err)
    Right () -> pure ()

generateWallet :: MonadIO m => m ()
generateWallet = liftIO $ do
  putStrLn "Generating wallet key..."
  key <- Wallet.generateWallet
  Text.putStrLn (Wallet.privateKey key)

getConfig :: MonadIO m => Config 'Str -> m (Config 'Typed)
getConfig c = case Config.mkTyped c of
  Left err -> liftIO $ do
    putStrLn (show err)
    exitFailure
  Right k -> pure k

showAddress :: (MonadError C.InitialLedgerStateError m, MonadIO m) => Config 'Typed -> m ()
showAddress Config{wallet, cardanoNodeConfigFile, cardanoNodeSocket} = do
  liftIO $ putStr "Wallet key:"
  liftIO $ Text.putStrLn (Wallet.privateKey wallet)
  (C.LocalNodeConnectInfo{C.localNodeNetworkId}, _) <- loadConnectInfo cardanoNodeConfigFile cardanoNodeSocket
  liftIO $ putStr "Wallet address: "
  liftIO $ Text.putStrLn (C.serialiseToBech32 $ Wallet.address localNodeNetworkId wallet)

runWallet :: (MonadError C.InitialLedgerStateError m, MonadIO m) => Config 'Typed -> m ()
runWallet Config{cardanoNodeConfigFile, cardanoNodeSocket, wallet} = do
  let client C.LocalNodeConnectInfo{C.localNodeNetworkId} env = do
        pure (NC.walletClient localNodeNetworkId wallet env)
  result <- liftIO $ runExceptT (runNodeClient cardanoNodeConfigFile cardanoNodeSocket client)
  case result of
    Left err -> throwError err
    Right () -> pure ()
