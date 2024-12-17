{-# LANGUAGE OverloadedStrings #-}
module Convex.TxMod.Cli(
  runMain
) where

import           Blammo.Logging.Simple      (Message ((:#)), MonadLogger,
                                             MonadLoggerIO, WithLogger (..),
                                             logError, logInfo,
                                             runLoggerLoggingT)
import           Blockfrost.Client.Core     (BlockfrostError)
import           Cardano.Api                (TxId)
import           Control.Lens               (view)
import           Control.Monad.Except       (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader       (MonadReader, ReaderT, asks,
                                             runReaderT)
import qualified Convex.Blockfrost
import           Convex.Blockfrost.Orphans  ()
import           Convex.Blockfrost.Types    (DecodingError)
import           Convex.ResolvedTx          (ResolvedTx)
import           Convex.TxMod.Command       (TxModCommand (..), parseCommand)
import           Convex.TxMod.Env           (Env)
import qualified Convex.TxMod.Env           as Env
import qualified Convex.TxMod.Logging       as L
import           Convex.Utils               (liftEither, mapError)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.String                (IsString (fromString))
import           Options.Applicative        (customExecParser, disambiguate,
                                             helper, idm, info, prefs,
                                             showHelpOnEmpty, showHelpOnError)

runMain :: IO ()
runMain = do
  customExecParser
    (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
    (info (helper <*> parseCommand) idm)
    >>= runCommand

runCommand :: TxModCommand -> IO ()
runCommand com = do
  env <- Env.loadEnv
  result <- runTxModApp env $ case com of
    Download txId output -> downloadTx txId output
    Graph -> do
      logInfo "Generating graph"
  case result of
    Left err -> runLoggerLoggingT env $ do
      logError (fromString $ show err)
    Right a -> pure a

newtype TxModApp a = TxModApp{ unTxModApp :: ReaderT Env (ExceptT AppError IO) a }
  deriving newtype (Monad, Applicative, Functor, MonadIO, MonadReader Env, MonadError AppError)
  deriving (MonadLogger, MonadLoggerIO)
    via (WithLogger Env (ExceptT AppError IO))

data AppError =
  DecodingErr DecodingError
  | BlockfrostErr BlockfrostError
  deriving stock Show

runTxModApp :: Env -> TxModApp a -> IO (Either AppError a)
runTxModApp env TxModApp{unTxModApp} = runExceptT (runReaderT unTxModApp env)

writeTx :: (MonadLoggerIO m) => Maybe FilePath -> ResolvedTx -> m ()
writeTx = \case
  Nothing -> \tx -> do
    logInfo "Writing tx to stdout"
    liftIO . LBS.putStrLn . encodePretty $ tx
  Just fp -> \tx -> do
    logInfo $ "Writing tx to file" :# [L.txFile fp]
    liftIO . LBS.writeFile fp . encodePretty $ tx

resolveTx :: (MonadLoggerIO m, MonadReader Env m, MonadError AppError m) => TxId -> m ResolvedTx
resolveTx txId = do
  logInfo $ "Downloading tx" :# [L.txId txId]
  project <- asks (view Env.blockfrostProject)
  liftEither BlockfrostErr (mapError DecodingErr (Convex.Blockfrost.evalBlockfrostT project (Convex.Blockfrost.resolveTx txId)))

downloadTx :: (MonadLoggerIO m, MonadReader Env m, MonadError AppError m) => TxId -> Maybe FilePath -> m ()
downloadTx txId filePath = resolveTx txId >>= writeTx filePath
