{-# LANGUAGE OverloadedStrings #-}

module Convex.TxMod.Cli (
  runMain,
) where

import Blammo.Logging.Simple (
  Message ((:#)),
  MonadLogger,
  MonadLoggerIO,
  WithLogger (..),
  logError,
  logInfo,
  logWarn,
  runLoggerLoggingT,
 )
import Blockfrost.Client.Core (BlockfrostError)
import Cardano.Api (TxId)
import Control.Lens (view)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (
  MonadReader,
  ReaderT,
  asks,
  runReaderT,
 )
import Convex.Blockfrost qualified
import Convex.Blockfrost.Orphans ()
import Convex.Blockfrost.Types (DecodingError)
import Convex.ResolvedTx (ResolvedTx)
import Convex.ResolvedTx qualified
import Convex.TxMod.Command (
  ResolvedTxInput (..),
  TxModCommand (..),
  parseCommand,
 )
import Convex.TxMod.Env (Env)
import Convex.TxMod.Env qualified as Env
import Convex.TxMod.Logging qualified as L
import Convex.Utils (liftEither, mapError)
import Data.Aeson qualified
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.String (IsString (fromString))
import Data.Text.IO qualified as Text.IO
import Options.Applicative (
  customExecParser,
  disambiguate,
  helper,
  idm,
  info,
  prefs,
  showHelpOnEmpty,
  showHelpOnError,
 )

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
    Graph input output -> graph input output
  case result of
    Left err -> runLoggerLoggingT env $ do
      logError (fromString $ show err)
    Right a -> pure a

downloadTx :: (MonadLoggerIO m, MonadReader Env m, MonadError AppError m) => TxId -> Maybe FilePath -> m ()
downloadTx txId filePath = resolveTx txId >>= writeTx filePath

graph :: (MonadLoggerIO m, MonadReader Env m, MonadError AppError m) => [ResolvedTxInput] -> Maybe FilePath -> m ()
graph inputs outFile = do
  when (null inputs) $ logWarn "No resolved transactions provided, graph will be empty"
  traverse getTx inputs >>= writeGraph outFile

newtype TxModApp a = TxModApp {unTxModApp :: ReaderT Env (ExceptT AppError IO) a}
  deriving newtype (Monad, Applicative, Functor, MonadIO, MonadReader Env, MonadError AppError)
  deriving
    (MonadLogger, MonadLoggerIO)
    via (WithLogger Env (ExceptT AppError IO))

data AppError
  = DecodingErr DecodingError
  | BlockfrostErr BlockfrostError
  | FileDecodeErr String
  deriving stock (Show)

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

loadTx :: (MonadLoggerIO m, MonadReader Env m, MonadError AppError m) => FilePath -> m ResolvedTx
loadTx fp = do
  logInfo $ "Reading tx from file" :# [L.txFile fp]
  liftEither FileDecodeErr (Data.Aeson.eitherDecode <$> liftIO (LBS.readFile fp))

getTx :: (MonadLoggerIO m, MonadReader Env m, MonadError AppError m) => ResolvedTxInput -> m ResolvedTx
getTx (ResolvedTxInput k) = either loadTx resolveTx k

writeGraph :: (MonadLoggerIO m) => Maybe FilePath -> [ResolvedTx] -> m ()
writeGraph = \case
  Nothing -> \tx -> do
    logInfo "Writing graph to stdout"
    liftIO . Text.IO.putStrLn $ Convex.ResolvedTx.dot tx
  Just fp -> \tx -> do
    logInfo $ "Writing graph to file" :# [L.dotGraphFile fp]
    liftIO . Convex.ResolvedTx.dotFile fp $ tx
