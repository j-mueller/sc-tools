{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- thank you hydra

{- | Adapter module to the actual logging framework.
All Hydra node components implements /Structured logging/ via [contra-tracer](https://hackage.haskell.org/package/contra-tracer)
generic logging framework. All logs are output in [JSON](https://www.json.org/json-en.html) in a format which is
documented in a [JSON-Schema](https://github.com/input-output-hk/hydra/blob/master/hydra-node/json-schemas/logs.yaml).
-}
module Convex.Devnet.Logging (
  -- * Tracer
  Tracer (..),
  natTracer,
  nullTracer,
  traceWith,
  ToObject (..),
  TracingVerbosity (..),

  -- * Using it
  Verbosity (..),
  Envelope (..),
  withTracer,
  withTracerOutputTo,
  showLogsOnFailure,
  traceInTVar,
  contramap,
) where

import Cardano.BM.Tracing (
  ToObject (..),
  TracingVerbosity (..),
 )
import Control.Concurrent.STM.TBQueue (
  flushTBQueue,
  newTBQueueIO,
  readTBQueue,
  writeTBQueue,
 )
import Control.Concurrent.STM.TVar (
  TVar,
  modifyTVar,
  newTVarIO,
  readTVarIO,
 )
import Control.Monad (forM_, forever, (>=>))
import Control.Monad.Class.MonadAsync (withAsync)
import Control.Monad.Class.MonadFork (MonadFork, myThreadId)
import Control.Monad.Class.MonadSTM (MonadSTM, atomically)
import Control.Monad.Class.MonadSay (MonadSay, say)
import Control.Monad.Class.MonadThrow (
  MonadCatch,
  finally,
  onException,
 )
import Control.Monad.Class.MonadTime (MonadTime, getCurrentTime)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Tracer (
  Tracer (..),
  contramap,
  natTracer,
  nullTracer,
  traceWith,
 )
import Data.Aeson (FromJSON, ToJSON, pairs, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.IO (Handle, hFlush, stdout)
import Text.Read (readMaybe)

import Prelude

data Verbosity = Quiet | Verbose Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Provides logging metadata for entries.
data Envelope a = Envelope
  { timestamp :: UTCTime
  , threadId :: Int
  , namespace :: Text
  , message :: a
  }
  deriving (Eq, Show, Generic, FromJSON)

instance (ToJSON a) => ToJSON (Envelope a) where
  toEncoding Envelope{timestamp, threadId, namespace, message} =
    pairs $
      mconcat
        [ "timestamp" .= timestamp
        , "threadId" .= threadId
        , "namespace" .= namespace
        , "message" .= message
        ]

defaultQueueSize :: Natural
defaultQueueSize = 500

{- | Start logging thread and acquire a 'Tracer'. This tracer will dump all
messsages on @stdout@, one message per line, formatted as JSON. This tracer
is wrapping 'msg' into an 'Envelope' with metadata.
-}
withTracer
  :: forall m msg a
   . (MonadIO m, MonadFork m, MonadTime m, ToJSON msg)
  => Verbosity
  -> (Tracer m msg -> IO a)
  -> IO a
withTracer Quiet = ($ nullTracer)
withTracer (Verbose namespace) = withTracerOutputTo stdout namespace

{- | Start logging thread acquiring a 'Tracer', outputting JSON formatted
messages to some 'Handle'. This tracer is wrapping 'msg' into an 'Envelope'
with metadata.
-}
withTracerOutputTo
  :: forall m msg a
   . (MonadIO m, MonadFork m, MonadTime m, ToJSON msg)
  => Handle
  -> Text
  -> (Tracer m msg -> IO a)
  -> IO a
withTracerOutputTo hdl namespace action = do
  msgQueue <- liftIO (newTBQueueIO @(Envelope msg) defaultQueueSize)
  withAsync (writeLogs msgQueue) $ \_ ->
    action (tracer msgQueue) `finally` flushLogs msgQueue
 where
  tracer queue =
    Tracer $
      mkEnvelope namespace >=> liftIO . atomically . writeTBQueue queue

  writeLogs queue =
    forever $ do
      atomically (readTBQueue queue) >>= write . Aeson.encode
      hFlush hdl

  flushLogs queue = liftIO $ do
    entries <- atomically $ flushTBQueue queue
    forM_ entries (write . Aeson.encode)
    hFlush hdl

  write bs = LBS.hPut hdl (bs <> "\n")

{- | Capture logs and output them to stdout when an exception was raised by the
given 'action'. This tracer is wrapping 'msg' into an 'Envelope' with
metadata.
-}
showLogsOnFailure
  :: (MonadSTM m, MonadCatch m, MonadIO m, MonadFork m, MonadTime m, MonadSay m, ToJSON msg)
  => (Tracer m msg -> m a)
  -> m a
showLogsOnFailure action = do
  tvar <- liftIO (newTVarIO [])
  action (traceInTVar tvar)
    `onException` (liftIO (readTVarIO tvar) >>= mapM_ (say . TL.unpack . decodeUtf8 . Aeson.encode) . reverse)

traceInTVar
  :: (MonadIO m, MonadTime m, MonadFork m)
  => TVar [Envelope msg]
  -> Tracer m msg
traceInTVar tvar = Tracer $ \msg -> do
  envelope <- mkEnvelope "" msg
  liftIO $ atomically $ modifyTVar tvar (envelope :)
-- * Internal functions

mkEnvelope :: (MonadFork m, MonadTime m) => Text -> msg -> m (Envelope msg)
mkEnvelope namespace message = do
  timestamp <- getCurrentTime
  threadId <- mkThreadId <$> myThreadId
  pure $ Envelope{namespace, timestamp, threadId, message}
 where
  -- NOTE(AB): This is a bit contrived but we want a numeric threadId and we
  -- get some text which we know the structure of
  mkThreadId = fromMaybe 0 . readMaybe . Text.unpack . Text.drop 9 . Text.pack . show
