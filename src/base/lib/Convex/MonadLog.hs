{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Simple logging
-}
module Convex.MonadLog(
  MonadLog(..),
  logInfo,
  logInfoS,
  logWarn,
  logWarnS,
  logDebug,
  logDebugS,
  MonadLogIgnoreT(..),

  -- ** Logging with Katip
  MonadLogKatipT(..),
  runMonadLogKatip,
  KatipConfig,
  withKatipLogging,
  -- * Etc.
  logUnless
) where

import           Control.Monad              (unless)
import           Control.Monad.Catch        (MonadCatch, MonadMask, MonadThrow,
                                             bracket)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader       (ReaderT (..))
import           Control.Monad.State        (StateT (..))
import qualified Control.Monad.State.Strict as State.Strict
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..))
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Data.String                (IsString (..))
import           Data.Void                  (Void)
import           Katip                      (Environment, KatipContextT,
                                             LogContexts, LogEnv, Namespace,
                                             Severity (..))
import qualified Katip
import           Prettyprinter              (Doc, Pretty (..),
                                             defaultLayoutOptions, layoutPretty)
import qualified Prettyprinter.Render.Text  as Render
import           System.IO                  (stdout)

class Monad m => MonadLog m where
  logInfo'  :: Doc Void -> m ()
  logWarn'  :: Doc Void -> m ()
  logDebug' :: Doc Void -> m ()

instance MonadLog m => MonadLog (ReaderT e m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'
  logDebug' = lift . logDebug'

instance MonadLog m => MonadLog (StateT s m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'
  logDebug' = lift . logDebug'

instance MonadLog m => MonadLog (MaybeT m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'
  logDebug' = lift . logDebug'

instance MonadLog m => MonadLog (State.Strict.StateT s m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'
  logDebug' = lift . logDebug'

instance MonadLog m => MonadLog (ExceptT e m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'
  logDebug' = lift . logDebug'

logInfo :: forall a m. (Pretty a, MonadLog m) => a -> m ()
logInfo = logInfo' . pretty

logInfoS :: forall  m. (MonadLog m) => String -> m ()
logInfoS = logInfo' . fromString

logWarn :: forall a m. (Pretty a, MonadLog m) => a -> m ()
logWarn = logWarn' . pretty

logWarnS :: forall m. MonadLog m => String -> m ()
logWarnS = logWarn' . fromString

logDebug :: forall a m. (Pretty a, MonadLog m) => a -> m ()
logDebug = logDebug' . pretty

logDebugS :: forall m. MonadLog m => String -> m ()
logDebugS = logDebug' . fromString

newtype MonadLogIgnoreT m a = MonadLogIgnoreT { runMonadLogIgnoreT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadMask, MonadFail)

deriving newtype instance MonadError e m => MonadError e (MonadLogIgnoreT m)

instance MonadTrans MonadLogIgnoreT where
  lift = MonadLogIgnoreT

instance Monad m => MonadLog (MonadLogIgnoreT m) where
  logInfo' _ = pure ()
  logWarn' _ = pure ()
  logDebug' _ = pure ()

newtype MonadLogKatipT m a = MonadLogKatipT { runMonadLogKatipT :: KatipContextT m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadMask, MonadFail)

runMonadLogKatip :: KatipConfig -> MonadLogKatipT m a -> m a
runMonadLogKatip (env, context, ns) (MonadLogKatipT action) =
  Katip.runKatipContextT env context ns action

deriving newtype instance MonadError e m => MonadError e (MonadLogKatipT m)

instance MonadTrans MonadLogKatipT where
  lift = MonadLogKatipT . lift

instance MonadIO m => MonadLog (MonadLogKatipT m) where
    logInfo' s =
        let mkStr = Katip.logStr . Render.renderLazy . layoutPretty defaultLayoutOptions
        in MonadLogKatipT (Katip.logFM InfoS (mkStr s))
    logWarn' s =
        let mkStr = Katip.logStr . Render.renderLazy . layoutPretty defaultLayoutOptions
        in MonadLogKatipT (Katip.logFM WarningS (mkStr s))
    logDebug' s =
        let mkStr = Katip.logStr . Render.renderLazy . layoutPretty defaultLayoutOptions
        in MonadLogKatipT (Katip.logFM DebugS (mkStr s))

logUnless :: MonadLog m => Bool -> String -> m ()
logUnless w m = unless w (logInfoS m)

type KatipConfig = (LogEnv, LogContexts, Namespace)

{-| Set up a 'KatipConfig' with a single scribe that writes to stdout
at the given severity
-}
withKatipLogging :: Severity -> Environment -> Namespace -> (KatipConfig -> IO ()) -> IO ()
withKatipLogging severity environment initialNamespace action = do
  handleScribe <- Katip.mkHandleScribe Katip.ColorIfTerminal stdout (Katip.permitItem severity) Katip.V2
  let makeLogEnv = Katip.registerScribe (fromString "stdout") handleScribe Katip.defaultScribeSettings =<< Katip.initLogEnv initialNamespace environment
  -- closeScribes will stop accepting new logs, flush existing ones and clean up resources
  bracket makeLogEnv Katip.closeScribes $ \le -> do
    let initialContext = mempty -- this context will be attached to every log in your app and merged w/ subsequent contexts
    action (le, initialContext, initialNamespace)
