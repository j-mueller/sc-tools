{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances to ease stacking transformers over MaestroT
module Convex.Maestro.Orphans (

) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (..), mapReaderT)
import Convex.Maestro (MaestroT (..))

-- Lift a surrounding Reader through MaestroT
instance (MonadReader e m) => MonadReader e (MaestroT m) where
  ask = lift ask
  local f (MaestroT action) = MaestroT (mapReaderT (local f) action)

-- Lift a surrounding Error through MaestroT
instance (MonadError e m) => MonadError e (MaestroT m) where
  throwError = MaestroT . lift . throwError
  catchError (MaestroT action) handler = MaestroT $ ReaderT $ \r ->
    catchError (runReaderT action r) (\e -> runReaderT (unMaestroT (handler e)) r)
