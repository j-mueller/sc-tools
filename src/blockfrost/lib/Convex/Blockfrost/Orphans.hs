

{-# OPTIONS_GHC -Wno-orphans #-}
{-| Missing instances for BlockfrostClientT
-}
module Convex.Blockfrost.Orphans(

) where

import           Blockfrost.Client.Types    (MonadBlockfrost (..))
import           Control.Monad.Except       (ExceptT (..))
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans.Class  (MonadTrans (..))

instance MonadBlockfrost m => MonadBlockfrost (ExceptT e m) where
  liftBlockfrostClient = lift . liftBlockfrostClient
  getConf = lift getConf

instance MonadBlockfrost m => MonadBlockfrost (StateT s m) where
  liftBlockfrostClient = lift . liftBlockfrostClient
  getConf = lift getConf
