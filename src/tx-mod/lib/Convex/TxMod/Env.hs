{-# LANGUAGE TemplateHaskell #-}
module Convex.TxMod.Env(
  Env(..),
  loadEnv,
  -- ** Lenses
  logger,
  blockfrostProject
) where

import           Blammo.Logging                 (Logger)
import           Blammo.Logging.Logger          (HasLogger (..), newLogger)
import qualified Blammo.Logging.LogSettings.Env as LogSettingsEnv
import           Blockfrost.Auth                (mkProject)
import           Blockfrost.Client.Auth         (Project)
import           Control.Lens                   (makeLensesFor)
import qualified Data.Text                      as Text
import qualified System.Environment

data Env =
  Env
    { envLogger            :: Logger
    , envBlockfrostProject :: Project
    }

makeLensesFor
  [ ("envLogger", "logger")
  , ("envBlockfrostProject", "blockfrostProject")
  ]
  'Env

instance HasLogger Env where
  loggerL = logger

{-| Load the 'Env' from environment variables
-}
loadEnv :: IO Env
loadEnv =
  Env
    <$> (LogSettingsEnv.parse >>= newLogger)
    <*> fmap (mkProject . Text.pack) (System.Environment.getEnv "BLOCKFROST_TOKEN")
