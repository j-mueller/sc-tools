{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module Convex.Wallet.Cli.Config(
  -- * Wallet config
  Config(..),
  configParser
) where

import           Options.Applicative (Parser, help, long, strOption)

data Config =
  Config
    { cardanoNodeConfigFile :: FilePath
    , cardanoNodeSocket     :: FilePath
    , walletFile            :: FilePath
    }
    deriving stock (Eq, Show)

configParser :: Parser Config
configParser =
  Config
    <$> strOption (long "node-config" <> help "Cardano node config JSON file")
    <*> strOption (long "node-socket" <> help "Cardano node socket")
    <*> strOption (long "wallet-file" <> help "JSON file with the wallet state. This will be created if it doesn't exist")
