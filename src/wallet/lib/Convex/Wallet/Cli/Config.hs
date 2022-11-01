{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeFamilies       #-}
module Convex.Wallet.Cli.Config(
  ConfigMode(..),
  Config(..),
  configParser,
  mkTyped
) where

import qualified Cardano.Api         as C
import           Convex.Wallet       (Wallet (..))
import qualified Convex.Wallet       as Wallet
import           Data.Bifunctor      (Bifunctor (..))
import qualified Data.Text           as Text
import           Options.Applicative (Parser, help, long, strOption)

data ConfigMode = Str | Typed

type family WalletField t where
  WalletField 'Str = String
  WalletField 'Typed = Wallet

data Config (m :: ConfigMode) =
  Config
    { cardanoNodeConfigFile :: FilePath
    , cardanoNodeSocket     :: FilePath
    , wallet                :: WalletField m
    }

deriving stock instance Eq (Config 'Str)
deriving stock instance Ord (Config 'Str)
deriving stock instance Show (Config 'Str)
deriving stock instance Show (Config 'Typed)

configParser :: Parser (Config 'Str)
configParser =
  Config
    <$> strOption (long "node-config" <> help "Cardano node config JSON file")
    <*> strOption (long "node-socket" <> help "Cardano node socket")
    <*> strOption (long "wallet-key" <> help "Serialised private key of the wallet")

data ConfigError =
  ParseKeyError C.Bech32DecodeError
  deriving Show

mkTyped :: Config 'Str -> Either ConfigError (Config 'Typed)
mkTyped Config{cardanoNodeConfigFile, cardanoNodeSocket, wallet} = do
  key <- first ParseKeyError (Wallet.parse (Text.pack wallet))
  pure Config{cardanoNodeSocket, cardanoNodeConfigFile, wallet = key}
