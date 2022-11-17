{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module Convex.Wallet.Cli.Config(
  -- * Config values
  ConfigMode(..),
  ConfigField,
  ConfigError(..),
  ParseFields(..),
  ParseField(..),

  -- * Wallet config
  Config(..),
  configParser
) where

import           Cardano.Api         (AssetName, PolicyId)
import qualified Cardano.Api         as C
import           Convex.Wallet       (Wallet (..))
import qualified Convex.Wallet       as Wallet
import           Data.Bifunctor      (Bifunctor (..))
import           Data.Proxy          (Proxy (..))
import           Data.String         (IsString (..))
import qualified Data.Text           as Text
import           Options.Applicative (Parser, help, long, strOption)

data ConfigMode = Str | Typed

type family ConfigField (m :: ConfigMode) t where
  ConfigField 'Str   x = String
  ConfigField 'Typed x = x

data Config (m :: ConfigMode) =
  Config
    { cardanoNodeConfigFile :: FilePath
    , cardanoNodeSocket     :: FilePath
    , wallet                :: ConfigField m Wallet
    }

deriving stock instance Eq (Config 'Str)
deriving stock instance Ord (Config 'Str)
deriving stock instance Show (Config 'Str)
deriving stock instance Show (Config 'Typed)

{-| Values that can be parsed from strings
-}
class ParseField t where
  parseField :: String -> Either ConfigError t

instance ParseField Wallet where
  parseField = first ParseKeyError . Wallet.parse . Text.pack

instance ParseField PolicyId where
  parseField = first (DeserialiseError . show) . C.deserialiseFromRawBytesHex (C.proxyToAsType $ Proxy @PolicyId) . fromString

instance ParseField AssetName where
  parseField = first (DeserialiseError . show) . C.deserialiseFromRawBytesHex (C.proxyToAsType $ Proxy @AssetName) . fromString

class ParseFields c where
  parseFields :: c 'Str -> Either ConfigError (c 'Typed)

instance ParseFields Config where
  parseFields Config{cardanoNodeConfigFile, cardanoNodeSocket, wallet} =
    Config cardanoNodeConfigFile cardanoNodeSocket
      <$> parseField wallet

configParser :: Parser (Config 'Str)
configParser =
  Config
    <$> strOption (long "node-config" <> help "Cardano node config JSON file")
    <*> strOption (long "node-socket" <> help "Cardano node socket")
    <*> strOption (long "wallet-key" <> help "Serialised private key of the wallet")

data ConfigError =
  ParseKeyError C.Bech32DecodeError
  | DeserialiseError String
  deriving Show
