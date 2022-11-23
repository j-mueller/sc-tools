{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeFamilies       #-}
{-|
-}
module Convex.TradingBot.Cli.Config(
  Order(..),
  orderParser
) where

import           Cardano.Api              (AssetName, Lovelace, PolicyId,
                                           Quantity)
import qualified Cardano.Api              as C
import           Convex.Wallet.Cli.Config (ConfigField, ConfigMode (..),
                                           ParseField (..), ParseFields (..))
import           Options.Applicative      (Parser, auto, help, long, option,
                                           strOption)

data Order (m :: ConfigMode) =
  Order
    { policyId  :: ConfigField m PolicyId
    , assetName :: ConfigField m AssetName
    , quantity  :: Quantity
    , lovelace  :: Lovelace
    }

deriving stock instance Eq (Order 'Str)
deriving stock instance Ord (Order 'Str)
deriving stock instance Show (Order 'Str)
deriving stock instance Show (Order 'Typed)

orderParser :: Parser (Order 'Str)
orderParser =
  Order
    <$> strOption (long "policy-id" <> help "Policy ID (hex) of the native currency")
    <*> strOption (long "asset-name" <> help "Asset name (hex) of the native currency")
    <*> fmap C.Quantity (option auto (long "quantity" <> help "Amount of units of the native currency"))
    <*> fmap C.Lovelace (option auto (long "lovelace" <> help "Price in lovelace"))

instance ParseFields Order where
  parseFields Order{policyId, assetName, quantity, lovelace} =
    Order
      <$> parseField policyId
      <*> parseField assetName
      <*> pure quantity
      <*> pure lovelace
