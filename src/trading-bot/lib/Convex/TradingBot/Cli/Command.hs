{-# LANGUAGE DataKinds #-}
{-|
-}
module Convex.TradingBot.Cli.Command(
  CliCommand(..),
  commandParser
  ) where

import           Convex.TradingBot.Cli.Config (Order, orderParser)
import           Convex.Wallet.Cli.Config     (Config, ConfigMode (..),
                                               configParser)
import           Options.Applicative          (CommandFields, Mod, Parser,
                                               command, fullDesc, help, info,
                                               long, progDesc, strOption,
                                               subparser)
data CliCommand =
  StartTrading (Config 'Str)
  | Buy (Config 'Str) (Order 'Str)
  | Sell (Config 'Str) (Order 'Str)
  | Optimise FilePath
  | ExportPrices (Config 'Str) FilePath

commandParser :: Parser CliCommand
commandParser =
  subparser $
    mconcat
      [ startTrading
      , buy
      , sell
      , optimise
      , exportPrices
      ]

startTrading :: Mod CommandFields CliCommand
startTrading = command "start-trading" $
  info (StartTrading <$> configParser) (fullDesc <> progDesc "Start the trading bot")

buy :: Mod CommandFields CliCommand
buy = command "buy" $
  info (Buy <$> configParser <*> orderParser) (fullDesc <> progDesc "Buy assets on MuesliSwap")

sell :: Mod CommandFields CliCommand
sell = command "sell" $
  info (Sell <$> configParser <*> orderParser) (fullDesc <> progDesc "Sell assets on MuesliSwap")

optimise :: Mod CommandFields CliCommand
optimise = command "optimise" $
  info (Optimise <$> strOption (long "prices" <> help "CSV file for the prices")) (fullDesc <> progDesc "Use the annealing algorithm to optimise a set of trading rules")

exportPrices :: Mod CommandFields CliCommand
exportPrices = command "export-prices" $
  info (ExportPrices <$> configParser <*> strOption (long "out" <> help "CSV file for the prices")) (fullDesc <> progDesc "Export the price history to CSV")
