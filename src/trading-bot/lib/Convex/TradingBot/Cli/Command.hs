{-# LANGUAGE DataKinds #-}
{-|
-}
module Convex.TradingBot.Cli.Command(
  CliCommand(..),
  commandParser
  ) where

import           Convex.TradingBot.Cli.Config (BuyOrder, buyOrderParser)
import           Convex.Wallet.Cli.Config     (Config, ConfigMode (..),
                                               configParser)
import           Options.Applicative          (CommandFields, Mod, Parser,
                                               command, fullDesc, info,
                                               progDesc, subparser)
data CliCommand =
  StartMatcher (Config 'Str)
  | Buy (Config 'Str) (BuyOrder 'Str)

commandParser :: Parser CliCommand
commandParser =
  subparser $
    mconcat
      [ startMatcher
      , buy
      ]

startMatcher :: Mod CommandFields CliCommand
startMatcher = command "start-matcher" $
  info (StartMatcher <$> configParser) (fullDesc <> progDesc "Start the muesli matcher")

buy :: Mod CommandFields CliCommand
buy = command "buy" $
  info (Buy <$> configParser <*> buyOrderParser) (fullDesc <> progDesc "Buy assets on MuesliSwap")
