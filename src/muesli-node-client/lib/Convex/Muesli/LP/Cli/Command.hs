{-# LANGUAGE DataKinds #-}
{-|
-}
module Convex.Muesli.LP.Cli.Command(
  CliCommand(..),
  commandParser
  ) where

import           Convex.Muesli.LP.Cli.Config (Config, ConfigMode (..),
                                                     configParser)
import           Options.Applicative                (CommandFields, Mod, Parser,
                                                     command, fullDesc, info,
                                                     progDesc, subparser)
data CliCommand =
  StartMatcher (Config 'Str)

commandParser :: Parser CliCommand
commandParser =
  subparser $
    mconcat
      [ startMatcher
      ]

startMatcher :: Mod CommandFields CliCommand
startMatcher = command "start-matcher" $
  info (StartMatcher <$> configParser) (fullDesc <> progDesc "Start the muesli matcher")
