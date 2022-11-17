{-# LANGUAGE DataKinds #-}
{-| Commands for the wallet CLI
-}
module Convex.Wallet.Cli.Command(
  CliCommand(..),
  commandParser
  ) where

import           Convex.Wallet.Cli.Config (Config, ConfigMode (..), configParser)
import           Options.Applicative      (CommandFields, Mod, Parser, command,
                                           fullDesc, info, progDesc, subparser)

data CliCommand =
  GenerateWallet
  | RunWallet (Config 'Str)
  | ShowAddress (Config 'Str)

commandParser :: Parser CliCommand
commandParser =
  subparser $
    mconcat
      [ generateWallet
      , runWallet
      , showAddress
      ]

generateWallet :: Mod CommandFields CliCommand
generateWallet = command "generate-wallet" $
  info (pure GenerateWallet) (fullDesc <> progDesc "Generate a private key for a wallet")

runWallet :: Mod CommandFields CliCommand
runWallet = command "run-wallet" $
  info (RunWallet <$> configParser) (fullDesc <> progDesc "Start the wallet")

showAddress :: Mod CommandFields CliCommand
showAddress = command "show-address" $
  info (ShowAddress <$> configParser) (fullDesc <> progDesc "Show the wallet's address")
