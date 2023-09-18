{-# LANGUAGE DataKinds #-}
{-| Commands for the wallet CLI
-}
module Convex.Wallet.Cli.Command(
  CliCommand(..),
  commandParser
  ) where

import           Convex.Wallet.Cli.Config (Config, configParser)
import           Convex.Wallet.Operator   (OperatorConfigVerification,
                                           parseOperatorConfigVerification)
import           Options.Applicative      (CommandFields, Mod, Parser, auto,
                                           command, fullDesc, help, info, long,
                                           option, progDesc, strOption,
                                           subparser, value)

data CliCommand =
  GenerateWallet
  | GenerateSigningKey{verificationKeyFile :: FilePath, signingKeyFile :: FilePath }
  | RunWallet Config OperatorConfigVerification Int
  | ShowAddress Config OperatorConfigVerification

commandParser :: Parser CliCommand
commandParser =
  subparser $
    mconcat
      [ generateWallet
      , generateSigningKey
      , runWallet
      , showAddress
      ]

generateWallet :: Mod CommandFields CliCommand
generateWallet = command "generate-wallet" $
  info (pure GenerateWallet) (fullDesc <> progDesc "Generate a private key for a wallet")

generateSigningKey :: Mod CommandFields CliCommand
generateSigningKey = command "generate-signing-key" $
  info (GenerateSigningKey <$> verificationKeyFileParser <*> signingKeyFileParser) (fullDesc <> progDesc "Generate a signing and verification key pair")

runWallet :: Mod CommandFields CliCommand
runWallet = command "run-wallet" $
  info (RunWallet <$> configParser <*> parseOperatorConfigVerification <*> portParser) (fullDesc <> progDesc "Start the wallet")

showAddress :: Mod CommandFields CliCommand
showAddress = command "show-address" $
  info (ShowAddress <$> configParser <*> parseOperatorConfigVerification) (fullDesc <> progDesc "Show the wallet's address")

portParser :: Parser Int
portParser =
  option auto
  (long "http.port" <> value 9988 <> help "The port of the wallet HTTP server")

verificationKeyFileParser :: Parser FilePath
verificationKeyFileParser = strOption (long "verification.file" <> help "File for the verification key")

signingKeyFileParser :: Parser FilePath
signingKeyFileParser = strOption (long "signing.file" <> help "File for the signing key")
