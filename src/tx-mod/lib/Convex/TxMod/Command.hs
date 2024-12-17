{-| Command type and parser
-}
module Convex.TxMod.Command(
  TxModCommand(..),
  parseCommand
) where

import           Cardano.Api         (TxId)
import           Options.Applicative (CommandFields, Mod, Parser, argument,
                                      command, fullDesc, help, info, long,
                                      metavar, optional, progDesc, short, str,
                                      strOption, subparser)

data TxModCommand =
  Download TxId (Maybe FilePath) -- ^ Download the transaction from blockfrost and print it to stdout, or write it to the file if a file is provided
  | Graph -- ^ visualise the transaction

parseCommand :: Parser TxModCommand
parseCommand = subparser $ mconcat [parseDownload, parseGraph]

parseDownload :: Mod CommandFields TxModCommand
parseDownload = command "download" $
  info (Download <$> parseTxId <*> optional parseTxOutFile) (fullDesc <> progDesc "Download a fully resolved transaction from blockfrost")

parseGraph :: Mod CommandFields TxModCommand
parseGraph = command "graph" $
  info (pure Graph) (fullDesc <> progDesc "Generate a dot graph (graphviz) from a fully resolved transaction")

parseTxId :: Parser TxId
parseTxId = argument str
  (metavar "TX_ID" <> help "The transaction ID")

parseTxOutFile :: Parser FilePath
parseTxOutFile = strOption (long "out.file" <> short 'o' <> help "File to write the fully resolved transaction to")
