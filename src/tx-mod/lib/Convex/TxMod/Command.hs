-- | Command type and parser
module Convex.TxMod.Command (
  TxModCommand (..),
  ResolvedTxInput (..),
  parseCommand,
) where

import Cardano.Api (TxId)
import Cardano.Api qualified as C
import Cardano.Api.Parser.Text qualified as Parser
import Data.Text qualified as Text
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  argument,
  command,
  eitherReader,
  fullDesc,
  help,
  info,
  long,
  many,
  metavar,
  optional,
  progDesc,
  short,
  strOption,
  subparser,
  (<|>),
 )

data TxModCommand
  = -- | Download the transaction from blockfrost and print it to stdout, or write it to the file if a file is provided
    Download TxId (Maybe FilePath)
  | -- | visualise the transaction
    Graph [ResolvedTxInput] (Maybe FilePath)

parseCommand :: Parser TxModCommand
parseCommand = subparser $ mconcat [parseDownload, parseGraph]

parseDownload :: Mod CommandFields TxModCommand
parseDownload =
  command "download" $
    info (Download <$> parseTxId <*> optional parseTxOutFile) (fullDesc <> progDesc "Download a fully resolved transaction from blockfrost")

parseGraph :: Mod CommandFields TxModCommand
parseGraph =
  command "graph" $
    info (Graph <$> many parseResolvedTxInput <*> optional parseGraphOutFile) (fullDesc <> progDesc "Generate a dot graph (graphviz) from a fully resolved transaction")

parseTxId :: Parser TxId
parseTxId =
  argument
    (eitherReader (Parser.runParser C.parseTxId . Text.pack))
    (metavar "TX_ID" <> help "The transaction ID")

parseTxOutFile :: Parser FilePath
parseTxOutFile = strOption (long "out.file" <> short 'o' <> help "File to write the fully resolved transaction to")

parseTxInFile :: Parser FilePath
parseTxInFile = strOption (long "in.file" <> short 'f' <> help "JSON file with the fully resolved transaction")

parseGraphOutFile :: Parser FilePath
parseGraphOutFile = strOption (long "out.file" <> short 'o' <> help "File to write the dot graph to")

-- | Resolved tx either provided as a file path or as a transaction ID
newtype ResolvedTxInput = ResolvedTxInput (Either FilePath TxId)

parseResolvedTxInput :: Parser ResolvedTxInput
parseResolvedTxInput =
  ResolvedTxInput <$> (fmap Left parseTxInFile <|> fmap Right parseTxId)
