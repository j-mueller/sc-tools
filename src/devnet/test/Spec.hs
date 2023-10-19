{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Main where

import qualified Cardano.Api                as C
import qualified Cardano.Api.Shelley        as C
import           Control.Monad.Except       (runExceptT)
import           Convex.Devnet.CardanoNode  (NodeLog, RunningNode (..),
                                             allowLargeTransactions,
                                             getCardanoNodeVersion,
                                             withCardanoNodeDevnet,
                                             withCardanoNodeDevnetConfig)
import           Convex.Devnet.Logging      (contramap, showLogsOnFailure)
import           Convex.Devnet.NodeQueries  (loadConnectInfo)
import           Convex.Devnet.Utils        (failAfter, failure, withTempDir)
import           Convex.Devnet.Wallet       (WalletLog)
import qualified Convex.Devnet.Wallet       as W
import           Convex.Devnet.WalletServer (getUTxOs, withWallet)
import qualified Convex.Devnet.WalletServer as WS
import           Convex.NodeQueries         (queryProtocolParameters)
import qualified Convex.Utxos               as Utxos
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.List                  (isInfixOf)
import qualified Data.Text                  as Text
import           GHC.Generics               (Generic)
import           GHC.IO.Encoding            (setLocaleEncoding, utf8)
import           Test.Tasty                 (defaultMain, testGroup)
import           Test.Tasty.HUnit           (assertBool, assertEqual, testCase)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $ testGroup "test"
    [ testCase "cardano-node is available" checkCardanoNode
    , testCase "start local node" startLocalNode
    , testCase "make a payment" makePayment
    , testCase "run the wallet server" runWalletServer
    , testCase "change max tx size" changeMaxTxSize
    ]

checkCardanoNode :: IO ()
checkCardanoNode =
  let expectedVersion = "8.1.1"
  in getCardanoNodeVersion >>= assertBool ("cardano-node version should be " <> expectedVersion) . isInfixOf expectedVersion

startLocalNode :: IO ()
startLocalNode = do
    showLogsOnFailure $ \tr -> do
      failAfter 5 $
        withTempDir "cardano-cluster" $ \tmp -> do
          withCardanoNodeDevnet tr tmp $ \RunningNode{rnNodeSocket, rnNodeConfigFile} -> do
            runExceptT (loadConnectInfo rnNodeConfigFile rnNodeSocket) >>= \case
              Left err -> failure (Text.unpack (C.renderInitialLedgerStateError err))
              Right{}  -> pure ()

makePayment :: IO ()
makePayment = do
  showLogsOnFailure $ \tr -> do
    failAfter 10 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet (contramap TLNode tr) tmp $ \runningNode -> do
          let lovelacePerUtxo = 100_000_000
              numUtxos        = 10
          wllt <- W.createSeededWallet (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          bal <- Utxos.totalBalance <$> W.walletUtxos runningNode wllt
          assertEqual "Wallet should have the expected balance" (fromIntegral numUtxos * lovelacePerUtxo) (C.selectLovelace bal)

runWalletServer :: IO ()
runWalletServer =
  showLogsOnFailure $ \tr -> do
    withTempDir "cardano-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap TLNode tr) tmp $ \node ->
        withWallet (contramap TWallet tr) tmp node $ \wllt -> do
          bal <- Utxos.totalBalance <$> getUTxOs wllt
          let lovelacePerUtxo = 100_000_000
              numUtxos        = 10 :: Int
          assertEqual "Wallet should have the correct balance" (fromIntegral numUtxos * lovelacePerUtxo) (C.selectLovelace bal)

changeMaxTxSize :: IO ()
changeMaxTxSize =
  let getMaxTxSize = fmap (C.protocolParamMaxTxSize . C.unbundleProtocolParams) . queryProtocolParameters . fst . rnConnectInfo in
  showLogsOnFailure $ \tr -> do
    withTempDir "cardano-cluster" $ \tmp -> do
      standardTxSize <- withCardanoNodeDevnetConfig (contramap TLNode tr) tmp mempty getMaxTxSize
      largeTxSize <- withCardanoNodeDevnetConfig (contramap TLNode tr) tmp allowLargeTransactions getMaxTxSize
      assertEqual "tx size should be large" (2 * standardTxSize) largeTxSize

data TestLog =
  TLWallet WalletLog | TLNode NodeLog | TWallet WS.WalletLog | SubmitTx{ txId :: C.TxId } | FoundTx{txId :: C.TxId }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
