{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Main where

import qualified Cardano.Api               as C
import           Control.Monad.Except      (runExceptT)
import           Convex.Devnet.CardanoNode (NodeLog, RunningNode (..),
                                            getCardanoNodeVersion,
                                            withCardanoNodeDevnet)
import           Convex.Devnet.Logging     (contramap, showLogsOnFailure)
import           Convex.Devnet.NodeQueries (loadConnectInfo)
import           Convex.Devnet.Utils       (failAfter, failure, withTempDir)
import           Convex.Devnet.Wallet      (WalletLog)
import qualified Convex.Devnet.Wallet      as W
import qualified Convex.Utxos              as Utxos
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.List                 (isInfixOf)
import qualified Data.Text                 as Text
import           GHC.Generics              (Generic)
import           GHC.IO.Encoding           (setLocaleEncoding, utf8)
import           Test.Tasty                (defaultMain, testGroup)
import           Test.Tasty.HUnit          (assertBool, assertEqual, testCase)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $ testGroup "test"
    [ testCase "cardano-node is available" checkCardanoNode
    , testCase "start local node" startLocalNode
    , testCase "make a payment" makePayment
    ]

checkCardanoNode :: IO ()
checkCardanoNode =
  let expectedVersion = "1.35.4"
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
          wllt <- W.createSeededWallet (contramap TLWallet tr) runningNode 100_000_000
          bal <- Utxos.totalBalance <$> W.walletUtxos runningNode wllt
          assertEqual "Wallet should have the expected balance" 100_000_000 (C.selectLovelace bal)

data TestLog =
  TLWallet WalletLog | TLNode NodeLog | SubmitTx{ txId :: C.TxId } | FoundTx{txId :: C.TxId }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
