{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Main where

import qualified Cardano.Api               as C
import           Control.Monad.Except      (runExceptT)
import qualified Convex.BuildTx            as BuildTx
import           Convex.Devnet.CardanoNode (NodeLog, RunningNode (..),
                                            getCardanoNodeVersion,
                                            withCardanoNodeDevnet)
import           Convex.Devnet.Logging     (contramap, showLogsOnFailure,
                                            traceWith)
import           Convex.Devnet.NodeQueries (loadConnectInfo, waitForTxn)
import           Convex.Devnet.Utils       (failAfter, failure, withTempDir)
import           Convex.Devnet.Wallet      (WalletLog)
import qualified Convex.Devnet.Wallet      as W
import           Convex.Lenses             (emptyTx)
import           Convex.Utils              (txnUtxos)
import qualified Convex.Wallet             as W
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Function             ((&))
import           Data.List                 (isInfixOf)
import qualified Data.Text                 as Text
import           GHC.Generics              (Generic)
import           GHC.IO.Encoding           (setLocaleEncoding, utf8)
import           Test.Tasty                (defaultMain, testGroup)
import           Test.Tasty.HUnit          (assertBool, testCase)

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
  let expectedVersion = "1.35.3"
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
        withCardanoNodeDevnet (contramap TLNode tr) tmp $ \runningNode@RunningNode{rnNodeSocket, rnNetworkId} -> do
          fct <- W.faucet
          wllt <- W.generateWallet
          tx <- W.balanceAndSubmit (contramap TLWallet tr) runningNode fct $
            emptyTx & BuildTx.payToAddress (W.addressInEra rnNetworkId wllt) (C.lovelaceToValue 100_000_000)
          let txi = C.getTxId (C.getTxBody tx)
          traceWith tr (SubmitTx txi)
          waitForTxn rnNetworkId rnNodeSocket (head $ txnUtxos tx)
          traceWith tr (FoundTx txi)

data TestLog =
  TLWallet WalletLog | TLNode NodeLog | SubmitTx{ txId :: C.TxId } | FoundTx{txId :: C.TxId }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
