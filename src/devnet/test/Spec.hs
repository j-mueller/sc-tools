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
                                             RunningStakePoolNode (..),
                                             StakePoolNodeParams (..),
                                             GenesisConfigChanges (..),
                                             defaultStakePoolNodeParams,
                                             allowLargeTransactions,
                                             getCardanoNodeVersion,
                                             withCardanoNodeDevnet,
                                             withCardanoNodeDevnetConfig,
                                             withCardanoStakePoolNodeDevnetConfig,
                                             waitForNextEpoch)
import           Convex.Devnet.Logging      (contramap, showLogsOnFailure)
import           Convex.Devnet.NodeQueries  (loadConnectInfo)
import           Convex.Devnet.Utils        (failAfter, failure, withTempDir)
import           Convex.Devnet.Wallet       (WalletLog)
import qualified Convex.Devnet.Wallet       as W
import           Convex.Devnet.WalletServer (getUTxOs, withWallet)
import qualified Convex.Devnet.WalletServer as WS
import           Convex.NodeQueries         (queryProtocolParameters,
                                             queryStakePools, queryStakeAddresses)
import qualified Convex.Utxos               as Utxos
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.List                  (isInfixOf)
import qualified Data.Text                  as Text
import           Data.Ratio                 ((%))
import qualified Data.Set                   as Set
import qualified Data.Map                   as Map
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
    , testCase "start local stake pool node" startLocalStakePoolNode
    , testCase "stake pool registration" registeredStakePoolNode
    , testCase "stake reward" stakeReward
    , testCase "run the wallet server" runWalletServer
    , testCase "change max tx size" changeMaxTxSize
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

startLocalStakePoolNode :: IO ()
startLocalStakePoolNode = do
  showLogsOnFailure $ \tr -> do
    failAfter 10 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet (contramap TLNode tr)  tmp $ \runningNode -> do
          let lovelacePerUtxo = 100_000_000
              numUtxos        = 10
          wllt <- W.createSeededWallet (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          withTempDir "cardano-cluster-stakepool" $ \tmp' -> do
            withCardanoStakePoolNodeDevnetConfig (contramap TLNode tr) tmp' wllt defaultStakePoolNodeParams mempty runningNode $ \RunningStakePoolNode{rspnNode} -> do
              runExceptT (loadConnectInfo (rnNodeConfigFile rspnNode) (rnNodeSocket rspnNode)) >>= \case
                Left err -> failure (Text.unpack (C.renderInitialLedgerStateError err))
                Right{}  -> pure ()

registeredStakePoolNode :: IO ()
registeredStakePoolNode = do
  showLogsOnFailure $ \tr -> do
    failAfter 10 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet (contramap TLNode tr)  tmp $ \runningNode@RunningNode{rnConnectInfo} -> do
          let lovelacePerUtxo = 100_000_000
              numUtxos        = 10
          wllt <- W.createSeededWallet (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          let mode = fst rnConnectInfo
          initialStakePools <- queryStakePools mode
          withTempDir "cardano-cluster-stakepool" $ \tmp' -> do
            withCardanoStakePoolNodeDevnetConfig (contramap TLNode tr) tmp' wllt defaultStakePoolNodeParams mempty runningNode $ \_ -> do
              currentStakePools <- queryStakePools mode
              let
                initial = length initialStakePools
                current = length currentStakePools
              assertEqual "Blockchain should have one new registered stake pool" 1 (current - initial)

stakeReward :: IO ()
stakeReward = do
  showLogsOnFailure $ \tr -> do
    failAfter 10 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnetConfig (contramap TLNode tr) tmp confChange $ \runningNode@RunningNode{rnConnectInfo} -> do
          let lovelacePerUtxo = 1_000_000_000
              numUtxos        = 10
          wllt <- W.createSeededWallet (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          let mode = fst rnConnectInfo
              stakepoolParams = StakePoolNodeParams 400 (1 % 100) 100_000_000
          withTempDir "cardano-cluster-stakepool" $ \tmp' -> do
            withCardanoStakePoolNodeDevnetConfig (contramap TLNode tr) tmp' wllt stakepoolParams confChange runningNode $ \RunningStakePoolNode{rspnNode, rspnStakeKey} -> do
              let nid = rnNetworkId rspnNode
                  stakeHash = C.verificationKeyHash . C.getVerificationKey $ rspnStakeKey
                  stakeCred = C.StakeCredentialByKey stakeHash
              oldRewards <- sum . Map.elems . fst <$> queryStakeAddresses mode (Set.singleton stakeCred) nid
              _ <- waitForNextEpoch runningNode
              newRewards <- sum . Map.elems . fst <$> queryStakeAddresses mode (Set.singleton stakeCred) nid

              assertBool "Expect staking rewards" $ oldRewards < newRewards
  where
    confChange =
      GenesisConfigChanges (\g -> g {C.sgEpochLength = 1}) id

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
  let getMaxTxSize = fmap C.protocolParamMaxTxSize . queryProtocolParameters . fst . rnConnectInfo in
  showLogsOnFailure $ \tr -> do
    withTempDir "cardano-cluster" $ \tmp -> do
      standardTxSize <- withCardanoNodeDevnetConfig (contramap TLNode tr) tmp mempty getMaxTxSize
      largeTxSize <- withCardanoNodeDevnetConfig (contramap TLNode tr) tmp allowLargeTransactions getMaxTxSize
      assertEqual "tx size should be large" (2 * standardTxSize) largeTxSize

data TestLog =
  TLWallet WalletLog | TLNode NodeLog | TWallet WS.WalletLog | SubmitTx{ txId :: C.TxId } | FoundTx{txId :: C.TxId }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
