{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Main where

import qualified Cardano.Api                     as C
import qualified Cardano.Api.Shelley             as C
import           Cardano.Ledger.Api.PParams      (ppMaxTxSizeL)
import           Cardano.Ledger.Slot             (EpochSize (..))
import           Control.Concurrent              (threadDelay)
import           Control.Lens                    (view)
import           Control.Monad                   (unless)
import           Control.Monad.Except            (runExceptT)
import           Convex.Devnet.CardanoNode       (NodeLog (..),
                                                  getCardanoNodeVersion,
                                                  withCardanoNodeDevnet,
                                                  withCardanoNodeDevnetConfig,
                                                  withCardanoStakePoolNodeDevnetConfig)
import           Convex.Devnet.CardanoNode.Types (GenesisConfigChanges (..),
                                                  PortsConfig (..),
                                                  RunningNode (..),
                                                  RunningStakePoolNode (..),
                                                  StakePoolNodeParams (..),
                                                  allowLargeTransactions,
                                                  defaultPortsConfig,
                                                  defaultStakePoolNodeParams,
                                                  forkIntoConwayInEpoch)
import           Convex.Devnet.Logging           (contramap, showLogsOnFailure)
import           Convex.Devnet.NodeQueries       (loadConnectInfo)
import qualified Convex.Devnet.NodeQueries       as Queries
import           Convex.Devnet.Utils             (failAfter, failure,
                                                  withTempDir)
import           Convex.Devnet.Wallet            (WalletLog)
import qualified Convex.Devnet.Wallet            as W
import           Convex.Devnet.WalletServer      (getUTxOs, withWallet)
import qualified Convex.Devnet.WalletServer      as WS
import qualified Convex.Eras                     as Eras
import           Convex.NodeQueries              (tryQueryProtocolParameters,
                                                  tryQueryStakeAddresses,
                                                  tryQueryStakePools)
import qualified Convex.Utxos                    as Utxos
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.List                       (isInfixOf)
import qualified Data.Map                        as Map
import           Data.Ratio                      ((%))
import qualified Data.Set                        as Set
import           GHC.Generics                    (Generic)
import           GHC.IO.Encoding                 (setLocaleEncoding, utf8)
import           System.FilePath                 ((</>))
import           Test.Tasty                      (defaultMain, testGroup)
import           Test.Tasty.HUnit                (assertBool, assertEqual,
                                                  testCase)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $ testGroup "test"
    [ testCase "cardano-node is available" checkCardanoNode
    , testCase "start local stake pool node" startLocalStakePoolNode
    , testCase "stake pool registration" registeredStakePoolNode
    , testCase "stake pool rewards" stakePoolRewards
    , testCase "change max tx size" changeMaxTxSize
    , testGroup "babbage"
        [ testCase "start local node in babbage era" (startLocalNodeInEra mempty (C.anyCardanoEra C.BabbageEra))
        , testCase "run wallet in babbage era" (runWalletServer mempty)
        , testCase "make a payment in babbage era" (makePayment mempty)
        ]
    , testGroup "conway"
        [ testCase "start local node in conway era" (startLocalNodeInEra (forkIntoConwayInEpoch 0) (C.anyCardanoEra C.ConwayEra))
        , testCase "run wallet in conway era" (runWalletServer $ forkIntoConwayInEpoch 0)
        , testCase "make a payment in conway era" (makePayment $ forkIntoConwayInEpoch 0)
        ]
    ]

checkCardanoNode :: IO ()
checkCardanoNode = do
  let expectedVersion = "9.1.0"
  version <- getCardanoNodeVersion
  let isExpected = expectedVersion `isInfixOf` version
  unless isExpected (putStrLn version)
  assertBool ("cardano-node version should be " <> expectedVersion) isExpected

startLocalNodeInEra :: GenesisConfigChanges -> C.AnyCardanoEra -> IO ()
startLocalNodeInEra config expectedEra = do
    showLogsOnFailure $ \tr -> do
      failAfter 5 $
        withTempDir "cardano-cluster" $ \tmp -> do
          withCardanoNodeDevnetConfig tr tmp config defaultPortsConfig $ \RunningNode{rnNodeSocket, rnNodeConfigFile, rnNetworkId} -> do
            runExceptT (loadConnectInfo rnNodeConfigFile rnNodeSocket) >>= \case
              Left err -> failure (show err)
              Right{}  -> do
                Queries.queryEra rnNetworkId rnNodeSocket
                  >>= assertBool "Should be in expected era" . (==) expectedEra

startLocalStakePoolNode :: IO ()
startLocalStakePoolNode = do
  showLogsOnFailure $ \tr -> do
    failAfter 10 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet (contramap TLNode tr) tmp $ \runningNode -> do
          let lovelacePerUtxo = 100_000_000
              numUtxos        = 10
              nodeConfigFile  = tmp </> "cardano-node.json"
          wllt <- W.createSeededWallet (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          withTempDir "cardano-cluster-stakepool" $ \tmp' -> do
            withCardanoStakePoolNodeDevnetConfig (contramap TLNode tr) tmp' wllt defaultStakePoolNodeParams nodeConfigFile (PortsConfig 3002 [3001]) runningNode $ \RunningStakePoolNode{rspnNode} -> do
              runExceptT (loadConnectInfo (rnNodeConfigFile rspnNode) (rnNodeSocket rspnNode)) >>= \case
                Left err -> failure (show err)
                Right{}  -> pure ()

registeredStakePoolNode :: IO ()
registeredStakePoolNode = do
  showLogsOnFailure $ \tr -> do
    failAfter 10 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet (contramap TLNode tr)  tmp $ \runningNode@RunningNode{rnConnectInfo} -> do
          let lovelacePerUtxo = 100_000_000
              numUtxos        = 10
              nodeConfigFile  = tmp </> "cardano-node.json"
          wllt <- W.createSeededWallet (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          let mode = fst rnConnectInfo
          initialStakePools <- tryQueryStakePools mode
          withTempDir "cardano-cluster-stakepool" $ \tmp' -> do
            withCardanoStakePoolNodeDevnetConfig (contramap TLNode tr) tmp' wllt defaultStakePoolNodeParams nodeConfigFile (PortsConfig 3002 [3001])  runningNode $ \_ -> do
              currentStakePools <- tryQueryStakePools mode
              let
                initial = length initialStakePools
                current = length currentStakePools
              assertEqual "Blockchain should have one new registered stake pool" 1 (current - initial)

stakePoolRewards :: IO ()
stakePoolRewards = do
  showLogsOnFailure $ \tr -> do
    failAfter 50 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnetConfig (contramap TLNode tr) tmp confChange (PortsConfig 3001 [3002]) $ \runningNode -> do
          let lovelacePerUtxo = 10_000_000_000
              numUtxos        = 4
              nodeConfigFile  = tmp </> "cardano-node.json"
          wllt <- W.createSeededWallet (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          let stakepoolParams = StakePoolNodeParams 340_000_000 (1 % 100) 10_000_000_000 --100_000_000
          withTempDir "cardano-cluster-stakepool" $ \tmp' -> do
            withCardanoStakePoolNodeDevnetConfig (contramap TLNode tr) tmp' wllt stakepoolParams nodeConfigFile (PortsConfig 3002 [3001]) runningNode $ \RunningStakePoolNode{rspnNode, rspnStakeKey} -> do
              let stakeHash = C.verificationKeyHash . C.getVerificationKey $ rspnStakeKey
                  stakeCred = C.StakeCredentialByKey stakeHash
              rewards <- waitForStakeRewards rspnNode  stakeCred
              assertBool "Expect staking rewards" $ rewards > 0
    where
      confChange =
        GenesisConfigChanges
          (\g -> g
            { C.sgEpochLength = EpochSize 10
            , C.sgSlotLength = 1
            , C.sgSecurityParam = 1
            }
          ) id id id

      getStakeRewards :: RunningNode -> C.StakeCredential -> IO C.Quantity
      getStakeRewards RunningNode{rnConnectInfo} cred =
        let
          mode  = fst rnConnectInfo
          creds = Set.singleton cred
        in
         sum . Map.elems . fst <$> tryQueryStakeAddresses mode creds

      waitForStakeRewards :: RunningNode -> C.StakeCredential -> IO C.Quantity
      waitForStakeRewards node cred =
        getStakeRewards node cred >>= waitForStakeRewards' node cred

      waitForStakeRewards' :: RunningNode -> C.StakeCredential -> C.Quantity -> IO C.Quantity
      waitForStakeRewards' node cred amount = do
        rewards <- getStakeRewards node cred
        if rewards > amount then
          pure rewards else
          threadDelay 1_000_000 >> waitForStakeRewards' node cred amount

makePayment :: GenesisConfigChanges -> IO ()
makePayment config = do
  showLogsOnFailure $ \tr -> do
    failAfter 10 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnetConfig (contramap TLNode tr) tmp config defaultPortsConfig $ \runningNode -> do
          let lovelacePerUtxo = 100_000_000
              numUtxos        = 10
          wllt <- W.createSeededWallet (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          bal <- Utxos.totalBalance <$> W.walletUtxos runningNode wllt
          assertEqual "Wallet should have the expected balance" (fromIntegral numUtxos * lovelacePerUtxo) (C.lovelaceToQuantity $ C.selectLovelace bal)

runWalletServer :: GenesisConfigChanges -> IO ()
runWalletServer config =
  showLogsOnFailure $ \tr -> do
    withTempDir "cardano-cluster" $ \tmp -> do
      withCardanoNodeDevnetConfig (contramap TLNode tr) tmp config defaultPortsConfig $ \node ->
        withWallet (contramap TWallet tr) tmp node $ \wllt -> do
          bal <- Utxos.totalBalance <$> getUTxOs wllt
          let lovelacePerUtxo = 100_000_000
              numUtxos        = 10 :: Int
          assertEqual "Wallet should have the correct balance" (fromIntegral numUtxos * lovelacePerUtxo) (C.selectLovelace bal)

changeMaxTxSize :: IO ()
changeMaxTxSize =
  let getMaxTxSize = fmap (view ppMaxTxSizeL . C.unLedgerProtocolParameters . Eras.babbageProtocolParams) . tryQueryProtocolParameters . fst . rnConnectInfo in
  showLogsOnFailure $ \tr -> do
    withTempDir "cardano-cluster" $ \tmp -> do
      standardTxSize <- withCardanoNodeDevnet (contramap TLNode tr) tmp getMaxTxSize
      largeTxSize <- withCardanoNodeDevnetConfig (contramap TLNode tr) tmp allowLargeTransactions defaultPortsConfig getMaxTxSize
      assertEqual "tx size should be large" (2 * standardTxSize) largeTxSize

data TestLog =
  TLWallet WalletLog | TLNode NodeLog | TWallet WS.WalletLog | SubmitTx{ txId :: C.TxId } | FoundTx{txId :: C.TxId }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
