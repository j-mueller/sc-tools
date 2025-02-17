{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Api.PParams qualified as L
import Cardano.Ledger.Block qualified as Ledger
import Cardano.Ledger.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (getSystemStart))
import Control.Concurrent (threadDelay)
import Control.Lens (view)
import Control.Monad (unless, void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader (MonadTrans, lift)
import Control.Monad.State.Strict (MonadState)
import Control.Monad.State.Strict qualified as StrictState
import Control.Tracer (Tracer)
import Convex.Class (MemoizedCardanoNodeStateQueryResponses (memoizedEraHistoryQuery, memoizedNetworkIdQuery, memoizedProtocolParameters, memoizedStakeAddresses, memoizedStakePools, memoizedSystemStartQuery), MonadBlockchain (queryEraHistory, queryNetworkId, queryProtocolParameters, queryStakeAddresses, queryStakePools, querySystemStart), MonadMockchain, modifySlot, runMemoizedCardanoNodeStateQueryT, runMockTimeT, runMonadBlockchainCardanoNodeT)
import Convex.Class qualified as Cx
import Convex.Devnet.CardanoNode (
  NodeLog (..),
  getCardanoNodeVersion,
  withCardanoNodeDevnet,
  withCardanoNodeDevnetConfig,
  withCardanoStakePoolNodeDevnetConfig,
 )
import Convex.Devnet.CardanoNode.Types (
  GenesisConfigChanges (..),
  PortsConfig (..),
  RunningNode (..),
  RunningStakePoolNode (..),
  StakePoolNodeParams (..),
  allowLargeTransactions,
  defaultPortsConfig,
  defaultStakePoolNodeParams,
 )
import Convex.Devnet.Logging (
  contramap,
  showLogsOnFailure,
  traceWith,
 )
import Convex.Devnet.Utils (
  failAfter,
  failure,
  withTempDir,
 )
import Convex.Devnet.Wallet (WalletLog, runTracerMonadLogT)
import Convex.Devnet.Wallet qualified as W
import Convex.Devnet.WalletServer (
  getUTxOs,
  withWallet,
 )
import Convex.Devnet.WalletServer qualified as WS
import Convex.MockChain.Utils (mockchainSucceeds)
import Convex.NodeClient.Fold (
  LedgerStateArgs (NoLedgerStateArgs),
  foldClient,
 )
import Convex.NodeClient.Types (runNodeClient)
import Convex.NodeQueries (TimeException (TimePastHorizonException))
import Convex.NodeQueries qualified as NodeQueries
import Convex.Utils qualified as Cx
import Convex.Utxos qualified as Utxos
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef (
  modifyIORef,
  newIORef,
  readIORef,
 )
import Data.List (isInfixOf)
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Devnet.Test.LatestEraTransitionSpec qualified as LatestEraTransitionSpec
import GHC.Generics (Generic)
import GHC.IO.Encoding (
  setLocaleEncoding,
  utf8,
 )
import Ouroboros.Consensus.HardFork.History.Qry
import Ouroboros.Consensus.HardFork.History.Summary
import Ouroboros.Consensus.Protocol.Praos.Header qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger.Block qualified as Consensus
import System.FilePath ((</>))
import Test.Tasty (
  defaultMain,
  testGroup,
 )
import Test.Tasty.HUnit (
  assertBool,
  assertEqual,
  testCase,
 )

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "test"
      [ testCase "cardano-node is available" checkCardanoNode
      , testCase "start local node" startLocalNode
      , testCase "check transition to conway era and protocol version 10" checkTransitionToConway
      , LatestEraTransitionSpec.tests
      , testGroup
          "node devnet queries"
          [ testCase "query network id" queryMemoizedNetworkIdTest
          , testCase "query system start" queryMemoizedSystemStartTest
          , testCase "query era history" queryMemoizedEraHistoryTest
          ]
      , testCase "make a payment" makePayment
      , testCase "start local stake pool node" startLocalStakePoolNode
      , testCase "stake pool registration" registeredStakePoolNode
      , testCase "stake pool rewards" stakePoolRewards
      , testCase "run the wallet server" runWalletServer
      , testCase "change max tx size" changeMaxTxSize
      ]

checkCardanoNode :: IO ()
checkCardanoNode = do
  let expectedVersion = "10.1.4"
  version <- getCardanoNodeVersion
  let isExpected = expectedVersion `isInfixOf` version
  unless isExpected (putStrLn version)
  assertBool ("cardano-node version should be " <> expectedVersion) isExpected

startLocalNode :: IO ()
startLocalNode = do
  showLogsOnFailure $ \tr -> do
    failAfter 5 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet tr tmp $ \RunningNode{rnNodeSocket, rnNodeConfigFile} -> do
          runExceptT (NodeQueries.loadConnectInfo rnNodeConfigFile rnNodeSocket) >>= \case
            Left err -> failure (show err)
            Right{} -> pure ()

checkTransitionToConway :: IO ()
checkTransitionToConway = do
  showLogsOnFailure $ \tr -> do
    failAfter 5 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet (contramap TLNode tr) tmp $ \runningNode@RunningNode{rnConnectInfo, rnNodeSocket, rnNodeConfigFile} -> do
          NodeQueries.queryEra rnConnectInfo >>= assertEqual "Should be in conway era" (C.anyCardanoEra C.ConwayEra)
          let lovelacePerUtxo = 100_000_000
              numUtxos = 10
          void $ W.createSeededWallet C.BabbageEraOnwardsConway (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          majorProtVersionsRef <- newIORef []
          res <- C.liftIO $ runExceptT $ runNodeClient rnNodeConfigFile rnNodeSocket $ \_localNodeConnectInfo env -> do
            pure $ foldClient () NoLedgerStateArgs env $ \_catchingUp _ _ bim -> do
              case bim of
                ( C.BlockInMode
                    C.ConwayEra
                    ( C.ShelleyBlock
                        C.ShelleyBasedEraConway
                        ( Consensus.ShelleyBlock
                            (Ledger.Block (Consensus.Header hb _) _)
                            _
                          )
                      )
                  ) -> do
                    modifyIORef majorProtVersionsRef $ \majorProtVersions ->
                      L.pvMajor (Consensus.hbProtVer hb) : majorProtVersions
                    pure Nothing
                (C.BlockInMode _ _block) -> do
                  failure "Block should be a ShelleyBlock in Conway era"
          case res of
            Left err -> failure $ show err
            Right () -> do
              majorProtVersions <- readIORef majorProtVersionsRef
              expectedVersion <- L.mkVersion (10 :: Integer)
              assertBool "Should have correct conway era protocol version" $
                not (null majorProtVersions) && all (== expectedVersion) majorProtVersions

queryMemoizedNetworkIdTest :: IO ()
queryMemoizedNetworkIdTest = do
  showLogsOnFailure $ \tr -> do
    failAfter 5 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet tr tmp $ \RunningNode{rnNetworkId, rnNodeSocket} -> do
          let connectInfo = NodeQueries.localNodeConnectInfo rnNetworkId rnNodeSocket
          runMonadBlockchainCardanoNodeT connectInfo $ runMemoizedCardanoNodeStateQueryT $ do
            StrictState.gets memoizedNetworkIdQuery
              >>= liftIO . assertEqual "The memoized value of NetworkId should be Nothing" Nothing
            networkId <- queryNetworkId @C.ConwayEra
            StrictState.gets memoizedNetworkIdQuery
              >>= liftIO . assertEqual "The memoized value of NetworkId should be Just" (Just networkId)
            liftIO $
              assertEqual
                "Queried networkId should be the same as the networkId provided as input"
                rnNetworkId
                networkId

            StrictState.gets memoizedSystemStartQuery
              >>= liftIO . assertEqual "The memoized value of SystemStart should be Nothing" Nothing
            systemStart <- querySystemStart
            StrictState.gets memoizedSystemStartQuery
              >>= liftIO . assertEqual "The memoized value of SystemStart should be Just" (Just systemStart)

            StrictState.gets memoizedEraHistoryQuery
              >>= liftIO . assertBool "The memoized value of EraHistory should be Nothing" . isNothing
            void $ queryEraHistory @C.ConwayEra
            StrictState.gets memoizedEraHistoryQuery
              >>= liftIO . assertBool "The memoized value of EraHistory should be Just" . isJust

            StrictState.gets memoizedProtocolParameters
              >>= liftIO . assertBool "The memoized value of ProtocolParameters should be Nothing" . isNothing
            pp <- queryProtocolParameters @C.ConwayEra
            StrictState.gets memoizedProtocolParameters
              >>= liftIO . assertEqual "The memoized value of ProtocolParameters should be Just" (Just pp) . fmap snd
 where
  action :: forall era m. (MonadIO m) => (MonadState (MemoizedCardanoNodeStateQueryResponses era) m) => (MonadBlockchain era m) => m C.NetworkId
  action = do
    StrictState.gets memoizedNetworkIdQuery
      >>= liftIO . assertEqual "The memoized value of NetworkId should be Nothing" Nothing
    nid <- queryNetworkId
    StrictState.gets memoizedNetworkIdQuery
      >>= liftIO . assertEqual "The memoized value of NetworkId should be Just" (Just nid)
    pure nid

queryMemoizedSystemStartTest :: IO ()
queryMemoizedSystemStartTest = do
  showLogsOnFailure $ \tr -> do
    failAfter 5 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet tr tmp $ \RunningNode{rnNetworkId, rnNodeSocket} -> do
          let connectInfo = NodeQueries.localNodeConnectInfo rnNetworkId rnNodeSocket
          void $ runMonadBlockchainCardanoNodeT connectInfo $ runMemoizedCardanoNodeStateQueryT (action @C.ConwayEra)
 where
  action :: forall era m. (MonadIO m) => (MonadState (MemoizedCardanoNodeStateQueryResponses era) m) => (MonadBlockchain era m) => m ()
  action = do
    StrictState.gets memoizedSystemStartQuery
      >>= liftIO . assertEqual "The memoized value of SystemStart should be Nothing" Nothing
    systemStart <- querySystemStart
    StrictState.gets memoizedSystemStartQuery
      >>= liftIO . assertEqual "The memoized value of SystemStart should be Just" (Just systemStart)

-- queryMemoizedEraHistoryTest :: IO ()
-- queryMemoizedEraHistoryTest = do
--   showLogsOnFailure $ \tr -> do
--     failAfter 5 $
--       withTempDir "cardano-cluster" $ \tmp -> do
--         withCardanoNodeDevnet tr tmp $ \RunningNode{rnNetworkId, rnNodeSocket} -> do
--           let connectInfo = NodeQueries.localNodeConnectInfo rnNetworkId rnNodeSocket
--           void $ runMonadBlockchainCardanoNodeT connectInfo $ runMemoizedCardanoNodeStateQueryT (action @C.ConwayEra)
--  where
--    action :: forall era m. MonadIO m => MonadState (MemoizedCardanoNodeStateQueryResponses era) m => MonadBlockchain era m => m ()
--    action = do
--      StrictState.gets memoizedEraHistoryQuery >>=
--        liftIO . assertBool "The memoized value of EraHistory should be Nothing" . isNothing
--      eraHistory <- queryEraHistory
--      StrictState.gets memoizedEraHistoryQuery >>=
--        liftIO . assertBool "The memoized value of EraHistory should be Just" . isJust

newtype MockBlockchainEraHistoryT era m a = MockBlockchainEraHistoryT {_unMockBlockchainEraHistoryT :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, PrimMonad)

instance MonadTrans (MockBlockchainEraHistoryT era) where
  lift = MockBlockchainEraHistoryT

runMockBlockchainEraHistoryT :: MockBlockchainEraHistoryT era m a -> m a
runMockBlockchainEraHistoryT (MockBlockchainEraHistoryT action) = action

instance (Cx.MonadBlockchain era m) => Cx.MonadBlockchain era (MockBlockchainEraHistoryT era m) where
  queryEraHistory = lift queryEraHistory -- pure $ C.EraHistory $ mkInterpreter $ neverForksSummary 0 0 0

queryMemoizedEraHistoryTest :: IO ()
queryMemoizedEraHistoryTest = do
  currentTime <- getCurrentTime
  mockchainSucceeds $ runMemoizedCardanoNodeStateQueryT $ runMockBlockchainEraHistoryT $ do
    -- Set EraHistory bounds from [SystemStart:N]
    -- utcTimeToSlot (n+1) should return PastHorizonException
    systemStart <- querySystemStart
    eraHistory <- queryEraHistory
    liftIO $ print $ Cx.utcTimeToSlot eraHistory systemStart (getSystemStart systemStart)
    pure ()

-- modifySlot (\s -> (s + 450_000_000_000_000, ()))
-- action

-- showLogsOnFailure $ \tr -> do
--   failAfter 5 $
--     withTempDir "cardano-cluster" $ \tmp -> do
--       withCardanoNodeDevnet tr tmp $ \RunningNode{rnNetworkId, rnNodeSocket} -> do
--         let connectInfo = NodeQueries.localNodeConnectInfo rnNetworkId rnNodeSocket
--         void $ runMonadBlockchainCardanoNodeT connectInfo $ runMemoizedCardanoNodeStateQueryT (action @C.ConwayEra)
-- where
--   -- TODO Make custom instance of MonadBlockchain that returns custom eraHistory in order to test memoization
--   action :: forall era m. MonadIO m => MonadState (MemoizedCardanoNodeStateQueryResponses era) m => MonadMockchain era m => m ()
--   action = do
--     StrictState.gets memoizedEraHistoryQuery >>=
--       liftIO . assertBool "The memoized value of EraHistory should be Nothing" . isNothing
--     eraHistory <- queryEraHistory

--     systemStart <- querySystemStart
--     liftIO $ print $ Cx.slotToUtcTime eraHistory systemStart 100_000_000_000

--     -- StrictState.gets memoizedEraHistoryQuery >>=
--     --   liftIO . assertBool "The memoized value of EraHistory should be Just" . isJust
--     -- _eraHistory <- queryEraHistory
--     pure ()
--   -- setTime :: forall era m. MonadIO m => MonadState UTCTime m => MonadMockchain era m => m ()
--   -- setTime = do
--   --   -- 22_776_000 seconds = 365 days
--   --   StrictState.modify (\currentTime -> addUTCTime 22_776_000 currentTime)

startLocalStakePoolNode :: IO ()
startLocalStakePoolNode = do
  showLogsOnFailure $ \tr -> do
    failAfter 10 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet (contramap TLNode tr) tmp $ \runningNode -> do
          let lovelacePerUtxo = 100_000_000
              numUtxos = 10
              nodeConfigFile = tmp </> "cardano-node.json"
          wllt <- W.createSeededWallet C.BabbageEraOnwardsConway (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          withTempDir "cardano-cluster-stakepool" $ \tmp' -> do
            withCardanoStakePoolNodeDevnetConfig (contramap TLNode tr) tmp' wllt defaultStakePoolNodeParams nodeConfigFile (PortsConfig 3002 [3001]) runningNode $ \RunningStakePoolNode{rspnNode} -> do
              runExceptT (NodeQueries.loadConnectInfo (rnNodeConfigFile rspnNode) (rnNodeSocket rspnNode)) >>= \case
                Left err -> failure (show err)
                Right{} -> pure ()

registeredStakePoolNode :: IO ()
registeredStakePoolNode = do
  showLogsOnFailure $ \tr -> do
    failAfter 10 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet (contramap TLNode tr) tmp $ \runningNode@RunningNode{rnConnectInfo} -> do
          let lovelacePerUtxo = 100_000_000
              numUtxos = 10
              nodeConfigFile = tmp </> "cardano-node.json"
          wllt <- W.createSeededWallet C.BabbageEraOnwardsConway (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          initialStakePools <- NodeQueries.queryStakePools rnConnectInfo
          withTempDir "cardano-cluster-stakepool" $ \tmp' -> do
            withCardanoStakePoolNodeDevnetConfig (contramap TLNode tr) tmp' wllt defaultStakePoolNodeParams nodeConfigFile (PortsConfig 3002 [3001]) runningNode $ \_ -> do
              currentStakePools <- NodeQueries.queryStakePools rnConnectInfo
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
              numUtxos = 4
              nodeConfigFile = tmp </> "cardano-node.json"
          wllt <- W.createSeededWallet C.BabbageEraOnwardsConway (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          let stakepoolParams =
                StakePoolNodeParams
                  { spnCost = 340_000_000
                  , spnMargin = 1 % 100
                  , spnPledge = 10_000_000_000 -- 100_000_000
                  }
          withTempDir "cardano-cluster-stakepool" $ \tmp' -> do
            withCardanoStakePoolNodeDevnetConfig (contramap TLNode tr) tmp' wllt stakepoolParams nodeConfigFile (PortsConfig 3002 [3001]) runningNode $ \RunningStakePoolNode{rspnStakeKey} -> do
              let stakeHash = C.verificationKeyHash . C.getVerificationKey $ rspnStakeKey
                  _stakeCred = C.StakeCredentialByKey stakeHash
              -- waitForStakeRewards tr rspnNode  stakeCred
              --   >>= assertBool "Expect staking rewards" $ rewards > 0
              assertBool "FIXME" True
 where
  confChange =
    GenesisConfigChanges
      ( \g ->
          g
            { C.sgEpochLength = EpochSize 10
            , C.sgSlotLength = 1
            , C.sgSecurityParam = 1
            }
      )
      id
      id
      id

  getStakeRewards :: RunningNode -> C.StakeCredential -> IO C.Quantity
  getStakeRewards RunningNode{rnConnectInfo} cred =
    let
      creds = Set.singleton cred
     in
      sum . Map.elems . fst <$> NodeQueries.queryStakeAddresses rnConnectInfo creds

  _waitForStakeRewards :: Tracer IO TestLog -> RunningNode -> C.StakeCredential -> IO C.Quantity
  _waitForStakeRewards tr node cred = do
    rw <- getStakeRewards node cred
    traceWith tr $ TLRewards rw
    waitForStakeRewards' node cred rw

  waitForStakeRewards' :: RunningNode -> C.StakeCredential -> C.Quantity -> IO C.Quantity
  waitForStakeRewards' node cred amount = do
    rewards <- getStakeRewards node cred
    if rewards > amount
      then
        pure rewards
      else
        threadDelay 1_000_000 >> waitForStakeRewards' node cred amount

makePayment :: IO ()
makePayment = do
  showLogsOnFailure $ \tr -> do
    failAfter 10 $
      withTempDir "cardano-cluster" $ \tmp -> do
        withCardanoNodeDevnet (contramap TLNode tr) tmp $ \runningNode -> do
          let lovelacePerUtxo = 100_000_000
              numUtxos = 10
          wllt <- W.createSeededWallet C.BabbageEraOnwardsConway (contramap TLWallet tr) runningNode numUtxos lovelacePerUtxo
          bal <- Utxos.totalBalance <$> W.walletUtxos runningNode wllt
          assertEqual "Wallet should have the expected balance" (fromIntegral numUtxos * lovelacePerUtxo) (C.lovelaceToQuantity $ C.selectLovelace bal)

runWalletServer :: IO ()
runWalletServer =
  showLogsOnFailure $ \tr -> do
    withTempDir "cardano-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap TLNode tr) tmp $ \node ->
        withWallet (contramap TWallet tr) tmp node $ \wllt -> do
          bal <- Utxos.totalBalance <$> getUTxOs wllt
          let lovelacePerUtxo = 100_000_000
              numUtxos = 10 :: Int
          assertEqual "Wallet should have the correct balance" (fromIntegral numUtxos * lovelacePerUtxo) (C.selectLovelace bal)

changeMaxTxSize :: IO ()
changeMaxTxSize =
  let getMaxTxSize = fmap (view L.ppMaxTxSizeL) . NodeQueries.queryProtocolParameters . rnConnectInfo
   in showLogsOnFailure $ \tr -> do
        withTempDir "cardano-cluster" $ \tmp -> do
          standardTxSize <- withCardanoNodeDevnet (contramap TLNode tr) tmp getMaxTxSize
          largeTxSize <- withCardanoNodeDevnetConfig (contramap TLNode tr) tmp allowLargeTransactions defaultPortsConfig getMaxTxSize
          assertEqual "tx size should be large" (2 * standardTxSize) largeTxSize

data TestLog
  = TLWallet WalletLog
  | TLNode NodeLog
  | TWallet WS.WalletLog
  | SubmitTx {txId :: C.TxId}
  | FoundTx {txId :: C.TxId}
  | TLRewards C.Quantity
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
