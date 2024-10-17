{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-| Startup cardano nodes programmatically
-}
module Convex.Devnet.CardanoNode(
  NodeLog(..),
  CardanoNodeArgs(..),
  defaultCardanoNodeArgs,
  withCardanoNode,
  getCardanoNodeVersion,
  waitForFullySynchronized,
  waitForBlock,
  waitForSocket,
  waitForNextBlock,
  waitForNextBlock',
  waitForNextEpoch,
  waitForNextEpoch',
  withCardanoNodeDevnet,
  withCardanoNodeDevnetConfig,
  withCardanoStakePoolNodeDevnetConfig,
) where

import           Cardano.Api                     (NetworkId,
                                                  StakeAddressReference (..))
import qualified Cardano.Api                     as C
import           Cardano.Api.Shelley             (KESPeriod (..),
                                                  OperationalCertificateIssueCounter (..),
                                                  StakeAddressRequirements (..),
                                                  StakeCredential (..),
                                                  StakeDelegationRequirements (..),
                                                  StakePoolParameters (..),
                                                  StakePoolRegistrationRequirements (..),
                                                  toShelleyPoolParams)
import qualified Cardano.Api.Shelley             as C
import qualified Cardano.Ledger.Conway.TxCert    as L
import           Cardano.Slotting.Slot           (withOriginToMaybe)
import           Cardano.Slotting.Time           (diffRelativeTime,
                                                  getRelativeTime,
                                                  toRelativeTime)
import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.Async        (race)
import           Control.Exception               (finally, throwIO)
import           Control.Monad                   (unless, when, (>=>))
import           Control.Monad.Except            (runExceptT)
import           Control.Tracer                  (Tracer, traceWith)
import           Convex.BuildTx                  (addCertificate, execBuildTx,
                                                  payToAddress)
import           Convex.CoinSelection            (ChangeOutputPosition (TrailingChange))
import           Convex.Devnet.CardanoNode.Types (GenesisConfigChanges (..),
                                                  Port, PortsConfig (..),
                                                  RunningNode (..),
                                                  RunningStakePoolNode (..),
                                                  StakePoolNodeParams (..),
                                                  defaultPortsConfig)
import           Convex.Devnet.Utils             (checkProcessHasNotDied,
                                                  defaultNetworkId, failure,
                                                  readConfigFile, withLogFile)
import qualified Convex.Devnet.Wallet            as W
import qualified Convex.NodeQueries              as Q
import           Convex.Wallet                   (Wallet, paymentCredential)
import           Data.Aeson                      (FromJSON, ToJSON (toJSON),
                                                  (.=))
import qualified Data.Aeson                      as Aeson
import qualified Data.Aeson.KeyMap               as Aeson.KeyMap
import qualified Data.ByteString                 as BS
import           Data.Fixed                      (Centi)
import           Data.Functor                    ((<&>))
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Time.Clock                 (UTCTime, addUTCTime,
                                                  getCurrentTime)
import           Data.Time.Clock.POSIX           (posixSecondsToUTCTime,
                                                  utcTimeToPOSIXSeconds)
import           GHC.Generics                    (Generic)
import           System.Directory                (createDirectoryIfMissing,
                                                  doesFileExist, removeFile)
import           System.FilePath                 ((</>))
import           System.IO                       (BufferMode (NoBuffering),
                                                  hSetBuffering)
import           System.Posix                    (ownerReadMode, setFileMode)
import           System.Process                  (CreateProcess (..),
                                                  StdStream (UseHandle), proc,
                                                  readProcess,
                                                  withCreateProcess)

import           Prelude

-- | Arguments given to the 'cardano-node' command-line to run a node.
data CardanoNodeArgs = CardanoNodeArgs
  { nodeSocket             :: FilePath
  , nodeConfigFile         :: FilePath
  , nodeByronGenesisFile   :: FilePath
  , nodeShelleyGenesisFile :: FilePath
  , nodeAlonzoGenesisFile  :: FilePath
  , nodeConwayGenesisFile  :: FilePath
  , nodeTopologyFile       :: FilePath
  , nodeDatabaseDir        :: FilePath
  , nodeDlgCertFile        :: Maybe FilePath
  , nodeSignKeyFile        :: Maybe FilePath
  , nodeOpCertFile         :: Maybe FilePath
  , nodeKesKeyFile         :: Maybe FilePath
  , nodeVrfKeyFile         :: Maybe FilePath
  , nodePort               :: Maybe Port
  }

defaultCardanoNodeArgs :: CardanoNodeArgs
defaultCardanoNodeArgs =
  CardanoNodeArgs
    { nodeSocket = "node.socket"
    , nodeConfigFile = "cardano-node.json"
    , nodeByronGenesisFile = "genesis-byron.json"
    , nodeShelleyGenesisFile = "genesis-shelley.json"
    , nodeAlonzoGenesisFile = "genesis-alonzo.json"
    , nodeConwayGenesisFile = "genesis-conway.json"
    , nodeTopologyFile = "topology.json"
    , nodeDatabaseDir = "db"
    , nodeDlgCertFile = Nothing
    , nodeSignKeyFile = Nothing
    , nodeOpCertFile = Nothing
    , nodeKesKeyFile = Nothing
    , nodeVrfKeyFile = Nothing
    , nodePort = Nothing
    }

getCardanoNodeVersion :: IO String
getCardanoNodeVersion =
  readProcess "cardano-node" ["--version"] ""

data NodeLog
  = MsgNodeCmdSpec Text
  | MsgCLI [Text]
  | MsgCLIStatus Text Text
  | MsgCLIRetry Text
  | MsgCLIRetryResult Text Int
  | MsgNodeStarting {stateDirectory :: FilePath}
  | MsgSocketIsReady FilePath
  | MsgSynchronizing {percentDone :: Centi}
  | MsgNodeIsReady
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

withCardanoNode ::
  Tracer IO NodeLog ->
  NetworkId ->
  FilePath ->
  CardanoNodeArgs ->
  (RunningNode -> IO a) ->
  IO a
withCardanoNode tr networkId stateDirectory args@CardanoNodeArgs{nodeSocket, nodeConfigFile} action = do
  traceWith tr $ MsgNodeCmdSpec (Text.pack $ show $ cmdspec process)
  traceWith tr $ MsgNodeStarting{stateDirectory}
  withLogFile logFilePath $ \out -> do
    hSetBuffering out NoBuffering
    withCreateProcess process{std_out = UseHandle out, std_err = UseHandle out} $
      \_stdin _stdout _stderr processHandle -> do
        race
          (checkProcessHasNotDied "cardano-node" processHandle)
          waitForNode
          `finally` cleanupSocketFile >>= \case
          Left _    -> failure "withCardanoNode: unexpected termination"
          Right res -> pure res

 where
  process = cardanoNodeProcess (Just stateDirectory) args
  logFilePath = stateDirectory </> "logs" </> "cardano-node.log"
  socketPath = stateDirectory </> nodeSocket

  waitForNode = do
    waitForFile socketPath
    let rnNodeConfigFile = stateDirectory </> nodeConfigFile
    traceWith tr $ MsgSocketIsReady socketPath
    (rnConnectInfo, rnEnv) <- runExceptT (Q.loadConnectInfo rnNodeConfigFile socketPath) >>= either (error . (<>) "Failed to load connect info: " . show) pure
    let rn = RunningNode{rnNodeSocket = socketPath, rnNetworkId = networkId, rnNodeConfigFile, rnConnectInfo, rnEnv}
    action rn

  cleanupSocketFile = do
    x <- doesFileExist socketPath
    when x (removeFile socketPath)

-- | Generate command-line arguments for launching @cardano-node@.
cardanoNodeProcess :: Maybe FilePath -> CardanoNodeArgs -> CreateProcess
cardanoNodeProcess cwd args =
  (proc "cardano-node" strArgs){cwd}
 where
  CardanoNodeArgs
    { nodeConfigFile
    , nodeTopologyFile
    , nodeDatabaseDir
    , nodeSocket
    , nodePort
    , nodeSignKeyFile
    , nodeDlgCertFile
    , nodeOpCertFile
    , nodeKesKeyFile
    , nodeVrfKeyFile
    } = args

  strArgs =
    "run" :
    mconcat
      [ ["--config", nodeConfigFile]
      , ["--topology", nodeTopologyFile]
      , ["--database-path", nodeDatabaseDir]
      , ["--socket-path", nodeSocket]
      , opt "--port" (show <$> nodePort)
      , opt "--byron-signing-key" nodeSignKeyFile
      , opt "--byron-delegation-certificate" nodeDlgCertFile
      , opt "--shelley-operational-certificate" nodeOpCertFile
      , opt "--shelley-kes-key" nodeKesKeyFile
      , opt "--shelley-vrf-key" nodeVrfKeyFile
      ]

  opt :: a -> Maybe a -> [a]
  opt arg = \case
    Nothing  -> []
    Just val -> [arg, val]

-- | Wait for the node socket file to become available.
waitForSocket :: RunningNode -> IO ()
waitForSocket = waitForFile . rnNodeSocket

-- | Wait until a file exists
waitForFile :: FilePath -> IO ()
waitForFile fp = do
  x <- doesFileExist fp
  unless x $ do
    threadDelay 100_000
    waitForFile fp

-- | Wait until the node is fully caught up with the network. This can take a
-- while!
waitForFullySynchronized ::
  Tracer IO NodeLog ->
  RunningNode ->
  IO ()
waitForFullySynchronized tracer RunningNode{rnConnectInfo} = do
  systemStart <- Q.querySystemStart rnConnectInfo
  check systemStart
 where
  check systemStart = do
    targetTime <- toRelativeTime systemStart <$> getCurrentTime
    eraHistory <- Q.queryEraHistory rnConnectInfo
    (tipSlotNo, _slotLength) <- Q.queryTipSlotNo rnConnectInfo
    (tipTime, _slotLength) <- either throwIO pure $ C.getProgress tipSlotNo eraHistory
    let timeDifference = diffRelativeTime targetTime tipTime
    let percentDone = realToFrac (100.0 * getRelativeTime tipTime / getRelativeTime targetTime)
    traceWith tracer $ MsgSynchronizing{percentDone}
    if timeDifference < 20 -- TODO: derive from known network and block times
      then pure ()
      else threadDelay 3_000_000 >> check systemStart

{-| Wait until at least one block has been produced (ie. the tip is not genesis)
-}
waitForBlock :: RunningNode -> IO C.BlockNo
waitForBlock n@RunningNode{rnConnectInfo} = do
  withOriginToMaybe <$> Q.queryTipBlock rnConnectInfo >>= \case
    Just blockNo | blockNo >= 1 -> pure blockNo
    _ -> do
      threadDelay 1_000_000 >> waitForBlock n

waitForNextBlock :: RunningNode -> IO C.BlockNo
waitForNextBlock node = do
  blockNo <- waitForBlock node
  waitForNextBlock' node blockNo

waitForNextBlock' :: RunningNode -> C.BlockNo -> IO C.BlockNo
waitForNextBlock' node blockNo = do
  currentBlockNo <- waitForBlock node
  if currentBlockNo > blockNo then
    pure currentBlockNo else
    threadDelay 1_000_000 >> waitForNextBlock' node blockNo

waitForNextEpoch :: RunningNode -> IO C.EpochNo
waitForNextEpoch n@RunningNode{rnConnectInfo} = do
  currentEpochNo <- Q.queryEpoch rnConnectInfo
  waitForNextEpoch' n currentEpochNo

waitForNextEpoch' :: RunningNode -> C.EpochNo -> IO C.EpochNo
waitForNextEpoch' n@RunningNode{rnConnectInfo} epochNo = do
  currentEpochNo <- Q.queryEpoch rnConnectInfo
  if currentEpochNo > epochNo then
    pure currentEpochNo else
    threadDelay 1_000_000 >> waitForNextEpoch' n epochNo

-- | Start a single cardano-node devnet using the config from config/ and
-- credentials from config/credentials/. Only the 'Faucet' actor will receive
-- "initialFunds". Use 'seedFromFaucet' to distribute funds other wallets.
withCardanoNodeDevnet ::
  Tracer IO NodeLog ->
  -- | State directory in which credentials, db & logs are persisted.
  FilePath ->
  (RunningNode -> IO a) ->
  IO a
withCardanoNodeDevnet tracer stateDirectory action =
  withCardanoNodeDevnetConfig tracer stateDirectory mempty defaultPortsConfig action

-- | Start a single cardano-node devnet using the config from config/ and
-- credentials from config/credentials/. Only the 'Faucet' actor will receive
-- "initialFunds". Use 'seedFromFaucet' to distribute funds other wallets.
withCardanoNodeDevnetConfig ::
  Tracer IO NodeLog ->
  -- | State directory in which credentials, db & logs are persisted.
  FilePath ->
  -- | Changes to apply to the default genesis configurations
  GenesisConfigChanges ->
  -- | Ports config
  PortsConfig ->
  -- | Action
  (RunningNode -> IO a) ->
  IO a
withCardanoNodeDevnetConfig tracer stateDirectory configChanges PortsConfig{ours, peers} action = do
  createDirectoryIfMissing True stateDirectory
  [dlgCert, signKey, vrfKey, kesKey, opCert] <-
    mapM
      copyDevnetCredential
      [ "byron-delegation.cert"
      , "byron-delegate.key"
      , "vrf.skey"
      , "kes.skey"
      , "opcert.cert"
      ]
  let args =
        defaultCardanoNodeArgs
          { nodeDlgCertFile = Just dlgCert
          , nodeSignKeyFile = Just signKey
          , nodeVrfKeyFile = Just vrfKey
          , nodeKesKeyFile = Just kesKey
          , nodeOpCertFile = Just opCert
          , nodePort = Just ours
          }
  copyDevnetFiles args
  refreshSystemStart stateDirectory args
  writeTopology peers stateDirectory args

  withCardanoNode tracer networkId stateDirectory args $ \rn -> do
    traceWith tracer MsgNodeIsReady
    action rn
 where
  -- NOTE: This needs to match what's in config/genesis-shelley.json
  networkId = defaultNetworkId

  copyDevnetCredential file = do
    let destination = stateDirectory </> file
    x <- doesFileExist destination
    unless x $
      readConfigFile ("devnet" </> file)
        >>= BS.writeFile destination
    setFileMode destination ownerReadMode
    pure destination

  GenesisConfigChanges{cfAlonzo, cfConway, cfShelley, cfNodeConfig} = configChanges

  copyDevnetFiles args = do
    readConfigFile ("devnet" </> "cardano-node.json")
      >>= copyAndChangeJSONFile
        cfNodeConfig
        (stateDirectory </> nodeConfigFile args)
    readConfigFile ("devnet" </> "genesis-byron.json")
      >>= BS.writeFile
        (stateDirectory </> nodeByronGenesisFile args)
    readConfigFile ("devnet" </> "genesis-shelley.json")
      >>= copyAndChangeJSONFile
        cfShelley
        (stateDirectory </> nodeShelleyGenesisFile args)
    readConfigFile ("devnet" </> "genesis-alonzo.json")
      >>= copyAndChangeJSONFile
        cfAlonzo
        (stateDirectory </> nodeAlonzoGenesisFile args)
    readConfigFile ("devnet" </> "genesis-conway.json")
      >>= copyAndChangeJSONFile
        cfConway
        (stateDirectory </> nodeConwayGenesisFile args)

writeTopology :: [Port] -> FilePath -> CardanoNodeArgs -> IO ()
writeTopology peers stateDirectory args =
  Aeson.encodeFile (stateDirectory </> nodeTopologyFile args) $
    mkTopology peers

{-| Decode a json file, change the value, and write the result to another JSON file
-}
copyAndChangeJSONFile :: (FromJSON a, ToJSON a) => (a -> a) -> FilePath -> BS.ByteString -> IO ()
copyAndChangeJSONFile modification target =
  BS.writeFile
        target
        . BS.toStrict
        . Aeson.encode
        . either (error . (<>) "Failed to decode json: ") modification
        . Aeson.eitherDecode
        . BS.fromStrict

-- | Re-generate configuration and genesis files with fresh system start times.
refreshSystemStart ::
  -- | Working directory in which paths of 'CardanoNodeArgs' are resolved.
  FilePath ->
  CardanoNodeArgs ->
  IO ()
refreshSystemStart stateDirectory args = do
  systemStart <- initSystemStart
  let startTime = round @_ @Int $ utcTimeToPOSIXSeconds systemStart
  byronGenesis <-
    unsafeDecodeJsonFile (stateDirectory </> nodeByronGenesisFile args)
      <&> addField "startTime" startTime

  let systemStartUTC =
        posixSecondsToUTCTime . fromRational . toRational $ startTime
  shelleyGenesis <-
    unsafeDecodeJsonFile (stateDirectory </> nodeShelleyGenesisFile args)
      <&> addField "systemStart" systemStartUTC

  Aeson.encodeFile
    (stateDirectory </> nodeByronGenesisFile args)
    byronGenesis
  Aeson.encodeFile
    (stateDirectory </> nodeShelleyGenesisFile args)
    shelleyGenesis

  byronGenesisHash <- computeGenesisHash (stateDirectory </> nodeByronGenesisFile args)
  shelleyGenesisHash <- computeGenesisHash (stateDirectory </> nodeShelleyGenesisFile args)
  alonzoGenesisHash <- computeGenesisHash (stateDirectory </> nodeAlonzoGenesisFile args)
  conwayGenesisHash <- computeGenesisHash (stateDirectory </> nodeConwayGenesisFile args)

  config <-
    unsafeDecodeJsonFile (stateDirectory </> nodeConfigFile args)
      <&> addField "ByronGenesisFile" (nodeByronGenesisFile args)
      <&> addField "ByronGenesisHash" byronGenesisHash
      <&> addField "ShelleyGenesisFile" (nodeShelleyGenesisFile args)
      <&> addField "ShelleyGenesisHash" shelleyGenesisHash
      <&> addField "AlonzoGenesisFile" (nodeAlonzoGenesisFile args)
      <&> addField "AlonzoGenesisHash" alonzoGenesisHash
      <&> addField "ConwayGenesisFile" (nodeConwayGenesisFile args)
      <&> addField "ConwayGenesisHash" conwayGenesisHash

  Aeson.encodeFile (stateDirectory </> nodeConfigFile args) config

-- | Generate a topology file from a list of peers.
mkTopology :: [Port] -> Aeson.Value
mkTopology peers =
  Aeson.object ["Producers" .= map encodePeer peers]
 where
  encodePeer :: Int -> Aeson.Value
  encodePeer port =
    Aeson.object
      ["addr" .= ("127.0.0.1" :: Text), "port" .= port, "valency" .= (1 :: Int)]

-- | Initialize the system start time to now (modulo a small offset needed to
-- give time to the system to bootstrap correctly).
initSystemStart :: IO UTCTime
initSystemStart =
  addUTCTime 1 <$> getCurrentTime

unsafeDecodeJsonFile :: FromJSON a => FilePath -> IO a
unsafeDecodeJsonFile = Aeson.eitherDecodeFileStrict >=> either fail pure

addField :: ToJSON a => Aeson.Key -> a -> Aeson.Value -> Aeson.Value
addField k v = withObject (Aeson.KeyMap.insert k (toJSON v))

-- | Do something with an a JSON object. Fails if the given JSON value isn't an
-- object.
withObject :: (Aeson.Object -> Aeson.Object) -> Aeson.Value -> Aeson.Value
withObject fn = \case
  Aeson.Object m -> Aeson.Object (fn m)
  x              -> x

{-| Compute the hash of a genesis file using cardano-cli
-}
computeGenesisHash :: FilePath -> IO String
computeGenesisHash fp =
  -- drop the last character (newline)
  take 64 <$> readProcess "cardano-cli" ["genesis", "hash", "--genesis", fp] ""

-- | Launch a Cardano stake pool node with predefined node configuration, port and topology.
withCardanoStakePoolNodeDevnetConfig ::
  Tracer IO NodeLog ->
  -- | State directory in which credentials, db & logs are persisted.
  FilePath ->
  -- | Pool owner
  Wallet ->
  -- | Stake pool params
  StakePoolNodeParams ->
  -- | The absolute path of the cardano-node.json configuration file
  FilePath ->
  -- | Ports config
  PortsConfig ->
  -- | Running node
  RunningNode ->
  -- | Action
  (RunningStakePoolNode -> IO a) ->
  IO a
withCardanoStakePoolNodeDevnetConfig tracer stateDirectory wallet params nodeConfigFile PortsConfig{ours, peers} node@RunningNode{rnConnectInfo, rnNetworkId} action = do
  createDirectoryIfMissing True stateDirectory

  stakeKey <- C.generateSigningKey C.AsStakeKey
  vrfKey <- C.generateSigningKey C.AsVrfKey
  kesKey <- C.generateSigningKey C.AsKesKey
  stakePoolKey <- C.generateSigningKey C.AsStakePoolKey

  C.SlotNo slotNo <- fst <$> Q.queryTipSlotNo rnConnectInfo

  let
    minDeposit = 500_000_000

    vrfHash =
      C.verificationKeyHash . C.getVerificationKey $ vrfKey

    stakeHash =
      C.verificationKeyHash . C.getVerificationKey $ stakeKey

    stakeCred = StakeCredentialByKey stakeHash

    stakeCert =
      C.makeStakeAddressRegistrationCertificate
      . StakeAddrRegistrationConway C.ConwayEraOnwardsConway minDeposit
      $ stakeCred
    stakeAddress = C.makeStakeAddress rnNetworkId stakeCred

    paymentAddress =
      C.makeShelleyAddressInEra
        C.ShelleyBasedEraConway
        rnNetworkId
        (paymentCredential wallet)
        (StakeAddressByValue stakeCred)

    stakePoolVerKey = C.getVerificationKey stakePoolKey
    poolId = C.verificationKeyHash stakePoolVerKey

    delegationCert =
      C.makeStakeAddressDelegationCertificate
      $ StakeDelegationRequirementsConwayOnwards C.ConwayEraOnwardsConway stakeCred (L.DelegStake $ C.unStakePoolKeyHash poolId)

    stakePoolParams =
     StakePoolParameters
       poolId
       vrfHash
       (spnCost params)
       (spnMargin params)
       stakeAddress
       (spnPledge params)
       [stakeHash] -- owners
       [] -- relays
       Nothing

    poolCert =
      C.makeStakePoolRegistrationCertificate
      . StakePoolRegistrationRequirementsConwayOnwards C.ConwayEraOnwardsConway
      . toShelleyPoolParams
      $ stakePoolParams

  -- create the node certificate
  let
    opCertCounter =
      OperationalCertificateIssueCounter 0 stakePoolVerKey
    slotsPerKESPeriod = 129600 -- slotsPerKESPeriod from config/genesis-shelley.json
    kesPeriod = KESPeriod . fromIntegral . div slotNo $ slotsPerKESPeriod -- Word64 to Word
    opCert = C.issueOperationalCertificate
      (C.getVerificationKey kesKey)
      (Left stakePoolKey)
      kesPeriod
      opCertCounter

  (nextOpCert, nextOpCertCounter) <- case opCert of
    Left _    -> failure "withCardanoStakePoolNodeDevnetConfig: Issue operational certificate failure"
    Right res -> pure res

  let
    stakeCertTx = execBuildTx $ do
      addCertificate stakeCert

    poolCertTx = execBuildTx $ do
      let pledge = spnPledge params
      when (pledge > 0) $
        payToAddress paymentAddress (C.lovelaceToValue pledge)
      addCertificate poolCert

    delegCertTx = execBuildTx $ do
      addCertificate delegationCert

  _ <- W.balanceAndSubmit mempty node wallet stakeCertTx TrailingChange [C.WitnessStakeKey stakeKey]
  _ <- waitForNextBlock node

  _ <- W.balanceAndSubmit mempty node wallet poolCertTx TrailingChange [C.WitnessStakeKey stakeKey, C.WitnessStakePoolKey stakePoolKey]
  _ <- waitForNextBlock node

  _ <- W.balanceAndSubmit mempty node wallet delegCertTx TrailingChange [C.WitnessStakeKey stakeKey]
  _ <- waitForNextBlock node

  vrfKeyFile <- writeEnvelope vrfKey "vrf.skey"
  kesKeyFile <- writeEnvelope kesKey "kes.skey"
  opCertFile <- writeEnvelope nextOpCert "opcert.cert"

  let
    args =
        defaultCardanoNodeArgs
          { nodeVrfKeyFile = Just vrfKeyFile
          , nodeKesKeyFile = Just kesKeyFile
          , nodeOpCertFile = Just opCertFile
          , nodePort = Just ours
          , nodeConfigFile = nodeConfigFile
          }

  writeTopology peers stateDirectory args

  withCardanoNode tracer rnNetworkId stateDirectory args $ \rn -> do
    traceWith tracer MsgNodeIsReady
    action (RunningStakePoolNode rn stakeKey vrfKey kesKey stakePoolKey nextOpCertCounter)
 where

  writeEnvelope :: C.HasTextEnvelope a => a -> FilePath -> IO FilePath
  writeEnvelope envelope name = do
    let destination = stateDirectory </> name
    _ <- C.writeFileTextEnvelope (C.File destination) Nothing envelope
    setFileMode destination ownerReadMode
    return name
