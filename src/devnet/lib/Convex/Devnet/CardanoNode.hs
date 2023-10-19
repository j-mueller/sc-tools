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
  NodeId(..),
  NodeLog(..),
  RunningNode(..),
  DevnetConfig(..),
  CardanoNodeArgs(..),
  defaultCardanoNodeArgs,
  withCardanoNode,
  getCardanoNodeVersion,
  waitForFullySynchronized,
  waitForBlock,
  waitForSocket,
  withCardanoNodeDevnet,
  GenesisConfigChanges(..),
  allowLargeTransactions,
  withCardanoNodeDevnetConfig
) where

import           Cardano.Api                      (CardanoMode, Env,
                                                   LocalNodeConnectInfo,
                                                   NetworkId)
import qualified Cardano.Api                      as C
import           Cardano.Ledger.Alonzo.Genesis    (AlonzoGenesis)
import           Cardano.Ledger.Shelley.Genesis   (ShelleyGenesis (..))
import           Cardano.Ledger.Shelley.PParams   (PParams, _maxTxSize)
import           Cardano.Slotting.Slot            (withOriginToMaybe)
import           Cardano.Slotting.Time            (diffRelativeTime,
                                                   getRelativeTime,
                                                   toRelativeTime)
import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.Async         (race)
import           Control.Exception                (finally, throwIO)
import           Control.Monad                    (unless, when, (>=>))
import           Control.Monad.Except             (runExceptT)
import           Control.Tracer                   (Tracer, traceWith)
import qualified Convex.Devnet.NodeQueries        as Q
import           Convex.Devnet.Utils              (checkProcessHasNotDied,
                                                   defaultNetworkId, failure,
                                                   readConfigFile, withLogFile)
import           Data.Aeson                       (FromJSON, ToJSON (toJSON),
                                                   (.=))
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.KeyMap                as Aeson.KeyMap
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import           Data.Fixed                       (Centi)
import           Data.Functor                     ((<&>))
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time.Clock                  (UTCTime, addUTCTime,
                                                   getCurrentTime)
import           Data.Time.Clock.POSIX            (posixSecondsToUTCTime,
                                                   utcTimeToPOSIXSeconds)
import           GHC.Generics                     (Generic)
import           Ouroboros.Consensus.Shelley.Eras (BabbageEra, StandardCrypto)
import           System.Directory                 (createDirectoryIfMissing,
                                                   doesFileExist, removeFile)
import           System.FilePath                  ((</>))
import           System.IO                        (BufferMode (NoBuffering),
                                                   hSetBuffering)
import           System.Posix                     (ownerReadMode, setFileMode)
import           System.Process                   (CreateProcess (..),
                                                   StdStream (UseHandle), proc,
                                                   readProcess,
                                                   withCreateProcess)

import           Prelude

type Port = Int

newtype NodeId = NodeId Int
  deriving newtype (Eq, Show, Num, ToJSON, FromJSON)

data RunningNode = RunningNode
  { rnNodeSocket     :: FilePath -- ^ Cardano node socket
  , rnNetworkId      :: NetworkId -- ^ Network ID used by the cardano node
  , rnNodeConfigFile :: FilePath -- ^ Cardano node config file (JSON)
  , rnConnectInfo    :: (LocalNodeConnectInfo CardanoMode, Env) -- ^ Connection info for node queries
  }

-- | Configuration parameters for a single node devnet
data DevnetConfig = DevnetConfig
  { -- | Parent state directory
    dcStateDirectory :: FilePath
  , -- | Blockchain start time
    dcSystemStart    :: UTCTime
  , -- | A list of port
    dcPorts          :: PortsConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Arguments given to the 'cardano-node' command-line to run a node.
data CardanoNodeArgs = CardanoNodeArgs
  { nodeSocket             :: FilePath
  , nodeConfigFile         :: FilePath
  , nodeByronGenesisFile   :: FilePath
  , nodeShelleyGenesisFile :: FilePath
  , nodeAlonzoGenesisFile  :: FilePath
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
    , nodeConfigFile = "configuration.json"
    , nodeByronGenesisFile = "genesis-byron.json"
    , nodeShelleyGenesisFile = "genesis-shelley.json"
    , nodeAlonzoGenesisFile = "genesis-alonzo.json"
    , nodeTopologyFile = "topology.json"
    , nodeDatabaseDir = "db"
    , nodeDlgCertFile = Nothing
    , nodeSignKeyFile = Nothing
    , nodeOpCertFile = Nothing
    , nodeKesKeyFile = Nothing
    , nodeVrfKeyFile = Nothing
    , nodePort = Nothing
    }

-- | Configuration of ports from the perspective of a peer in the context of a
-- fully sockected topology.
data PortsConfig = PortsConfig
  { -- | Our node TCP port.
    ours  :: Port
  , -- | Other peers TCP ports.
    peers :: [Port]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
    rnConnectInfo <- runExceptT (Q.loadConnectInfo rnNodeConfigFile socketPath) >>= either (error . (<>) "Failed to load connect info: " . Text.unpack . C.renderInitialLedgerStateError) pure
    let rn = RunningNode{rnNodeSocket = socketPath, rnNetworkId = networkId, rnNodeConfigFile, rnConnectInfo}
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
waitForFullySynchronized tracer RunningNode{rnNodeSocket, rnNetworkId} = do
  systemStart <- Q.querySystemStart rnNetworkId rnNodeSocket
  check systemStart
 where
  check systemStart = do
    targetTime <- toRelativeTime systemStart <$> getCurrentTime
    eraHistory <- Q.queryEraHistory rnNetworkId rnNodeSocket
    (tipSlotNo, _slotLength) <- Q.queryTipSlotNo rnNetworkId rnNodeSocket
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
waitForBlock n@RunningNode{rnNodeSocket, rnNetworkId} = do
  withOriginToMaybe <$> Q.queryTipBlock rnNetworkId rnNodeSocket >>= \case
    Just blockNo | blockNo >= 1 -> pure blockNo
    _ -> do
      threadDelay 1_000_000 >> waitForBlock n

{-| Modifications to apply to the default genesis configurations
-}
data GenesisConfigChanges =
  GenesisConfigChanges
    { cfShelley :: ShelleyGenesis (BabbageEra StandardCrypto) -> ShelleyGenesis (BabbageEra StandardCrypto)
    , cfAlonzo  :: AlonzoGenesis -> AlonzoGenesis
    }

instance Semigroup GenesisConfigChanges where
  l <> r =
    GenesisConfigChanges
      { cfShelley = cfShelley r . cfShelley l
      , cfAlonzo  = cfAlonzo  r . cfAlonzo l
      }

instance Monoid GenesisConfigChanges where
  mempty = GenesisConfigChanges id id

-- {-| Change the alonzo genesis config to allow transactions with up to twice the normal size
-- -}
allowLargeTransactions :: GenesisConfigChanges
allowLargeTransactions =
  let change :: ShelleyGenesis (BabbageEra StandardCrypto) -> ShelleyGenesis (BabbageEra StandardCrypto)
      change g = g{sgProtocolParams = double (sgProtocolParams g)}
      double :: PParams (BabbageEra StandardCrypto) -> PParams (BabbageEra StandardCrypto)
      double pp = pp{_maxTxSize = 2 * _maxTxSize pp}
  in mempty{cfShelley = change}

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
  withCardanoNodeDevnetConfig tracer stateDirectory mempty action

-- | Start a single cardano-node devnet using the config from config/ and
-- credentials from config/credentials/. Only the 'Faucet' actor will receive
-- "initialFunds". Use 'seedFromFaucet' to distribute funds other wallets.
withCardanoNodeDevnetConfig ::
  Tracer IO NodeLog ->
  -- | State directory in which credentials, db & logs are persisted.
  FilePath ->
  -- | Changes to apply to the default genesis configurations
  GenesisConfigChanges ->
  (RunningNode -> IO a) ->
  IO a
withCardanoNodeDevnetConfig tracer stateDirectory configChanges action = do
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
          }
  copyDevnetFiles args
  refreshSystemStart stateDirectory args
  writeTopology [] args

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

  GenesisConfigChanges{cfAlonzo, cfShelley} = configChanges

  copyDevnetFiles args = do
    readConfigFile ("devnet" </> "cardano-node.json")
      >>= BS.writeFile
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

  writeTopology peers args =
    Aeson.encodeFile (stateDirectory </> nodeTopologyFile args) $
      mkTopology peers

{-| Decode a json file, change the value, and write the result to another JSON file
-}
copyAndChangeJSONFile :: (FromJSON a, ToJSON a) => (a -> a) -> FilePath -> BS.ByteString -> IO ()
copyAndChangeJSONFile modification target =
  BS.writeFile
        target
        . BSL.toStrict
        . Aeson.encode
        . either (error . (<>) "Failed to decode json: ") modification
        . Aeson.eitherDecode
        . BSL.fromStrict

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

  config <-
    unsafeDecodeJsonFile (stateDirectory </> nodeConfigFile args)
      <&> addField "ByronGenesisFile" (nodeByronGenesisFile args)
      <&> addField "ByronGenesisHash" byronGenesisHash
      <&> addField "ShelleyGenesisFile" (nodeShelleyGenesisFile args)
      <&> addField "ShelleyGenesisHash" shelleyGenesisHash
      <&> addField "AlonzoGenesisHash" alonzoGenesisHash

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
