{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-| Start a wallet / operator server
-}
module Convex.Devnet.WalletServer(
  RunningWalletServer(..),
  WalletLog(..),
  withWallet,
  getUTxOs,
  waitUntilAvailable,
  sendFundsToOperator
) where

import           Cardano.Api                     (BabbageEra, CtxTx, Tx, Quantity)
import qualified Cardano.Api                     as C
import           Control.Concurrent              (threadDelay)
import           Control.Tracer                  (Tracer, contramap, traceWith)
import           Convex.Devnet.CardanoNode.Types (RunningNode (..))
import qualified Convex.Devnet.NodeQueries       as NodeQueries
import           Convex.Devnet.Utils             (failure, withLogFile)
import qualified Convex.Devnet.Wallet            as Wallet
import           Convex.Utxos                    (UtxoSet)
import qualified Convex.Wallet.API               as API
import           Convex.Wallet.Cli.Command       (CliCommand (..))
import           Convex.Wallet.Cli.Config        (Config (..))
import           Convex.Wallet.Operator          (Operator,
                                                  OperatorConfigSigning (..),
                                                  OperatorConfigVerification (..),
                                                  Signing, loadOperatorFiles,
                                                  operatorAddress)
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)
import           GHC.IO.Handle.Types             (Handle)
import           Network.HTTP.Client             (Manager, defaultManagerSettings,
                                                  newManager)
import           Servant.Client                  (ClientEnv, ClientError (..),
                                                  mkClientEnv)
import           Servant.Client.Core.BaseUrl     (BaseUrl (..), Scheme (..))
import           System.FilePath                 ((</>))
import           System.IO                       (BufferMode (NoBuffering),
                                                  hSetBuffering)
import           System.Process                  (CreateProcess (..), ProcessHandle,
                                                  StdStream (UseHandle), proc,
                                                  readCreateProcess,
                                                  withCreateProcess)

data WalletLog =
  WMsgText{ wmsgText :: Text}
  | WMsgWaiting String
  | WCreatingKey{ keyFile :: FilePath }
  | WWallet Wallet.WalletLog
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RunningWalletServer =
  RunningWalletServer
    { rwsOperator             :: Operator Signing
    , rwsOpConfigVerification :: OperatorConfigVerification
    , rwsOpConfigSigning      :: OperatorConfigSigning
    , rwsClient               :: ClientEnv
    , rwsManager              :: Manager
    , rwsHandle               :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
    }

withWallet :: Tracer IO WalletLog -> FilePath -> RunningNode -> (RunningWalletServer -> IO a) -> IO a
withWallet tracer stateDirectory rn@RunningNode{rnNodeSocket, rnNodeConfigFile, rnNetworkId} action = do
  let logFilePath = stateDirectory </> "wallet-server.log"
      signingKeyFile = stateDirectory </> "operator-signing-key.vkey"
      verificationKeyFile = stateDirectory </> "operator-verification-key.vkey"
      walletFile = stateDirectory </> "wallet-state.json"
      mkProc = walletCliProcess (Just stateDirectory)
      signingConf = OperatorConfigSigning{ocSigningKeyFile = signingKeyFile, ocStakeVerificationKeyFile = Nothing}
      verificationConf = OperatorConfigVerification{ocvPaymentKeyFile = verificationKeyFile, ocvStakeVerificationKeyFile = Nothing}
      walletCfg = Config{walletFile, cardanoNodeConfigFile=rnNodeConfigFile, cardanoNodeSocket=rnNodeSocket}

      walletPort = 9988

  -- TODO
  -- 1. Generate key
  -- 2. start wallet server
  rwsManager <- newManager defaultManagerSettings
  let rwsClient = mkClientEnv rwsManager (BaseUrl Http "localhost" walletPort "")
  traceWith tracer WCreatingKey{keyFile = signingKeyFile}
  _ <- readCreateProcess (mkProc GenerateSigningKey{verificationKeyFile, signingKeyFile}) ""
  op <- loadOperatorFiles signingConf
  withLogFile logFilePath $ \out -> do
    hSetBuffering out NoBuffering
    let p = mkProc $ RunWallet walletCfg verificationConf walletPort
    withCreateProcess p{std_out = UseHandle out, std_err = UseHandle out} $
      \stdin stdout stderr processHandle -> do
        let rws = RunningWalletServer
                { rwsHandle = (stdin, stdout, stderr, processHandle)
                , rwsOpConfigVerification = verificationConf
                , rwsOpConfigSigning = signingConf
                , rwsOperator = op
                , rwsManager
                , rwsClient
                }
        _ <- sendFundsToOperator tracer rn op (C.Quantity 100_000_000) >>= NodeQueries.waitForTxn rnNetworkId rnNodeSocket
        waitUntilAvailable tracer rws
        action rws

walletCliProcess :: Maybe FilePath -> CliCommand -> CreateProcess
walletCliProcess cwd com = (proc walletExecutable strArgs){cwd} where
  comString = \case
    GenerateWallet       -> "generate-wallet"
    GenerateSigningKey{} -> "generate-signing-key"
    RunWallet{}          -> "run-wallet"
    ShowAddress{}        -> "show-address"

  keyFiles = \case
    GenerateSigningKey{verificationKeyFile, signingKeyFile} ->
      [ "--verification.file", verificationKeyFile
      , "--signing.file", signingKeyFile
      ]
    _ -> []

  config = \case
    RunWallet cfg _ _ -> configArgs cfg
    ShowAddress cfg _ -> configArgs cfg
    _                 -> []

  opConfig = \case
    RunWallet _ opCfg _ -> opConfigArgs opCfg
    ShowAddress _ opCfg -> opConfigArgs opCfg
    _                   -> []

  portConfig = \case
    RunWallet _ _ port -> ["--http.port", show port]
    _                  -> []

  strArgs =
    mconcat
      [ [comString com]
      , keyFiles com
      , config com
      , opConfig com
      , portConfig com
      ]

walletExecutable :: String
walletExecutable = "convex-wallet"

configArgs :: Config -> [String]
configArgs Config{cardanoNodeConfigFile, cardanoNodeSocket, walletFile} =
  [ "--node-config", cardanoNodeConfigFile
  , "--node-socket", cardanoNodeSocket
  , "--wallet-file", walletFile
  ]

opConfigArgs :: OperatorConfigVerification -> [String]
opConfigArgs OperatorConfigVerification{ocvPaymentKeyFile, ocvStakeVerificationKeyFile} =
  [ "--verification-key-file", ocvPaymentKeyFile
  ] ++
    maybe [] (\f -> ["--stake-verification-key-file", f]) ocvStakeVerificationKeyFile

getUTxOs :: RunningWalletServer -> IO (UtxoSet CtxTx ())
getUTxOs RunningWalletServer{rwsClient} = API.getUTxOs rwsClient >>= \case
  Left err -> error ("getUTxOs failed: " <> show err)
  Right x  -> pure x

{-| Wait until the service is online (healthcheck route respons with 200)
-}
waitUntilAvailable :: Tracer IO WalletLog -> RunningWalletServer -> IO ()
waitUntilAvailable tr RunningWalletServer{rwsClient} =
  let go n | n > 0 = API.getHealth rwsClient >>= \case
        Left (ConnectionError _err) -> do
          traceWith tr (WMsgWaiting "waitUntilAvailable")
          threadDelay 2_000_000
          go (n - 1)
        Left err -> failure ("waitUntilAvailable: Failed with unexpected error: " <> show err)
        Right{}  -> pure ()
          | otherwise = failure "waitUntilAvailable: Number of retries exceeded"
  in go (20 :: Integer)

{-| Send faucet funds to the operator
-}
sendFundsToOperator :: Tracer IO WalletLog -> RunningNode -> Operator k -> Quantity -> IO (Tx BabbageEra)
sendFundsToOperator tr node@RunningNode{rnNetworkId} op lvl = do
  let opAddress = operatorAddress rnNetworkId op
  Wallet.sendFaucetFundsTo (contramap WWallet tr) node (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) opAddress) 10 lvl
