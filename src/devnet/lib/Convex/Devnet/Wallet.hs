{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RankNTypes         #-}
{-| Glue code
-}
module Convex.Devnet.Wallet(
  faucet,
  walletUtxos,
  balanceAndSubmit,
  WalletLog(..),
  -- * Tracer / MonadLog interop
  TracerMonadLogT(..),
  runTracerMonadLogT,
  runningNodeBlockchain
) where

import           Cardano.Api               (BabbageEra, BuildTx, Tx,
                                            TxBodyContent)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Reader      (ReaderT (..), ask, lift)
import           Control.Tracer            (Tracer, traceWith)
import           Convex.Class              (MonadBlockchain,
                                            runMonadBlockchainCardanoNodeT,
                                            sendTx)
import qualified Convex.CoinSelection      as CoinSelection
import           Convex.Devnet.CardanoNode (RunningNode (..))
import qualified Convex.Devnet.NodeQueries as NodeQueries
import           Convex.Devnet.Utils       (keysFor)
import           Convex.MonadLog           (MonadLog (..))
import           Convex.Utxos              (UtxoSet)
import qualified Convex.Utxos              as Utxos
import           Convex.Wallet             (Wallet (..), address)
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Prettyprinter             (defaultLayoutOptions, layoutPretty)
import qualified Prettyprinter.Render.Text as Render

faucet :: IO Wallet
faucet = Wallet . snd <$> keysFor "faucet"

{-| Query the node for UTXOs that belong to the wallet
-}
walletUtxos :: RunningNode -> Wallet -> IO UtxoSet
walletUtxos RunningNode{rnNodeSocket, rnNetworkId} wllt =
  Utxos.fromApiUtxo <$> NodeQueries.queryUTxO rnNetworkId rnNodeSocket [address rnNetworkId wllt]

{-| Run a 'MonadBlockchain' action, using the @Tracer@ for log messages and the
@RunningNode@ for blockchain stuff
-}
runningNodeBlockchain :: Tracer IO WalletLog -> RunningNode -> (forall m. (MonadFail m, MonadLog m, MonadBlockchain m) => m a) -> IO a
runningNodeBlockchain tracer RunningNode{rnNodeSocket, rnNetworkId} =
  let info = NodeQueries.localNodeConnectInfo rnNetworkId rnNodeSocket
  in runTracerMonadLogT tracer . runMonadBlockchainCardanoNodeT info

{-| Balance and submit the transaction using the wallet's UTXOs
-}
balanceAndSubmit :: Tracer IO WalletLog -> RunningNode -> Wallet -> TxBodyContent BuildTx BabbageEra -> IO (Tx BabbageEra)
balanceAndSubmit tracer node wallet tx = do
  utxos <- walletUtxos node wallet
  runningNodeBlockchain tracer node $ do
    (tx', _) <- CoinSelection.balanceForWallet wallet utxos tx
    _ <- sendTx tx'
    pure tx'

data WalletLog =
  WalletLogInfo Text
  | WalletLogWarn Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TracerMonadLogT m a = TracerMonadLogT{unTracerMonadLogT :: ReaderT (Tracer m WalletLog) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO)

runTracerMonadLogT :: Tracer m WalletLog -> TracerMonadLogT m a -> m a
runTracerMonadLogT t (TracerMonadLogT r) = runReaderT r t

instance Monad m => MonadLog (TracerMonadLogT m) where
  logInfo' msg = TracerMonadLogT $ do
    tr <- ask
    let rendered = Render.renderStrict (layoutPretty defaultLayoutOptions msg)
    lift (traceWith tr (WalletLogInfo rendered))
  logWarn' msg = TracerMonadLogT $ do
    tr <- ask
    let rendered = Render.renderStrict (layoutPretty defaultLayoutOptions msg)
    lift (traceWith tr (WalletLogWarn rendered))
