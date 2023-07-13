{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Glue code
-}
module Convex.Devnet.Wallet(
  faucet,
  sendFaucetFundsTo,
  createSeededWallet,
  walletUtxos,
  balanceAndSubmit,
  WalletLog(..),
  -- * Tracer / MonadLog interop
  TracerMonadLogT(..),
  runTracerMonadLogT,
  runningNodeBlockchain
) where

import           Cardano.Api               (AddressInEra, BabbageEra, BuildTx,
                                            Lovelace, Tx, TxBodyContent)
import qualified Cardano.Api               as C
import           Control.Monad.Except      (MonadError,
                                            runExceptT)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Reader      (ReaderT (..), ask, lift)
import           Control.Tracer            (Tracer, traceWith)
import qualified Convex.BuildTx            as BuildTx
import           Convex.Class              (MonadBlockchain,
                                            runMonadBlockchainCardanoNodeT,
                                            sendTx)
import qualified Convex.CoinSelection      as CoinSelection
import           Convex.Devnet.CardanoNode (RunningNode (..))
import qualified Convex.Devnet.NodeQueries as NodeQueries
import           Convex.Devnet.Utils       (keysFor)
import           Convex.Lenses             (emptyTx)
import           Convex.MonadLog           (MonadLog (..))
import           Convex.Utxos              (UtxoSet)
import qualified Convex.Utxos              as Utxos
import           Convex.Wallet             (Wallet (..), address)
import qualified Convex.Wallet             as Wallet
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Function             ((&))
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Prettyprinter             (defaultLayoutOptions, layoutPretty)
import qualified Prettyprinter.Render.Text as Render

faucet :: IO Wallet
faucet = Wallet . snd <$> keysFor "faucet"

{-| Query the node for UTXOs that belong to the wallet
-}
walletUtxos :: RunningNode -> Wallet -> IO (UtxoSet C.CtxUTxO ())
walletUtxos RunningNode{rnNodeSocket, rnNetworkId} wllt =
  Utxos.fromApiUtxo <$> NodeQueries.queryUTxO rnNetworkId rnNodeSocket [address rnNetworkId wllt]

{-| Send the desired amount of lovelace to the address
-}
sendFaucetFundsTo :: Tracer IO WalletLog -> RunningNode -> AddressInEra BabbageEra -> Lovelace -> IO (Tx BabbageEra)
sendFaucetFundsTo tracer node destination amount = do
  fct <- faucet
  balanceAndSubmit tracer node fct (emptyTx & BuildTx.payToAddress destination (C.lovelaceToValue amount))

{-| Create a new wallet and send some funds to it. Returns when the seed txn has been registered
on the chain.
-}
createSeededWallet :: Tracer IO WalletLog -> RunningNode -> Lovelace -> IO Wallet
createSeededWallet tracer node@RunningNode{rnNetworkId, rnNodeSocket} amount = do
  wallet <- Wallet.generateWallet
  traceWith tracer (GeneratedWallet wallet)
  sendFaucetFundsTo tracer node (Wallet.addressInEra rnNetworkId wallet) amount >>= NodeQueries.waitForTxn rnNetworkId rnNodeSocket
  pure wallet

{-| Run a 'MonadBlockchain' action, using the @Tracer@ for log messages and the
@RunningNode@ for blockchain stuff
-}
runningNodeBlockchain ::
  Tracer IO WalletLog
  -> RunningNode
  -> (forall m. (MonadFail m, MonadLog m, MonadError String m, MonadBlockchain m) => m a)
  -> IO a
runningNodeBlockchain tracer RunningNode{rnNodeSocket, rnNetworkId} h =
  let info = NodeQueries.localNodeConnectInfo rnNetworkId rnNodeSocket
  in runTracerMonadLogT tracer $ do
    (runExceptT $ runMonadBlockchainCardanoNodeT info h) >>= either fail pure

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
  | GeneratedWallet Wallet
  deriving stock (Show, Generic)
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
  logDebug' msg = TracerMonadLogT $ do
    tr <- ask
    let rendered = Render.renderStrict (layoutPretty defaultLayoutOptions msg)
    lift (traceWith tr (WalletLogWarn rendered))
