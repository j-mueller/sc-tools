{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Glue code
-}
module Convex.Devnet.Wallet(
  faucet,
  sendFaucetFundsTo,
  createSeededWallet,
  walletUtxos,
  balanceAndSubmit,
  balanceAndSubmitReturn,
  WalletLog(..),
  -- * Tracer / MonadLog interop
  TracerMonadLogT(..),
  runTracerMonadLogT,
  runningNodeBlockchain
) where

import           Cardano.Api                     (AddressInEra, ConwayEra,
                                                  Quantity, Tx)
import qualified Cardano.Api                     as C
import           Control.Monad                   (replicateM)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Reader            (ReaderT (..), ask, lift)
import           Control.Tracer                  (Tracer, traceWith)
import           Convex.BuildTx                  (TxBuilder)
import qualified Convex.BuildTx                  as BuildTx
import           Convex.CardanoApi.Lenses        (emptyTxOut)
import           Convex.Class                    (MonadBlockchain (queryNetworkId),
                                                  runMonadBlockchainCardanoNodeT,
                                                  sendTx)
import           Convex.CoinSelection            (ChangeOutputPosition (TrailingChange))
import qualified Convex.CoinSelection            as CoinSelection
import           Convex.Devnet.CardanoNode.Types (RunningNode (..))
import           Convex.Devnet.Utils             (keysFor)
import           Convex.MonadLog                 (MonadLog (..))
import qualified Convex.NodeQueries.Debug        as NodeQueries
import           Convex.Utils                    (failOnError)
import           Convex.Utxos                    (UtxoSet)
import           Convex.Wallet                   (Wallet (..), address)
import qualified Convex.Wallet                   as Wallet
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)
import           Prettyprinter                   (defaultLayoutOptions,
                                                  layoutPretty)
import qualified Prettyprinter.Render.Text       as Render

faucet :: IO Wallet
faucet = Wallet . snd <$> keysFor "faucet"

{-| Query the node for UTXOs that belong to the wallet
-}
walletUtxos :: RunningNode -> Wallet -> IO (UtxoSet C.CtxUTxO ())
walletUtxos RunningNode{rnConnectInfo, rnNetworkId} wllt =
  NodeQueries.queryUTxOByAddress rnConnectInfo [C.toAddressAny $ address rnNetworkId wllt]

{-| Send @n@ times the given amount of lovelace to the address
-}
sendFaucetFundsTo :: Tracer IO WalletLog -> RunningNode -> AddressInEra ConwayEra -> Int -> Quantity -> IO (Tx ConwayEra)
sendFaucetFundsTo tracer node destination n amount = do
  fct <- faucet
  balanceAndSubmit tracer node fct (BuildTx.execBuildTx $ replicateM n (BuildTx.payToAddress destination (C.lovelaceToValue $ C.quantityToLovelace amount))) TrailingChange []

{-| Create a new wallet and send @n@ times the given amount of lovelace to it. Returns when the seed txn has been registered
on the chain.
-}
createSeededWallet :: Tracer IO WalletLog -> RunningNode -> Int -> Quantity -> IO Wallet
createSeededWallet tracer node@RunningNode{rnNetworkId, rnConnectInfo} n amount = do
  wallet <- Wallet.generateWallet
  traceWith tracer (GeneratedWallet wallet)
  sendFaucetFundsTo tracer node (Wallet.addressInEra rnNetworkId wallet) n amount >>= NodeQueries.waitForTx rnConnectInfo
  pure wallet

{-| Run a 'MonadBlockchain' action, using the @Tracer@ for log messages and the
@RunningNode@ for blockchain stuff
-}
runningNodeBlockchain ::
  forall era a. (C.IsShelleyBasedEra era)
  => Tracer IO WalletLog
  -> RunningNode
  -> (forall m. (MonadFail m, MonadLog m, MonadBlockchain era m) => m a)
  -> IO a
runningNodeBlockchain tracer RunningNode{rnConnectInfo} =
  runTracerMonadLogT tracer . runMonadBlockchainCardanoNodeT @era rnConnectInfo

{-| Balance and submit the transaction using the wallet's UTXOs
-}
balanceAndSubmit :: forall era. (C.IsBabbageBasedEra era) => Tracer IO WalletLog -> RunningNode -> Wallet -> TxBuilder era -> ChangeOutputPosition -> [C.ShelleyWitnessSigningKey] -> IO (Tx era)
balanceAndSubmit tracer node wallet tx changePosition keys = do
  n <- runningNodeBlockchain @era tracer node queryNetworkId
  let walletAddress = Wallet.addressInEra n wallet
      txOut = emptyTxOut walletAddress
  balanceAndSubmitReturn tracer node wallet txOut tx changePosition keys

{-| Balance and submit the transaction using the wallet's UTXOs
-}
balanceAndSubmitReturn
  :: forall era. (C.IsBabbageBasedEra era)
  => Tracer IO WalletLog
  -> RunningNode
  -> Wallet
  -> C.TxOut C.CtxTx era
  -> TxBuilder era
  -> ChangeOutputPosition
  -> [C.ShelleyWitnessSigningKey]
  -> IO (Tx era)
balanceAndSubmitReturn tracer node wallet returnOutput tx changePosition keys = do
  utxos <- walletUtxos node wallet
  runningNodeBlockchain tracer node $ do
    (C.Tx body wit, _) <- failOnError (CoinSelection.balanceForWalletReturn @era mempty wallet utxos returnOutput tx changePosition)
    let wit' = (C.makeShelleyKeyWitness C.shelleyBasedEra body <$> keys) ++ wit
        tx'  = C.makeSignedTransaction wit' body

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
