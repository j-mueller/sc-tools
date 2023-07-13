{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
{-| A wallet client for executing buy and sell orders
-}
module Convex.TradingBot.NodeClient.OrderClient(
  buyOrderClient,
  sellOrderClient
) where

import           Cardano.Api                  (BlockInMode, CardanoMode, Env,
                                               LocalNodeConnectInfo)
import qualified Cardano.Api                  as C
import           Control.Applicative          (Alternative (..))
import           Control.Lens                 (_3, preview, set, (&))
import           Control.Monad                (void, when)
import           Control.Monad.Trans.Maybe    (runMaybeT)
import           Convex.Class                 (runMonadBlockchainCardanoNodeT,
                                               sendTx)
import           Convex.CoinSelection         (ERA, balanceForWallet)
import qualified Convex.Constants             as Constants
import           Convex.Lenses                (emptyTx)
import qualified Convex.Lenses                as L
import           Convex.MonadLog              (MonadLogKatipT (..), logInfoS)
import qualified Convex.Muesli.LP.BuildTx     as BuildTx
import qualified Convex.Muesli.LP.Types       as T
import           Convex.NodeClient.Fold       (CatchingUp (..), catchingUp,
                                               foldClient)
import           Convex.NodeClient.Resuming   (resumingClient)
import           Convex.NodeClient.Types      (PipelinedLedgerStateClient)
import           Convex.TradingBot.Cli.Config (Order (..))
import           Convex.Utxos                 (UtxoSet, apply, toUtxoTx)
import qualified Convex.Utxos                 as Utxos
import           Convex.Wallet                (Wallet)
import qualified Convex.Wallet                as Wallet
import           Convex.Wallet.Cli.Config     (ConfigMode (..))
import           Data.Proxy                   (Proxy (..))
import qualified Katip                        as K

buyOrderClient :: LocalNodeConnectInfo CardanoMode -> K.LogEnv -> K.Namespace -> Wallet -> Order 'Typed -> Env -> PipelinedLedgerStateClient
buyOrderClient info logEnv ns wallet order env = do
  let network = C.localNodeNetworkId info
      addr = Wallet.addressInEra network wallet & set (L._AddressInEra . L._Address . _3) namiStakeRef
      tx = BuildTx.buyOrder addr Nothing (convBuyOrder order) emptyTx
  resumingClient [Constants.recent] $ \_ ->
    foldClient
      mempty
      env
      (applyBlock info logEnv ns wallet tx)

sellOrderClient :: LocalNodeConnectInfo CardanoMode -> K.LogEnv -> K.Namespace -> Wallet -> Order 'Typed -> Env -> PipelinedLedgerStateClient
sellOrderClient info logEnv ns wallet order env = do
  let network = C.localNodeNetworkId info
      addr = Wallet.addressInEra network wallet & set (L._AddressInEra . L._Address . _3) namiStakeRef
      tx   = BuildTx.sellOrder addr Nothing (convSellOrder order) emptyTx
  resumingClient [Constants.recent] $ \_ ->
    foldClient
      mempty
      env
      (applyBlock info logEnv ns wallet tx)

applyBlock :: LocalNodeConnectInfo CardanoMode -> K.LogEnv -> K.Namespace -> Wallet -> C.TxBodyContent C.BuildTx ERA -> CatchingUp -> UtxoSet C.CtxTx () -> BlockInMode CardanoMode -> IO (Maybe (UtxoSet C.CtxTx ()))
applyBlock info logEnv ns wallet tx (catchingUp -> isCatchingUp) state block = K.runKatipContextT logEnv () ns $ runMonadLogKatipT $ runMaybeT $ do
  let change = Utxos.extract_ (Wallet.shelleyPaymentCredential wallet) state block
      newState = apply state change

  when (not isCatchingUp) $ do
    let action =
          runMonadBlockchainCardanoNodeT info $ do
           (tx_, change_) <- balanceForWallet wallet (toUtxoTx state) tx
           logInfoS (show tx_)
           logInfoS (show change_)
           sendTx tx_
    void $ action >>= either fail pure
    empty
  pure newState

convBuyOrder :: Order 'Typed -> T.BuyOrder
convBuyOrder Order{policyId, assetName, quantity, lovelace} =
  T.BuyOrder
    { T.buyCurrency = (policyId, assetName)
    , T.buyQuantity = quantity
    , T.buyPrice = T.unitPrice quantity lovelace
    }

convSellOrder :: Order 'Typed -> T.SellOrder
convSellOrder Order{policyId, assetName, quantity, lovelace} =
  T.SellOrder
    { T.sellCurrency = (policyId, assetName)
    , T.sellQuantity = quantity
    , T.sellPrice = T.unitPrice quantity lovelace
    }

namiAddress :: C.AddressInEra C.BabbageEra
namiAddress = maybe (error "") id $ C.deserialiseAddress (C.proxyToAsType Proxy) "addr1qx4jckkq2gqey7pnzayptkfuxh93lrp79kqwl5zvuejgquurc9hqawk27ans9d45ss8pnukglu6mxpmnslvtznev6j0qd0dc2n"

namiStakeRef = maybe (error "") id $ preview (L._AddressInEra . L._Address . _3) namiAddress
