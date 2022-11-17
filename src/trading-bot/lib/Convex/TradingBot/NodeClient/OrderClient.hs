{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-| A wallet client for executing buy and sell orders
-}
module Convex.TradingBot.NodeClient.OrderClient(
  orderClient
) where

import           Cardano.Api                  (BlockInMode, CardanoMode, Env,
                                               LocalNodeConnectInfo)
import qualified Cardano.Api                  as C
import           Control.Applicative          (Alternative (..))
import           Control.Lens                 (_3, preview, set, (&))
import           Control.Monad                (when)
import           Control.Monad.Trans.Maybe    (runMaybeT)
import           Convex.Class                 (runMonadBlockchainCardanoNodeT,
                                               sendTx)
import           Convex.CoinSelection         (balanceForWallet)
import qualified Convex.Constants             as Constants
import           Convex.Lenses                (emptyTx)
import qualified Convex.Lenses                as L
import           Convex.MonadLog              (MonadLogKatipT (..), logInfoS)
import           Convex.Muesli.LP.BuildTx     (LimitBuyOrder (..))
import qualified Convex.Muesli.LP.BuildTx     as BuildTx
import           Convex.NodeClient.Fold       (CatchingUp (..), catchingUp,
                                               foldClient)
import           Convex.NodeClient.Resuming   (resumingClient)
import           Convex.NodeClient.Types      (PipelinedLedgerStateClient)
import           Convex.TradingBot.Cli.Config (BuyOrder (..))
import           Convex.Wallet                (Wallet)
import qualified Convex.Wallet                as Wallet
import           Convex.Wallet.Cli.Config     (ConfigMode (..))
import           Convex.Wallet.Utxos          (UtxoState, apply)
import qualified Convex.Wallet.Utxos          as Utxos
import           Data.Proxy                   (Proxy (..))
import qualified Katip                        as K

orderClient :: LocalNodeConnectInfo CardanoMode -> K.LogEnv -> K.Namespace -> Wallet -> BuyOrder 'Typed -> Env -> PipelinedLedgerStateClient
orderClient info logEnv ns wallet order env = do
  resumingClient [Constants.recent] $ \_ ->
    foldClient
      mempty
      env
      (applyBlock info logEnv ns wallet order)

applyBlock :: LocalNodeConnectInfo CardanoMode -> K.LogEnv -> K.Namespace -> Wallet -> BuyOrder 'Typed -> CatchingUp -> UtxoState -> BlockInMode CardanoMode -> IO (Maybe UtxoState)
applyBlock info@C.LocalNodeConnectInfo{C.localNodeNetworkId} logEnv ns wallet order (catchingUp -> isCatchingUp) state block = K.runKatipContextT logEnv () ns $ runMonadLogKatipT $ runMaybeT $ do
  let change = Utxos.extract (Wallet.shelleyPaymentCredential wallet) state block
      newState = apply state change

  when (not isCatchingUp) $ do
    runMonadBlockchainCardanoNodeT info $ do
      logInfoS $ "Placing order: " <> show order
      let addr = Wallet.addressInEra localNodeNetworkId wallet & set (L._AddressInEra . L._Address . _3) namiStakeRef
      let tx = BuildTx.buyOrder localNodeNetworkId (convBuyOrder addr order) emptyTx
      (tx_, change_) <- balanceForWallet wallet state tx
      logInfoS (show tx_)
      logInfoS (show change_)
      sendTx tx_
    empty

  pure newState

convBuyOrder :: C.AddressInEra C.BabbageEra -> BuyOrder 'Typed -> LimitBuyOrder
convBuyOrder returnAddress BuyOrder{policyId, assetName, quantity, lovelace} =
  LimitBuyOrder
    returnAddress
    policyId
    assetName
    quantity
    lovelace

namiAddress :: C.AddressInEra C.BabbageEra
namiAddress = maybe (error "") id $ C.deserialiseAddress (C.proxyToAsType Proxy) "addr1qx4jckkq2gqey7pnzayptkfuxh93lrp79kqwl5zvuejgquurc9hqawk27ans9d45ss8pnukglu6mxpmnslvtznev6j0qd0dc2n"

namiStakeRef = maybe (error "") id $ preview (L._AddressInEra . L._Address . _3) namiAddress
