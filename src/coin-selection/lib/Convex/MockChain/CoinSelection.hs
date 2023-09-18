{-# LANGUAGE NumericUnderscores #-}
{-| Conveniences for balancing transactions and selecting coins
on the mockchain
-}
module Convex.MockChain.CoinSelection(
  balanceAndSubmit,
  balanceAndSubmitReturn,
  paymentTo
) where

import           Cardano.Api.Shelley       (BabbageEra, BuildTx, TxBodyContent)
import qualified Cardano.Api.Shelley       as C
import           Convex.BuildTx            (execBuildTx, payToAddress)
import           Convex.Class              (MonadBlockchain (..),
                                            MonadMockchain)
import qualified Convex.CoinSelection      as CoinSelection
import           Convex.Lenses             (emptyTx, emptyTxOut)
import qualified Convex.MockChain          as MockChain
import qualified Convex.MockChain.Defaults as Defaults
import           Convex.Wallet             (Wallet)
import qualified Convex.Wallet             as Wallet

{-| Balance and submit a transaction using the wallet's UTXOs
on the mockchain, using the default network ID
-}
balanceAndSubmit :: (MonadMockchain m, MonadFail m) => Wallet -> TxBodyContent BuildTx BabbageEra -> m (C.Tx CoinSelection.ERA)
balanceAndSubmit wallet tx = do
  n <- networkId
  let walletAddress = Wallet.addressInEra n wallet
      txOut = emptyTxOut walletAddress
  balanceAndSubmitReturn wallet txOut tx

{-| Balance and submit a transaction using the given return output and the wallet's UTXOs
on the mockchain, using the default network ID
-}
balanceAndSubmitReturn :: (MonadMockchain m, MonadFail m) => Wallet -> C.TxOut C.CtxTx C.BabbageEra -> TxBodyContent BuildTx BabbageEra -> m (C.Tx CoinSelection.ERA)
balanceAndSubmitReturn wallet returnOutput tx = do
  u <- MockChain.walletUtxo wallet
  (tx', _) <- CoinSelection.balanceForWalletReturn wallet u returnOutput tx
  _ <- sendTx tx'
  pure tx'

{-| Pay ten Ada from one wallet to another
-}
paymentTo :: (MonadMockchain m, MonadFail m) => Wallet -> Wallet -> m (C.Tx CoinSelection.ERA)
paymentTo wFrom wTo = do
  let tx = execBuildTx (payToAddress (Wallet.addressInEra Defaults.networkId wTo) (C.lovelaceToValue 10_000_000)) emptyTx
  balanceAndSubmit wFrom tx

