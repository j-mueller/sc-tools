{-# LANGUAGE NumericUnderscores #-}
{-| Conveniences for balancing transactions and selecting coins
on the mockchain
-}
module Convex.MockChain.CoinSelection(
  balanceAndSubmit,
  paymentTo
) where

import           Cardano.Api.Shelley       (BabbageEra, BuildTx, TxBodyContent)
import qualified Cardano.Api.Shelley       as C
import           Control.Lens              ((&))
import           Convex.BuildTx            (payToAddress)
import           Convex.Class              (MonadBlockchain (..),
                                            MonadMockchain)
import qualified Convex.CoinSelection      as CoinSelection
import           Convex.Lenses             (emptyTx)
import qualified Convex.MockChain          as MockChain
import qualified Convex.MockChain.Defaults as Defaults
import           Convex.Wallet             (Wallet)
import qualified Convex.Wallet             as Wallet

{-| Balance and submit a transaction using the wallet's UTXOs
on the mockchain, using the default network ID
-}
balanceAndSubmit :: (MonadMockchain m, MonadFail m) => Wallet -> TxBodyContent BuildTx BabbageEra -> m (C.Tx CoinSelection.ERA)
balanceAndSubmit wallet tx = do
  u <- MockChain.walletUtxo wallet
  (tx', _) <- CoinSelection.balanceForWallet wallet u tx
  _ <- sendTx tx'
  pure tx'

{-| Pay ten Ada from one wallet to another
-}
paymentTo :: (MonadMockchain m, MonadFail m) => Wallet -> Wallet -> m (C.Tx CoinSelection.ERA)
paymentTo wFrom wTo = do
  let tx = emptyTx & payToAddress (Wallet.addressInEra Defaults.networkId wTo) (C.lovelaceToValue 10_000_000)
  balanceAndSubmit wFrom tx

