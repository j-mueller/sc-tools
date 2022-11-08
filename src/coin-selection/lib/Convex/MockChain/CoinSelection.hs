{-# LANGUAGE NumericUnderscores #-}
{-| Conveniences for balancing transactions and selecting coins
on the mockchain
-}
module Convex.MockChain.CoinSelection(
  balanceAndSubmit,
  paymentTo
) where

import           Cardano.Api.Shelley       (BabbageEra, BuildTx, TxBodyContent,
                                            TxId)
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
balanceAndSubmit :: (MonadMockchain m, MonadBlockchain m, MonadFail m) => Wallet -> TxBodyContent BuildTx BabbageEra -> m TxId
balanceAndSubmit wallet tx = do
  u <- MockChain.walletUtxo Defaults.networkId wallet
  CoinSelection.balanceForWallet Defaults.nodeParams wallet u tx >>= sendTx . fst

{-| Pay ten Ada from one wallet to another
-}
paymentTo :: (MonadBlockchain m, MonadMockchain m, MonadFail m) => Wallet -> Wallet -> m TxId
paymentTo wFrom wTo = do
  let tx = emptyTx & payToAddress (Wallet.addressInEra Defaults.networkId wTo) (C.lovelaceToValue 10_000_000)
  balanceAndSubmit wFrom tx

