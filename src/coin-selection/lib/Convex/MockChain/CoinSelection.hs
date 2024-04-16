{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-| Conveniences for balancing transactions and selecting coins
on the mockchain
-}
module Convex.MockChain.CoinSelection(
  balanceAndSubmit,
  balanceAndSubmitReturn,
  paymentTo,
  payToOperator,
  payToOperator'
) where

import           Cardano.Api.Shelley       (BabbageEra, BuildTx, TxBodyContent,
                                            Value)
import qualified Cardano.Api.Shelley       as C
import           Control.Monad.Except      (MonadError)
import           Control.Tracer            (Tracer)
import           Convex.BuildTx            (buildTx, execBuildTx, execBuildTx',
                                            payToAddress, setMinAdaDepositAll)
import           Convex.Class              (MonadBlockchain (..),
                                            MonadMockchain)
import           Convex.CoinSelection      (BalanceTxError, TxBalancingMessage,
                                            TransactionSignatureCount)
import qualified Convex.CoinSelection      as CoinSelection
import           Convex.Lenses             (emptyTxOut)
import qualified Convex.MockChain          as MockChain
import qualified Convex.MockChain.Defaults as Defaults
import           Convex.Wallet             (Wallet)
import qualified Convex.Wallet             as Wallet
import           Convex.Wallet.Operator    (Operator (..), verificationKey)

{-| Balance and submit a transaction using the wallet's UTXOs
on the mockchain, using the default network ID
-}
balanceAndSubmit :: (MonadMockchain m, MonadError BalanceTxError m) => Tracer m TxBalancingMessage -> Wallet -> TxBodyContent BuildTx BabbageEra -> TransactionSignatureCount -> m (C.Tx CoinSelection.ERA)
balanceAndSubmit dbg wallet tx count = do
  n <- networkId
  let walletAddress = Wallet.addressInEra n wallet
      txOut = emptyTxOut walletAddress
  balanceAndSubmitReturn dbg wallet txOut tx count

{-| Balance and submit a transaction using the given return output and the wallet's UTXOs
on the mockchain, using the default network ID
-}
balanceAndSubmitReturn :: (MonadMockchain m, MonadError BalanceTxError m) => Tracer m TxBalancingMessage -> Wallet -> C.TxOut C.CtxTx C.BabbageEra -> TxBodyContent BuildTx BabbageEra -> TransactionSignatureCount -> m (C.Tx CoinSelection.ERA)
balanceAndSubmitReturn dbg wallet returnOutput tx count = do
  u <- MockChain.walletUtxo wallet
  (tx', _) <- CoinSelection.balanceForWalletReturn dbg wallet u returnOutput tx count
  _ <- sendTx tx'
  pure tx'

{-| Pay ten Ada from one wallet to another
-}
paymentTo :: (MonadMockchain m, MonadError BalanceTxError m) => Wallet -> Wallet -> m (C.Tx CoinSelection.ERA)
paymentTo wFrom wTo = do
  let tx = buildTx $ execBuildTx (payToAddress (Wallet.addressInEra Defaults.networkId wTo) (C.lovelaceToValue 10_000_000))
  balanceAndSubmit mempty wFrom tx 0

{-| Pay 100 Ada from one of the seed addresses to an @Operator@
-}
payToOperator :: (MonadMockchain m, MonadError BalanceTxError m) => Tracer m TxBalancingMessage -> Wallet -> Operator k -> m (C.Tx C.BabbageEra)
payToOperator dbg wFrom = payToOperator' dbg (C.lovelaceToValue 100_000_000) wFrom

{-| Pay some Ada from one of the seed addresses to an @Operator@
-}
payToOperator' :: (MonadMockchain m, MonadError BalanceTxError m) => Tracer m TxBalancingMessage -> Value -> Wallet -> Operator k -> m (C.Tx C.BabbageEra)
payToOperator' dbg value wFrom Operator{oPaymentKey} = do
  p <- queryProtocolParameters
  let addr =
        C.makeShelleyAddressInEra Defaults.networkId
        (C.PaymentCredentialByKey $ C.verificationKeyHash $ verificationKey oPaymentKey)
        C.NoStakeAddress
      tx = execBuildTx' $ payToAddress addr value >> setMinAdaDepositAll (snd p)
  balanceAndSubmit dbg wFrom tx 0
