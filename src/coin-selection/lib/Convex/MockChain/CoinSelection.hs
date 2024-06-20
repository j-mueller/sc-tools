{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-| Conveniences for balancing transactions and selecting coins
on the mockchain
-}
module Convex.MockChain.CoinSelection(
  balanceAndSubmit,
  tryBalanceAndSubmit,
  SendTxFailed(..),
  balanceAndSubmitReturn,
  paymentTo,
  payToOperator,
  payToOperator'
) where

import           Cardano.Api.Shelley       (Value)
import qualified Cardano.Api.Shelley       as C
import           Control.Monad.Except      (MonadError)
import           Control.Tracer            (Tracer)
import           Convex.BuildTx            (TxBuilder, execBuildTx,
                                            payToAddress, setMinAdaDepositAll)
import           Convex.Class              (MonadBlockchain (..),
                                            MonadMockchain, SendTxFailed (..))
import           Convex.CoinSelection      (BalanceTxError, TxBalancingMessage)
import qualified Convex.CoinSelection      as CoinSelection
import           Convex.CardanoApi.Lenses (emptyTxOut)
import qualified Convex.MockChain          as MockChain
import qualified Convex.MockChain.Defaults as Defaults
import           Convex.Wallet             (Wallet)
import qualified Convex.Wallet             as Wallet
import           Convex.Wallet.Operator    (Operator (..), verificationKey)
import           Data.Functor              (($>))

{-| Balance and submit a transaction using the wallet's UTXOs
on the mockchain, using the default network ID
-}
balanceAndSubmit
  :: (MonadMockchain m, MonadError BalanceTxError m)
  => Tracer m TxBalancingMessage
  -> Wallet
  -> TxBuilder
  -> m (Either SendTxFailed (C.Tx CoinSelection.ERA))
balanceAndSubmit dbg wallet tx = do
  n <- networkId
  let walletAddress = Wallet.addressInEra n wallet
      txOut = C.InAnyCardanoEra C.BabbageEra $ emptyTxOut walletAddress
  balanceAndSubmitReturn dbg wallet txOut tx

{-| Balance and submit a transaction using the wallet's UTXOs
on the mockchain, using the default network ID. Fail if the
transaction is not accepted by the node.
-}
tryBalanceAndSubmit
  :: (MonadMockchain m, MonadError BalanceTxError m, MonadFail m)
  => Tracer m TxBalancingMessage
  -> Wallet
  -> TxBuilder
  -> m (C.Tx CoinSelection.ERA)
tryBalanceAndSubmit dbg wallet tx = do
  n <- networkId
  let walletAddress = Wallet.addressInEra n wallet
      txOut = C.InAnyCardanoEra C.BabbageEra $ emptyTxOut walletAddress
  balanceAndSubmitReturn dbg wallet txOut tx >>= either (fail . show) pure

{-| Balance and submit a transaction using the given return output and the wallet's UTXOs
on the mockchain, using the default network ID
-}
balanceAndSubmitReturn
  :: (MonadMockchain m, MonadError BalanceTxError m)
  => Tracer m TxBalancingMessage
  -> Wallet
  -> C.InAnyCardanoEra (C.TxOut C.CtxUTxO)
  -> TxBuilder
  -> m (Either SendTxFailed (C.Tx CoinSelection.ERA))
balanceAndSubmitReturn dbg wallet returnOutput tx = do
  u <- MockChain.walletUtxo wallet
  (tx', _) <- CoinSelection.balanceForWalletReturn dbg wallet u returnOutput tx
  k <- sendTx tx'
  pure (k $> tx')

{-| Pay ten Ada from one wallet to another
-}
paymentTo :: (MonadMockchain m, MonadError BalanceTxError m) => Wallet -> Wallet -> m (Either SendTxFailed (C.Tx CoinSelection.ERA))
paymentTo wFrom wTo = do
  let tx = execBuildTx (payToAddress (Wallet.addressInEra Defaults.networkId wTo) (C.lovelaceToValue 10_000_000))
  balanceAndSubmit mempty wFrom tx

{-| Pay 100 Ada from one of the seed addresses to an @Operator@
-}
payToOperator :: (MonadMockchain m, MonadError BalanceTxError m) => Tracer m TxBalancingMessage -> Wallet -> Operator k -> m (Either SendTxFailed (C.Tx C.BabbageEra))
payToOperator dbg wFrom = payToOperator' dbg (C.lovelaceToValue 100_000_000) wFrom

{-| Pay some Ada from one of the seed addresses to an @Operator@
-}
payToOperator' :: (MonadMockchain m, MonadError BalanceTxError m) => Tracer m TxBalancingMessage -> Value -> Wallet -> Operator k -> m (Either SendTxFailed (C.Tx C.BabbageEra))
payToOperator' dbg value wFrom Operator{oPaymentKey} = do
  p <- queryProtocolParameters
  let addr =
        C.makeShelleyAddressInEra C.ShelleyBasedEraBabbage Defaults.networkId
        (C.PaymentCredentialByKey $ C.verificationKeyHash $ verificationKey oPaymentKey)
        C.NoStakeAddress
      tx = execBuildTx (payToAddress addr value >> setMinAdaDepositAll p)
  balanceAndSubmit dbg wFrom tx
