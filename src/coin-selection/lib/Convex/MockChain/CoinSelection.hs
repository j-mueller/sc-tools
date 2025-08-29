{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

{- | Conveniences for balancing transactions and selecting coins
on the mockchain
-}
module Convex.MockChain.CoinSelection (
  balanceAndSubmit,
  tryBalanceAndSubmit,
  balanceAndSubmitReturn,
  paymentTo,
  payToOperator,
  payToOperator',
) where

import Cardano.Api (Value)
import Cardano.Api qualified as C
import Control.Monad.Except (MonadError)
import Control.Tracer (Tracer)
import Convex.BuildTx (
  TxBuilder,
  execBuildTx,
  payToAddress,
  setMinAdaDepositAll,
 )
import Convex.CardanoApi.Lenses (emptyTxOut)
import Convex.Class (
  MonadBlockchain (..),
  MonadMockchain,
  ValidationError,
 )
import Convex.CoinSelection (
  BalanceTxError,
  ChangeOutputPosition (TrailingChange),
  TxBalancingMessage,
 )
import Convex.CoinSelection qualified as CoinSelection
import Convex.MockChain qualified as MockChain
import Convex.MockChain.Defaults qualified as Defaults
import Convex.Utils (inBabbage)
import Convex.Wallet (Wallet)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.Operator (Operator (..), verificationKey)
import Data.Functor (($>))

{- | Balance and submit a transaction using the wallet's UTXOs
on the mockchain, using the default network ID
-}
balanceAndSubmit
  :: forall era m
   . (MonadMockchain era m, MonadError (BalanceTxError era) m, C.IsBabbageBasedEra era)
  => Tracer m TxBalancingMessage
  -> Wallet
  -> TxBuilder era
  -> ChangeOutputPosition
  -> [C.ShelleyWitnessSigningKey]
  -> m (Either (ValidationError era) (C.Tx era))
balanceAndSubmit dbg wallet tx changePosition keys = inBabbage @era $ do
  n <- queryNetworkId
  let walletAddress = Wallet.addressInEra n wallet
      txOut = emptyTxOut walletAddress
  balanceAndSubmitReturn dbg wallet txOut tx changePosition keys

{- | Balance and submit a transaction using the wallet's UTXOs
on the mockchain, using the default network ID. Fail if the
transaction is not accepted by the node.
-}
tryBalanceAndSubmit
  :: forall era m
   . (MonadMockchain era m, MonadError (BalanceTxError era) m, MonadFail m, C.IsBabbageBasedEra era)
  => Tracer m TxBalancingMessage
  -> Wallet
  -> TxBuilder era
  -> ChangeOutputPosition
  -> [C.ShelleyWitnessSigningKey]
  -> m (C.Tx era)
tryBalanceAndSubmit dbg wallet tx changePosition keys = inBabbage @era $ do
  n <- queryNetworkId
  let walletAddress = Wallet.addressInEra n wallet
      txOut = emptyTxOut walletAddress
  balanceAndSubmitReturn dbg wallet txOut tx changePosition keys >>= either (fail . show) pure

{- | Balance and submit a transaction using the given return output and the wallet's UTXOs
on the mockchain, using the default network ID
-}
balanceAndSubmitReturn
  :: forall era m
   . (MonadMockchain era m, MonadError (BalanceTxError era) m, C.IsBabbageBasedEra era)
  => Tracer m TxBalancingMessage
  -> Wallet
  -> C.TxOut C.CtxTx era
  -> TxBuilder era
  -> ChangeOutputPosition
  -> [C.ShelleyWitnessSigningKey]
  -> m (Either (ValidationError era) (C.Tx era))
balanceAndSubmitReturn dbg wallet returnOutput tx changePosition keys = inBabbage @era $ do
  u <- MockChain.walletUtxo wallet
  (tx', _) <- CoinSelection.balanceForWalletReturn dbg wallet u returnOutput tx changePosition
  let
    appendKeys (C.Tx body wit) =
      C.makeSignedTransaction
        ((C.makeShelleyKeyWitness C.shelleyBasedEra body <$> keys) ++ wit)
        body
    signedTx = appendKeys tx'
  k <- sendTx signedTx
  pure (k $> signedTx)

-- | Pay ten Ada from one wallet to another
paymentTo
  :: forall era m
   . (MonadMockchain era m, MonadError (BalanceTxError era) m, C.IsBabbageBasedEra era)
  => Wallet -> Wallet -> m (Either (ValidationError era) (C.Tx era))
paymentTo wFrom wTo = inBabbage @era $ do
  let tx = execBuildTx (payToAddress (Wallet.addressInEra Defaults.networkId wTo) (C.lovelaceToValue 10_000_000))
  balanceAndSubmit mempty wFrom tx TrailingChange []

-- | Pay 100 Ada from one of the seed addresses to an @Operator@
payToOperator :: (MonadMockchain era m, MonadError (BalanceTxError era) m, C.IsBabbageBasedEra era) => Tracer m TxBalancingMessage -> Wallet -> Operator k -> m (Either (ValidationError era) (C.Tx era))
payToOperator dbg = payToOperator' dbg (C.lovelaceToValue 100_000_000)

-- | Pay some Ada from one of the seed addresses to an @Operator@
payToOperator' :: forall era k m. (MonadMockchain era m, MonadError (BalanceTxError era) m, C.IsBabbageBasedEra era) => Tracer m TxBalancingMessage -> Value -> Wallet -> Operator k -> m (Either (ValidationError era) (C.Tx era))
payToOperator' dbg value wFrom Operator{oPaymentKey} = inBabbage @era $ do
  p <- queryProtocolParameters
  let addr =
        C.makeShelleyAddressInEra @era
          (C.convert C.babbageBasedEra)
          Defaults.networkId
          (C.PaymentCredentialByKey $ C.verificationKeyHash $ verificationKey oPaymentKey)
          C.NoStakeAddress
      tx = execBuildTx (payToAddress @era addr value >> setMinAdaDepositAll p)
  balanceAndSubmit dbg wFrom tx TrailingChange []
