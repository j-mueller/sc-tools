{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Custom class to encapsulate the general purpose
queries that we need for building transactions
-}
module Convex.Query(
  MonadUtxoQuery(..),
  utxosByPaymentCredential,
  balanceTx,

  -- * Tx balancing for operator
  balanceAndSubmitOperator,
  balancePaymentCredential,
  balancePaymentCredentials,
  signTxOperator,
  signAndSubmitOperator,
  operatorUtxos,
  selectOperatorUTxO,
  BalanceAndSubmitError(..),

  -- * Wallet API queries
  WalletAPIQueryT(..),
  runWalletAPIQueryT
) where

import           Cardano.Api                        (BabbageEra, BalancedTxBody,
                                                     PaymentCredential (..))
import qualified Cardano.Api                        as C
import           Control.Monad.Except               (MonadError)
import           Control.Monad.IO.Class             (MonadIO (..))
import           Control.Monad.Reader               (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Class          (MonadTrans (..))
import           Control.Monad.Trans.Except         (ExceptT, runExceptT)
import           Control.Monad.Trans.Except.Result  (ResultT)
import qualified Control.Monad.Trans.State          as StrictState
import qualified Control.Monad.Trans.State.Strict   as LazyState
import           Control.Tracer                     (Tracer, natTracer)
import           Convex.BuildTx                     (TxBuilder)
import           Convex.Class                       (MonadBlockchain (..),
                                                     MonadBlockchainCardanoNodeT)
import           Convex.CoinSelection               (BalanceTxError, ChangeOutputPosition,
                                                     TxBalancingMessage)
import qualified Convex.CoinSelection
import           Convex.MockChain                   (MockchainT, utxoSet)
import           Convex.MonadLog                    (MonadLog, MonadLogIgnoreT)
import           Convex.NodeClient.WaitForTxnClient (MonadBlockchainWaitingT (..))
import           Convex.Utils                       (liftEither, liftResult)
import           Convex.Utxos                       (BalanceChanges,
                                                     fromUtxoTx,
                                                     onlyCredentials)
import qualified Convex.Wallet.API                  as Wallet.API
import           Convex.Wallet.Operator             (Operator (..), Signing,
                                                     operatorPaymentCredential,
                                                     returnOutputFor,
                                                     signTxOperator)
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Functor                       (($>))
import qualified Data.Map                           as Map
import           Data.Maybe                         (listToMaybe)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           GHC.Generics                       (Generic)
import           Servant.Client                     (ClientEnv)
import           Test.QuickCheck.Monadic            (PropertyM)
import Convex.Utxos (UtxoSet(_utxos))

class Monad m => MonadUtxoQuery m where
  utxosByPaymentCredentials :: Set PaymentCredential -> m (UtxoSet C.CtxUTxO ())

utxosByPaymentCredential :: MonadUtxoQuery m => PaymentCredential -> m (UtxoSet C.CtxUTxO ())
utxosByPaymentCredential = utxosByPaymentCredentials . Set.singleton

instance Monad m => MonadUtxoQuery (MockchainT m) where
  utxosByPaymentCredentials cred = onlyCredentials cred <$> utxoSet

instance MonadUtxoQuery m => MonadUtxoQuery (ResultT m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (ExceptT e m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (ReaderT e m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (StrictState.StateT s m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (LazyState.StateT s m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (MonadBlockchainCardanoNodeT e m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (MonadLogIgnoreT m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

instance MonadUtxoQuery m => MonadUtxoQuery (PropertyM m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

deriving newtype instance MonadUtxoQuery m => MonadUtxoQuery (MonadBlockchainWaitingT m)

{-| Balance the transaction body using the UTxOs locked by the payment credentials,
returning any unused funds to the given return output
|-}
balanceTx
  :: (MonadBlockchain m, MonadUtxoQuery m)
  => Tracer m TxBalancingMessage
  -> [PaymentCredential]
  -> C.InAnyCardanoEra (C.TxOut C.CtxTx)
  -> TxBuilder
  -> ChangeOutputPosition
  -> m (Either (BalanceTxError C.BabbageEra) (BalancedTxBody BabbageEra, BalanceChanges))
balanceTx dbg inputCredentials changeOutput txBody changePosition = do
  o <- utxosByPaymentCredentials (Set.fromList inputCredentials)
  runExceptT (Convex.CoinSelection.balanceTx (natTracer lift dbg) changeOutput o txBody changePosition)

newtype WalletAPIQueryT m a = WalletAPIQueryT{ runWalletAPIQueryT_ :: ReaderT ClientEnv m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBlockchain, MonadLog)

runWalletAPIQueryT :: ClientEnv -> WalletAPIQueryT m a -> m a
runWalletAPIQueryT env (WalletAPIQueryT action) = runReaderT action env

instance MonadIO m => MonadUtxoQuery (WalletAPIQueryT m) where
  utxosByPaymentCredentials credentials = WalletAPIQueryT $ do
    result <- ask >>= liftIO . Wallet.API.getUTxOs
    case result of
      Left err -> do
        -- TODO: Better error handling
        let msg = "WalletAPI: Error when calling remote server: " <> show err
        liftIO (putStrLn msg)
        error msg
      Right x  -> pure (onlyCredentials credentials $ fromUtxoTx x)

deriving newtype instance MonadError e m => MonadError e (WalletAPIQueryT m)

{-| Balance a transaction body using the funds locked by one of a list of payment credentials
-}
balancePaymentCredentials ::
  (MonadBlockchain m, MonadUtxoQuery m, MonadError (BalanceAndSubmitError C.BabbageEra) m) =>
  Tracer m TxBalancingMessage ->
  C.PaymentCredential -> -- ^ Primary payment credential, used for return output
  [C.PaymentCredential] -> -- ^ Other payment credentials, used for balancing
  Maybe (C.TxOut C.CtxTx C.BabbageEra) ->
  TxBuilder ->
  ChangeOutputPosition ->
  m (C.Tx C.BabbageEra)
balancePaymentCredentials dbg primaryCred otherCreds returnOutput txBody changePosition = do
  output <- fmap (C.InAnyCardanoEra C.BabbageEra) $ maybe (returnOutputFor primaryCred) pure returnOutput
  (C.BalancedTxBody _ txbody _changeOutput _fee, _) <- liftEither BalanceError (balanceTx dbg (primaryCred:otherCreds) output txBody changePosition)
  pure (C.makeSignedTransaction [] txbody)

{-| Balance a transaction body using the funds locked by the payment credential
-}
balancePaymentCredential
  :: (MonadBlockchain m, MonadUtxoQuery m, MonadError (BalanceAndSubmitError C.BabbageEra) m)
  => Tracer m TxBalancingMessage
  -> C.PaymentCredential
  -> Maybe (C.TxOut C.CtxTx C.BabbageEra)
  -> TxBuilder
  -> ChangeOutputPosition
  -> m (C.Tx C.BabbageEra)
balancePaymentCredential dbg cred = balancePaymentCredentials dbg cred []

-- | Balance a transaction body, sign it with the operator's key, and submit it to the network.
balanceAndSubmitOperator
  :: (MonadBlockchain m, MonadUtxoQuery m, MonadError (BalanceAndSubmitError C.BabbageEra) m)
  => Tracer m TxBalancingMessage
  -> Operator Signing
  -> Maybe (C.TxOut C.CtxTx C.BabbageEra)
  -> TxBuilder
  -> ChangeOutputPosition
  -> m (C.Tx C.BabbageEra)
balanceAndSubmitOperator dbg op changeOut txBody changePosition = balancePaymentCredential dbg (operatorPaymentCredential op) changeOut txBody changePosition >>= signAndSubmitOperator op

{-| Add the operator's signature to the transaction and send it to the blockchain
-}
signAndSubmitOperator
  :: (MonadBlockchain m, MonadError (BalanceAndSubmitError C.BabbageEra) m)
  => Operator Signing
  -> C.Tx C.BabbageEra
  -> m (C.Tx C.BabbageEra)
signAndSubmitOperator op tx = do
  let finalTx = signTxOperator op tx
  liftResult SubmitError (sendTx finalTx) $> finalTx

{-| UTxOs that are locked by the operator's payment credential
|-}
operatorUtxos :: MonadUtxoQuery m => Operator k -> m [(C.TxIn, C.InAnyCardanoEra (C.TxOut C.CtxUTxO))]
operatorUtxos = fmap (Map.toList . fmap fst . _utxos) . utxosByPaymentCredential . operatorPaymentCredential

{-| Select a single UTxO that is controlled by the operator. |-}
selectOperatorUTxO :: MonadUtxoQuery m => Operator k -> m (Maybe (C.TxIn, C.InAnyCardanoEra (C.TxOut (C.CtxUTxO))))
selectOperatorUTxO operator = fmap listToMaybe (operatorUtxos operator)

-- | Failures during txn balancing and submission
data BalanceAndSubmitError era =
  BalanceError (BalanceTxError era)
  | SubmitError String
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)
