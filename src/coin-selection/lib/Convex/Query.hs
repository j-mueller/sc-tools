{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Custom class to encapsulate the general purpose
queries that we need for building transactions
-}
module Convex.Query(
  MonadUtxoQuery(..),
  BalancingError(..),
  balanceTx,

  -- * Tx balancing for operator
  balanceAndSubmitOperator,
  operatorUtxos,
  selectOperatorUTxO,
  BalanceAndSubmitError(..),

  -- * Wallet API queries
  WalletAPIQueryT(..),
  runWalletAPIQueryT
) where

import           Cardano.Api                       (BabbageEra, BalancedTxBody,
                                                    BuildTx, CtxTx,
                                                    PaymentCredential (..),
                                                    TxBodyContent, TxOut,
                                                    UTxO (..))
import qualified Cardano.Api                       as C
import           Control.Monad.Except              (MonadError)
import           Control.Monad.IO.Class            (MonadIO (..))
import           Control.Monad.Reader              (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Class         (MonadTrans (..))
import           Control.Monad.Trans.Except        (ExceptT, runExceptT)
import           Control.Monad.Trans.Except.Result (ResultT)
import qualified Control.Monad.Trans.State         as StrictState
import qualified Control.Monad.Trans.State.Strict  as LazyState
import           Convex.Class                      (MonadBlockchain (..),
                                                    MonadBlockchainCardanoNodeT)
import qualified Convex.CoinSelection
import           Convex.MockChain                  (MockchainT, utxoSet)
import           Convex.MonadLog                   (MonadLogIgnoreT)
import           Convex.Utils                      (liftEither, liftResult)
import           Convex.Utxos                      (BalanceChanges, fromApiUtxo,
                                                    fromUtxoTx, onlyCredential,
                                                    toApiUtxo)
import qualified Convex.Wallet.API                 as Wallet.API
import           Convex.Wallet.Operator            (Operator (..), Signing,
                                                    operatorPaymentCredential,
                                                    operatorReturnOutput,
                                                    signTx)
import qualified Data.Map                          as Map
import           Data.Maybe                        (listToMaybe)
import           Servant.Client                    (ClientEnv)

class Monad m => MonadUtxoQuery m where
  utxosByPayment :: PaymentCredential -> m (UTxO BabbageEra)

instance Monad m => MonadUtxoQuery (MockchainT m) where
  utxosByPayment cred = toApiUtxo . onlyCredential cred <$> utxoSet

instance MonadUtxoQuery m => MonadUtxoQuery (ResultT m) where
  utxosByPayment = lift . utxosByPayment

instance MonadUtxoQuery m => MonadUtxoQuery (ExceptT e m) where
  utxosByPayment = lift . utxosByPayment

instance MonadUtxoQuery m => MonadUtxoQuery (ReaderT e m) where
  utxosByPayment = lift . utxosByPayment

instance MonadUtxoQuery m => MonadUtxoQuery (StrictState.StateT s m) where
  utxosByPayment = lift . utxosByPayment

instance MonadUtxoQuery m => MonadUtxoQuery (LazyState.StateT s m) where
  utxosByPayment = lift . utxosByPayment

instance MonadUtxoQuery m => MonadUtxoQuery (MonadBlockchainCardanoNodeT e m) where
  utxosByPayment = lift . utxosByPayment

instance MonadUtxoQuery m => MonadUtxoQuery (MonadLogIgnoreT m) where
  utxosByPayment = lift . utxosByPayment

newtype BalancingError = BalancingError String
  deriving stock (Eq, Ord, Show)

{-| Balance the transaction body using the UTxOs locked by the payment credential,
and returning any unused funds to the given payment credential and stake credential
|-}
balanceTx :: (MonadBlockchain m, MonadUtxoQuery m) => PaymentCredential -> TxOut CtxTx BabbageEra -> TxBodyContent BuildTx BabbageEra -> m (Either BalancingError (BalancedTxBody BabbageEra, BalanceChanges))
balanceTx operator changeOutput txBody = do
  o <- fromApiUtxo <$> utxosByPayment operator
  runExceptT $ liftResult BalancingError (Convex.CoinSelection.balanceTx changeOutput o txBody)

newtype WalletAPIQueryT m a = WalletAPIQueryT{ runWalletAPIQueryT_ :: ReaderT ClientEnv m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runWalletAPIQueryT :: ClientEnv -> WalletAPIQueryT m a -> m a
runWalletAPIQueryT env (WalletAPIQueryT action) = runReaderT action env

instance MonadIO m => MonadUtxoQuery (WalletAPIQueryT m) where
  utxosByPayment credential = WalletAPIQueryT $ do
    result <- ask >>= liftIO . Wallet.API.getUTxOs
    case result of
      Left err -> do
        -- TODO: Better error handling
        let msg = "WalletAPI: Error when calling remote server: " <> show err
        liftIO (putStrLn msg)
        error msg
      Right x  -> pure (toApiUtxo $ onlyCredential credential $ fromUtxoTx x)

-- | Balance a transaction body, sign it with the operator's key, and submit it to the network.
balanceAndSubmitOperator :: (MonadBlockchain m, MonadUtxoQuery m, MonadError BalanceAndSubmitError m) => Operator Signing -> Maybe (C.TxOut C.CtxTx C.BabbageEra) -> C.TxBodyContent C.BuildTx C.BabbageEra -> m (C.Tx C.BabbageEra)
balanceAndSubmitOperator op@Operator{oPaymentKey} returnOutput txBody = do
  output <- maybe (operatorReturnOutput op) pure returnOutput
  (C.BalancedTxBody balancedTxBody _changeOutput _fee, _) <- liftEither BalanceError $
    balanceTx (operatorPaymentCredential op) output txBody
  let finalTx = signTx oPaymentKey $ C.makeSignedTransaction [] balancedTxBody
  liftResult SubmitError (sendTx finalTx) >> pure finalTx

{-| UTxOs that are locked by the operator's payment credential
|-}
operatorUtxos :: MonadUtxoQuery m => Operator k -> m (C.UTxO C.BabbageEra)
operatorUtxos = utxosByPayment . operatorPaymentCredential

{-| Select a single UTxO that is controlled by the operator. |-}
selectOperatorUTxO :: MonadUtxoQuery m => Operator k -> m (Maybe (C.TxIn, C.TxOut C.CtxUTxO C.BabbageEra))
selectOperatorUTxO operator = fmap (listToMaybe . Map.toList . C.unUTxO) (operatorUtxos operator)

-- | Failures during txn balancing and submission
data BalanceAndSubmitError =
  BalanceError BalancingError
  | SubmitError String
  deriving Show
