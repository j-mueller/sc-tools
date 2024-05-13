{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-| An effect for balancing transactions
-}
module Convex.CoinSelection.Class(
  MonadBalance(..),
  BalancingT(..),

  -- * Tracing
  TracingBalancingT(..),
  runTracingBalancingT
) where

import           Cardano.Api.Shelley              (AddressInEra, BabbageEra,
                                                   BuildTx, TxBodyContent)
import qualified Cardano.Api.Shelley              as C
import           Control.Monad.Catch              (MonadCatch, MonadMask,
                                                   MonadThrow)
import           Control.Monad.Except             (ExceptT, MonadError,
                                                   runExceptT)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Reader             (ReaderT (runReaderT), ask)
import           Control.Monad.Trans.Class        (MonadTrans (..))
import qualified Control.Monad.Trans.State        as StrictState
import qualified Control.Monad.Trans.State.Strict as LazyState
import           Control.Tracer                   (Tracer, natTracer)
import           Convex.Class                     (MonadBlockchain (..),
                                                   MonadMockchain (..),
                                                   MonadUtxoQuery (..))
import           Convex.CoinSelection             (BalanceTxError,
                                                   TxBalancingMessage)
import qualified Convex.CoinSelection
import           Convex.Lenses                    (emptyTxOut)
import           Convex.MonadLog                  (MonadLog, MonadLogIgnoreT)
import           Convex.Utxos                     (BalanceChanges (..),
                                                   UtxoSet (..))

{- Note [Transaction Balancing]

Why do we need an extra class for balancing when
we could just use 'Convex.CoinSelection.balanceTx'?

The reason is that we can use this class to inject modifications to the unbalanced tx,
which is the way we simulate attacks in the testing framework. So for normal operations
we do indeed just call 'Convex.CoinSelection.balanceTx', but for

-}

{-| Balancing a transaction
-}
class Monad m => MonadBalance m where
  {-| Balance the transaction using the given UTXOs and return address.
  -}
  balanceTx ::
    -- | Address used for leftover funds
    AddressInEra BabbageEra ->

    -- | Set of UTxOs that can be used to supply missing funds
    UtxoSet C.CtxUTxO a ->

    -- | The unbalanced transaction body
    TxBodyContent BuildTx BabbageEra ->

    -- | The balanced transaction body and the balance changes (per address)
    m (Either BalanceTxError (C.BalancedTxBody BabbageEra, BalanceChanges))

newtype BalancingT m a = BalancingT{runBalancingT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadFail, MonadLog, MonadThrow, MonadMask, MonadBlockchain)

instance MonadTrans BalancingT where
  lift = BalancingT

deriving newtype instance MonadError e m => MonadError e (BalancingT m)

instance MonadBalance m => MonadBalance (ExceptT e m) where
  balanceTx addr utxos = lift . balanceTx addr utxos

instance MonadBalance m => MonadBalance (ReaderT e m) where
  balanceTx addr utxos = lift . balanceTx addr utxos

instance MonadBalance m => MonadBalance (StrictState.StateT s m) where
  balanceTx addr utxos = lift . balanceTx addr utxos

instance MonadBalance m => MonadBalance (LazyState.StateT s m) where
  balanceTx addr utxos = lift . balanceTx addr utxos

instance MonadBalance m => MonadBalance (MonadLogIgnoreT m) where
  balanceTx addr utxos = lift . balanceTx addr utxos

instance (MonadBlockchain m) => MonadBalance (BalancingT m) where
  balanceTx addr utxos txb = runExceptT (Convex.CoinSelection.balanceTx mempty (emptyTxOut addr) utxos txb)

instance MonadMockchain m => MonadMockchain (BalancingT m) where
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo
  resolveDatumHash = lift . resolveDatumHash

instance MonadUtxoQuery m => MonadUtxoQuery (BalancingT m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

{-| Implementation of @MonadBalance@ that uses the provided tracer for debugging output
-}
newtype TracingBalancingT m a = TracingBalancingT{ runTracingBalancingT' :: ReaderT (Tracer m TxBalancingMessage) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadFail, MonadLog, MonadThrow, MonadMask, MonadBlockchain)

instance MonadTrans TracingBalancingT where
  lift = TracingBalancingT . lift

deriving newtype instance MonadError e m => MonadError e (TracingBalancingT m)

instance (MonadBlockchain m) => MonadBalance (TracingBalancingT m) where
  balanceTx addr utxos txb = TracingBalancingT $ do
    tr <- ask
    runExceptT (Convex.CoinSelection.balanceTx (natTracer (lift . lift) tr) (emptyTxOut addr) utxos txb)

instance MonadMockchain m => MonadMockchain (TracingBalancingT m) where
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo
  resolveDatumHash = lift . resolveDatumHash

instance MonadUtxoQuery m => MonadUtxoQuery (TracingBalancingT m) where
  utxosByPaymentCredentials = lift . utxosByPaymentCredentials

runTracingBalancingT :: Tracer m TxBalancingMessage -> TracingBalancingT m a -> m a
runTracingBalancingT tracer (TracingBalancingT action) = runReaderT action tracer
