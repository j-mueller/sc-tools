{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An effect for balancing transactions
module Convex.CoinSelection.Class (
  MonadBalance (..),
  BalancingT (..),

  -- * Tracing
  TracingBalancingT (..),
  runTracingBalancingT,
) where

import Cardano.Api.Shelley (AddressInEra)
import Cardano.Api.Shelley qualified as C
import Control.Monad.Catch (
  MonadCatch,
  MonadMask,
  MonadThrow,
 )
import Control.Monad.Except (
  ExceptT,
  MonadError,
  runExceptT,
 )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.State qualified as StrictState
import Control.Monad.Trans.State.Strict qualified as LazyState
import Control.Tracer (Tracer, natTracer)
import Convex.BuildTx (TxBuilder)
import Convex.CardanoApi.Lenses (emptyTxOut)
import Convex.Class (
  MonadBlockchain (..),
  MonadDatumQuery (queryDatumFromHash),
  MonadMockchain (..),
  MonadUtxoQuery,
 )
import Convex.CoinSelection (
  BalanceTxError,
  ChangeOutputPosition,
  TxBalancingMessage,
 )
import Convex.CoinSelection qualified
import Convex.MockChain (MockchainT)
import Convex.MonadLog (MonadLog, MonadLogIgnoreT)
import Convex.Utils (inBabbage)
import Convex.Utxos (
  BalanceChanges (..),
  UtxoSet (..),
 )

{- Note [Transaction Balancing]

Why do we need an extra class for balancing when
we could just use 'Convex.CoinSelection.balanceTx'?

The reason is that we can use this class to inject modifications to the unbalanced tx,
which is the way we simulate attacks in the testing framework. So for normal operations
we do indeed just call 'Convex.CoinSelection.balanceTx', but for

-}

-- | Balancing a transaction
class (Monad m) => MonadBalance era m where
  -- | Balance the transaction using the given UTXOs and return address.
  balanceTx
    :: AddressInEra era
    -- ^ Address used for leftover funds
    -> UtxoSet C.CtxUTxO a
    -- ^ Set of UTxOs that can be used to supply missing funds
    -> TxBuilder era
    -- ^ The unbalanced transaction body
    -> ChangeOutputPosition
    -- ^ The position of the change output
    -> m (Either (BalanceTxError era) (C.BalancedTxBody era, BalanceChanges))
    -- ^ The balanced transaction body and the balance changes (per address)
  default balanceTx
    :: (MonadTrans t, m ~ t n, MonadBalance era n)
    => AddressInEra era -> UtxoSet C.CtxUTxO a -> TxBuilder era -> ChangeOutputPosition -> m (Either (BalanceTxError era) (C.BalancedTxBody era, BalanceChanges))
  balanceTx = (((lift .) .) .) . balanceTx

newtype BalancingT m a = BalancingT {runBalancingT :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadFail, MonadLog, MonadThrow, MonadMask, Convex.Class.MonadBlockchain era)

instance MonadTrans BalancingT where
  lift = BalancingT

-- Same for BalancingT
instance (PrimMonad m) => PrimMonad (BalancingT m) where
  type PrimState (BalancingT m) = PrimState m
  {-# INLINEABLE primitive #-}
  primitive f = lift (primitive f)

deriving newtype instance (MonadError e m) => MonadError e (BalancingT m)

instance (MonadBalance era m) => MonadBalance era (ExceptT e m)
instance (MonadBalance era m) => MonadBalance era (ReaderT e m)
instance (MonadBalance era m) => MonadBalance era (StrictState.StateT s m)
instance (MonadBalance era m) => MonadBalance era (LazyState.StateT s m)
instance (MonadBalance era m) => MonadBalance era (MonadLogIgnoreT m)
instance (MonadBalance era m) => MonadBalance era (MockchainT era m)
instance (C.IsBabbageBasedEra era, Convex.Class.MonadBlockchain era m) => MonadBalance era (BalancingT m) where
  balanceTx addr utxos txb changePosition = runExceptT (Convex.CoinSelection.balanceTx mempty (inBabbage @era emptyTxOut addr) utxos txb changePosition)

instance (Convex.Class.MonadMockchain era m) => Convex.Class.MonadMockchain era (BalancingT m)
instance (Convex.Class.MonadUtxoQuery m) => Convex.Class.MonadUtxoQuery (BalancingT m)

instance (Convex.Class.MonadDatumQuery m) => Convex.Class.MonadDatumQuery (BalancingT m) where
  queryDatumFromHash = lift . Convex.Class.queryDatumFromHash

-- | Implementation of @MonadBalance@ that uses the provided tracer for debugging output
newtype TracingBalancingT m a = TracingBalancingT {runTracingBalancingT' :: ReaderT (Tracer m TxBalancingMessage) m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadFail, MonadLog, MonadThrow, MonadMask, Convex.Class.MonadBlockchain era)

instance MonadTrans TracingBalancingT where
  lift = TracingBalancingT . lift

deriving newtype instance (MonadError e m) => MonadError e (TracingBalancingT m)

instance (C.IsBabbageBasedEra era, Convex.Class.MonadBlockchain era m) => MonadBalance era (TracingBalancingT m) where
  balanceTx addr utxos txb changePosition = TracingBalancingT $ do
    tr <- ask
    runExceptT (Convex.CoinSelection.balanceTx (natTracer (lift . lift) tr) (inBabbage @era emptyTxOut addr) utxos txb changePosition)

instance (Convex.Class.MonadMockchain era m) => Convex.Class.MonadMockchain era (TracingBalancingT m)
instance (Convex.Class.MonadUtxoQuery m) => Convex.Class.MonadUtxoQuery (TracingBalancingT m)

instance (Convex.Class.MonadDatumQuery m) => Convex.Class.MonadDatumQuery (TracingBalancingT m) where
  queryDatumFromHash = lift . Convex.Class.queryDatumFromHash

runTracingBalancingT :: Tracer m TxBalancingMessage -> TracingBalancingT m a -> m a
runTracingBalancingT tracer (TracingBalancingT action) = runReaderT action tracer
