{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Blockfrost-backed implementation of @MonadBlockchain@
module Convex.Blockfrost (
  BlockfrostT (..),
  evalBlockfrostT,
  runBlockfrostT,

  -- * Utility functions
  streamUtxos,
) where

import Blockfrost.Client qualified as Client
import Blockfrost.Client.Types (
  BlockfrostClientT,
  BlockfrostError,
  Project,
 )
import Blockfrost.Client.Types qualified as Types
import Cardano.Api qualified as C
import Control.Monad ((>=>))
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Convex.Blockfrost.MonadBlockchain (BlockfrostCache)
import Convex.Blockfrost.MonadBlockchain qualified as MonadBlockchain
import Convex.Blockfrost.Orphans ()
import Convex.Blockfrost.Types qualified as Types
import Convex.Class (
  MonadBlockchain (..),
  MonadUtxoQuery (..),
 )
import Convex.Utxos qualified as Utxos
import Data.Bifunctor (Bifunctor (..))
import Data.Set qualified as Set
import Streaming.Prelude (Of, Stream)
import Streaming.Prelude qualified as S

{- | Monad transformer that implements the @MonadBlockchain@
class using blockfrost's API
-}
newtype BlockfrostT m a = BlockfrostT {unBlockfrostT :: StateT BlockfrostCache (BlockfrostClientT m) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, Types.MonadBlockfrost)

-- TODO: More instances (need to be defined on BlockfrostClientT')

instance (MonadIO m) => MonadUtxoQuery (BlockfrostT m) where
  utxosByPaymentCredentials credentials = BlockfrostT $ do
    let addresses = Set.toList credentials
    results' <- S.toList_ $ S.for (S.each addresses) $ \paymentCredential ->
      -- TODO: by using 'mapMaybe' we simply drop the outputs that have script resolution failures
      -- We should at least log them
      S.mapMaybe (either (const Nothing) Just) $ streamUtxos paymentCredential

    pure $
      Utxos.fromList @C.ConwayEra $
        fmap (second (,Nothing)) results'

instance (MonadIO m) => MonadBlockchain C.ConwayEra (BlockfrostT m) where
  sendTx = MonadBlockchain.sendTxBlockfrost
  utxoByTxIn = BlockfrostT . MonadBlockchain.getUtxoByTxIn
  queryProtocolParameters = BlockfrostT MonadBlockchain.getProtocolParams
  queryStakeAddresses s _ = BlockfrostT (MonadBlockchain.getStakeAddresses s)
  queryStakePools = BlockfrostT MonadBlockchain.getStakePools
  querySystemStart = BlockfrostT MonadBlockchain.getSystemStart
  queryEraHistory = BlockfrostT MonadBlockchain.getEraHistory
  querySlotNo = BlockfrostT MonadBlockchain.getSlotNo
  queryNetworkId = BlockfrostT MonadBlockchain.getNetworkId

lookupUtxo :: (Types.MonadBlockfrost m) => Client.AddressUtxo -> m (Either Types.ScriptResolutionFailure (C.TxIn, C.TxOut C.CtxUTxO C.ConwayEra))
lookupUtxo addr = runExceptT $ do
  k <- either (Types.resolveScript >=> liftEither) pure (Types.addressUtxo @C.ConwayEra addr)
  pure (Types.addressUtxoTxIn addr, k)

-- | Load all UTxOs for a payment credential in a stream. This includes resolution of reference scripts with 'Types.resolveScript'
streamUtxos :: (Types.MonadBlockfrost m) => C.PaymentCredential -> Stream (Of (Either Types.ScriptResolutionFailure (C.TxIn, C.TxOut C.CtxUTxO C.ConwayEra))) m ()
streamUtxos a =
  S.mapM lookupUtxo $
    Types.pagedStream (\p -> Client.getAddressUtxos' (Types.fromPaymentCredential a) p Client.Ascending)

-- | Run the 'BlockfrostT' transformer using the given blockfrost 'Project'
evalBlockfrostT :: (MonadIO m) => Project -> BlockfrostT m a -> m (Either BlockfrostError a)
evalBlockfrostT proj =
  Types.runBlockfrostClientT proj
    . flip State.evalStateT MonadBlockchain.emptyBlockfrostCache
    . unBlockfrostT

{- | Run the 'BlockfrostT' transformer using the given blockfrost 'Project' and the 'BlockfrostCache'
Returns the new blockfrost state.
-}
runBlockfrostT :: (MonadIO m) => BlockfrostCache -> Project -> BlockfrostT m a -> m (Either BlockfrostError (a, BlockfrostCache))
runBlockfrostT state proj =
  Types.runBlockfrostClientT proj
    . flip State.runStateT state
    . unBlockfrostT
