{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Blockfrost-backed implementation of @MonadBlockchain@
module Convex.Blockfrost (
  BlockfrostT (..),
  evalBlockfrostT,
  runBlockfrostT,

  -- * Utility functions
  streamUtxos,

  -- * Obtaining fully resolved transactions
  resolveTx,
  streamUTxOsWithAssetId,
) where

import Blockfrost.Client qualified as Client
import Blockfrost.Client.Types (
  BlockfrostClientT (..),
  BlockfrostError,
  ClientConfig,
  MonadBlockfrost,
  Project,
 )
import Blockfrost.Client.Types qualified as Types
import Blockfrost.Types.Cardano.Assets qualified as BF.Assets
import Blockfrost.Types.Shared.Address qualified as Types
import Cardano.Api qualified as C
import Control.Monad ((>=>))
import Control.Monad.Except (
  MonadError (..),
  liftEither,
  runExceptT,
 )
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Class (MonadTrans (lift))
import Convex.Blockfrost.MonadBlockchain (BlockfrostCache)
import Convex.Blockfrost.MonadBlockchain qualified as MonadBlockchain
import Convex.Blockfrost.Orphans ()
import Convex.Blockfrost.Types qualified as Types
import Convex.Class (
  MonadBlockchain (..),
  MonadUtxoQuery (..),
 )
import Convex.ResolvedTx (ResolvedTx (..))
import Convex.Utils (requiredTxIns)
import Convex.Utxos qualified as Utxos
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Set qualified as Set
import Streaming.Prelude (Of, Stream)
import Streaming.Prelude qualified as S

{- | Monad transformer that implements the @MonadBlockchain@
class using blockfrost's API
-}
newtype BlockfrostT m a = BlockfrostT {unBlockfrostT :: StateT BlockfrostCache (BlockfrostClientT m) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, Types.MonadBlockfrost)

-- | Unwrapped representation of BlockfrostT
type Block m a = BlockfrostCache -> ClientConfig -> m (Either BlockfrostError (a, BlockfrostCache))

-- | Application of coerce, making it easier to implement catchError
up :: BlockfrostT m a -> Block m a
up = coerce

-- | Application of coerce, making it easier to implement catchError
down :: Block m a -> BlockfrostT m a
down = coerce

instance MonadTrans BlockfrostT where
  lift = BlockfrostT . lift . lift

instance (MonadReader e m) => MonadReader e (BlockfrostT m) where
  ask = lift ask
  local f (up -> a) = down $ \cache config -> local f (a cache config)

instance (MonadError e m) => MonadError e (BlockfrostT m) where
  throwError = lift . throwError
  catchError (up -> action) handler = down $ \cache config ->
    catchError (action cache config) (fmap ((\block -> block cache config) . up) handler)

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
  queryStakeAddresses stakeCreds _ = BlockfrostT (MonadBlockchain.getStakeAddresses stakeCreds)
  queryStakePools = BlockfrostT MonadBlockchain.getStakePools
  queryStakeVoteDelegatees stakeCreds = BlockfrostT (MonadBlockchain.getStakeVoteDelegatees stakeCreds)
  querySystemStart = BlockfrostT MonadBlockchain.getSystemStart
  queryEraHistory = BlockfrostT MonadBlockchain.getEraHistory
  querySlotNo = BlockfrostT MonadBlockchain.getSlotNo
  queryNetworkId = BlockfrostT MonadBlockchain.getNetworkId

lookupUtxo :: (Types.MonadBlockfrost m) => Client.AddressUtxo -> m (Either Types.ScriptResolutionFailure (C.TxIn, C.TxOut C.CtxUTxO C.ConwayEra))
lookupUtxo addr = runExceptT $ do
  k <- either (Types.resolveScript >=> liftEither) pure (Types.addressUtxo @C.ConwayEra addr)
  pure (either (error . (<>) "lookupUtxo: hex conversion failed: ") id $ Types.addressUtxoTxIn addr, k)

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

-- | Download the transaction and all of its inputs
resolveTx :: (MonadBlockfrost m, MonadError Types.DecodingError m) => C.TxId -> m ResolvedTx
resolveTx txId = do
  rtxTransaction <- Types.resolveTx txId >>= liftEither
  let (C.Tx (C.getTxBodyContent -> txBodyContent) _witnesses) = rtxTransaction
  let reqTxIns = requiredTxIns txBodyContent
  utxo <- State.evalStateT (MonadBlockchain.getUtxoByTxIn reqTxIns) MonadBlockchain.emptyBlockfrostCache
  pure ResolvedTx{rtxTransaction, rtxInputs = C.unUTxO utxo}

{- | Stream all UTxOs with a given AssetId
It executes two queries to Blockfrost:
1. Get all addresses that currently hold this asset
2. For each such address, stream only UTxOs that contain *this* asset at that address
-}
streamUTxOsWithAssetId
  :: (Types.MonadBlockfrost m)
  => C.AssetId
  -> Stream (Of (Either Types.ScriptResolutionFailure (C.TxIn, C.TxOut C.CtxUTxO C.ConwayEra))) m ()
streamUTxOsWithAssetId (Types.assetIdToBlockfrostAssetId -> assetId) = do
  -- Stream all addresses that currently hold this asset
  S.for (Types.pagedStream (\p -> Client.getAssetAddresses' assetId p Client.Ascending)) $ \aa -> do
    -- 'aa' :: BF.Assets.AssetAddress
    let addr :: Types.Address
        addr = BF.Assets._assetAddressAddress aa
    -- For each such address, stream only UTxOs that contain *this* asset at that address
    S.mapM lookupUtxo $
      Types.pagedStream (\p -> Client.getAddressUtxosAsset' addr assetId p Client.Ascending)
