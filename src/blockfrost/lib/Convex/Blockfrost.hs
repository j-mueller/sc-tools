{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- Need this because of missing instances for BlockfrostClientT
{-# OPTIONS_GHC -Wno-orphans #-}
{-| Blockfrost-backed implementation of @MonadBlockchain@
-}
module Convex.Blockfrost(
  BlockfrostT(..),
  runBlockfrostT,
  -- * Utility functions
  streamUtxos
) where

import qualified Blockfrost.Client         as Client
import           Blockfrost.Client.Types   (BlockfrostClientT, BlockfrostError,
                                            MonadBlockfrost (..), Project)
import qualified Blockfrost.Client.Types   as Types
import qualified Cardano.Api               as C
import           Control.Monad             ((>=>))
import           Control.Monad.Except      (ExceptT (..), liftEither,
                                            runExceptT)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import qualified Convex.Blockfrost.Types   as Types
import           Convex.Class              (MonadUtxoQuery (..))
import qualified Convex.Utxos              as Utxos
import           Data.Bifunctor            (Bifunctor (..))
import qualified Data.Set                  as Set
import qualified Streaming.Prelude         as S
import           Streaming.Prelude         (Of, Stream)

{-| Monad transformer that implements the @MonadBlockchain@
class using blockfrost's API
-}
newtype BlockfrostT m a = BlockfrostT{ unBlockfrostT :: BlockfrostClientT m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadBlockfrost m => MonadBlockfrost (ExceptT e m) where
  liftBlockfrostClient = lift . liftBlockfrostClient
  getConf = lift getConf

-- TODO: More instances (need to be defined on BlockfrostClientT')

instance MonadIO m => MonadUtxoQuery (BlockfrostT m) where
  utxosByPaymentCredentials credentials = BlockfrostT $ do
    let addresses = Set.toList credentials
    results' <- S.toList_ $ S.for (S.each addresses) $ \paymentCredential ->
      -- TODO: by using 'mapMaybe' we simply drop the outputs that have script resolution failures
      -- We should at least log them
      S.mapMaybe (either (const Nothing) Just) $ streamUtxos paymentCredential

    pure
      $ Utxos.fromList @C.ConwayEra
      $ fmap (second (, Nothing)) results'

lookupUtxo :: Types.MonadBlockfrost m => Client.AddressUtxo -> m (Either Types.ScriptResolutionFailure (C.TxIn, C.TxOut C.CtxUTxO C.ConwayEra))
lookupUtxo addr = runExceptT $ do
  k <- either (Types.resolveScript >=> liftEither) pure (Types.addressUtxo @C.ConwayEra addr)
  pure (Types.addressUtxoTxIn addr, k)

{-| Load all UTxOs for a payment credential in a stream. This includes resolution of reference scripts with 'Types.resolveScript'
-}
streamUtxos :: Types.MonadBlockfrost m => C.PaymentCredential -> Stream (Of (Either Types.ScriptResolutionFailure (C.TxIn, C.TxOut C.CtxUTxO C.ConwayEra))) m ()
streamUtxos a =
  S.mapM lookupUtxo
  $ pagedStream (\p -> Client.getAddressUtxos' (Types.fromPaymentCredential a) p Client.Ascending)

{-| Stream a list of results from a paged query
-}
pagedStream :: Monad m => (Types.Paged -> m [a]) -> Stream (Of a) m ()
pagedStream action = flip S.for S.each $ flip S.unfoldr 1 $ \pageNumber -> do
  let paged = Client.Paged{Client.countPerPage = 100, Client.pageNumber = pageNumber}
  action paged >>= \case
    [] -> pure (Left ())
    xs -> pure (Right (xs, succ pageNumber))

{-| Run the 'BlockfrostT' transformer using the given blockfrost 'Project'
-}
runBlockfrostT :: MonadIO m => Project -> BlockfrostT m a -> m (Either BlockfrostError a)
runBlockfrostT proj = Types.runBlockfrostClientT proj . unBlockfrostT
