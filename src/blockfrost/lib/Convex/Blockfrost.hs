{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Blockfrost-backed implementation of @MonadBlockchain@
-}
module Convex.Blockfrost(
  BlockfrostT(..)
) where

import qualified Blockfrost.Client         as Client
import           Blockfrost.Client.Types   (BlockfrostClientT)
import qualified Cardano.Api               as C
import           Control.Monad             (join)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Primitive   (PrimMonad)
import qualified Convex.Blockfrost.Types   as Types
import           Convex.Class              (MonadBlockchain (..),
                                            MonadUtxoQuery (..))
import           Data.Bifunctor            (Bifunctor (..))
import qualified Data.Set                  as Set

{-| Monad transformer that implements the @MonadBlockchain@
class using blockfrost's API
-}
newtype BlockfrostT m a = BlockfrostT{ runBlockfrostT :: BlockfrostClientT m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadBlockchain C.ConwayEra (BlockfrostT m) where
  -- FIXME: Implement
  sendTx = undefined
  -- sendTx = BlockfrostT . fmap (Right . Types.toTxHash) . Client.submitTx . Types.toCBORString . undefined

  utxoByTxIn = undefined
  queryProtocolParameters = undefined
  queryStakeAddresses = undefined
  queryStakePools = undefined
  querySystemStart = undefined
  queryEraHistory = undefined
  querySlotNo = undefined
  queryNetworkId = undefined

instance MonadIO m => MonadUtxoQuery (BlockfrostT m) where
  utxosByPaymentCredentials credentials = BlockfrostT $ do
    let addresses = Set.toList credentials
        paged     = Client.Paged{Client.countPerPage = 10000, Client.pageNumber = 1}
    results <-
      fmap (second $ Types.addressUtxo @C.ConwayEra)
      . join
      <$> traverse (\a -> fmap (a,) <$> Client.getAddressUtxos' (Types.fromPaymentCredential a) paged Client.Ascending) addresses
    results' <- traverse (traverse (either Types.resolveScript (pure . Right))) results
    -- _ results
    undefined
