{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-| blockfrost-based implementation of MonadBlockchain
-}
module Convex.Blockfrost.MonadBlockchain(
  BlockfrostState(..)
) where

import           Blockfrost.Client                            (Block (..))
import qualified Blockfrost.Client                            as Client
import           Blockfrost.Client.Cardano.Transactions       (submitTx)
import           Blockfrost.Client.Types                      (MonadBlockfrost (..),
                                                               SortOrder (Ascending))
import           Blockfrost.Types.Cardano.Epochs              (EpochInfo (..))
import           Blockfrost.Types.Cardano.Genesis             (Genesis)
import qualified Blockfrost.Types.Cardano.Genesis             as Genesis
import           Blockfrost.Types.Shared.CBOR                 (CBORString (..))
import           Cardano.Api                                  (ConwayEra,
                                                               NetworkId, Tx,
                                                               TxId, TxIn (..),
                                                               serialiseToCBOR)
import           Cardano.Api.NetworkId                        (fromNetworkMagic)
import           Cardano.Api.Shelley                          (CtxUTxO,
                                                               LedgerProtocolParameters (..),
                                                               PoolId, TxOut,
                                                               UTxO)
import qualified Cardano.Api.Shelley                          as C
import           Cardano.Slotting.Time                        (SlotLength,
                                                               SystemStart)
import           Control.Lens                                 (Lens', at,
                                                               makeLensesFor,
                                                               use, (.=), (<>=),
                                                               (?=))
import           Control.Monad.Except                         (runExceptT)
import           Control.Monad.IO.Class                       (MonadIO (..))
import           Control.Monad.State                          (MonadState)
import           Convex.Blockfrost.Orphans                    ()
import qualified Convex.Blockfrost.Types                      as Types
import           Convex.Class                                 (ValidationError)
import           Convex.Utils                                 (slotToUtcTime,
                                                               txnUtxos)
import           Data.Bifunctor                               (Bifunctor (second))
import qualified Data.ByteString.Lazy                         as BSL
import           Data.Map                                     (Map)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (fromJust)
import           Data.Set                                     (Set)
import qualified Data.Set                                     as Set
import           Data.SOP.NonEmpty                            (NonEmpty (..))
import qualified Data.SOP.NonEmpty                            as NonEmpty
import           Data.Time.Clock                              (UTCTime,
                                                               getCurrentTime)
import qualified Data.Time.Clock.POSIX                        as Clock
import           Data.Traversable                             (for)
import           Ouroboros.Consensus.Cardano.Block            (CardanoEras,
                                                               StandardCrypto)
import qualified Ouroboros.Consensus.HardFork.History.Qry     as Qry
import qualified Ouroboros.Consensus.HardFork.History.Summary as Summary
import           Ouroboros.Network.Magic                      (NetworkMagic (..))
import qualified Streaming.Prelude                            as S

-- TODO
-- stake addresses

-- DONE
-- protocol params
-- slot no
-- era history
-- utxoByTxIn
-- send Tx
-- query network id
-- stake pools
-- system start

data BlockfrostState =
  BlockfrostState
    { bfsGenesis        :: Maybe Genesis
    , bfsEndOfEpoch     :: Maybe UTCTime
      -- ^ End of current epoch
    , bfsStakePools     :: Maybe (Set PoolId)
      -- ^ Stake pool IDs
    , bfsTxInputs       :: Map TxIn (TxOut CtxUTxO ConwayEra)
      -- ^ Resolved tx inputs. We keep them around for a while because the
      --   lookup on blockfrost is quite expensive (in terms HTTP requests
      --   and CPU/memory usage)

    , bfsProtocolParams :: Maybe (LedgerProtocolParameters ConwayEra)

    , bfsEraHistory     :: Maybe C.EraHistory
      -- ^ Era history
    }

makeLensesFor
  [ ("bfsGenesis", "genesis")
  , ("bfsEndOfEpoch", "endOfEpoch")
  , ("bfsStakePools", "stakePools")
  , ("bfsTxInputs", "txInputs")
  , ("bfsProtocolParams", "protocolParams")
  , ("bfsEraHistory", "eraHistory")
  ]
  ''BlockfrostState

{-| Check whether the next epoch has begun, and expire
    all short-lived data if necessary.
-}
checkCurrentEpoch :: (MonadBlockfrost m, MonadState BlockfrostState m) => m ()
checkCurrentEpoch = do
  epochEnd <- use endOfEpoch
  now <- liftIO getCurrentTime
  case epochEnd of
    Just t | t > now -> pure () -- we are still in the same epoch
    _ -> do
      -- set current epoch end
      EpochInfo{_epochInfoEndTime} <- Client.getLatestEpoch
      endOfEpoch .= Just (Clock.posixSecondsToUTCTime _epochInfoEndTime)

      -- reset everything
      stakePools .= Nothing
      protocolParams .= Nothing

      -- the (txIn -> txOut) mapping does not change at the epoch boundary.
      -- So there is no risk of returning stale / incorrect data.
      -- But we still clear the txInputs map here to avoid memory leaks.
      txInputs   .= mempty

emptyBlockfrostState :: BlockfrostState
emptyBlockfrostState =
  BlockfrostState
    { bfsGenesis    = Nothing
    , bfsEndOfEpoch = Nothing
    , bfsStakePools = Nothing
    , bfsTxInputs   = Map.empty
    , bfsProtocolParams = Nothing
    , bfsEraHistory = Nothing
    }

getGenesis :: (MonadBlockfrost m, MonadState BlockfrostState m) => m Genesis
getGenesis = getOrRetrieve genesis Client.getLedgerGenesis

{-| Get a field from the state, using the action to load the value
if it is 'Nothing'
-}
getOrRetrieve :: MonadState s m => Lens' s (Maybe a) -> m a -> m a
getOrRetrieve lens action = use lens >>= \case
  Just g -> pure g
  Nothing -> do
    k <- action
    lens ?= k
    pure k

getSystemStart :: (MonadBlockfrost m, MonadState BlockfrostState m) => m SystemStart
getSystemStart = Types.systemStart <$> getGenesis

getNetworkId :: (MonadBlockfrost m, MonadState BlockfrostState m) => m NetworkId
getNetworkId = fromNetworkMagic . NetworkMagic . fromIntegral . Genesis._genesisNetworkMagic <$> getGenesis

getStakePools :: (MonadBlockfrost m, MonadState BlockfrostState m) => m (Set PoolId)
getStakePools = do
  checkCurrentEpoch
  getOrRetrieve stakePools $
    Set.fromList . fmap Types.poolId <$> S.toList_ (Types.pagedStream $ \page -> Client.listPools' page Ascending)

{-| Send a transaction to the network using blockfrost's API
-}
sendTxBlockfrost :: MonadBlockfrost m => Tx ConwayEra -> m (Either (ValidationError ConwayEra) TxId)
sendTxBlockfrost =
  fmap (Right . Types.toTxHash) . submitTx . CBORString . BSL.fromStrict . serialiseToCBOR

{-| Get a single 'TxIn'. If it is not in the cache, download the entire transaction
    and add all of its UTxOs to the cache.
-}
resolveTxIn :: (MonadBlockfrost m, MonadState BlockfrostState m) => TxIn -> m (TxOut CtxUTxO ConwayEra)
resolveTxIn txI@(TxIn txId (C.TxIx txIx)) = getOrRetrieve (txInputs . at txI) $ do
  utxos <- runExceptT (Client.getTxCBOR (Types.fromTxHash txId) >>= Types.decodeTransactionCBOR)
          -- FIXME: Error handling
          >>= either (error . show) (pure . fmap (second  C.toCtxUTxOTxOut) . txnUtxos)
  txInputs <>= Map.fromList utxos
  pure $ snd $ utxos !! fromIntegral txIx

{-| Resolve the given tx inputs
-}
getUtxoByTxIn :: (MonadBlockfrost m, MonadState BlockfrostState m) => Set TxIn -> m (UTxO ConwayEra)
getUtxoByTxIn txIns = fmap (C.UTxO . Map.fromList) $ for (Set.toList txIns) $ \txIn ->
  (txIn,) <$> resolveTxIn txIn

{-| Get the 'EraHistory' for slot time computations
-}
getEraHistory :: (MonadBlockfrost m, MonadState BlockfrostState m) => m C.EraHistory
getEraHistory = getOrRetrieve eraHistory $ do
  networkEras <- Client.getNetworkEras
  let summaries :: [Summary.EraSummary] = fmap Types.eraSummary networkEras
  pure
    $ C.EraHistory
    $ Qry.mkInterpreter
    $ Summary.Summary
    $ fromJust (error "getEraHistory: Unexpected number of entries")
    $ NonEmpty.nonEmptyFromList @(CardanoEras StandardCrypto) summaries

{-| Get the current slot number, slot length and UTC time of the start
of the current slot.
-}
getSlotNo :: (MonadBlockfrost m, MonadState BlockfrostState m) => m (C.SlotNo, SlotLength, UTCTime)
getSlotNo = do
  (eraHistory@(C.EraHistory interpreter), systemStart) <- (,) <$> getEraHistory <*> getSystemStart
  Block{_blockSlot} <- Client.getLatestBlock
  let currentSlot = maybe (error "getSlotNo: Expected slot") Types.slot _blockSlot
  let utctime     = either (error . (<>) "getSlotNo: slotToUtcTime failed " . show) id (slotToUtcTime eraHistory systemStart currentSlot)
      l           = either (error . (<>) "getSlotNo: slotToSlotLength failed " . show) id (Qry.interpretQuery interpreter $ Qry.slotToSlotLength currentSlot)
  pure (currentSlot, l, utctime)

{-| Get the current protocol parameters
-}
getProtocolParams :: (MonadBlockfrost m, MonadState BlockfrostState m) => m (LedgerProtocolParameters ConwayEra)
getProtocolParams = do
  checkCurrentEpoch
  getOrRetrieve protocolParams $
    LedgerProtocolParameters . Types.protocolParametersConway <$> Client.getLatestEpochProtocolParams
