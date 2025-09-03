{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Maestro-backed implementation helpers for MonadBlockchain
module Convex.Maestro.MonadBlockchain (
  -- Function set to be used by a higher-level transformer
  sendTxMaestro,
  getUtxoByTxIn,
  getProtocolParams,
  getStakeAddresses,
  getStakePools,
  getStakeVoteDelegatees,
  getSystemStart,
  getEraHistory,
  getSlotNo,
  getNetworkId,
) where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Slotting.Time (SlotLength, SystemStart)
import Control.Monad.Trans.Class (lift)
import Convex.Class (ValidationError)
import Convex.Maestro.Types qualified as CMTypes
import Convex.Utils (
  slotToUtcTime,
 )
import Data.Bifunctor (Bifunctor (second))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime)
import Maestro.Client.Env qualified as Env
import Maestro.Client.V1.Addresses ()

import Cardano.Api.Ledger qualified as L
import Data.List (isInfixOf)
import Data.Maybe (fromJust, mapMaybe)
import Data.SOP.NonEmpty qualified as NonEmpty
import Maestro.Client.V1.Accounts qualified as Maestro
import Maestro.Client.V1.Core.Pagination
import Maestro.Client.V1.General qualified as Maestro.General
import Maestro.Client.V1.Pools qualified as Maestro.Pools
import Maestro.Client.V1.Transactions (outputsByReferences)
import Maestro.Client.V1.TxManager qualified as Maestro
import Maestro.Types.V1 qualified as M
import Maestro.Types.V1 qualified as Maestro
import Ouroboros.Consensus.Cardano.Block (
  CardanoEras,
  StandardCrypto,
 )
import Ouroboros.Consensus.HardFork.History.Qry qualified as Qry
import Ouroboros.Consensus.HardFork.History.Summary qualified as Summary
import Servant.Client qualified as Servant
import Streaming.Prelude (Of, Stream)
import Streaming.Prelude qualified as S

-- | Submit a transaction (not yet supported by Maestro SDK)
sendTxMaestro :: Bool -> Env.MaestroEnv 'Env.V1 -> C.Tx C.ConwayEra -> IO (Either (ValidationError CMTypes.CurrentEra) C.TxId)
sendTxMaestro turboFlag env tx = do
  let txCbor = C.serialiseToCBOR tx
  CMTypes.maestroSubmitResult
    <$> if turboFlag
      then
        Maestro.turboSubmitAndMonitorTx env txCbor
      else
        Maestro.submitAndMonitorTx env txCbor

-- | Resolve a set of TxIns to UTxO (Maestro lacks direct TxIn lookup)
getUtxoByTxIn :: Env.MaestroEnv 'Env.V1 -> Set C.TxIn -> IO (C.UTxO C.ConwayEra)
getUtxoByTxIn _env txIns = do
  -- get the outputs by references
  let outputRefs = map CMTypes.toMaestroTxIn (Set.toList txIns)
  _outputs <- concatMap M.paginatedUtxoData <$> S.toList_ (pagedStreamMaestro (flip (outputsByReferences _env Nothing (Just True)) outputRefs))

  -- Silently discards the tx outs which had decoding errors.
  let couts :: [(C.TxIn, C.TxOut C.CtxUTxO C.ConwayEra)] = mapMaybe (either (const Nothing) Just . CMTypes.toCardanoApiUTxO) _outputs

  pure (C.UTxO $ Map.fromList couts)

-- | Get protocol parameters (timestamped) and convert to ledger params
getProtocolParams :: Env.MaestroEnv 'Env.V1 -> IO (C.LedgerProtocolParameters C.ConwayEra)
getProtocolParams env = do
  protocolParams <- Maestro.getTimestampedData <$> Maestro.General.getProtocolParameters env
  pure $ C.LedgerProtocolParameters $ CMTypes.toCardanoApiProtocolParams protocolParams

-- | Look up the stake rewards and delegation targets
getStakeAddresses :: Env.MaestroEnv 'Env.V1 -> Set C.StakeCredential -> IO (Map C.StakeAddress C.Quantity, Map C.StakeAddress C.PoolId)
getStakeAddresses env credentials = do
  entries <-
    traverse (\cred -> C.StakeAddress <$> fmap C.toShelleyNetwork (getNetworkId env) <*> pure (C.toShelleyStakeCredential cred)) (Set.toList credentials)
      >>= traverse (\r -> (r,) <$> getStakeRewardsSingle env r)
  pure
    ( Map.fromList $ fmap (second fst) entries
    , Map.fromList $ mapMaybe (traverse snd) entries
    )

getStakeRewardsSingle :: Env.MaestroEnv 'Env.V1 -> C.StakeAddress -> IO (C.Quantity, Maybe C.PoolId)
getStakeRewardsSingle env addr = do
  stakeRewardsForAddress env addr

stakeRewardsForAddress :: Env.MaestroEnv 'Env.V1 -> C.StakeAddress -> IO (C.Quantity, Maybe C.PoolId)
stakeRewardsForAddress env addr = do
  Maestro.AccountInfo{Maestro.accountInfoDelegatedPool, Maestro.accountInfoRewardsAvailable} <- Maestro.getTimestampedData <$> Maestro.accountInfo env (CMTypes.toMaestroStakeAddress addr)
  pure
    ( C.lovelaceToQuantity $ L.Coin (fromIntegral accountInfoRewardsAvailable)
    , fmap CMTypes.poolId accountInfoDelegatedPool
    )

-- | Stake pools
getStakePools :: Env.MaestroEnv 'Env.V1 -> IO (Set C.PoolId)
getStakePools env = do
  Set.fromList . map (CMTypes.poolId . M.poolListInfoPoolIdBech32) . concatMap M.paginatedPoolListInfoData <$> S.toList_ (pagedStreamMaestro (Maestro.Pools.listPools env))

-- Get the DRep votes for a set of stake credentials.
-- TODO: Maestro doesn't support this yet.
getStakeVoteDelegatees :: Env.MaestroEnv 'Env.V1 -> Set C.StakeCredential -> IO (Map C.StakeCredential Ledger.DRep)
getStakeVoteDelegatees _env _ = pure mempty

-- | System start
getSystemStart :: Env.MaestroEnv 'Env.V1 -> IO SystemStart
getSystemStart env = do
  CMTypes.toCardanoApiSystemStart . Maestro.getTimestampedData <$> Maestro.General.getSystemStart env

-- | Era history
getEraHistory :: Env.MaestroEnv 'Env.V1 -> IO C.EraHistory
getEraHistory env = do
  eraSumms' <- Maestro.getTimestampedData <$> Maestro.General.getEraHistory env
  let eraSumms = map CMTypes.toLedgerEraSummary eraSumms'
  pure $
    C.EraHistory $
      Qry.mkInterpreter $
        Summary.Summary $
          fromJust (error "getEraHistory: Unexpected number of entries") $
            NonEmpty.nonEmptyFromList @(CardanoEras StandardCrypto) eraSumms

-- | Current slotNo, slot length, and UTC begin time
getSlotNo :: Env.MaestroEnv 'Env.V1 -> IO (C.SlotNo, SlotLength, UTCTime)
getSlotNo env = do
  (eraHistory_@(C.EraHistory interpreter), systemStart) <- (,) <$> getEraHistory env <*> getSystemStart env
  currentSlot <- CMTypes.toCardanoApiSlotNo . Maestro.chainTipSlot . Maestro.getTimestampedData <$> Maestro.General.getChainTip env
  let utctime = either (error . (<>) "getSlotNo: slotToUtcTime failed " . show) id (slotToUtcTime eraHistory_ systemStart currentSlot)
      l = either (error . (<>) "getSlotNo: slotToSlotLength failed " . show) id (Qry.interpretQuery interpreter $ Qry.slotToSlotLength currentSlot)
  pure (currentSlot, l, utctime)

-- | Network id
getNetworkId :: Env.MaestroEnv 'Env.V1 -> IO C.NetworkId
getNetworkId Env.MaestroEnv{Env.maeClientEnv} = do
  let bu = Servant.baseUrl maeClientEnv
      haystack = Servant.baseUrlHost bu <> Servant.baseUrlPath bu
  pure $
    if "mainnet" `isInfixOf` haystack
      then C.Mainnet
      else
        if "preprod" `isInfixOf` haystack
          then C.Testnet (C.NetworkMagic 1)
          else
            if "preview" `isInfixOf` haystack
              then C.Testnet (C.NetworkMagic 2)
              else error ("Unknown Maestro base URL: " <> haystack)

-- | Stream a paginated Maestro endpoint by repeatedly following the cursor
pagedStreamMaestro :: forall a. (M.HasCursor a) => (Cursor -> IO a) -> Stream (Of a) IO ()
pagedStreamMaestro action = go Nothing
 where
  go mCur = do
    page <- lift (action Cursor{resultPerPage = maxResultsPerPage, cursor = mCur})
    S.yield page
    case M.getNextCursor page of
      Nothing -> pure ()
      Just next -> go (Just next)
