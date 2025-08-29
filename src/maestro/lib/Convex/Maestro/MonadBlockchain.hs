{-# LANGUAGE DataKinds #-}

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
import Cardano.Api.Shelley qualified as C
import Cardano.Slotting.Time (SlotLength, SystemStart)
import Control.Monad.Trans.Class (lift)
import Convex.Maestro.Types qualified as CMTypes
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime)
import Maestro.Client.Env qualified as Env
import Maestro.Client.V1.Addresses ()
import Maestro.Client.V1.Core.Pagination
import Maestro.Client.V1.General qualified as Maestro.General
import Maestro.Client.V1.Pools qualified as Maestro.Pools
import Maestro.Client.V1.Transactions (outputsByReferences)
import Maestro.Types.V1 qualified as M
import Streaming.Prelude (Of, Stream)
import Streaming.Prelude qualified as S

-- TODO: Convert Maestro types to cardano-api types in Convex.Maestro.Types and use them here

-- | Submit a transaction (not yet supported by Maestro SDK)
sendTxMaestro :: Env.MaestroEnv 'Env.V1 -> C.Tx C.ConwayEra -> IO (Either e C.TxId)
sendTxMaestro _env _tx = pure (Left (error "Maestro: sendTx not implemented"))

-- | Resolve a set of TxIns to UTxO (Maestro lacks direct TxIn lookup)
getUtxoByTxIn :: Env.MaestroEnv 'Env.V1 -> Set C.TxIn -> IO (C.UTxO C.ConwayEra)
getUtxoByTxIn _env txIns = do
  -- get the outputs by references
  let outputRefs = map CMTypes.toMaestroTxIn (Set.toList txIns)
  _outputs <- concatMap M.paginatedUtxoData <$> S.toList_ (pagedStreamMaestro (flip (outputsByReferences _env Nothing (Just True)) outputRefs))

  -- filter the outputs by the txIns
  -- convert the outputs to utxos
  -- return the utxos
  pure (C.UTxO Map.empty)

-- | Get protocol parameters (timestamped) and convert to ledger params
getProtocolParams :: Env.MaestroEnv 'Env.V1 -> IO (C.LedgerProtocolParameters C.ConwayEra)
getProtocolParams env = do
  _ts <- Maestro.General.getProtocolParameters env
  -- TODO: Convert
  fail "Maestro: getProtocolParams not implemented"

-- | Stake addresses: rewards and pools (not in SDK yet)
getStakeAddresses :: Env.MaestroEnv 'Env.V1 -> Set C.StakeCredential -> IO (Map C.StakeAddress C.Quantity, Map C.StakeAddress C.PoolId)
getStakeAddresses _env _creds = pure (Map.empty, Map.empty)

-- | Stake pools
getStakePools :: Env.MaestroEnv 'Env.V1 -> IO (Set C.PoolId)
getStakePools env = do
  -- Stream pages via cursor (Maestro pagination)
  Set.fromList . map (CMTypes.poolId . M.poolListInfoPoolIdBech32) . concatMap M.paginatedPoolListInfoData <$> S.toList_ (pagedStreamMaestro (Maestro.Pools.listPools env))

-- | Stake vote delegatees (not in SDK yet)
getStakeVoteDelegatees :: Env.MaestroEnv 'Env.V1 -> Set C.StakeCredential -> IO (Map C.StakeCredential Ledger.DRep)
getStakeVoteDelegatees _env _ = pure mempty

-- | System start
getSystemStart :: Env.MaestroEnv 'Env.V1 -> IO SystemStart
getSystemStart env = do
  _ts <- Maestro.General.getSystemStart env
  -- TODO: Convert
  fail "Maestro: getSystemStart not implemented"

-- | Era history
getEraHistory :: Env.MaestroEnv 'Env.V1 -> IO C.EraHistory
getEraHistory env = do
  _ts <- Maestro.General.getEraHistory env
  -- TODO: Convert
  fail "Maestro: getEraHistory not implemented"

-- | Current slotNo, slot length, and UTC begin time
getSlotNo :: Env.MaestroEnv 'Env.V1 -> IO (C.SlotNo, SlotLength, UTCTime)
getSlotNo env = do
  _tip <- Maestro.General.getChainTip env
  -- TODO: Extract from ChainTip
  fail "Maestro: getSlotNo not implemented"

-- | Network id
getNetworkId :: Env.MaestroEnv 'Env.V1 -> IO C.NetworkId
getNetworkId env = do
  _tip <- Maestro.General.getChainTip env
  -- TODO: Extract
  fail "Maestro: getNetworkId not implemented"

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
