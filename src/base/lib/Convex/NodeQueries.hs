{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Conveniences for working with a local @cardano-node@
module Convex.NodeQueries (
  -- $eras
  loadConnectInfo,
  localNodeConnectInfo,

  -- * Era-independent queries
  queryEra,
  queryEraHistory,
  querySystemStart,
  queryChainPoint,
  queryTipBlock,
  queryTip,
  queryTipSlotNo,

  -- * Era-specific queries
  EraQuery (..),
  QueryException (..),
  TimeException (..),
  queryInSupportedEra,
  runEraQuery,
  queryEpoch,
  queryLocalState,
  queryProtocolParameters,
  queryProtocolParametersUpdate,
  queryStakeAddresses,
  queryStakePools,
  queryStakeVoteDelegatees,
  queryUTxOFilter,
) where

import Cardano.Api qualified as C
import Cardano.Api.Experimental (Era)
import Cardano.Api.Experimental qualified as C.Experimental
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley (
  PoolId,
  StakeAddress,
  StakeCredential,
 )
import Cardano.Chain.Genesis qualified
import Cardano.Crypto (
  RequiresNetworkMagic (..),
  getProtocolMagic,
 )
import Cardano.Ledger.Core (PParams)
import Cardano.Slotting.Slot (WithOrigin)
import Cardano.Slotting.Time (SlotLength)
import Control.Exception (
  Exception,
  throwIO,
 )
import Control.Monad.Except (
  MonadError,
  throwError,
 )
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (runExceptT)
import Convex.Utxos (UtxoSet)
import Convex.Utxos qualified as Utxos
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import Data.SOP.Strict (NP ((:*)))
import Data.Set (Set)
import Data.Word (Word64)
import Ouroboros.Consensus.Cardano.CanHardFork qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras qualified as HFC
import Ouroboros.Consensus.HardFork.Combinator.Basics qualified as HFC
import Ouroboros.Consensus.HardFork.History (
  interpretQuery,
  slotToSlotLength,
 )
import Ouroboros.Consensus.Shelley.Eras (StandardConway)
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as T

{- $eras
This module provides some conviences for querying a running cardano node.
Some queries are /era-specific/: They have an @era@ parameter which must
match the era of the node. If they don't match then we get an 'EraMismatch'
error.
This is fine most of the time as we mainly work with the current era. But
some care must be taken when querying the node around the time a hard-fork
to a new era is scheduled, to make sure that we only issue queries
for the next era after the hard-fork.
To make it easier to implement era-specific queries, this module exports
'queryInSupportedEra'. This function allows us to run queries that return
the same result type both before and after a hard-fork.
We use the 'Cardano.Api.Experimental.Era' type from @cardano-api@ to
distinguish the current era from the next era.
-}

-- | Exceptions thrown while querying the cardano node
data QueryException
  = -- | Failed to connect to the node
    QueryAcquireException String
  | -- | Mismatch between node era and query era
    QueryEraMismatchException EraMismatch
  | -- | Attempting to query the node in an era that is not one of the supported eras
    QueryEraNotSupported C.AnyCardanoEra
  deriving (Eq, Show)

instance Exception QueryException

-- | Exceptions thrown while converting between slot time and clock time
data TimeException
  = TimePastHorizonException Consensus.PastHorizonException
  | ChainPointAtGenesisFailure
  deriving stock (Show)

instance Exception TimeException

-- | Load the node config file and create 'LocalNodeConnectInfo' and 'Env' values that can be used to talk to the node.
loadConnectInfo
  :: (MonadError C.InitialLedgerStateError m, MonadIO m)
  => FilePath
  -- ^ Node config file (JSON)
  -> FilePath
  -- ^ Node socket
  -> m (C.LocalNodeConnectInfo, C.Env)
loadConnectInfo nodeConfigFilePath socketPath = do
  (env, _) <- liftIO (runExceptT (C.initialLedgerState (C.File nodeConfigFilePath))) >>= either throwError pure

  -- Derive the NetworkId as described in network-magic.md from the
  -- cardano-ledger-specs repo.
  let byronConfig =
        (\(Consensus.WrapPartialLedgerConfig (Consensus.ByronPartialLedgerConfig bc _) :* _) -> bc)
          . HFC.getPerEraLedgerConfig
          . HFC.hardForkLedgerConfigPerEra
          $ C.envLedgerConfig env

      networkMagic =
        getProtocolMagic $
          Cardano.Chain.Genesis.configProtocolMagic byronConfig

      networkId = case Cardano.Chain.Genesis.configReqNetMagic byronConfig of
        RequiresNoMagic -> C.Mainnet
        RequiresMagic -> C.Testnet (C.NetworkMagic networkMagic)

      localConsensusModeParams = C.CardanoModeParams . C.EpochSlots $ 10 * C.envSecurityParam env

  -- Connect to the node.
  let connectInfo :: C.LocalNodeConnectInfo
      connectInfo =
        C.LocalNodeConnectInfo
          { C.localConsensusModeParams
          , C.localNodeNetworkId = networkId
          , C.localNodeSocketPath = C.File socketPath
          }
  pure (connectInfo, env)

{- | 'LocalNodeConnectInfo' for a network ID and a socket path,
    assuming default values for the 'C.ConsensusModeParams'.
    Cf. 'loadConnectInfo' which constructs the 'LocalNodeConnectInfo'
    based on a node configuration file
-}
localNodeConnectInfo :: C.NetworkId -> FilePath -> C.LocalNodeConnectInfo
localNodeConnectInfo localNodeNetworkId (C.File -> localNodeSocketPath) =
  C.LocalNodeConnectInfo
    { C.localConsensusModeParams = cardanoModeParams
    , C.localNodeNetworkId
    , C.localNodeSocketPath
    }

cardanoModeParams :: C.ConsensusModeParams
cardanoModeParams = C.CardanoModeParams $ C.EpochSlots defaultByronEpochSlots
 where
  -- NOTE(AB): extracted from Parsers in cardano-cli, this is needed to run in 'cardanoMode' which
  -- is the default for cardano-cli
  defaultByronEpochSlots = 21600 :: Word64

-- | Get the system start from the local cardano node
querySystemStart :: C.LocalNodeConnectInfo -> IO C.SystemStart
querySystemStart = queryLocalState C.QuerySystemStart

-- | Get the era history from the local cardano node
queryEraHistory :: C.LocalNodeConnectInfo -> IO C.EraHistory
queryEraHistory = queryLocalState C.QueryEraHistory

-- | Get the chain point from the local cardano node
queryChainPoint :: C.LocalNodeConnectInfo -> IO C.ChainPoint
queryChainPoint = queryLocalState C.QueryChainPoint

{- | Get the tip (slot no and block hash) as well as the length of the current slot
  Throws at least 'TimeException' if the time conversion fails and 'QueryException'
  if the node query fails.
-}
queryTip :: C.LocalNodeConnectInfo -> IO (C.SlotNo, SlotLength, C.Hash C.BlockHeader)
queryTip connectInfo =
  queryChainPoint connectInfo >>= \case
    C.ChainPointAtGenesis -> throwIO ChainPointAtGenesisFailure
    C.ChainPoint slot hsh -> do
      sl <- querySlotLength connectInfo slot
      pure (slot, sl, hsh)

{- | Get the tip (slot no and block hash)
  Throws at least 'TimeException' if the time conversion fails and 'QueryException'
  if the node query fails.
-}
queryTipSlotNo :: C.LocalNodeConnectInfo -> IO (C.SlotNo, SlotLength)
queryTipSlotNo = fmap (\(s, l, _) -> (s, l)) . queryTip

querySlotLength :: C.LocalNodeConnectInfo -> C.SlotNo -> IO SlotLength
querySlotLength connectInfo slotNo = do
  C.EraHistory interpreter <- queryEraHistory connectInfo
  case interpretQuery interpreter (slotToSlotLength slotNo) of
    Left err -> throwIO $ TimePastHorizonException err
    Right slength -> pure slength

-- | Get the block number from the local cardano node
queryTipBlock :: C.LocalNodeConnectInfo -> IO (WithOrigin C.BlockNo)
queryTipBlock = queryLocalState C.QueryChainBlockNo

-- | Get the node's era from the local cardano node
queryEra :: C.LocalNodeConnectInfo -> IO C.AnyCardanoEra
queryEra = queryLocalState C.QueryCurrentEra

{- | Run a local state query on the local cardano node, using the volatile tip
  Throws 'QueryException' if connection to the node cannot be acquired
-}
queryLocalState :: C.QueryInMode b -> C.LocalNodeConnectInfo -> IO b
queryLocalState query connectInfo = do
  runExceptT (C.queryNodeLocalState connectInfo T.VolatileTip query) >>= \case
    Left err -> do
      throwIO $ QueryAcquireException $ show err
    Right result -> pure result

-- TODO: Add missing queries from Convex.Devnet.NodeQueries

-- | Era-specific query with an era-independent result
data EraQuery era result
  = forall eraResult. EraQuery
  { eqQuery :: C.QueryInShelleyBasedEra era eraResult
  , eqResult :: eraResult -> result
  }

-- | Run an 'EraQuery', throwing 'QueryException' if the node's era does not match the query era
runEraQuery :: (C.IsShelleyBasedEra era) => C.LocalNodeConnectInfo -> EraQuery era result -> IO result
runEraQuery connectInfo EraQuery{eqQuery, eqResult} =
  queryLocalState (C.QueryInEra $ C.QueryInShelleyBasedEra C.shelleyBasedEra eqQuery) connectInfo >>= \case
    Left eraMismatch -> throwIO (QueryEraMismatchException eraMismatch)
    Right x -> pure (eqResult x)

{- | Query the node in one of the supported eras.
  Throws 'QueryException' if connection to the node cannot be acquired,
  if the node's era is not one of the supported eras, or if the node's
  era changes between us asking for the era and sending the era-specific query
-}
queryInSupportedEra :: C.LocalNodeConnectInfo -> (forall era. Era era -> EraQuery era result) -> IO result
queryInSupportedEra connectInfo qry = do
  queryEra connectInfo >>= \case
    C.AnyCardanoEra C.BabbageEra -> runEraQuery connectInfo (qry C.Experimental.BabbageEra)
    C.AnyCardanoEra C.ConwayEra -> runEraQuery connectInfo (qry C.Experimental.ConwayEra)
    era -> throwIO (QueryEraNotSupported era)

{- | Get the conway protocol parameters from the local cardano node
  Throws 'QueryException' if the node's era is not conway or if the connection
  to the node cannot be acquired
-}
queryProtocolParameters :: C.LocalNodeConnectInfo -> IO (PParams StandardConway)
queryProtocolParameters connectInfo =
  runEraQuery connectInfo $
    EraQuery{eqQuery = C.QueryProtocolParameters, eqResult = id}

{- | Get all the protocol parameter updates from the local cardano node
  Throws 'QueryException' if the node's era is not conway or if the connection
  to the node cannot be acquired
-}
queryProtocolParametersUpdate :: C.LocalNodeConnectInfo -> IO (Map (C.Hash C.GenesisKey) C.ProtocolParametersUpdate)
queryProtocolParametersUpdate connectInfo =
  runEraQuery @C.ConwayEra connectInfo $
    EraQuery{eqQuery = C.QueryProtocolParametersUpdate, eqResult = id}

{- | Get the stake and the IDs of the stake pool for a set of stake credentials
  Throws 'QueryException' if the node's era is not supported or if the connection
  to the node cannot be acquired
-}
queryStakeAddresses :: C.LocalNodeConnectInfo -> Set StakeCredential -> IO (Map StakeAddress C.Quantity, Map StakeAddress PoolId)
queryStakeAddresses info creds = do
  let C.LocalNodeConnectInfo{C.localNodeNetworkId} = info
  queryInSupportedEra info $ \case
    C.Experimental.BabbageEra -> EraQuery{eqQuery = C.QueryStakeAddresses creds localNodeNetworkId, eqResult = first (fmap C.lovelaceToQuantity)}
    C.Experimental.ConwayEra -> EraQuery{eqQuery = C.QueryStakeAddresses creds localNodeNetworkId, eqResult = first (fmap C.lovelaceToQuantity)}

{- | Get the set of registered stake pools
  Throws 'QueryException' if the node's era is not supported or if the connection
  to the node cannot be acquired
-}
queryStakePools :: C.LocalNodeConnectInfo -> IO (Set PoolId)
queryStakePools connectInfo = queryInSupportedEra connectInfo $ \case
  C.Experimental.BabbageEra -> EraQuery{eqQuery = C.QueryStakePools, eqResult = id}
  C.Experimental.ConwayEra -> EraQuery{eqQuery = C.QueryStakePools, eqResult = id}

{- | Get the delegatees for a set of stake credentials (only works starting at ConwayEra).
  Throws 'QueryException' if the node's era is not supported or if the connection
  to the node cannot be acquired.
-}
queryStakeVoteDelegatees :: C.LocalNodeConnectInfo -> Set StakeCredential -> IO (Map StakeCredential (Ledger.DRep Ledger.StandardCrypto))
queryStakeVoteDelegatees info creds = do
  queryInSupportedEra info $ \case
    C.Experimental.BabbageEra -> EraQuery{eqQuery = C.QueryStakeVoteDelegatees creds, eqResult = id}
    C.Experimental.ConwayEra -> EraQuery{eqQuery = C.QueryStakeVoteDelegatees creds, eqResult = id}

{- | Get the current epoch
  Throws 'QueryException' if the node's era is not supported or if the connection
  to the node cannot be acquired
-}
queryEpoch :: C.LocalNodeConnectInfo -> IO C.EpochNo
queryEpoch connectInfo = queryInSupportedEra connectInfo $ \case
  C.Experimental.BabbageEra -> EraQuery{eqQuery = C.QueryEpoch, eqResult = id}
  C.Experimental.ConwayEra -> EraQuery{eqQuery = C.QueryEpoch, eqResult = id}

{- | Query UTxO for all given addresses at given point.
  Throws 'QueryException' if the node's era is not supported or if the connection
  to the node cannot be acquired
-}
queryUTxOFilter :: C.LocalNodeConnectInfo -> C.QueryUTxOFilter -> IO (UtxoSet C.CtxUTxO ())
queryUTxOFilter connectInfo flt = queryInSupportedEra connectInfo $ \case
  C.Experimental.BabbageEra -> EraQuery{eqQuery = C.QueryUTxO flt, eqResult = Utxos.fromApiUtxo}
  C.Experimental.ConwayEra -> EraQuery{eqQuery = C.QueryUTxO flt, eqResult = Utxos.fromApiUtxo}
