{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Conveniences for working with a local @cardano-node@
-}
module Convex.NodeQueries(
  -- * Local node connect info
  loadConnectInfo,
  localNodeConnectInfo,

  -- * Queries
  queryEraHistory,
  tryQueryEraHistory,
  querySystemStart,
  tryQuerySystemStart,
  queryTip,
  tryQueryTip,
  queryProtocolParameters,
  tryQueryProtocolParameters,
  queryTipBlock,
  tryQueryTipBlock,
  queryEra,
  tryQueryEra,
  queryStakePools,
  tryQueryStakePools,
  queryStakeAddresses,
  tryQueryStakeAddresses
) where


import           Cardano.Api                                        (BlockNo,
                                                                     ConsensusModeParams (..),
                                                                     Env,
                                                                     EpochSlots (..),
                                                                     EraHistory (..),
                                                                     InitialLedgerStateError,
                                                                     LocalNodeConnectInfo (..),
                                                                     NetworkId (..),
                                                                     NetworkMagic (..),
                                                                     Quantity (..),
                                                                     StakeAddress,
                                                                     StakeCredential,
                                                                     envLedgerConfig,
                                                                     envSecurityParam)
import qualified Cardano.Api                                        as C
import           Cardano.Api.Shelley                                (LedgerProtocolParameters (..),
                                                                     PoolId)
import qualified Cardano.Chain.Genesis
import           Cardano.Crypto                                     (RequiresNetworkMagic (..),
                                                                     getProtocolMagic)
import           Cardano.Ledger.Coin                                (Coin (..))
import           Cardano.Slotting.Slot                              (WithOrigin)
import           Cardano.Slotting.Time                              (SystemStart)
import           Control.Exception                                  (Exception,
                                                                     throw)
import           Control.Monad.Error.Class                          (MonadError (..),
                                                                     throwError)
import           Control.Monad.Except                               (ExceptT,
                                                                     runExceptT)
import           Control.Monad.IO.Class                             (MonadIO (..))
import           Control.Monad.Reader                               (ReaderT,
                                                                     runReaderT)
import           Control.Monad.Reader.Class                         (MonadReader,
                                                                     ask)
import           Convex.Eras                                        (InAnyBabbageEraOnwards (..))
import qualified Convex.Eras                                        as Eras
import           Convex.MonadLog                                    (MonadLog (..),
                                                                     MonadLogIgnoreT (..),
                                                                     logWarnS)
import           Data.Bifunctor                                     (Bifunctor (..))
import           Data.Map                                           (Map)
import           Data.Set                                           (Set)
import           Data.SOP.Strict                                    (NP ((:*)))
import           Data.Word                                          (Word64)
import qualified Ouroboros.Consensus.Cardano.CanHardFork            as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator            as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics     as HFC
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type    as T

{-| Load the node config file and create 'LocalNodeConnectInfo' and 'Env' values that can be used to talk to the node.
-}
loadConnectInfo ::
  (MonadError InitialLedgerStateError m, MonadIO m)
  => FilePath
  -- ^ Node config file (JSON)
  -> FilePath
  -- ^ Node socket
  -> m (LocalNodeConnectInfo, Env)
loadConnectInfo nodeConfigFilePath socketPath = do
  (env, _) <- liftIO (runExceptT (C.initialLedgerState (C.File nodeConfigFilePath))) >>= either throwError pure

  -- Derive the NetworkId as described in network-magic.md from the
  -- cardano-ledger-specs repo.
  let byronConfig
        = (\(Consensus.WrapPartialLedgerConfig (Consensus.ByronPartialLedgerConfig bc _) :* _) -> bc)
        . HFC.getPerEraLedgerConfig
        . HFC.hardForkLedgerConfigPerEra
        $ envLedgerConfig env

      networkMagic
        = getProtocolMagic
        $ Cardano.Chain.Genesis.configProtocolMagic byronConfig

      networkId = case Cardano.Chain.Genesis.configReqNetMagic byronConfig of
        RequiresNoMagic -> Mainnet
        RequiresMagic   -> Testnet (NetworkMagic networkMagic)

      cardanoModeParams_ = CardanoModeParams . EpochSlots $ 10 * envSecurityParam env

  -- Connect to the node.
  let connectInfo :: LocalNodeConnectInfo
      connectInfo =
          LocalNodeConnectInfo {
            localConsensusModeParams = cardanoModeParams_,
            localNodeNetworkId       = networkId,
            localNodeSocketPath      = C.File socketPath
          }
  pure (connectInfo, env)

{-| 'C.LocalNodeConnectInfo' with default values for consensus mode params.
Use 'loadConnectInfo' to load the consensus mode params from the node config.
-}
localNodeConnectInfo :: NetworkId -> FilePath -> C.LocalNodeConnectInfo
localNodeConnectInfo localNodeNetworkId (C.File -> localNodeSocketPath) =
  C.LocalNodeConnectInfo
    { localConsensusModeParams = cardanoModeParams
    , localNodeNetworkId
    , localNodeSocketPath
    }

cardanoModeParams :: C.ConsensusModeParams
cardanoModeParams = C.CardanoModeParams $ C.EpochSlots defaultByronEpochSlots
 where
  -- NOTE(AB): extracted from Parsers in cardano-cli, this is needed to run in 'cardanoMode' which
  -- is the default for cardano-cli
  defaultByronEpochSlots = 21600 :: Word64

data QueryError
  = QueryAcquireError String
  | QueryEraMismatchError EraMismatch
  | QueryUnsupportedEra C.AnyCardanoEra
  deriving (Eq, Show)

newtype QueryException = QueryException QueryError
  deriving newtype (Eq, Show)

instance Exception QueryException

-- | Get the 'EraHistory' from the node
queryEraHistory :: (MonadIO m, MonadLog m, MonadReader LocalNodeConnectInfo m, MonadError QueryError m) => m EraHistory
queryEraHistory = runQuery C.QueryEraHistory

-- | Get the 'EraHistory' from the node
tryQueryEraHistory :: LocalNodeConnectInfo -> IO EraHistory
tryQueryEraHistory = tryRunQuery queryEraHistory

-- | Get the 'SystemStart' from the node
querySystemStart :: (MonadIO m, MonadLog m, MonadReader LocalNodeConnectInfo m, MonadError QueryError m) => m SystemStart
querySystemStart = runQuery C.QuerySystemStart

-- | Get the 'SystemStart' from the node
tryQuerySystemStart :: LocalNodeConnectInfo -> IO SystemStart
tryQuerySystemStart = tryRunQuery querySystemStart

queryTipBlock :: (MonadIO m, MonadLog m, MonadReader LocalNodeConnectInfo m, MonadError QueryError m) => m (WithOrigin BlockNo)
queryTipBlock = runQuery C.QueryChainBlockNo

tryQueryTipBlock :: LocalNodeConnectInfo -> IO (WithOrigin BlockNo)
tryQueryTipBlock = tryRunQuery queryTipBlock

queryEra :: (MonadIO m, MonadLog m, MonadReader LocalNodeConnectInfo m, MonadError QueryError m) => m C.AnyCardanoEra
queryEra = runQuery C.QueryCurrentEra

tryQueryEra :: LocalNodeConnectInfo -> IO C.AnyCardanoEra
tryQueryEra = tryRunQuery queryEra

-- | Get the tip from the local cardano node
queryTip :: (MonadIO m, MonadLog m, MonadReader LocalNodeConnectInfo m, MonadError QueryError m) => m C.ChainPoint
queryTip = runQuery C.QueryChainPoint

tryQueryTip :: LocalNodeConnectInfo -> IO C.ChainPoint
tryQueryTip = tryRunQuery queryTip

{-| Query the node for the current era.
Fails with 'UnsupportedEra' if the current era is not one of (Babbage, Conway)
-}
getSupportedEra :: forall m.
  ( MonadIO m
  , MonadReader LocalNodeConnectInfo m
  , MonadError QueryError m
  ) =>
  m (InAnyBabbageEraOnwards C.BabbageEraOnwards)
getSupportedEra = do
  connectInfo <- ask
  era <- liftIO (runExceptT $ C.queryNodeLocalState connectInfo T.VolatileTip C.QueryCurrentEra) >>= either (throwError . QueryAcquireError . show) pure
  case era of
    C.AnyCardanoEra C.BabbageEra -> pure $ Eras.inAnyBabbageEraOnwardsBabbage C.BabbageEraOnwardsBabbage
    C.AnyCardanoEra C.ConwayEra  -> pure $ Eras.inAnyBabbageEraOnwardsConway C.BabbageEraOnwardsConway
    otherEra -> throwError (QueryUnsupportedEra otherEra)

tryRunQuery :: ReaderT LocalNodeConnectInfo (MonadLogIgnoreT (ExceptT QueryError IO)) a -> LocalNodeConnectInfo -> IO a
tryRunQuery action connectInfo = runExceptT (runMonadLogIgnoreT (runReaderT action connectInfo)) >>= \case
  Left err -> throw (QueryException err)
  Right k -> pure k

runQuery :: (MonadIO m, MonadLog m, MonadReader LocalNodeConnectInfo m, MonadError QueryError m) => C.QueryInMode a -> m a
runQuery qry = do
  info <- ask
  result <- liftIO (runExceptT $ C.queryNodeLocalState info T.VolatileTip qry)
  case result of
    Left err -> do
      let msg = "runQuery: Query failed: " <> show err
      logWarnS msg
      throwError $ QueryAcquireError $ show err
    Right result' -> do
      pure result'

{-| An query that returns an era-specific value wrapped in an era-agnostic type
-}
data QueryWithHandler a =
  forall b. QueryWithHandler
    { qryQuery   :: C.QueryInMode (Either EraMismatch b) -- ^ Era-specific query returning value of type b
    , qryHandler :: b -> a -- ^ Era-agnostic return type @a@
    }

runQuery' :: (MonadIO m, MonadLog m, MonadReader LocalNodeConnectInfo m, MonadError QueryError m) => (InAnyBabbageEraOnwards C.BabbageEraOnwards -> QueryWithHandler b) -> m b
runQuery' qry = do
  era <- getSupportedEra
  case qry era of
    QueryWithHandler{qryQuery, qryHandler} ->
      runQuery qryQuery >>= \case
        Left err -> do
          -- *should* not happen as we pass the era to the handler to construct the right query
          -- However, if the HF happens at exactly the moment between "asking for era" and "submitting query"
          -- then we will still fail
          let msg = "runQuery': Era mismatch: " <> show err
          logWarnS msg
          throwError $ QueryEraMismatchError err
        Right result' -> pure $ qryHandler result'

queryProtocolParameters :: (MonadIO m, MonadLog m, MonadReader LocalNodeConnectInfo m, MonadError QueryError m) => m (InAnyBabbageEraOnwards LedgerProtocolParameters)
queryProtocolParameters = runQuery' $ \case
  InAnyBabbageEraOnwards C.BabbageEraOnwardsBabbage C.BabbageEraOnwardsBabbage ->
    QueryWithHandler
      { qryQuery   = C.QueryInEra (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryProtocolParameters)
      , qryHandler = Eras.inAnyBabbageEraOnwardsBabbage . LedgerProtocolParameters
      }
  InAnyBabbageEraOnwards C.BabbageEraOnwardsConway C.BabbageEraOnwardsConway ->
    QueryWithHandler
      { qryQuery   = C.QueryInEra (C.QueryInShelleyBasedEra C.ShelleyBasedEraConway C.QueryProtocolParameters)
      , qryHandler = Eras.inAnyBabbageEraOnwardsConway . LedgerProtocolParameters
      }

tryQueryProtocolParameters :: LocalNodeConnectInfo -> IO (InAnyBabbageEraOnwards LedgerProtocolParameters)
tryQueryProtocolParameters = tryRunQuery queryProtocolParameters

queryStakePools :: (MonadIO m, MonadLog m, MonadReader LocalNodeConnectInfo m, MonadError QueryError m) => m (Set PoolId)
queryStakePools = runQuery' $ \case
  InAnyBabbageEraOnwards C.BabbageEraOnwardsBabbage C.BabbageEraOnwardsBabbage ->
    QueryWithHandler
      { qryQuery   = C.QueryInEra (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryStakePools)
      , qryHandler = id
      }
  InAnyBabbageEraOnwards C.BabbageEraOnwardsConway C.BabbageEraOnwardsConway ->
    QueryWithHandler
      { qryQuery   = C.QueryInEra (C.QueryInShelleyBasedEra C.ShelleyBasedEraConway C.QueryStakePools)
      , qryHandler = id
      }

tryQueryStakePools :: LocalNodeConnectInfo -> IO (Set PoolId)
tryQueryStakePools = tryRunQuery queryStakePools

queryStakeAddresses :: (MonadIO m, MonadLog m, MonadReader LocalNodeConnectInfo m, MonadError QueryError m) => Set StakeCredential -> m (Map StakeAddress Quantity, Map StakeAddress PoolId)
queryStakeAddresses creds = do
  LocalNodeConnectInfo{localNodeNetworkId} <- ask
  let coinToQuantity (Coin i) = Quantity i
  runQuery' $ \case
    InAnyBabbageEraOnwards C.BabbageEraOnwardsBabbage C.BabbageEraOnwardsBabbage ->
      QueryWithHandler
        { qryQuery   = C.QueryInEra (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage (C.QueryStakeAddresses creds localNodeNetworkId))
        , qryHandler = first (fmap coinToQuantity)
        }
    InAnyBabbageEraOnwards C.BabbageEraOnwardsConway C.BabbageEraOnwardsConway ->
      QueryWithHandler
        { qryQuery   = C.QueryInEra (C.QueryInShelleyBasedEra C.ShelleyBasedEraConway (C.QueryStakeAddresses creds localNodeNetworkId))
        , qryHandler = first (fmap coinToQuantity)
        }

tryQueryStakeAddresses :: LocalNodeConnectInfo -> Set StakeCredential -> IO (Map StakeAddress Quantity, Map StakeAddress PoolId)
tryQueryStakeAddresses connectInfo creds = tryRunQuery (queryStakeAddresses creds) connectInfo
