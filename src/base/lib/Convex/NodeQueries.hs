{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-| Conveniences for working with a local @cardano-node@
-}
module Convex.NodeQueries(
  loadConnectInfo,
  queryEraHistory,
  querySystemStart,
  queryLocalState,
  queryTip
) where

import           Cardano.Api                                        (CardanoMode,
                                                                     ChainPoint,
                                                                     ConsensusModeParams (..),
                                                                     Env (..),
                                                                     EpochSlots (..),
                                                                     EraHistory,
                                                                     InitialLedgerStateError,
                                                                     LocalNodeConnectInfo (..),
                                                                     NetworkId (Mainnet, Testnet),
                                                                     NetworkMagic (..),
                                                                     SystemStart,
                                                                     envSecurityParam)
import qualified Cardano.Api                                        as CAPI
import qualified Cardano.Chain.Genesis
import           Cardano.Crypto                                     (RequiresNetworkMagic (..),
                                                                     getProtocolMagic)
import           Control.Monad.Except                               (MonadError,
                                                                     throwError)
import           Control.Monad.IO.Class                             (MonadIO (..))
import           Control.Monad.Trans.Except                         (runExceptT)
import           Data.SOP.Strict                                    (NP ((:*)))
import qualified Ouroboros.Consensus.Cardano.CanHardFork            as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator            as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics     as HFC

{-| Load the node config file and create 'LocalNodeConnectInfo' and 'Env' values that can be used to talk to the node.
-}
loadConnectInfo ::
  (MonadError InitialLedgerStateError m, MonadIO m)
  => FilePath
  -- ^ Node config file (JSON)
  -> FilePath
  -- ^ Node socket
  -> m (LocalNodeConnectInfo CardanoMode, Env)
loadConnectInfo nodeConfigFilePath socketPath = do
  (env, _) <- liftIO (runExceptT (CAPI.initialLedgerState nodeConfigFilePath)) >>= either throwError pure

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

      cardanoModeParams = CardanoModeParams . EpochSlots $ 10 * envSecurityParam env

  -- Connect to the node.
  let connectInfo :: LocalNodeConnectInfo CardanoMode
      connectInfo =
          LocalNodeConnectInfo {
            localConsensusModeParams = cardanoModeParams,
            localNodeNetworkId       = networkId,
            localNodeSocketPath      = socketPath
          }
  pure (connectInfo, env)

querySystemStart :: LocalNodeConnectInfo CardanoMode -> IO SystemStart
querySystemStart = queryLocalState CAPI.QuerySystemStart

queryEraHistory :: LocalNodeConnectInfo CardanoMode -> IO (EraHistory CardanoMode)
queryEraHistory = queryLocalState (CAPI.QueryEraHistory CAPI.CardanoModeIsMultiEra)

queryTip :: LocalNodeConnectInfo CardanoMode -> IO ChainPoint
queryTip = queryLocalState (CAPI.QueryChainPoint CAPI.CardanoMode)

queryLocalState :: CAPI.QueryInMode CardanoMode b -> LocalNodeConnectInfo CardanoMode -> IO b
queryLocalState query connectInfo = do
  CAPI.queryNodeLocalState connectInfo Nothing query >>= \case
    Left err -> do
      fail ("queryLocalState: Failed with " <> show err)
    Right result -> pure result
