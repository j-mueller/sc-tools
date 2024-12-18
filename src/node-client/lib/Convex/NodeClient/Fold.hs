{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{- | A node client that applies a fold to the stream of blocks.
Unlike 'foldBlocks' from 'Cardano.Api', this one supports rollbacks.
-}
module Convex.NodeClient.Fold (
  LedgerStateArgs (..),
  LedgerStateUpdate (..),
  LedgerStateMode (..),
  CatchingUp (..),
  catchingUpWithNode,
  caughtUpWithNode,
  resumingFrom,
  catchingUp,
  caughtUp,
  getClientPoint,
  shouldLog,
  foldClient,
  foldClient',
) where

import Cardano.Api (
  BlockHeader (..),
  BlockInMode (..),
  BlockNo (..),
  ChainPoint (..),
  ChainTip (..),
  Env,
  LedgerState (..),
  SlotNo,
  ValidationMode (..),
  applyBlock,
  chainTipToChainPoint,
  envSecurityParam,
 )
import Cardano.Api qualified as CAPI
import Cardano.Api.Shelley qualified as CAPI
import Cardano.Slotting.Slot (WithOrigin (At))
import Convex.NodeClient.ChainTip (
  JSONBlockNo (..),
  JSONChainPoint (..),
  JSONChainTip (..),
  blockHeaderPoint,
 )
import Convex.NodeClient.Resuming (ResumingFrom)
import Convex.NodeClient.Resuming qualified as R
import Convex.NodeClient.Types (
  PipelinedLedgerStateClient (..),
  fromChainTip,
 )
import Data.Aeson (
  FromJSON,
  ToJSON,
 )
import Data.Functor ((<&>))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHC.Generics (Generic)
import Network.TypedProtocol.Pipelined (Nat (..))
import Ouroboros.Consensus.Block.Abstract (WithOrigin (..))
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined (
  ClientPipelinedStIdle (..),
  ClientStNext (..),
 )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined qualified as CSP
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision (
  PipelineDecision (..),
  pipelineDecisionMax,
 )

-- | Whether to keep track of the full ledger state on the client
data LedgerStateMode = FullLedgerState | NoLedgerState

-- | Whether we have the initial ledger state on the client
data LedgerStateArgs mode where
  NoLedgerStateArgs :: LedgerStateArgs 'NoLedgerState
  LedgerStateArgs :: LedgerState -> ValidationMode -> LedgerStateArgs 'FullLedgerState

-- | Whether we have the current ledger state for the client folding function
data LedgerStateUpdate mode where
  NoLedgerStateUpdate :: LedgerStateUpdate 'NoLedgerState
  LedgerStateUpdate :: LedgerState -> [CAPI.LedgerEvent] -> LedgerStateUpdate 'FullLedgerState

-- | A history of the last @k@ states
type History mode a = Seq (SlotNo, LedgerStateUpdate mode, a)

-- | Whether we have fully caught up with the node
data CatchingUp
  = -- | Client is still catching up
    CatchingUpWithNode {clientPoint :: JSONChainPoint, clientBlockNo :: Maybe JSONBlockNo, serverTip :: Maybe JSONChainPoint}
  | -- | Client fully caught up (client tip == server tip)
    CaughtUpWithNode {tip :: JSONChainTip}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

getClientPoint :: CatchingUp -> JSONChainPoint
getClientPoint = \case
  CatchingUpWithNode{clientPoint} -> clientPoint
  CaughtUpWithNode{tip} -> JSONChainPoint $ chainTipToChainPoint $ unJSONChainTip tip

catchingUpWithNode :: ChainPoint -> Maybe BlockNo -> Maybe ChainPoint -> CatchingUp
catchingUpWithNode (JSONChainPoint -> clientPoint) (fmap JSONBlockNo -> clientBlockNo) (fmap JSONChainPoint -> serverTip) =
  CatchingUpWithNode{clientPoint, serverTip, clientBlockNo}

caughtUpWithNode :: ChainTip -> CatchingUp
caughtUpWithNode (JSONChainTip -> tip) = CaughtUpWithNode{tip}

catchingUp :: CatchingUp -> Bool
catchingUp = \case
  CatchingUpWithNode{} -> True
  CaughtUpWithNode{} -> False

{- | Whether it is a good time to log something. Returns true if

* The client has fully caught up with the node, OR
* The client is catching up and the block number is a multiple of 10.000
-}
shouldLog :: CatchingUp -> Bool
shouldLog = \case
  CaughtUpWithNode{} -> True
  CatchingUpWithNode _ (Just (unJSONBlockNo -> BlockNo n)) _ -> n `mod` 10_000 == 0
  CatchingUpWithNode{} -> False

caughtUp :: CatchingUp -> Bool
caughtUp = not . catchingUp

resumingFrom :: ResumingFrom -> CatchingUp
resumingFrom = \case
  R.ResumingFromChainPoint cp st ->
    catchingUpWithNode cp Nothing (Just $ chainTipToChainPoint st)
  R.ResumingFromOrigin st ->
    catchingUpWithNode ChainPointAtGenesis Nothing (Just $ chainTipToChainPoint st)

-- | Run the client until 'Nothing' is returned
foldClient
  :: forall mode s
   . s
  -- ^ Initial state
  -> LedgerStateArgs mode
  -- ^ Initial ledger state arguments
  -> Env
  -- ^ Node connection data
  -> (CatchingUp -> s -> LedgerStateUpdate mode -> BlockInMode -> IO (Maybe s))
  -- ^ Fold
  -> PipelinedLedgerStateClient
foldClient initialState initialLedgerState env accumulate =
  foldClient' @mode @s @()
    initialState
    initialLedgerState
    env
    (\_ _ !s -> pure ((), s))
    (\c !s !args -> fmap (fmap pure) . accumulate c s args)

-- | A variant of 'foldClient' with more detailed control over rollbacks.
foldClient'
  :: forall mode s w
   . (Monoid w)
  => s
  -- ^ Initial state
  -> LedgerStateArgs mode
  -- ^ ^ Initial ledger state arguments
  -> Env
  -- ^ Node connection data
  -> (ChainPoint -> w -> s -> IO (w, s))
  -- ^ Rollback
  -> (CatchingUp -> s -> LedgerStateUpdate mode -> BlockInMode -> IO (Maybe (w, s)))
  -- ^ Fold
  -> PipelinedLedgerStateClient
foldClient' initialState ledgerStateArgs env applyRollback accumulate = PipelinedLedgerStateClient $ CSP.ChainSyncClientPipelined $ do
  -- NB: The code below was adapted from https://input-output-hk.github.io/cardano-node/cardano-api/src/Cardano.Api.LedgerState.html#foldBlocks

  let
    pipelineSize = 10 -- TODO: Configurable
    initialHistory = initialStateHistory ledgerStateArgs (mempty, initialState)

    clientIdle_RequestMoreN
      :: forall n
       . WithOrigin BlockNo
      -> WithOrigin BlockNo
      -> Nat n -- Number of requests inflight.
      -> History mode (w, s)
      -> CSP.ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ()
    clientIdle_RequestMoreN clientTip_ serverTip_ n history =
      case pipelineDecisionMax pipelineSize n clientTip_ serverTip_ of
        Collect -> case n of
          Succ predN -> CSP.CollectResponse Nothing (clientNextN predN history)
        _ -> CSP.SendMsgRequestNextPipelined (pure ()) (clientIdle_RequestMoreN clientTip_ serverTip_ (Succ n) history)

    clientNextN
      :: forall n
       . Nat n
      -> History mode (w, s)
      -> ClientStNext n BlockInMode ChainPoint ChainTip IO ()
    clientNextN n history =
      ClientStNext
        { recvMsgRollForward = \newBlock@(BlockInMode _ bim) serverChainTip -> do
            let CAPI.Block bh@(BlockHeader slotNo _blockHash currBlockNo) _ = bim
                newClientTip = At currBlockNo
                newServerTip = fromChainTip serverChainTip
                cu =
                  if newClientTip == newServerTip
                    then caughtUpWithNode serverChainTip
                    else catchingUpWithNode (blockHeaderPoint bh) (Just currBlockNo) (Just $ chainTipToChainPoint serverChainTip)

                update :: LedgerStateUpdate mode -> s -> IO (Maybe (LedgerStateUpdate mode, (w, s)))
                update NoLedgerStateUpdate currentState = do
                  state <-
                    accumulate
                      cu
                      currentState
                      NoLedgerStateUpdate
                      newBlock
                  return $ state <&> (,) NoLedgerStateUpdate
                update (LedgerStateUpdate currentLedgerState _) currentState = do
                  let
                    LedgerStateArgs _ validationMode = ledgerStateArgs
                    newLedgerStateE = applyBlock env currentLedgerState validationMode newBlock

                  case newLedgerStateE of
                    Left _ -> return Nothing
                    Right (newLedgerState, newLedgerEvents) -> do
                      let ledgerStateUpdate = LedgerStateUpdate newLedgerState newLedgerEvents
                      state <-
                        accumulate
                          cu
                          currentState
                          ledgerStateUpdate
                          newBlock
                      return $ state <&> (,) ledgerStateUpdate

                (currentLedgerStateUpdate, currentState') =
                  case Seq.viewl history of
                    (_, ledgerState, (_, s)) Seq.:< _ -> (ledgerState, s)
                    Seq.EmptyL -> error "foldClient: clientNextN: Impossible - empty history!"

            newState <- update currentLedgerStateUpdate currentState'
            case newState of
              Nothing -> do
                clientIdle_DoneN n
              Just (!ledgerStateUpdate, !s') -> do
                let (newHistory, _) = pushHistoryState env history slotNo ledgerStateUpdate s'
                return (clientIdle_RequestMoreN newClientTip newServerTip n newHistory)
        , recvMsgRollBackward = \chainPoint serverChainTip -> do
            let newClientTip = Origin
                newServerTip = fromChainTip serverChainTip
                (rolledBack, truncatedHistory) = case chainPoint of
                  ChainPointAtGenesis -> (Seq.empty, initialHistory)
                  ChainPoint slotNo _ -> rollbackStateHistory history slotNo
                (lastSlotNo, lastLedgerState, currentState) =
                  case Seq.viewl truncatedHistory of
                    (n', state, (_, x)) Seq.:< _ -> (n', state, x)
                    Seq.EmptyL -> error "foldClient: clientNextN: Impossible - empty history after rollback!"
            !rolledBackState <- applyRollback chainPoint (foldMap (\(_, _, (s, _)) -> s) rolledBack) currentState
            let (newHistory, _) = pushHistoryState env truncatedHistory lastSlotNo lastLedgerState rolledBackState
            return (clientIdle_RequestMoreN newClientTip newServerTip n newHistory)
        }

    clientIdle_DoneN
      :: Nat n
      -> IO (ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ())
    clientIdle_DoneN n = case n of
      Succ predN -> do
        return $ CollectResponse Nothing (clientNext_DoneN predN) -- Ignore remaining message responses
      Zero -> do
        return $ SendMsgDone ()

    clientNext_DoneN
      :: Nat n
      -> ClientStNext n BlockInMode ChainPoint ChainTip IO ()
    clientNext_DoneN n =
      ClientStNext
        { recvMsgRollForward = \_ _ -> clientIdle_DoneN n
        , recvMsgRollBackward = \_ _ -> clientIdle_DoneN n
        }

  return (clientIdle_RequestMoreN Origin Origin Zero initialHistory)

-- | Add a new state to the history
pushHistoryState
  :: forall mode a
   . Env
  -- ^ Environement used to get the security param, k.
  -> History mode a
  -- ^ History of k items.
  -> SlotNo
  -- ^ Slot number of the new item.
  -> LedgerStateUpdate mode
  -> a
  -- ^ New item to add to the history
  -> (History mode a, History mode a)
  -- ^ ( The new history with the new item appended
  --   , Any exisiting items that are now past the security parameter
  --      and hence can no longer be rolled back.
  --   )
pushHistoryState env hist ix ledgerStateUpdate st =
  Seq.splitAt
    (fromIntegral $ envSecurityParam env + 1)
    ((ix, ledgerStateUpdate, st) Seq.:<| hist)

initialStateHistory :: forall mode a. LedgerStateArgs mode -> a -> History mode a
initialStateHistory (LedgerStateArgs ledgerState0 _) a = Seq.singleton (0, LedgerStateUpdate ledgerState0 [], a)
initialStateHistory NoLedgerStateArgs a = Seq.singleton (0, NoLedgerStateUpdate, a)

{- | Split the history into bits that have been rolled back (1st elemnt) and
  bits that have not been rolled back (2nd element)
-}
rollbackStateHistory :: forall mode a. History mode a -> SlotNo -> (History mode a, History mode a)
rollbackStateHistory hist maxInc = Seq.spanl ((> maxInc) . (\(x, _, _) -> x)) hist
