{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}
{-| A node client that applies a fold to the stream of blocks.
Unlike 'foldBlocks' from 'Cardano.Api', this one supports rollbacks.
-}
module Convex.NodeClient.Fold(
  CatchingUp(..),
  catchingUpWithNode,
  caughtUpWithNode,
  resumingFrom,
  catchingUp,
  caughtUp,
  foldClient,
  foldClient'
  ) where

import           Cardano.Api                                           (Block (..),
                                                                        BlockHeader (..),
                                                                        BlockInMode (..),
                                                                        BlockNo (..),
                                                                        CardanoMode,
                                                                        ChainPoint (..),
                                                                        ChainTip (..),
                                                                        Env,
                                                                        SlotNo,
                                                                        chainTipToChainPoint,
                                                                        envSecurityParam)
import           Cardano.Slotting.Slot                                 (WithOrigin (At))
import           Convex.NodeClient.ChainTip                            (JSONChainPoint (..),
                                                                        blockHeaderPoint)
import           Convex.NodeClient.Types                               (ClientBlock,
                                                                        PipelinedLedgerStateClient (..),
                                                                        fromChainTip)
import           Data.Aeson                                            (FromJSON,
                                                                        ToJSON)
import           Data.Sequence                                         (Seq)
import qualified Data.Sequence                                         as Seq
import           GHC.Generics                                          (Generic)
import           Network.TypedProtocol.Pipelined                       (Nat (..))
import Convex.NodeClient.Resuming (ResumingFrom)
import qualified Convex.NodeClient.Resuming as R
import           Ouroboros.Consensus.Block.Abstract                    (WithOrigin (..))
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined  (ClientPipelinedStIdle (..),
                                                                        ClientStNext (..))
import qualified Ouroboros.Network.Protocol.ChainSync.ClientPipelined  as CSP
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision (PipelineDecision (..),
                                                                        pipelineDecisionMax)

{-| Whether we have fully caught up with the node
-}
data CatchingUp =
  CatchingUpWithNode{ clientTip :: JSONChainPoint, serverTip :: Maybe JSONChainPoint} -- ^ Client is still catching up
  | CaughtUpWithNode{ tip :: JSONChainPoint } -- ^ Client fully caught up (client tip == server tip)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

catchingUpWithNode :: ChainPoint -> Maybe ChainPoint -> CatchingUp
catchingUpWithNode (JSONChainPoint -> clientTip) (fmap JSONChainPoint -> serverTip) =
  CatchingUpWithNode{clientTip, serverTip}

caughtUpWithNode :: ChainPoint -> CatchingUp
caughtUpWithNode (JSONChainPoint -> tip) = CaughtUpWithNode{tip}

catchingUp :: CatchingUp -> Bool
catchingUp = \case
  CatchingUpWithNode{} -> True
  CaughtUpWithNode{}   -> False

caughtUp :: CatchingUp -> Bool
caughtUp = not . catchingUp

resumingFrom :: ResumingFrom -> CatchingUp
resumingFrom = \case
  R.ResumingFromChainPoint cp st -> catchingUpWithNode cp (Just $ chainTipToChainPoint st)
  R.ResumingFromOrigin st        -> catchingUpWithNode ChainPointAtGenesis (Just $ chainTipToChainPoint st)

{-| Run the client until 'Nothing' is returned
-}
foldClient ::
  forall s.
  s -> -- ^ Initial state
  Env -> -- ^ Node connection data
  (CatchingUp -> s -> BlockInMode CardanoMode -> IO (Maybe s)) -> -- ^ Fold
  PipelinedLedgerStateClient
foldClient initialState env applyBlock =
  foldClient' @s @()
    initialState
    env
    (\_ _ !s -> pure ((), s))
    (\c !s -> fmap (fmap pure) . applyBlock c s)

{-| A variant of 'foldClient' with more detailed control over rollbacks.
-}
foldClient' ::
  forall s w.
  Monoid w =>
  s -> -- ^ Initial state
  Env -> -- ^ Node connection data
  (ChainPoint -> w -> s -> IO (w, s)) -> -- ^ Rollback
  (CatchingUp -> s -> BlockInMode CardanoMode -> IO (Maybe (w, s))) -> -- ^ Fold
  PipelinedLedgerStateClient
foldClient' initialState env applyRollback applyBlock = PipelinedLedgerStateClient $ CSP.ChainSyncClientPipelined $ do

-- NB: The code below was adapted from https://input-output-hk.github.io/cardano-node/cardano-api/src/Cardano.Api.LedgerState.html#foldBlocks

  let
    pipelineSize = 10 -- TODO: Configurable

    initialHistory = initialStateHistory (mempty, initialState)

    clientIdle_RequestMoreN
      :: forall n. WithOrigin BlockNo
      -> WithOrigin BlockNo
      -> Nat n -- Number of requests inflight.
      -> History (w, s)
      -> CSP.ClientPipelinedStIdle n ClientBlock ChainPoint ChainTip IO ()
    clientIdle_RequestMoreN clientTip_ serverTip_ n history
      = case pipelineDecisionMax pipelineSize n clientTip_ serverTip_  of
          Collect -> case n of
            Succ predN -> CSP.CollectResponse Nothing (clientNextN predN history)
          _ -> CSP.SendMsgRequestNextPipelined (clientIdle_RequestMoreN clientTip_ serverTip_ (Succ n) history)

    clientNextN
      :: Nat n
      -> History (w, s)
      -> ClientStNext n ClientBlock ChainPoint ChainTip IO ()
    clientNextN n history =
      ClientStNext {
          recvMsgRollForward = \newBlock serverChainTip -> do
              let BlockInMode (Block bh@(BlockHeader slotNo _blockHash currBlockNo) _) _ = newBlock
                  newClientTip = At currBlockNo
                  newServerTip = fromChainTip serverChainTip
                  cu = if newClientTip == newServerTip
                        then caughtUpWithNode (chainTipToChainPoint serverChainTip)
                        else catchingUpWithNode (blockHeaderPoint bh) (Just $ chainTipToChainPoint serverChainTip)
                  currentState =
                    case Seq.viewl history of
                      (_, (_, x)) Seq.:< _ -> x
                      Seq.EmptyL           -> error "foldClient: clientNextN: Impossible - empty history!"

              newState <- applyBlock cu currentState newBlock
              case newState of
                Nothing -> do
                  putStrLn "foldClient: Shutting down"
                  clientIdle_DoneN n
                Just !s' -> do
                  let (newHistory, _) = pushHistoryState env history slotNo s'
                  return (clientIdle_RequestMoreN newClientTip newServerTip n newHistory)
        , recvMsgRollBackward = \chainPoint serverChainTip -> do
            let newClientTip = Origin
                newServerTip = fromChainTip serverChainTip
                (rolledBack, truncatedHistory) = case chainPoint of
                    ChainPointAtGenesis -> (Seq.empty, initialHistory)
                    ChainPoint slotNo _ -> rollbackStateHistory history slotNo
                (lastSlotNo, currentState) =
                    case Seq.viewl truncatedHistory of
                      (n', (_, x)) Seq.:< _ -> (n', x)
                      Seq.EmptyL      -> error "foldClient: clientNextN: Impossible - empty history after rollback!"
            !rolledBackState <- applyRollback chainPoint (foldMap (fst . snd) rolledBack) currentState
            let (newHistory, _) = pushHistoryState env truncatedHistory lastSlotNo rolledBackState
            return (clientIdle_RequestMoreN newClientTip newServerTip n newHistory)
        }

    clientIdle_DoneN
      :: Nat n
      -> IO (ClientPipelinedStIdle n ClientBlock ChainPoint ChainTip IO ())
    clientIdle_DoneN n = case n of
      Succ predN -> do
        return $ CollectResponse Nothing (clientNext_DoneN predN) -- Ignore remaining message responses
      Zero -> do
        putStrLn "Chain Sync: done!"
        return $ SendMsgDone ()

    clientNext_DoneN
      :: Nat n
      -> ClientStNext n ClientBlock ChainPoint ChainTip IO ()
    clientNext_DoneN n =
      ClientStNext {
          recvMsgRollForward = \_ _ -> clientIdle_DoneN n
        , recvMsgRollBackward = \_ _ -> clientIdle_DoneN n
        }

  return (clientIdle_RequestMoreN Origin Origin Zero initialHistory)

-- | A history of the last @k@ states
type History a = Seq (SlotNo, a)

-- | Add a new state to the history
pushHistoryState
  :: Env                -- ^ Environement used to get the security param, k.
  -> History a          -- ^ History of k items.
  -> SlotNo             -- ^ Slot number of the new item.
  -> a                  -- ^ New item to add to the history
  -> (History a, History a)
  -- ^ ( The new history with the new item appended
  --   , Any exisiting items that are now past the security parameter
  --      and hence can no longer be rolled back.
  --   )
pushHistoryState env hist ix st
  = Seq.splitAt
      (fromIntegral $ envSecurityParam env + 1)
      ((ix, st) Seq.:<| hist)

-- | Split the history into bits that have been rolled back (1st elemnt) and
--   bits that have not been rolled back (2nd element)
rollbackStateHistory :: History a -> SlotNo -> (History a, History a)
rollbackStateHistory hist maxInc = Seq.spanl ((> maxInc) . (\(x,_) -> x)) hist

initialStateHistory :: a -> History a
initialStateHistory a = Seq.singleton (0, a)
