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
  getClientPoint,
  shouldLog,
  foldClient,
  foldClient'
  ) where

import           Cardano.Api                                           (Block (..),
                                                                        BlockHeader (..),
                                                                        BlockInMode (..),
                                                                        BlockNo (..),
                                                                        ChainPoint (..),
                                                                        ChainTip (..),
                                                                        Env,
                                                                        SlotNo,
                                                                        chainTipToChainPoint,
                                                                        envSecurityParam)
import           Cardano.Slotting.Slot                                 (WithOrigin (At))
import           Convex.NodeClient.ChainTip                            (JSONBlockNo (..),
                                                                        JSONChainPoint (..),
                                                                        JSONChainTip (..),
                                                                        blockHeaderPoint)
import           Convex.NodeClient.Resuming                            (ResumingFrom)
import qualified Convex.NodeClient.Resuming                            as R
import           Convex.NodeClient.Types                               (PipelinedLedgerStateClient (..),
                                                                        fromChainTip)
import           Data.Aeson                                            (FromJSON,
                                                                        ToJSON)
import           Data.Sequence                                         (Seq)
import qualified Data.Sequence                                         as Seq
import           GHC.Generics                                          (Generic)
import           Network.TypedProtocol.Pipelined                       (Nat (..))
import           Ouroboros.Consensus.Block.Abstract                    (WithOrigin (..))
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined  (ClientPipelinedStIdle (..),
                                                                        ClientStNext (..))
import qualified Ouroboros.Network.Protocol.ChainSync.ClientPipelined  as CSP
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision (PipelineDecision (..),
                                                                        pipelineDecisionMax)

{-| Whether we have fully caught up with the node
-}
data CatchingUp =
  CatchingUpWithNode{ clientPoint :: JSONChainPoint, clientBlockNo :: Maybe JSONBlockNo, serverTip :: Maybe JSONChainPoint} -- ^ Client is still catching up
  | CaughtUpWithNode{ tip :: JSONChainTip } -- ^ Client fully caught up (client tip == server tip)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

getClientPoint :: CatchingUp -> JSONChainPoint
getClientPoint = \case
  CatchingUpWithNode{clientPoint} -> clientPoint
  CaughtUpWithNode{tip}           -> JSONChainPoint $ chainTipToChainPoint $ unJSONChainTip tip

catchingUpWithNode :: ChainPoint -> Maybe BlockNo -> Maybe ChainPoint -> CatchingUp
catchingUpWithNode (JSONChainPoint -> clientPoint) (fmap JSONBlockNo -> clientBlockNo) (fmap JSONChainPoint -> serverTip) =
  CatchingUpWithNode{clientPoint, serverTip, clientBlockNo}

caughtUpWithNode :: ChainTip -> CatchingUp
caughtUpWithNode (JSONChainTip -> tip) = CaughtUpWithNode{tip}

catchingUp :: CatchingUp -> Bool
catchingUp = \case
  CatchingUpWithNode{} -> True
  CaughtUpWithNode{}   -> False

{-| Whether it is a good time to log something. Returns true if

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
  R.ResumingFromOrigin st        ->
    catchingUpWithNode ChainPointAtGenesis Nothing (Just $ chainTipToChainPoint st)

{-| Run the client until 'Nothing' is returned
-}
foldClient ::
  forall s.
  -- | Initial state
  s ->
  -- | Node connection data
  Env ->
  -- | Fold
  (CatchingUp -> s -> BlockInMode -> IO (Maybe s)) ->
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
  -- | Initial state
  s ->
  -- | Node connection data
  Env ->
  -- | Rollback
  (ChainPoint -> w -> s -> IO (w, s)) ->
  -- | Fold
  (CatchingUp -> s -> BlockInMode -> IO (Maybe (w, s))) -> -- ^ Fold
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
      -> CSP.ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ()
    clientIdle_RequestMoreN clientTip_ serverTip_ n history
      = case pipelineDecisionMax pipelineSize n clientTip_ serverTip_  of
          Collect -> case n of
            Succ predN -> CSP.CollectResponse Nothing (clientNextN predN history)
          _ -> CSP.SendMsgRequestNextPipelined (pure ()) (clientIdle_RequestMoreN clientTip_ serverTip_ (Succ n) history)

    clientNextN
      :: Nat n
      -> History (w, s)
      -> ClientStNext n BlockInMode ChainPoint ChainTip IO ()
    clientNextN n history =
      ClientStNext {
          recvMsgRollForward = \newBlock serverChainTip -> do
              let BlockInMode _ (Block bh@(BlockHeader slotNo _blockHash currBlockNo) _) = newBlock
                  newClientTip = At currBlockNo
                  newServerTip = fromChainTip serverChainTip
                  cu = if newClientTip == newServerTip
                        then caughtUpWithNode serverChainTip
                        else catchingUpWithNode (blockHeaderPoint bh) (Just currBlockNo) (Just $ chainTipToChainPoint serverChainTip)
                  currentState =
                    case Seq.viewl history of
                      (_, (_, x)) Seq.:< _ -> x
                      Seq.EmptyL           -> error "foldClient: clientNextN: Impossible - empty history!"

              newState <- applyBlock cu currentState newBlock
              case newState of
                Nothing -> do
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
      ClientStNext {
          recvMsgRollForward = \_ _ -> clientIdle_DoneN n
        , recvMsgRollBackward = \_ _ -> clientIdle_DoneN n
        }

  return (clientIdle_RequestMoreN Origin Origin Zero initialHistory)

-- | A history of the last @k@ states
type History a = Seq (SlotNo, a)

-- | Add a new state to the history
pushHistoryState
  :: -- | Environement used to get the security param, k.
    Env
     -- | History of k items.
  -> History a
     -- | Slot number of the new item.
  -> SlotNo
     -- | New item to add to the history
  -> a
  -- | ( The new history with the new item appended
  --   , Any exisiting items that are now past the security parameter
  --      and hence can no longer be rolled back.
  --   )
  -> (History a, History a)

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
