{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Convex.NodeClient.Resuming (
  ResumingFrom (..),
  resumingClient,
) where

import Cardano.Api (
  BlockInMode,
  ChainPoint (..),
  ChainTip (..),
 )
import Cardano.Api.ChainSync.ClientPipelined (N (Z))
import Convex.NodeClient.Types (PipelinedLedgerStateClient (..))
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined qualified as CSP

{- | Where we start processing blocks. This is the intersection between the
chain points passed to 'runNodeClients' and the blockchain that the node has.
-}
data ResumingFrom
  = ResumingFromChainPoint {chainPoint :: ChainPoint, serverTip :: ChainTip}
  | ResumingFromOrigin {serverTip :: ChainTip}

{- | Turn a 'PipelinedLedgerStateClient' into one that resumes processing from one
of a list of 'ChainPoint's.
-}
resumingClient
  :: [ChainPoint]
  -- ^ List of synchronisation points. If the list is empty, the client will receive all blocks, starting from genesis.
  -> (ResumingFrom -> PipelinedLedgerStateClient)
  -- ^ Function that returns the actual node client, depending on where we resumed from
  -> PipelinedLedgerStateClient
resumingClient syncPoints f = PipelinedLedgerStateClient $ CSP.ChainSyncClientPipelined $ do
  let initialise :: CSP.ClientPipelinedStIdle 'Z BlockInMode ChainPoint ChainTip IO ()
      initialise =
        CSP.SendMsgFindIntersect syncPoints $
          CSP.ClientPipelinedStIntersect
            { CSP.recvMsgIntersectFound = \chainPoint srvTip -> do
                let CSP.ChainSyncClientPipelined{CSP.runChainSyncClientPipelined} = getPipelinedLedgerStateClient (f $ ResumingFromChainPoint chainPoint srvTip)
                runChainSyncClientPipelined
            , CSP.recvMsgIntersectNotFound = \srvTip -> do
                let CSP.ChainSyncClientPipelined{CSP.runChainSyncClientPipelined} = getPipelinedLedgerStateClient (f $ ResumingFromOrigin srvTip)
                runChainSyncClientPipelined
            }

  pure initialise
