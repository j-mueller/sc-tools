{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
-- FIXME (j-mueller) The deprecation warning on Cardano.Api.Block covers the
--  whole type 'Block', but it looks as if the intention was just to cover the
--  view pattern 'Block'. We can remove the flag once that has been fixed
{-# OPTIONS_GHC -Wno-deprecations #-}

module Convex.NodeClient.Progress (progressClient) where

import Cardano.Api (
  Block (..),
  BlockHeader (..),
  BlockInMode (..),
  BlockNo (..),
  ChainPoint (..),
  ChainTip (..),
 )
import Cardano.Api qualified as CAPI
import Cardano.Api.ChainSync.ClientPipelined (Nat (..))
import Cardano.Slotting.Slot (WithOrigin (At))
import Control.Monad (when)
import Convex.NodeClient.Types (
  PipelinedLedgerStateClient (..),
  fromChainTip,
 )
import Data.Text qualified as Text
import Data.Time (
  diffUTCTime,
  getCurrentTime,
 )
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

-- | A 'PipelinedLedgerStateClient' that simply shows the progress of synchronising with the node.
progressClient :: PipelinedLedgerStateClient
progressClient = PipelinedLedgerStateClient $ CSP.ChainSyncClientPipelined $ do
  startTime <- getCurrentTime
  let
    pipelineSize = 50 -- TODO: Configurable
    clientIdle_RequestMoreN
      :: WithOrigin BlockNo
      -> WithOrigin BlockNo
      -> Nat n -- Number of requests inflight.
      -> CSP.ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ()
    clientIdle_RequestMoreN clientTip serverTip n =
      case pipelineDecisionMax pipelineSize n clientTip serverTip of
        Collect -> case n of
          Succ predN -> CSP.CollectResponse Nothing (clientNextN predN)
        _ -> CSP.SendMsgRequestNextPipelined (pure ()) (clientIdle_RequestMoreN clientTip serverTip (Succ n))

    clientNextN :: Nat n -> ClientStNext n BlockInMode ChainPoint ChainTip IO ()
    clientNextN n =
      ClientStNext
        { recvMsgRollForward = \(BlockInMode _era block) serverChainTip -> do
            let BlockHeader _ _ currBlockNo@(BlockNo blockNo) = CAPI.getBlockHeader block
                newClientTip = At currBlockNo
                newServerTip = fromChainTip serverChainTip
            when (blockNo `mod` 10_000 == 0) $ do
              printBlock block
              printTip serverChainTip
              now <- getCurrentTime
              let elapsedTime = realToFrac (now `diffUTCTime` startTime) :: Double
                  rate = fromIntegral blockNo / elapsedTime
              putStrLn $ "Rate = " ++ show rate ++ " blocks/second"
            if newClientTip == newServerTip
              then clientIdle_DoneN n
              else return (clientIdle_RequestMoreN newClientTip newServerTip n)
        , recvMsgRollBackward = \k serverChainTip -> do
            putStrLn $ "Rollback to " <> show k
            let newClientTip = case k of
                  ChainPoint _slotNo _bhHash -> Origin
                  ChainPointAtGenesis -> Origin
                newServerTip = fromChainTip serverChainTip
            return (clientIdle_RequestMoreN newClientTip newServerTip n)
        }

    clientIdle_DoneN :: Nat n -> IO (ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ())
    clientIdle_DoneN n = case n of
      Succ predN -> do
        putStrLn "Chain Sync: done! (Ignoring remaining responses)"
        return $ CollectResponse Nothing (clientNext_DoneN predN) -- Ignore remaining message responses
      Zero -> do
        putStrLn "Chain Sync: done!"
        return $ SendMsgDone ()

    clientNext_DoneN :: Nat n -> ClientStNext n BlockInMode ChainPoint ChainTip IO ()
    clientNext_DoneN n =
      ClientStNext
        { recvMsgRollForward = \_ _ -> clientIdle_DoneN n
        , recvMsgRollBackward = \_ _ -> clientIdle_DoneN n
        }

    printBlock :: Block era -> IO ()
    printBlock block =
      let BlockHeader _ _ currBlockNo = CAPI.getBlockHeader block
          transactions = CAPI.getBlockTxs block
       in putStrLn $ show currBlockNo ++ " transactions: " ++ show (length transactions)

    printTip :: ChainTip -> IO ()
    printTip = \case
      ChainTipAtGenesis -> putStrLn "server tip at genesis"
      ChainTip slot hash block -> do
        let txt = CAPI.serialiseToRawBytesHexText hash
        putStrLn $ "server tip: " <> show slot <> "; " <> Text.unpack txt <> "; " <> show block

  return (clientIdle_RequestMoreN Origin Origin Zero)
