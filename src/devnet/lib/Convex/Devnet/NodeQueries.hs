{-# LANGUAGE LambdaCase #-}
module Convex.Devnet.NodeQueries(
  querySystemStart,
  queryEraHistory,
  queryTipBlock,
  queryTipSlotNo
) where

import           Cardano.Api           (BlockNo, CardanoMode, EraHistory,
                                        NetworkId, QueryInMode, SlotNo)
import qualified Cardano.Api           as C
import           Cardano.Slotting.Slot (WithOrigin)
import           Cardano.Slotting.Time (SystemStart)
import           Convex.Devnet.Utils   (failure)
import           Data.Word             (Word64)

import           Prelude

querySystemStart :: NetworkId -> FilePath -> IO SystemStart
querySystemStart = queryLocalState C.QuerySystemStart

queryEraHistory :: NetworkId -> FilePath -> IO (EraHistory CardanoMode)
queryEraHistory = queryLocalState (C.QueryEraHistory C.CardanoModeIsMultiEra)

queryLocalState :: QueryInMode CardanoMode b -> NetworkId -> FilePath -> IO b
queryLocalState query networkId socket = do
  let defaultByronEpochSlots = 21600 :: Word64
  let connectInfo = C.LocalNodeConnectInfo (C.CardanoModeParams (C.EpochSlots defaultByronEpochSlots)) networkId socket
  C.queryNodeLocalState connectInfo Nothing query >>= \case
    Left err -> do
      failure $ "querySystemStart: Failed with " <> show err
    Right result -> pure result

queryTipBlock :: NetworkId -> FilePath -> IO (WithOrigin BlockNo)
queryTipBlock = queryLocalState C.QueryChainBlockNo

queryTipSlotNo :: NetworkId -> FilePath -> IO SlotNo
queryTipSlotNo networkId socket = queryLocalState (C.QueryChainPoint C.CardanoMode) networkId socket >>= \case
  C.ChainPointAtGenesis -> failure "queryTipSlotNo: chain point at genesis"
  C.ChainPoint slot _   -> pure slot
