{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-| blockfrost-based implementation of MonadBlockchain
-}
module Convex.Blockfrost.MonadBlockchain(
  BlockfrostState(..)
) where

import qualified Blockfrost.Client.Cardano.Ledger as Client
import           Blockfrost.Client.Types          (MonadBlockfrost (..))
import           Blockfrost.Types.Cardano.Genesis (Genesis)
import           Cardano.Api                      (NetworkId)
import           Cardano.Slotting.Time            (SystemStart)
import           Control.Lens                     (makeLensesFor, use, (?=))
import           Control.Monad.State              (MonadState)
import qualified Convex.Blockfrost.Types          as Types

-- send Tx
-- utxoByTxIn
-- protocol params
-- stake addresses
-- stake pools
-- era history
-- slot no
-- query network id

data BlockfrostState =
  BlockfrostState
    { bfsGenesis :: Maybe Genesis
    }

makeLensesFor
  [ ("bfsGenesis", "genesis")
  ]
  ''BlockfrostState

emptyBlockfrostState :: BlockfrostState
emptyBlockfrostState =
  BlockfrostState
    { bfsGenesis = Nothing
    }

getSystemStart :: (MonadBlockfrost m, MonadState BlockfrostState m) => m SystemStart
getSystemStart = use genesis >>= \case
  Just g -> pure (Types.systemStart g)
  Nothing -> do
    k <- Client.getLedgerGenesis
    genesis ?= k
    pure (Types.systemStart k)

getNetworkId :: (MonadBlockfrost m, MonadState BlockfrostState m) => m NetworkId
getNetworkId = undefined

-- Cardano.Api.NetworkId.fromNetworkMagic
--   "network_magic": 764824073 (mainnet)