{-| Typeclass for blockchain operations
-}
module Convex.MockChain.Class(
  MonadBlockchain(..),
  MonadMockchain(..),
  MonadBlockchainQuery(..),
  getSlot,
  nextSlot
) where

import qualified Cardano.Api                as C
import           Cardano.Api.Shelley        (BabbageEra, SlotNo, Tx, TxId)
import           Cardano.Ledger.Shelley.API (UTxO)
import           Convex.Era                 (ERA)
import           Data.Set                   (Set)

{-| Send transactions and resolve tx inputs.
-}
class Monad m => MonadBlockchain m where
  sendTx :: Tx BabbageEra -> m TxId -- ^ Submit a transaction to the network
  utxoByTxIn :: Set C.TxIn -> m (C.UTxO C.BabbageEra) -- ^ Resolve tx inputs

{-| Modify the mockchain internals
-}
class Monad m => MonadMockchain m where
  modifySlot :: (SlotNo -> (SlotNo, a)) -> m a
  modifyUtxo :: (UTxO ERA -> (UTxO ERA, a)) -> m a

{-| UTxO queries that cannot be answered by the cardano node efficiently.
These require an extra index to be maintained somewhere.
-}
class Monad m => MonadBlockchainQuery m where
  utxoByAddress :: C.AddressInEra C.BabbageEra -> m (C.UTxO C.BabbageEra)

{-| Get the current slot number
-}
getSlot :: MonadMockchain m => m SlotNo
getSlot = modifySlot (\s -> (s, s))

{-| Increase the slot number by 1.
-}
nextSlot :: MonadMockchain m => m ()
nextSlot = modifySlot (\s -> (succ s, ()))
