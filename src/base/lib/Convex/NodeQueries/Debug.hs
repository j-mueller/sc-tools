{-| Node queries for use in testnet environments
-}
module Convex.NodeQueries.Debug(
    -- The queries in this module are either inefficient (such as 'queryUTxOByAddress')
    -- or brittle (such as 'waitForTxIn')
    queryUTxOByAddress,
    queryUTxOWhole,
    waitForTxIn,
    waitForTx,
    waitForTxInSpend
) where

import           Cardano.Api        (LocalNodeConnectInfo, Tx, TxIn)
import qualified Cardano.Api        as CAPI
import           Control.Concurrent (threadDelay)
import           Control.Monad      (unless, when)
import           Convex.NodeQueries (queryUTxOFilter)
import           Convex.Utils       (txnUtxos)
import           Convex.Utxos       (UtxoSet)
import qualified Data.Set           as Set

{-| Get the set of UTxOs for a list of addresses
-}
queryUTxOByAddress :: LocalNodeConnectInfo -> [CAPI.AddressAny] -> IO (UtxoSet CAPI.CtxUTxO ())
queryUTxOByAddress connectInfo addresses =
  queryUTxOFilter connectInfo $ CAPI.QueryUTxOByAddress $ Set.fromList addresses

{-| Get the entire UTxO set
-}
queryUTxOWhole :: LocalNodeConnectInfo -> IO (UtxoSet CAPI.CtxUTxO ())
queryUTxOWhole connectInfo = queryUTxOFilter connectInfo CAPI.QueryUTxOWhole

{-| Wait until the output appears on the chain
-}
waitForTxIn :: LocalNodeConnectInfo -> TxIn -> IO ()
waitForTxIn connectInfo txIn = go where
  go = do
    utxo <- queryUTxOFilter connectInfo (CAPI.QueryUTxOByTxIn (Set.singleton txIn))
    when (utxo == mempty) $ do
      threadDelay 2_000_000
      go

{-| Wait until the first output of the transaction appears on the chain
-}
waitForTx :: forall era. LocalNodeConnectInfo -> Tx era -> IO ()
waitForTx connectInfo tx = waitForTxIn connectInfo (fst $ head $ txnUtxos tx)

{-| Wait until the output disappears from the chain
-}
waitForTxInSpend :: LocalNodeConnectInfo -> TxIn -> IO ()
waitForTxInSpend connectInfo txIn = go where
  go = do
    utxo <- queryUTxOFilter connectInfo (CAPI.QueryUTxOByTxIn (Set.singleton txIn))
    unless (utxo == mempty) $ do
      threadDelay 2_000_000
      go
