{-# LANGUAGE OverloadedStrings #-}
{-| Chain client for the wallet
-}
module Convex.Wallet.NodeClient(
  walletClient,
  applyBlock,
  rollback
  ) where

import           Cardano.Api                (BlockInMode, CardanoMode,
                                             ChainPoint, Env, NetworkId)
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Maybe  (runMaybeT)
import qualified Convex.Constants           as Constants
import           Convex.NodeClient.Fold     (CatchingUp (..), foldClient')
import           Convex.NodeClient.Resuming (resumingClient)
import           Convex.NodeClient.Types    (PipelinedLedgerStateClient)
import           Convex.Wallet              (Wallet)
import qualified Convex.Wallet              as Wallet
import           Convex.Wallet.Utxos        (UtxoChange, UtxoState, apply)
import qualified Convex.Wallet.Utxos        as Utxos
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text

walletClient :: NetworkId -> Wallet -> Env -> PipelinedLedgerStateClient
walletClient networkId wallet env =
  resumingClient [Constants.recent] $ \_ ->
    foldClient'
      mempty
      env
      rollback
      (applyBlock networkId wallet)

{-| Apply a new block
-}
applyBlock :: NetworkId -> Wallet -> CatchingUp -> UtxoState -> BlockInMode CardanoMode -> IO (Maybe (UtxoChange, UtxoState))
applyBlock networkId wallet _catchingUp state block = runMaybeT $ do
  let change = Utxos.extract (Wallet.addressInEra networkId wallet) state block
      newState = apply state change

  when (not $ Utxos.null change) $ liftIO $ do
    Text.putStrLn $ "UTXO set changed: " <> Utxos.describeChange change
    Text.putStrLn $ "New balance: " <> Text.pack (show $ Utxos.totalBalance newState)
  pure (change, newState)

{-| Roll back to an earlier state
-}
rollback :: ChainPoint -> UtxoChange -> UtxoState -> IO (UtxoChange, UtxoState)
rollback _chainPoint rolledBackChange oldState = do
  let rolledBack = Utxos.inv rolledBackChange
      newState = apply oldState rolledBack
  when (not $ Utxos.null rolledBack) $ do
    Text.putStrLn $ "Rollback: " <> Utxos.describeChange rolledBack
    Text.putStrLn $ "New balance: " <> Text.pack (show $ Utxos.totalBalance newState)
  pure (mempty, newState) -- TODO: Is it correct to return mempty here?
