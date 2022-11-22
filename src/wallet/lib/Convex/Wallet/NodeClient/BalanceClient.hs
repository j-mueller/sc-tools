{-# LANGUAGE OverloadedStrings #-}
{-| A node client that shows the balance of the wallet
-}
module Convex.Wallet.NodeClient.BalanceClient(
  balanceClient
  ) where

import           Cardano.Api                (BlockInMode, CardanoMode,
                                             ChainPoint, Env)
import           Control.Monad              (when)
import           Control.Monad.Trans.Maybe  (runMaybeT)
import qualified Convex.Constants           as Constants
import           Convex.MonadLog            (MonadLogKatipT (..), logInfoS)
import           Convex.NodeClient.Fold     (CatchingUp (..), foldClient')
import           Convex.NodeClient.Resuming (resumingClient)
import           Convex.NodeClient.Types    (PipelinedLedgerStateClient)
import           Convex.Utxos               (UtxoChange, UtxoSet, apply)
import qualified Convex.Utxos               as Utxos
import           Convex.Wallet              (Wallet)
import qualified Convex.Wallet              as Wallet
import qualified Data.Text                  as Text
import qualified Katip                      as K

balanceClient :: K.LogEnv -> K.Namespace -> Wallet -> Env -> PipelinedLedgerStateClient
balanceClient logEnv ns wallet env =
  resumingClient [Constants.recent] $ \_ ->
    foldClient'
      mempty
      env
      (rollback logEnv ns)
      (applyBlock logEnv ns wallet)

{-| Apply a new block
-}
applyBlock :: K.LogEnv -> K.Namespace -> Wallet -> CatchingUp -> UtxoSet -> BlockInMode CardanoMode -> IO (Maybe (UtxoChange, UtxoSet))
applyBlock logEnv ns wallet _catchingUp state block = K.runKatipContextT logEnv () ns $ runMonadLogKatipT $ runMaybeT $ do
  let change = Utxos.extract (Wallet.shelleyPaymentCredential wallet) state block
      newState = apply state change

  when (not $ Utxos.null change) $ do
    logInfoS $ "UTXO set changed: " <> Text.unpack (Utxos.describeChange change)
    logInfoS $ "New balance: " <> show (Utxos.totalBalance newState)
  pure (change, newState)

{-| Roll back to an earlier state
-}
rollback :: K.LogEnv -> K.Namespace -> ChainPoint -> UtxoChange -> UtxoSet -> IO (UtxoChange, UtxoSet)
rollback logEnv ns _chainPoint rolledBackChange oldState = K.runKatipContextT logEnv () ns $ runMonadLogKatipT $ do
  let rolledBack = Utxos.inv rolledBackChange
      newState = apply oldState rolledBack
  when (not $ Utxos.null rolledBack) $ do
    logInfoS $ "Rollback: " <> Text.unpack (Utxos.describeChange rolledBack)
    logInfoS $ "New balance: " <> show (Utxos.totalBalance newState)
  pure (mempty, newState) -- TODO: Is it correct to return mempty here?
