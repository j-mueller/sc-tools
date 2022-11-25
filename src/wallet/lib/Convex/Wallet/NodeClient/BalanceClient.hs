{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-| A node client that shows the balance of the wallet
-}
module Convex.Wallet.NodeClient.BalanceClient(
  balanceClient
  ) where

import           Cardano.Api                (BlockInMode, CardanoMode,
                                             ChainPoint, Env)
import qualified Cardano.Api                as C
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Maybe  (runMaybeT)
import           Convex.MonadLog            (MonadLogKatipT (..), logInfo,
                                             logInfoS)
import           Convex.NodeClient.Fold     (CatchingUp (..), catchingUp,
                                             foldClient')
import           Convex.NodeClient.Resuming (resumingClient)
import           Convex.NodeClient.Types    (PipelinedLedgerStateClient)
import           Convex.Utxos               (PrettyBalance (..),
                                             PrettyUtxoChange (..), UtxoChange,
                                             UtxoSet, apply)
import qualified Convex.Utxos               as Utxos
import           Convex.Wallet              (Wallet)
import qualified Convex.Wallet              as Wallet
import           Convex.Wallet.WalletState  (WalletState, chainPoint, utxoSet)
import qualified Convex.Wallet.WalletState  as WalletState
import qualified Data.Text                  as Text
import qualified Katip                      as K

balanceClient :: K.LogEnv -> K.Namespace -> FilePath -> WalletState -> Wallet -> Env -> PipelinedLedgerStateClient
balanceClient logEnv ns filePath walletState wallet env =
  resumingClient [chainPoint walletState] $ \_ ->
    foldClient'
      (utxoSet walletState)
      env
      (rollback logEnv ns)
      (applyBlock logEnv ns filePath wallet)

{-| Apply a new block
-}
applyBlock :: K.LogEnv -> K.Namespace -> FilePath -> Wallet -> CatchingUp -> UtxoSet -> BlockInMode CardanoMode -> IO (Maybe (UtxoChange, UtxoSet))
applyBlock logEnv ns filePath wallet (catchingUp -> isCatchingUp) state block = K.runKatipContextT logEnv () ns $ runMonadLogKatipT $ runMaybeT $ do
  let change = Utxos.extract (Wallet.shelleyPaymentCredential wallet) state block
      newState = apply state change

  when (not $ Utxos.null change) $ do
    logInfo $ PrettyUtxoChange change
    logInfo $ PrettyBalance newState

  when (not isCatchingUp) $ do
    let C.BlockInMode (C.getBlockHeader -> header) _ = block
    liftIO (WalletState.writeToFile filePath (WalletState.walletState newState header))

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
