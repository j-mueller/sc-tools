{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-| A node client that shows the balance of the wallet
-}
module Convex.Wallet.NodeClient.BalanceClient(
  BalanceClientEnv(..),
  balanceClientEnv,
  balanceClient
  ) where

import           Cardano.Api                (BlockInMode, CardanoMode, Env)
import qualified Cardano.Api                as C
import           Control.Concurrent.STM     (TVar, atomically, newTVarIO,
                                             writeTVar)
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Maybe  (runMaybeT)
import           Convex.MonadLog            (MonadLogKatipT (..), logInfo,
                                             logInfoS)
import           Convex.NodeClient.Fold     (CatchingUp (..), catchingUp,
                                             catchingUpWithNode, foldClient,
                                             LedgerStateArgs (..), LedgerStateUpdate, LedgerStateMode (..))
import           Convex.NodeClient.Resuming (resumingClient)
import           Convex.NodeClient.Types    (PipelinedLedgerStateClient)
import           Convex.Utils               (toShelleyPaymentCredential)
import           Convex.Utxos               (PrettyBalance (..),
                                             PrettyUtxoChange (..), UtxoSet,
                                             apply)
import qualified Convex.Utxos               as Utxos
import           Convex.Wallet.WalletState  (WalletState, chainPoint, utxoSet)
import qualified Convex.Wallet.WalletState  as WalletState
import qualified Katip                      as K

data BalanceClientEnv =
  BalanceClientEnv
    { bceFile  :: FilePath
    , bceState :: TVar WalletState
    }

balanceClientEnv :: FilePath -> WalletState -> IO BalanceClientEnv
balanceClientEnv bceFile initialState =
  BalanceClientEnv bceFile <$> newTVarIO initialState

balanceClient :: K.LogEnv -> K.Namespace -> BalanceClientEnv -> WalletState -> C.PaymentCredential -> Env -> PipelinedLedgerStateClient
balanceClient logEnv ns clientEnv walletState wallet env =
  let cp = chainPoint walletState
      i  = catchingUpWithNode cp Nothing Nothing
  in resumingClient [cp] $ \_ ->
      foldClient
        (i, utxoSet walletState)
        NoLedgerStateArgs
        env
        (applyBlock logEnv ns clientEnv wallet)

{-| Apply a new block
-}
applyBlock :: K.LogEnv -> K.Namespace -> BalanceClientEnv -> C.PaymentCredential -> CatchingUp -> (CatchingUp, UtxoSet C.CtxTx ()) -> LedgerStateUpdate 'NoLedgerState -> BlockInMode CardanoMode -> IO (Maybe (CatchingUp, UtxoSet C.CtxTx ()))
applyBlock logEnv ns BalanceClientEnv{bceFile, bceState} wallet c (oldC, state) _ block = K.runKatipContextT logEnv () ns $ runMonadLogKatipT $ runMaybeT $ do
  let change = Utxos.extract_ (toShelleyPaymentCredential wallet) state block
      newUTxOs = apply state change
      C.BlockInMode (C.getBlockHeader -> header) _ = block
      newState = WalletState.walletState newUTxOs header

  when (not $ Utxos.null change) $ do
    logInfo $ PrettyUtxoChange change
    logInfo $ PrettyBalance newUTxOs

  when (catchingUp oldC &&  not (catchingUp c)) $
    logInfoS "Caught up with node"

  when (not $ catchingUp c) $ do
    liftIO (WalletState.writeToFile bceFile newState)

  liftIO $ writeState bceState newState

  pure (c, newUTxOs)

writeState :: TVar WalletState -> WalletState -> IO ()
writeState tvar state = atomically (writeTVar tvar state)
