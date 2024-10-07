{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-| A node client that shows the balance of the wallet
-}
module Convex.Wallet.NodeClient.BalanceClient(
  BalanceClientEnv(..),
  balanceClientEnv,
  balanceClient
  ) where

import           Cardano.Api                (Env)
import qualified Cardano.Api                as C
import           Control.Concurrent.STM     (TVar, atomically, newTVarIO,
                                             writeTVar)
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Convex.MonadLog            (MonadLogKatipT (..), logInfo,
                                             logInfoS)
import           Convex.NodeClient.Fold     (CatchingUp (..),
                                             LedgerStateArgs (..),
                                             LedgerStateMode (..),
                                             LedgerStateUpdate, catchingUp,
                                             catchingUpWithNode, foldClient)
import           Convex.NodeClient.Resuming (resumingClient)
import           Convex.NodeClient.Types    (PipelinedLedgerStateClient)
import           Convex.Utils               (toShelleyPaymentCredential)
import           Convex.Utxos               (PrettyBalance (..),
                                             PrettyUtxoChange (..), UtxoSet,
                                             apply)
import qualified Convex.Utxos               as Utxos
import           Convex.Wallet.WalletState  (WalletState, chainPoint, utxoSet)
import qualified Convex.Wallet.WalletState  as WalletState
import           Data.Type.Equality         (testEquality, (:~:) (Refl))
import qualified Katip                      as K

data BalanceClientEnv era =
  BalanceClientEnv
    { bceFile  :: FilePath
    , bceState :: TVar (WalletState era)
    }

balanceClientEnv :: FilePath -> WalletState era -> IO (BalanceClientEnv era)
balanceClientEnv bceFile initialState =
  BalanceClientEnv bceFile <$> newTVarIO initialState

balanceClient :: forall era. C.IsCardanoEra era => K.LogEnv -> K.Namespace -> BalanceClientEnv era -> WalletState era -> C.PaymentCredential -> Env -> PipelinedLedgerStateClient
balanceClient logEnv ns clientEnv walletState wallet env =
  let cp = chainPoint walletState
      i  = catchingUpWithNode cp Nothing Nothing
  in resumingClient [cp] $ \_ ->
      foldClient
        (i, utxoSet walletState)
        NoLedgerStateArgs
        env
        (\c s upd (C.BlockInMode era' block) ->
              case testEquality era' (C.cardanoEra @era) of
                Just Refl -> applyBlock logEnv ns clientEnv wallet c s upd block
                _         -> pure Nothing
        )

{-| Apply a new block
-}
applyBlock :: C.IsCardanoEra era => K.LogEnv -> K.Namespace -> BalanceClientEnv era -> C.PaymentCredential -> CatchingUp -> (CatchingUp, UtxoSet C.CtxTx era ()) -> LedgerStateUpdate 'NoLedgerState -> C.Block era -> IO (Maybe (CatchingUp, UtxoSet C.CtxTx era ()))
applyBlock logEnv ns BalanceClientEnv{bceFile, bceState} wallet c (oldC, state) _ block = K.runKatipContextT logEnv () ns $ runMonadLogKatipT $ do
  let change = Utxos.extract_ (toShelleyPaymentCredential wallet) state block
      newUTxOs = apply state change
      C.Block header _ = block
      newState = WalletState.walletState newUTxOs header

  unless (Utxos.null change) $ do
    logInfo $ PrettyUtxoChange change
    logInfo $ PrettyBalance newUTxOs

  when (catchingUp oldC &&  not (catchingUp c)) $
    logInfoS "Caught up with node"

  unless (catchingUp c) $ do
    liftIO (WalletState.writeToFile bceFile newState)

  liftIO $ writeState bceState newState

  pure $ Just (c, newUTxOs)

writeState :: TVar (WalletState era) -> WalletState era -> IO ()
writeState tvar state = atomically (writeTVar tvar state)
