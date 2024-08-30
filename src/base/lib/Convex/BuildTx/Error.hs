{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-| Validation and lookup errors for transaction building
-}
module Convex.BuildTx.Error(
  BuildTxError(..),
  resolveTxIn
) where

import qualified Cardano.Api                        as C
import           Control.Monad.Except               (MonadError (..))
import           Convex.BuildTx.BodyContentResolved (AnyOutput)
import           Convex.Class                       (MonadBlockchain (..), singleUTxO)

{-| Errors that can be encountered when building transactions
-}
data BuildTxError =
  FailedToResolveInput{ txIn :: C.TxIn } -- ^ Failed to resolve the given input
  | ScriptOutputKeyWitness{ txIn :: C.TxIn, txOut :: AnyOutput } -- ^ Attempting to spend a script output with a public key witness
  | KeyOutputScriptWitness{ txIn :: C.TxIn, txOut :: AnyOutput } -- ^ Attempting to spend a key output with a script witness

{-| Resolve the input
-}
resolveTxIn :: (MonadError BuildTxError m, MonadBlockchain m) => C.TxIn -> m AnyOutput
resolveTxIn txIn = singleUTxO txIn >>= \case
  Just o -> pure (C.inAnyCardanoEra C.BabbageEra o)
  Nothing -> throwError FailedToResolveInput{txIn}
