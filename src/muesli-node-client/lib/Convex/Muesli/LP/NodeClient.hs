{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module Convex.Muesli.LP.NodeClient(
  muesliClient
) where

import           Cardano.Api.Shelley        (BlockInMode, CardanoMode, Env,
                                             NetworkId)
import           Control.Lens               (makeLenses, (&), (.~), (^.))
import           Control.Monad              (unless)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (execStateT)
import           Control.Monad.Trans.Maybe  (runMaybeT)
import qualified Convex.Constants           as Constants
import           Convex.Event               (ResolvedInputs, TxWithEvents (..),
                                             extract)
import           Convex.Muesli.LP.Constants (ScriptType, scriptType)
import           Convex.Muesli.LP.Stats     (LPStats)
import qualified Convex.Muesli.LP.Stats     as Stats
import           Convex.NodeClient.Fold     (CatchingUp (..), catchingUp,
                                             foldClient)
import           Convex.NodeClient.Resuming (resumingClient)
import           Convex.NodeClient.Types    (PipelinedLedgerStateClient)
import           Data.Foldable              (toList)
import           Prelude                    hiding (log)

data ClientState =
  ClientState
    { _resolvedInputs :: !(ResolvedInputs ScriptType)
    , _lpStats        :: !LPStats
    }

makeLenses ''ClientState

initialState :: ClientState
initialState = ClientState mempty mempty

muesliClient :: NetworkId -> Env -> PipelinedLedgerStateClient
muesliClient networkId env =
  resumingClient [Constants.lessRecent] $ \_ ->
    foldClient
      initialState
      env
      (applyBlock networkId)

applyBlock :: NetworkId -> CatchingUp -> ClientState -> BlockInMode CardanoMode -> IO (Maybe ClientState)
applyBlock _networkId (catchingUp -> isCatchingUp) oldState block = runMaybeT $ do
  let (newEvents, newResolvedInputs) = extract scriptType (oldState ^. resolvedInputs) block
      newStats = foldMap (foldMap Stats.fromEvent . toList . twEvents) newEvents
      totalStats = (oldState ^. lpStats) <> newStats
      newState = oldState
                  & resolvedInputs .~ newResolvedInputs
                  & lpStats        .~ totalStats
  flip execStateT newState $ do
    -- logUnless (null newEvents) (unlines ["New stats:", Stats.prettyStats newStats])
    logUnless isCatchingUp (unlines ["Total stats:", Stats.prettyStats totalStats])

logUnless :: MonadIO m => Bool -> String -> m ()
logUnless w m = unless w (log m)

log :: MonadIO m => String -> m ()
log = liftIO . putStrLn
