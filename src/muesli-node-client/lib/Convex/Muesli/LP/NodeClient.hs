{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}
module Convex.Muesli.LP.NodeClient(
  muesliClient
) where

import           Cardano.Api.Shelley        (BlockInMode, CardanoMode, Env,
                                             Lovelace (..), NetworkId,
                                             Quantity (..))
import           Control.Lens               (makeLenses, (&), (.~), (^.))
import           Control.Monad              (unless)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (execStateT)
import           Control.Monad.Trans.Maybe  (runMaybeT)
import qualified Convex.Constants           as Constants
import           Convex.Event               (NewOutputEvent (..),
                                             ResolvedInputs (..),
                                             TxWithEvents (..), extract)
import           Convex.Muesli.LP.Constants (ScriptType (..), scriptType)
import           Convex.Muesli.LP.Stats     (LPStats)
import qualified Convex.Muesli.LP.Stats     as Stats
import           Convex.Muesli.LP.Types     (prettyPair)
import           Convex.NodeClient.Fold     (CatchingUp (..), catchingUp,
                                             foldClient)
import           Convex.NodeClient.Resuming (resumingClient)
import           Convex.NodeClient.Types    (PipelinedLedgerStateClient)
import           Data.Foldable              (toList)
import qualified Data.Map                   as Map
import           Data.Maybe                 (mapMaybe)
import           Data.Ratio                 ((%))
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
  resumingClient [Constants.recent] $ \_ ->
    foldClient
      initialState
      env
      (applyBlock networkId)

applyBlock :: NetworkId -> CatchingUp -> ClientState -> BlockInMode CardanoMode -> IO (Maybe ClientState)
applyBlock _networkId (catchingUp -> isCatchingUp) oldState block = runMaybeT $ do
  let (newEvents, newResolvedInputs) = extract scriptType (oldState ^. resolvedInputs) block
      newStats =
        foldMap (foldMap Stats.fromEvent . toList . twEvents) newEvents
        <> Stats.fromResolvedInputs newResolvedInputs
      totalStats = (oldState ^. lpStats) <> newStats
      newState = oldState
                  & resolvedInputs .~ newResolvedInputs
                  & lpStats        .~ totalStats
  flip execStateT newState $ do
    logUnless isCatchingUp (unlines ["Total stats:", Stats.prettyStats totalStats])
    logUnless isCatchingUp (showPairs newResolvedInputs)

logUnless :: MonadIO m => Bool -> String -> m ()
logUnless w m = unless w (log m)

log :: MonadIO m => String -> m ()
log = liftIO . putStrLn

showPairs :: ResolvedInputs ScriptType -> String
showPairs (ResolvedInputs inputs) =
  let showPrice Nothing = ""
      showPrice (Just (Lovelace lvl, Quantity q)) =
        show @Double (fromRational $ lvl % q)
      mkEntry = \case
        PoolScript (Right (pair, vl)) -> Just (" " <> prettyPair pair <> ": " <> showPrice vl)
        PoolScript (Left err) -> Just (" " <> show err)
        _ -> Nothing

  in unlines
      $ mapMaybe mkEntry
      $ fmap (neEvent . snd)
      $ Map.toList inputs
