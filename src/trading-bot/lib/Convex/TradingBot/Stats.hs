{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
module Convex.TradingBot.Stats(
  LPStats(..),
  outputsCreated,
  outputsSpent,
  outputCount,
  fromEvent,
  fromResolvedInputs,

  -- * Output counts
  OutputCount(..),
  numLPPools,
  numOrderBookOutputs,

  -- * Script counts
  ScriptCount(..),
  prettyStats,
  fromScriptType,
  poolEvents
) where

import           Control.Lens                  (makeLenses, over, (&), (.~))
import           Convex.Event                  (Event (..), NewOutputEvent (..),
                                                OutputSpentEvent (..),
                                                ResolvedInputs (..))
import           Convex.TradingBot.LPPoolEvent (LPPoolEvent (..),
                                                OrderbookEvent)
import           Data.Either                   (isRight)
import           Data.List                     (intercalate)
import qualified Data.Map                      as Map

data ScriptCount =
  ScriptCount
    { _poolEvents      :: !Int
    , _orderBookEvents :: !Int
    }

makeLenses ''ScriptCount

instance Semigroup ScriptCount where
  l <> r =
    ScriptCount
      { _poolEvents = _poolEvents l + _poolEvents r
      , _orderBookEvents = _orderBookEvents l + _orderBookEvents r
      }

instance Monoid ScriptCount where
  mempty = ScriptCount 0 9

data OutputCount =
  OutputCount
    { _numLPPools          :: !Int
    , _numOrderBookOutputs :: !Int
    }

makeLenses ''OutputCount

instance Semigroup OutputCount where
  _ <> r = r

data LPStats =
  LPStats
    { _outputsCreated :: !ScriptCount
    , _outputsSpent   :: !ScriptCount
    , _outputCount    :: !(Maybe OutputCount)
    }

makeLenses ''LPStats

instance Semigroup LPStats where
  l <> r =
    LPStats
      { _outputsCreated = _outputsCreated l <> _outputsCreated r
      , _outputsSpent   = _outputsSpent l <> _outputsSpent r
      , _outputCount    = _outputCount l <> _outputCount r
      }

instance Monoid LPStats where
  mempty = LPStats mempty mempty mempty

fromScriptType :: (Either LPPoolEvent OrderbookEvent) -> ScriptCount
fromScriptType st =
  let l = case st of
            Left LPPoolEvent{} -> poolEvents
            Right _            -> orderBookEvents
  in mempty & over l succ

fromEvent :: Event (Either LPPoolEvent OrderbookEvent) -> LPStats
fromEvent = \case
  AnOutputSpentEvent OutputSpentEvent{oseTxOutput=NewOutputEvent{neEvent}} ->
    mempty & outputsSpent .~ fromScriptType neEvent
  ANewOutputEvent NewOutputEvent{neEvent} ->
    mempty & outputsCreated .~ fromScriptType neEvent

fromResolvedInputs :: ResolvedInputs (Either LPPoolEvent OrderbookEvent) -> LPStats
fromResolvedInputs (ResolvedInputs mp) =
  let (lp, orderBook) = Map.partition (isRight . neEvent) mp
  in mempty
      & outputCount .~ Just (OutputCount (Map.size lp) (Map.size orderBook))

prettyCount :: ScriptCount -> String
prettyCount ScriptCount{_poolEvents} =
  intercalate ", "
    $ fmap (\(n, s) -> unwords [n <> ":", show s])
    $ filter (\(_, s) -> s > 0)
    $ [ ("Pool script", _poolEvents)
      ]

prettyStats :: LPStats -> String
prettyStats LPStats{_outputsCreated, _outputsSpent, _outputCount} =
  let x1 = prettyCount _outputsCreated
      x2 = prettyCount _outputsCreated
      x3 = case _outputCount of
            Just (OutputCount i j) | i+j > 0 -> "LPs: " <> show i <> ", orders: " <> show j
            _                            -> ""
  in intercalate ", "
      $ fmap (\(n, s) -> unwords [n, s])
      $ filter (not . null . snd)
      $ [ ("Outputs created:", x1)
        , ("Outputs spent:", x2)
        , ("Active outputs:", x3)
        ]
