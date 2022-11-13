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

  -- * Script counts
  ScriptCount(..),
  prettyStats,
  fromScriptType,
  batchOrderEvents,
  factoryMintingEvents,
  nftMintingEvents,
  lpMintingEvents,
  poolEvents
) where

import           Control.Lens               (makeLenses, over, (&), (.~))
import           Convex.Event               (Event (..), NewOutputEvent (..),
                                             OutputSpentEvent (..),
                                             ResolvedInputs (..))
import           Convex.Muesli.LP.Constants (ScriptType (..))
import           Data.List                  (intercalate)
import qualified Data.Map                   as Map

data ScriptCount =
  ScriptCount
    { _batchOrderEvents     :: !Int
    , _factoryMintingEvents :: !Int
    , _nftMintingEvents     :: !Int
    , _lpMintingEvents      :: !Int
    , _poolEvents           :: !Int
    }

makeLenses ''ScriptCount

instance Semigroup ScriptCount where
  l <> r =
    ScriptCount
      { _batchOrderEvents = _batchOrderEvents l + _batchOrderEvents r
      , _factoryMintingEvents = _factoryMintingEvents l + _factoryMintingEvents r
      , _nftMintingEvents = _nftMintingEvents l + _nftMintingEvents r
      , _lpMintingEvents = _lpMintingEvents l + _lpMintingEvents r
      , _poolEvents = _poolEvents l + _poolEvents r
      }

instance Monoid ScriptCount where
  mempty = ScriptCount 0 0 0 0 0

data OutputCount =
  OutputCount
    { _numLPPools :: !Int
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

fromScriptType :: ScriptType -> ScriptCount
fromScriptType st =
  let l = case st of
            BatchOrderScript -> batchOrderEvents
            FactoryMPS       -> factoryMintingEvents
            LPMPS            -> lpMintingEvents
            NFTMPS           -> nftMintingEvents
            PoolScript{}     -> poolEvents
  in mempty & over l succ

fromEvent :: Event ScriptType -> LPStats
fromEvent = \case
  AnOutputSpentEvent OutputSpentEvent{oseTxOutput=NewOutputEvent{neEvent}} ->
    mempty & outputsSpent .~ fromScriptType neEvent
  ANewOutputEvent NewOutputEvent{neEvent} ->
    mempty & outputsCreated .~ fromScriptType neEvent

fromResolvedInputs :: ResolvedInputs ScriptType -> LPStats
fromResolvedInputs (ResolvedInputs mp) =
  mempty & outputCount .~ Just (OutputCount $ Map.size mp)

prettyCount :: ScriptCount -> String
prettyCount ScriptCount{_batchOrderEvents, _factoryMintingEvents, _nftMintingEvents, _lpMintingEvents, _poolEvents} =
  intercalate ", "
    $ fmap (\(n, s) -> unwords [n <> ":", show s])
    $ filter (\(_, s) -> s > 0)
    $ [ ("Batch orders", _batchOrderEvents)
      , ("Factory minting", _factoryMintingEvents)
      , ("LP minting", _lpMintingEvents)
      , ("NFT minting", _nftMintingEvents)
      , ("Pool script", _poolEvents)
      ]

prettyStats :: LPStats -> String
prettyStats LPStats{_outputsCreated, _outputsSpent, _outputCount} =
  let x1 = prettyCount _outputsCreated
      x2 = prettyCount _outputsCreated
      x3 = case _outputCount of
            Just (OutputCount i) | i > 0 -> show i
            _                            -> ""
  in intercalate ", "
      $ fmap (\(n, s) -> unwords [n, s])
      $ filter (not . null . snd)
      $ [ ("Outputs created:", x1)
        , ("Outputs spent:", x2)
        , ("Active outputs:", x3)
        ]
