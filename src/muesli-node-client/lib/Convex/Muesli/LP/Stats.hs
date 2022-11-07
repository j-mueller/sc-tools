{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
module Convex.Muesli.LP.Stats(
  LPStats(..),
  outputsCreated,
  outputsSpent,
  fromEvent,

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
                                             OutputSpentEvent (..))
import           Convex.Muesli.LP.Constants (ScriptType (..))
import           Data.List                  (intercalate)

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

data LPStats =
  LPStats
    { _outputsCreated :: !ScriptCount
    , _outputsSpent   :: !ScriptCount
    }

makeLenses ''LPStats

instance Semigroup LPStats where
  l <> r =
    LPStats
      { _outputsCreated = _outputsCreated l <> _outputsCreated r
      , _outputsSpent   = _outputsSpent l <> _outputsSpent r
      }

instance Monoid LPStats where
  mempty = LPStats mempty mempty

fromScriptType :: ScriptType -> ScriptCount
fromScriptType st =
  let l = case st of
            BatchOrderScript -> batchOrderEvents
            FactoryMPS       -> factoryMintingEvents
            LPMPS            -> lpMintingEvents
            NFTMPS           -> nftMintingEvents
            PoolScript       -> poolEvents
  in mempty & over l succ

fromEvent :: Event ScriptType -> LPStats
fromEvent = \case
  AnOutputSpentEvent OutputSpentEvent{oseTxOutput=NewOutputEvent{neEvent}} ->
    mempty & outputsSpent .~ fromScriptType neEvent
  ANewOutputEvent NewOutputEvent{neEvent} ->
    mempty & outputsCreated .~ fromScriptType neEvent

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
prettyStats LPStats{_outputsCreated, _outputsSpent} =
  let x1 = prettyCount _outputsCreated
      x2 = prettyCount _outputsCreated
  in intercalate ", " [unwords ["Outputs created:", x1], unwords ["Outputs spent:", x2]]
