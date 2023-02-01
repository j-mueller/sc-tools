{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE ViewPatterns       #-}
{-| A model of the on-chain state of a cardano app.
-}
module Convex.OnChainState(
  Extract(..),
  mkExtract,
  currentValue,
  toJSONState,
  fromJSONState,

  NewState(..),
  fromMaybe,
  applyTx,

  -- * On chain states
  UtxoSubset(..),
  utxoSet
) where

import           Cardano.Api         (BabbageEra, Block (..), BlockHeader (..),
                                      BlockInMode (..), BlockNo (..),
                                      CardanoMode, CtxTx, Hash, SlotNo, TxIn,
                                      TxOut)
import           Control.Applicative (liftA2)
import           Convex.Utxos        (AddressCredential, UtxoChange, UtxoSet)
import qualified Convex.Utxos        as Utxos
import           Data.Aeson          (FromJSON, Result (..), ToJSON (..), Value,
                                      fromJSON)
import           GHC.Generics        (Generic)

data NewState a = Changed a | Unchanged a
  deriving stock (Functor, Foldable, Traversable)

fromMaybe :: a -> Maybe a -> NewState a
fromMaybe (Unchanged -> k) = maybe k Changed

val :: NewState a -> a
val = \case
  Changed a   -> a
  Unchanged a -> a

instance Applicative NewState where
  pure = Unchanged

  l <*> r = case (l, r) of
    (Changed f, Changed x)     -> Changed (f x)
    (Unchanged f, Changed x)   -> Changed (f x)
    (Changed f, Unchanged x)   -> Changed (f x)
    (Unchanged f, Unchanged x) -> Unchanged (f x)

{-|
-}
data Extract m a =
  -- TODO: Make the constraints configurable (parameter c)

  forall s. (ToJSON s, FromJSON s) => Extract
    { apTx  :: s -> BlockInMode CardanoMode -> m (NewState s)
    , state :: !s
    , getA  :: s -> a
    }

{-| Making an @Extract@ value
-}
mkExtract :: (ToJSON a, FromJSON a) => a -> (a -> BlockInMode CardanoMode -> m (NewState a)) -> Extract m a
mkExtract state apTx = Extract apTx state id

currentValue :: Extract m a -> a
currentValue Extract{state, getA} = getA state

{-| Save the current state to a JSON value
-}
toJSONState :: Extract m a -> Value
toJSONState Extract{state} = toJSON state

{-| Try to parse the current state from a JSON value
-}
fromJSONState :: Value -> Extract m a -> Maybe (Extract m a)
fromJSONState vl (Extract a _s g) = case fromJSON vl of
  Success s -> Just (Extract a s g)
  _         -> Nothing

instance Functor (Extract m) where
  fmap f (Extract ap s g) = Extract ap s (fmap f g)

instance Applicative m => Applicative (Extract m) where
  pure x = Extract (\_ _ -> pure (Unchanged ())) () (const x)

  (Extract lA lC lG) <*> (Extract rA rC rG) =
    Extract
      { apTx = \s block -> liftA2 (,) <$> (lA (fst s) block) <*> (rA (snd s) block)
      , state = (lC, rC)
      , getA = \(s1, s2) -> (lG s1) (rG s2)
      }

{-| Apply a new transaction to an on-chain state and return the result
together with the updated state
-}
applyTx :: Functor m => Extract m a -> BlockInMode CardanoMode -> m (NewState a, Extract m a)
applyTx Extract{apTx, state, getA} block =
  let g k = (getA <$> k, Extract apTx (val k) getA)
  in g <$> apTx state block

{-| Subset of the UTXO set, with information on when it was last changed
-}
data UtxoSubset a =
  UtxoSubset
    { ubcUtxo          :: UtxoSet CtxTx a
    , ubcLastChange    :: UtxoChange CtxTx a
    , ubcLastBlockHash :: Maybe (Hash BlockHeader)
    , ubcLastSlotNo    :: Maybe SlotNo
    , ubcLastBlockNo   :: Maybe Integer
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)

{-| Set of utxos that share a payment credential.
-}
utxoSet :: (ToJSON a, FromJSON a, Applicative m) => (TxIn -> TxOut CtxTx BabbageEra -> Maybe a) -> AddressCredential -> Extract m (UtxoSubset a)
utxoSet ex cred =
  let state = UtxoSubset mempty mempty Nothing Nothing Nothing
      apTx old@UtxoSubset{ubcUtxo=oldUtxo} block = do
        let BlockInMode (Block (BlockHeader (Just -> ubcLastSlotNo) (Just -> ubcLastBlockHash) (BlockNo (Just . fromIntegral -> ubcLastBlockNo))) _) _ = block
            i = Utxos.extract ex cred oldUtxo block
        if (Utxos.null i)
          then pure (Unchanged old)
          else pure (Changed UtxoSubset{ubcUtxo = Utxos.apply oldUtxo i, ubcLastChange = i, ubcLastBlockHash, ubcLastSlotNo, ubcLastBlockNo})
      getA = id
  in Extract{apTx , state, getA}
