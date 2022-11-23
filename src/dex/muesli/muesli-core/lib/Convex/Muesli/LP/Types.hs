{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-| Types for muesli LP pools
-}
module Convex.Muesli.LP.Types(
  Pair,
  PairFromOutputError(..),
  mkPair,
  adaPair,
  prettyPair,
  pairFromValue,
  pairFromOutput,

  -- * Orders
  BuyOrder(..),
  SellOrder(..),
  Price(..),
  scale,
  valueOf,
  unitPrice,
  unitsOf
  ) where

import           Cardano.Api                    (AssetId (..), AssetName,
                                                 Lovelace (..), PolicyId,
                                                 Quantity (..), Value)
import qualified Cardano.Api.Shelley            as C
import qualified Cardano.Crypto.Hash.Class      as CCHC
import qualified Cardano.Ledger.Hashes          as CLH
import           Convex.Muesli.LP.OnChain.Types (PoolDatum (..))
import           Data.ByteString                (ByteString)
import           Data.Ratio                     ((%))
import qualified Plutus.V2.Ledger.Api           as V2
import           PlutusTx.IsData.Class          (fromData)

{-| A unordered pair of currencies. Use the 'mkPair' constructor.
-}
data Pair = Pair{ a1 :: AssetId, a2 :: AssetId }
  deriving (Eq, Ord, Show)

prettyPair :: Pair -> String
prettyPair Pair{a1, a2} = show a1 <> "/" <> show a2

{-| A 'Pair' of two asset IDs
-}
mkPair :: (AssetId, Quantity) -> (AssetId, Quantity) -> (Pair, Quantity, Quantity)
mkPair a1 a2 =
  let ((x1, x2), (y1, y2)) = if (fst a1 <= fst a2) then (a1, a2) else (a2, a1)
  in (Pair x1 y1, x2, y2)

data PairFromOutputError =
  WrongTxDatum
  | FromDataFailed
  | PatternMatchFailed
  | HashFromBytesFailed
  deriving (Eq, Ord, Show)

pairFromOutput :: C.TxOut C.CtxTx C.BabbageEra -> Either PairFromOutputError (Pair, Maybe (Lovelace, Quantity))
pairFromOutput (C.TxOut _ (C.TxOutValue C.MultiAssetInBabbageEra vl) datum _) = do
  sd <- maybe (Left WrongTxDatum) Right $ case datum of
          C.TxOutDatumInTx C.ScriptDataInBabbageEra sd -> Just sd
          C.TxOutDatumInline _ sd                      -> Just sd
          _                                            -> Nothing
  PoolDatum{pdCoinA, pdCoinB} <- maybe (Left FromDataFailed) Right (fromData (C.toPlutusData sd))
  a <- uncurry fromPlutusAssetId pdCoinA
  b <- uncurry fromPlutusAssetId pdCoinB
  pure (pairFromValue vl a b)
pairFromOutput _ = Left PatternMatchFailed

{-| Extract a 'Pair' from a value. Returns 'Nothing' if there are not exactly
  1 or 2 asset IDs with non-zero quantities in the value.
-}
pairFromValue :: Value -> AssetId -> AssetId -> (Pair, Maybe (Lovelace, Quantity))
pairFromValue value k l =
  let kN = C.selectAsset value k
      lN = C.selectAsset value l
      (pair, Quantity q1, q2) = mkPair (k, kN) (l, lN)
      adaPrice =
        case a1 pair of
          C.AdaAssetId -> Just (Lovelace q1, q2)
          _            -> Nothing
  in (pair, adaPrice)

{-| The other side of the Ada pair (if one of the two currencies is Ada)
-}
adaPair :: Pair -> Maybe (PolicyId, AssetName)
adaPair Pair{a1=AdaAssetId, a2=AssetId p a} = Just (p, a)
adaPair Pair{a2=AdaAssetId, a1=AssetId p a} = Just (p, a)
adaPair _                                   = Nothing

fromPlutusAssetId :: V2.CurrencySymbol -> V2.TokenName -> Either PairFromOutputError AssetId
fromPlutusAssetId (V2.CurrencySymbol s) (V2.TokenName nm')
  | s == (V2.toBuiltin @ByteString "") = Right C.AdaAssetId
  | otherwise = do
      polId <- C.PolicyId . C.fromShelleyScriptHash . CLH.ScriptHash <$> maybe (Left HashFromBytesFailed) Right (CCHC.hashFromBytes $ V2.fromBuiltin s)
      let nm = C.AssetName $ V2.fromBuiltin nm'
      pure (C.AssetId polId nm)

data BuyOrder = BuyOrder{ buyCurrency :: (PolicyId, AssetName), buyQuantity :: Quantity, buyPrice :: Price }
  deriving (Eq, Ord, Show)

data SellOrder = SellOrder{ sellCurrency :: (PolicyId, AssetName), sellQuantity :: Quantity, sellPrice :: Price }
  deriving (Eq, Ord, Show)

-- | Price of one unit of an asset in Lovelace
newtype Price = Price Rational
  deriving stock (Eq, Ord, Show)
  deriving newtype Num

-- | Multiply the price by a scalar value
scale :: Rational -> Price -> Price
scale n (Price p) = Price (n * p)

-- | Value of a quantity priced in Ada
valueOf :: Quantity -> Price -> Lovelace
valueOf (Quantity q) (Price p) =
  Lovelace $ round $ fromIntegral q * p

{-| Price of one unit of the native token in Ada
-}
unitPrice :: Quantity -> Lovelace -> Price
unitPrice (Quantity q) (Lovelace l) = Price (l % q)

-- | Largest 'Quantity' whose Ada value is smaller than or
--   equal to the given amount
unitsOf :: Lovelace -> Price -> Quantity
unitsOf (Lovelace l) (Price p) =
  Quantity $ floor $ fromIntegral l / p
