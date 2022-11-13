{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE NamedFieldPuns #-}
module Convex.Muesli.Orderbook.Match(
  Match(..),
  Valid(..),
  matchValue,
  validateMatch,
  getOfferValue,
  firstOrderPlaced,
  matchFromOutputSpentEvents
--   valuePaid
  ) where

import qualified Cardano.Api                        as C
import qualified Cardano.Api.Shelley                as C
import           Convex.Event                       (NewOutputEvent (..),
                                                     OutputSpentEvent (..))
import           Convex.Muesli.Orderbook.Constants  (MuesliVersion)
import           Convex.Muesli.Orderbook.KnownOrder (KnownOrder (..),
                                                     knownOrderFromMetadata,
                                                     orderTxOut)
import qualified Convex.Muesli.Orderbook.KnownOrder as KnownOrder
import           Data.Validation                    (Validation (..))

data Match =
  Match
    { orderOne      :: KnownOrder
    , orderOneEvent :: NewOutputEvent MuesliVersion
    , orderTwo      :: KnownOrder
    , orderTwoEvent :: NewOutputEvent MuesliVersion
    } deriving Show

newtype Valid m = Valid{getValid :: m}
  deriving (Show)

{-| The 'Value' left over after the match has been made
-}
matchValue :: C.ProtocolParameters -> Match -> C.Value
matchValue params Match{orderOne, orderOneEvent, orderTwo, orderTwoEvent} =
  let output1 = orderTxOut params orderOne
      output2 = orderTxOut params orderTwo
      input1 = neOutput orderOneEvent
      input2 = neOutput orderTwoEvent
      outputVal' = outputVal . C.fromShelleyTxOut C.ShelleyBasedEraBabbage

  in outputVal' input1
      <> outputVal' input2
      <> C.negateValue (outputVal output1 <> outputVal output2)

outputVal :: C.TxOut ctx era -> C.Value
outputVal (C.TxOut _ (C.TxOutValue _ vl) _ _) = vl
outputVal _                                   = mempty

-- valuePaid :: Match -> C.Value
-- valuePaid Match{orderOne, orderTwo} =
--   let output1 = orderTxOut orderOne
--       output2 = orderTxOut orderTwo
--   in outputVal output1
--       <> outputVal output2
--       <> C.negateValue (C.valueFromList [(C.AdaAssetId, C.lovelaceToQuantity (2 * fromIntegral Constants.minDepositLovelace))])

getOfferValue :: NewOutputEvent a -> C.Value
getOfferValue event@NewOutputEvent{neOutput} =
  let lovelace = orderFees event
  in case C.fromShelleyTxOut C.ShelleyBasedEraBabbage neOutput of
      C.TxOut _ (C.TxOutValue _ offerValue') _ _ ->
        C.valueFromList
        $ filter (\(_, q) -> q > 0)
        $ C.valueToList
        $ offerValue' <> C.negateValue (C.valueFromList [(C.AdaAssetId, C.lovelaceToQuantity lovelace)])
      _ -> mempty

validateBidAgainstAsk :: KnownOrder -> NewOutputEvent a -> Validation [String] ()
validateBidAgainstAsk KnownOrder{orderAsk=(c, q)} event =
  let orderBid = C.selectAsset (getOfferValue event) c in
  if orderBid < q
    then Failure ["offer value too low"]
    else pure ()

validateMatch :: Match -> Validation [String] (Valid Match)
validateMatch m@Match{orderOne, orderOneEvent, orderTwo, orderTwoEvent} = do
  _ <- KnownOrder.validateKnownOrder orderOne orderOneEvent
  _ <- KnownOrder.validateKnownOrder orderTwo orderTwoEvent
  _ <- validateBidAgainstAsk orderOne orderTwoEvent
  _ <- validateBidAgainstAsk orderTwo orderOneEvent
  pure (Valid m)

{-| The fees included in an order output, consisting of minimum deposit and matchmaker fee
-}
orderFees :: NewOutputEvent a -> C.Lovelace
orderFees NewOutputEvent{neTxMetadata} =
  -- TODO: Should we also consider minDepositLovelace here? If so, how to compute?
  either (const 0) id $ KnownOrder.extractLovelaceFee neTxMetadata

-- | The slot in which the first order was placed
firstOrderPlaced :: Match -> C.SlotNo
firstOrderPlaced Match{orderOneEvent, orderTwoEvent} =
  min (neSlot orderOneEvent) (neSlot orderTwoEvent)

-- | Make a match from two output spent events
matchFromOutputSpentEvents :: OutputSpentEvent MuesliVersion -> OutputSpentEvent MuesliVersion -> Maybe Match
matchFromOutputSpentEvents OutputSpentEvent{oseTxOutput=orderOneEvent} OutputSpentEvent{oseTxOutput=orderTwoEvent} = do
  let extractOrder = either (const Nothing) Just . knownOrderFromMetadata . neTxMetadata
  Match
    <$> extractOrder orderOneEvent
    <*> pure orderOneEvent
    <*> extractOrder orderTwoEvent
    <*> pure orderTwoEvent
