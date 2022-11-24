{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}
module Convex.Muesli.LP.BuildTx(
  -- * Buy orders
  buyOrder,
  cancelBuyOrder,
  mkBuyOrderDatum,
  buyOrderFromScriptData,

  -- * Sell orders
  sellOrder,
  mkSellOrderDatum,
  -- sellOrderFromScriptData,

  -- * Etc.
  encodeAddress,
  decodeAddress,
  matchRedeemer,
  scriptAddress
  ) where

import           Cardano.Api.Shelley        (ScriptData (..))
import qualified Cardano.Api.Shelley        as C
import           Control.Lens               (_1, at, over, set)
import           Convex.BuildTx             (TxBuild, setScriptsValid)
import qualified Convex.Lenses              as L
import qualified Convex.Muesli.LP.Constants as Constants
import           Convex.Muesli.LP.Types     (BuyOrder (..), SellOrder (..),
                                             unitPrice, valueOf)
import           Data.Map                   (Map)
import           Data.Proxy                 (Proxy (..))
import qualified Data.Text                  as Text
import           Data.Word                  (Word64)

-- TODO:
-- consider frontend fee (for orders >= 100 Ada): 1 Ada + 5% of Ada amount

{-| Place a limit buy order on Muesliswap orderbook v3 for the given amount of native tokens
-}
buyOrder :: C.AddressInEra C.BabbageEra -> Maybe C.NetworkId -> BuyOrder -> TxBuild
buyOrder returnAddress (fmap C.toShelleyNetwork -> network) order =
  let val = C.TxOutValue C.MultiAssetInBabbageEra (C.lovelaceToValue $ muesliBuyOrderOutputLovelace order)
      addr = maybe scriptAddress (\n -> set (L._AddressInEra . L._Address . _1) n scriptAddress) network
      dat = C.TxOutDatumInTx C.ScriptDataInBabbageEra (mkBuyOrderDatum returnAddress order)
      txo = C.TxOut addr val dat C.ReferenceScriptNone
  in over L.txOuts ((:) txo)
      . over (L.txMetadata . L._TxMetadata) (setBuyOrderMetadata returnAddress order)

muesliBuyOrderOutputLovelace :: BuyOrder -> C.Lovelace
muesliBuyOrderOutputLovelace order@BuyOrder{buyPrice, buyQuantity} =
  let lovelace      = valueOf buyQuantity buyPrice
  in lovelace + muesliDeposit + muesliBuyOrderFee order

muesliDeposit :: C.Lovelace
muesliDeposit = C.Lovelace 1_700_000

muesliBuyOrderFee :: BuyOrder -> C.Lovelace
muesliBuyOrderFee _ =
  let matchmakerFee = C.Lovelace   950_000
      frontendFee   = C.Lovelace         0 -- FIXME
  in matchmakerFee + frontendFee

muesliSellOrderFee :: SellOrder -> C.Lovelace
muesliSellOrderFee _ =
  let matchmakerFee = C.Lovelace   950_000
      frontendFee   = C.Lovelace         0 -- FIXME
  in matchmakerFee + frontendFee

setBuyOrderMetadata :: C.AddressInEra C.BabbageEra -> BuyOrder -> Map Word64 C.TxMetadataValue -> Map Word64 C.TxMetadataValue
setBuyOrderMetadata returnAddress order@BuyOrder{buyCurrency, buyQuantity = C.Quantity q} =
  let C.Lovelace l = muesliBuyOrderOutputLovelace order
  in set (at 674) (Just $ C.TxMetaMap [(C.TxMetaText "msg", C.TxMetaList [C.TxMetaText "MuesliSwap Place Order"])])
      . set (at 1000) (Just $ C.TxMetaBytes $ C.serialiseToRawBytes returnAddress)
      . set (at 1002) (Just $ C.TxMetaText $ C.serialiseToRawBytesHexText $ fst buyCurrency)
      . set (at 1003) (Just $ C.TxMetaText $ C.serialiseToRawBytesHexText $ snd buyCurrency)
      . set (at 1004) (Just $ C.TxMetaText $ Text.pack $ show q) -- not sure why this is a text and not a number
      . set (at 1005) (Just $ C.TxMetaNumber l)
      . set (at 1007) (Just $ C.TxMetaNumber 1) -- ?? allow partial match?
      . set (at 1008) (Just $ C.TxMetaText "") -- Ada policy ID
      . set (at 1009) (Just $ C.TxMetaText "") -- Ada asset name

sellOrder :: C.AddressInEra C.BabbageEra -> Maybe C.NetworkId -> SellOrder -> TxBuild
sellOrder returnAddress (fmap C.toShelleyNetwork -> network) order@SellOrder{sellCurrency, sellQuantity} =
  let val = C.TxOutValue C.MultiAssetInBabbageEra
              $ (C.valueFromList [(C.AssetId (fst sellCurrency) (snd sellCurrency), sellQuantity)])
                <> C.lovelaceToValue (muesliSellOrderFee order + muesliDeposit)
      addr = maybe scriptAddress (\n -> set (L._AddressInEra . L._Address . _1) n scriptAddress) network
      dat = C.TxOutDatumInTx C.ScriptDataInBabbageEra (mkSellOrderDatum returnAddress order)
      txo = C.TxOut addr val dat C.ReferenceScriptNone
  in over L.txOuts ((:) txo)
      . over (L.txMetadata . L._TxMetadata) (setSellOrderMetadata returnAddress order)

setSellOrderMetadata :: C.AddressInEra C.BabbageEra -> SellOrder -> Map Word64 C.TxMetadataValue -> Map Word64 C.TxMetadataValue
setSellOrderMetadata returnAddress order@SellOrder{sellCurrency=(policyId, assetName), sellQuantity, sellPrice} =
  let C.Lovelace outputLovelace = muesliSellOrderFee order + muesliDeposit
      C.Lovelace price          = valueOf sellQuantity sellPrice
  in set (at 674) (Just $ C.TxMetaMap [(C.TxMetaText "msg", C.TxMetaList [C.TxMetaText "MuesliSwap Place Order"])])
      . set (at 1000) (Just $ C.TxMetaBytes $ C.serialiseToRawBytes returnAddress)
      . set (at 1002) (Just $ C.TxMetaText "") -- Ada policy ID
      . set (at 1003) (Just $ C.TxMetaText "") -- Ada asset name
      . set (at 1004) (Just $ C.TxMetaNumber price)
      . set (at 1005) (Just $ C.TxMetaNumber outputLovelace)
      . set (at 1007) (Just $ C.TxMetaNumber 1) -- ?? allow partial match?
      . set (at 1008) (Just $ C.TxMetaText $ C.serialiseToRawBytesHexText policyId)
      . set (at 1009) (Just $ C.TxMetaText $ C.serialiseToRawBytesHexText assetName)

cancelBuyOrder :: C.AddressInEra C.BabbageEra -> C.TxIn -> BuyOrder -> TxBuild
cancelBuyOrder returnAddress txIn (mkBuyOrderDatum returnAddress -> dat) =
  let red = cancelRedeemer
      wit = C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 (C.PScript Constants.orderBookV3) (C.ScriptDatumForTxIn dat) red (C.ExecutionUnits 0 0)
      wit' = C.BuildTxWith (C.ScriptWitness C.ScriptWitnessForSpending wit)
  in over L.txIns ((txIn, wit') :) . setScriptsValid

-- script hash
-- 00fb107bfbd51b3a5638867d3688e986ba38ff34fb738f5bd42b20d5
-- output address:

-- datum hash
-- f1dd826663bc492b29466aa6cf68e5f9c713bf2881325c591afc7cd85e929e96
-- datum
-- d8799fd8799fd8799fd8799f581cab2c5ac05201927833174815d93c35cb1f8c3e2d80efd04ce6648073ffd8799fd8799fd8799f581c83c16e0ebacaf76702b6b4840e19f2c8ff35b3077387d8b14f2cd49effffffff581c8a1cfae21368b8bebbbed9800fec304e95cce39a2a57dc35e2e3ebaa444d494c4b404008d87a801a00286f90ffff

-- redeemer
-- d87a9f00ff

-- redeeming txn:
-- https://cardanoscan.io/transaction/58259eec76e100dbaf48776b8815282323d6111cbc3d720fbf709f5ee525021b

-- AdaAssetId,22650000

{-| Encode a @cardano-api@ address in 'ScriptData', using a format compatible with the 'ToData' instance
of 'Plutus.V1.Ledger.Address.Address'
-}
encodeAddress :: C.AddressInEra C.BabbageEra -> C.ScriptData
encodeAddress = \case
  C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) (C.ShelleyAddress _network (C.fromShelleyPaymentCredential -> C.PaymentCredentialByKey payment) (C.fromShelleyStakeReference -> C.StakeAddressByValue (C.StakeCredentialByKey stakeRef))) ->
    ScriptDataConstructor 0
      [ ScriptDataConstructor 0
        [ScriptDataBytes (C.serialiseToRawBytes payment)]
      , ScriptDataConstructor 0
        [ScriptDataConstructor 0 [ScriptDataConstructor 0 [ScriptDataBytes (C.serialiseToRawBytes stakeRef)]]]
      ]
  C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) (C.ShelleyAddress _network (C.fromShelleyPaymentCredential -> C.PaymentCredentialByKey payment) (C.fromShelleyStakeReference -> C.NoStakeAddress)) ->
    ScriptDataConstructor 0
      [ ScriptDataConstructor 0
        [ScriptDataBytes (C.serialiseToRawBytes payment)]
      , ScriptDataConstructor 0
        [ScriptDataConstructor 1 []]
      -- , ScriptDataConstructor 1 []
      ]
  x -> error $ "encodeAddress: Address not supported: " <> show x

decodeAddress :: C.NetworkId -> C.ScriptData -> Maybe (C.AddressInEra C.BabbageEra)
decodeAddress networkId = \case
  ScriptDataConstructor 0
    [ ScriptDataConstructor 0
      [ScriptDataBytes (C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @(C.Hash C.PaymentKey)) -> Just payment)]
    , ScriptDataConstructor 0
      [ScriptDataConstructor 0 [ScriptDataConstructor 0 [ScriptDataBytes (C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @(C.Hash C.StakeKey)) -> Just stakeRef)]]]
    ] -> Just (C.shelleyAddressInEra (C.makeShelleyAddress networkId (C.PaymentCredentialByKey payment) (C.StakeAddressByValue $ C.StakeCredentialByKey stakeRef)))
  ScriptDataConstructor 0
    [ ScriptDataConstructor 0
      [ScriptDataBytes (C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @(C.Hash C.PaymentKey)) -> Just payment)]
    , ScriptDataConstructor 0
      [ScriptDataConstructor 1 []]
    ] -> Just (C.shelleyAddressInEra (C.makeShelleyAddress networkId (C.PaymentCredentialByKey payment) C.NoStakeAddress))
  _ -> Nothing

{-|
-}
mkSellOrderDatum :: C.AddressInEra C.BabbageEra -> SellOrder -> C.ScriptData
mkSellOrderDatum returnAddress order@SellOrder{sellCurrency=(policyId, assetName), sellQuantity, sellPrice} =
  let policyBS = C.serialiseToRawBytes policyId
      assetBS  = C.serialiseToRawBytes assetName
      C.Lovelace fees  = muesliSellOrderFee order + muesliDeposit
      C.Lovelace price = valueOf sellQuantity sellPrice - (muesliSellOrderFee order)
  in ScriptDataConstructor 0
      [ ScriptDataConstructor 0
        [ encodeAddress returnAddress
        , ScriptDataBytes "" -- Ada Policy ID
        , ScriptDataBytes "" -- Ada Asset ID
        , ScriptDataBytes policyBS
        , ScriptDataBytes assetBS
        , ScriptDataNumber price
        , ScriptDataConstructor 1 [] -- ?? Allow partial matches?
        , ScriptDataNumber fees
        ]
      ]

{-| Datum for a limit BUY order
-}
mkBuyOrderDatum :: C.AddressInEra C.BabbageEra -> BuyOrder -> C.ScriptData
mkBuyOrderDatum returnAddress order@BuyOrder{buyCurrency=(policyId, assetName), buyQuantity = C.Quantity q} =
  let policyBS = C.serialiseToRawBytes policyId
      assetBS = C.serialiseToRawBytes assetName
      C.Lovelace l = muesliDeposit + muesliBuyOrderFee order
  in ScriptDataConstructor 0
      [ ScriptDataConstructor 0
        [ encodeAddress returnAddress
        , ScriptDataBytes policyBS
        , ScriptDataBytes assetBS
        , ScriptDataBytes "" -- Ada Policy ID
        , ScriptDataBytes "" -- Ada Asset ID
        , ScriptDataNumber q
        , ScriptDataConstructor 1 [] -- ?? Allow partial matches?
        , ScriptDataNumber l
        ]
      ]

buyOrderFromScriptData ::
  -- | Network ID
  C.NetworkId ->
  -- | Total lovelace locked in the buy order output
  C.Lovelace ->
  -- | Output datum
  C.ScriptData ->
  Maybe BuyOrder
buyOrderFromScriptData network totalLovelace = \case
  ScriptDataConstructor 0
      [ ScriptDataConstructor 0
        [ (decodeAddress network -> Just{})
        , ScriptDataBytes (C.deserialiseFromRawBytes (C.proxyToAsType Proxy) -> Just policyId)
        , ScriptDataBytes (C.deserialiseFromRawBytes (C.proxyToAsType Proxy) -> Just assetName)
        , ScriptDataBytes "" -- Ada Policy ID
        , ScriptDataBytes "" -- Ada Asset ID
        , ScriptDataNumber (C.Quantity -> buyQuantity)
        , ScriptDataConstructor 1 [] -- ?? Allow partial matches?
        , ScriptDataNumber (C.Lovelace -> feeAndDeposit)
        ]
      ] ->
        Just BuyOrder{buyCurrency=(policyId, assetName), buyQuantity, buyPrice = unitPrice buyQuantity (totalLovelace - feeAndDeposit) }
  _ -> Nothing

matchRedeemer :: C.ScriptData
matchRedeemer = ScriptDataConstructor 1 [ScriptDataNumber 0]

cancelRedeemer :: C.ScriptData
cancelRedeemer = ScriptDataConstructor 2 []

scriptAddress :: C.AddressInEra C.BabbageEra
scriptAddress = maybe (error "") id $ C.deserialiseAddress (C.proxyToAsType Proxy) "addr1zyq0kyrml023kwjk8zr86d5gaxrt5w8lxnah8r6m6s4jp4g3r6dxnzml343sx8jweqn4vn3fz2kj8kgu9czghx0jrsyqqktyhv"
--                                                   https://cardanoscan.io/address/addr1zyq0kyrml023kwjk8zr86d5gaxrt5w8lxnah8r6m6s4jp4g3r6dxnzml343sx8jweqn4vn3fz2kj8kgu9czghx0jrsyqqktyhv
