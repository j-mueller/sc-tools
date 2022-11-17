{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
module Convex.Muesli.LP.BuildTx(
  LimitBuyOrder(..),
  buyOrder,
  cancelOrder,
  mkBuyOrderDatum,
  buyOrderFromScriptData,
  encodeAddress,
  decodeAddress,
  matchRedeemer,
  scriptAddress
  ) where

import           Cardano.Api.Shelley        (ScriptData (..))
import qualified Cardano.Api.Shelley        as C
import           Control.Lens               (at, over, set)
import           Convex.BuildTx             (TxBuild, setScriptsValid)
import qualified Convex.Lenses              as L
import qualified Convex.Muesli.LP.Constants as Constants
import           Data.Map                   (Map)
import           Data.Proxy                 (Proxy (..))
import qualified Data.Text                  as Text
import           Data.Word                  (Word64)

{-| Place a limit buy order on Muesliswap orderbook v3 for the given amount of native tokens
-}
buyOrder :: C.NetworkId -> LimitBuyOrder -> TxBuild
buyOrder _network order@LimitBuyOrder{lboLovelace} =
  let val = C.TxOutValue C.MultiAssetInBabbageEra (C.lovelaceToValue lboLovelace)
      addr = scriptAddress
      dat = C.TxOutDatumInTx C.ScriptDataInBabbageEra (mkBuyOrderDatum order)
      txo = C.TxOut addr val dat C.ReferenceScriptNone
  in over L.txOuts ((:) txo)
      . over (L.txMetadata . L._TxMetadata) (setBuyOrderMetadata order)

setBuyOrderMetadata :: LimitBuyOrder -> Map Word64 C.TxMetadataValue -> Map Word64 C.TxMetadataValue
setBuyOrderMetadata LimitBuyOrder{lboReturnAddress, lboPolicy, lboAssetName, lboQuantity = C.Quantity q, lboLovelace = C.Lovelace l} =
  set (at 674) (Just $ C.TxMetaMap [(C.TxMetaText "msg", C.TxMetaList [C.TxMetaText "MuesliSwap Place Order"])])
  . set (at 1000) (Just $ C.TxMetaBytes $ C.serialiseToRawBytes lboReturnAddress)
  . set (at 1002) (Just $ C.TxMetaText $ C.serialiseToRawBytesHexText lboPolicy)
  . set (at 1003) (Just $ C.TxMetaText $ C.serialiseToRawBytesHexText lboAssetName)
  . set (at 1004) (Just $ C.TxMetaText $ Text.pack $ show q) -- not sure why this is a text and not a number
  . set (at 1005) (Just $ C.TxMetaNumber l)
  . set (at 1007) (Just $ C.TxMetaNumber 1) -- ?? allow partial match?
  . set (at 1008) (Just $ C.TxMetaText "") -- Ada policy ID
  . set (at 1009) (Just $ C.TxMetaText "") -- Ada asset name

cancelOrder :: C.TxIn -> LimitBuyOrder -> TxBuild
cancelOrder txIn (mkBuyOrderDatum -> dat) =
  let red = cancelRedeemer
      wit = C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 (C.PScript Constants.orderBookV3) (C.ScriptDatumForTxIn dat) red (C.ExecutionUnits 0 0)
      wit' = C.BuildTxWith (C.ScriptWitness C.ScriptWitnessForSpending wit)
  in over L.txIns ((txIn, wit') :) . setScriptsValid

data LimitBuyOrder =
  LimitBuyOrder
    { lboReturnAddress :: C.AddressInEra C.BabbageEra
    , lboPolicy        :: C.PolicyId
    , lboAssetName     :: C.AssetName
    , lboQuantity      :: C.Quantity
    , lboLovelace      :: C.Lovelace
    }

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


{-| Datum for a limit order
-}
mkBuyOrderDatum :: LimitBuyOrder -> C.ScriptData
mkBuyOrderDatum LimitBuyOrder{lboReturnAddress, lboPolicy, lboAssetName, lboQuantity = C.Quantity q, lboLovelace = C.Lovelace l} =
  let policyBS = C.serialiseToRawBytes lboPolicy
      assetBS = C.serialiseToRawBytes lboAssetName
  in ScriptDataConstructor 0
      [ ScriptDataConstructor 0
        [ encodeAddress lboReturnAddress
        , ScriptDataBytes policyBS
        , ScriptDataBytes assetBS
        , ScriptDataBytes "" -- Ada Policy ID
        , ScriptDataBytes "" -- Ada Asset ID
        , ScriptDataNumber q
        , ScriptDataConstructor 1 [] -- ?? Allow partial matches?
        , ScriptDataNumber l
        ]
      ]

buyOrderFromScriptData :: C.NetworkId -> C.ScriptData -> Maybe LimitBuyOrder
buyOrderFromScriptData network = \case
  ScriptDataConstructor 0
      [ ScriptDataConstructor 0
        [ (decodeAddress network -> Just lboReturnAddress)
        , ScriptDataBytes (C.deserialiseFromRawBytes (C.proxyToAsType Proxy) -> Just lboPolicy)
        , ScriptDataBytes (C.deserialiseFromRawBytes (C.proxyToAsType Proxy) -> Just lboAssetName)
        , ScriptDataBytes "" -- Ada Policy ID
        , ScriptDataBytes "" -- Ada Asset ID
        , ScriptDataNumber (C.Quantity -> lboQuantity)
        , ScriptDataConstructor 1 [] -- ?? Allow partial matches?
        , ScriptDataNumber (C.Lovelace -> lboLovelace)
        ]
      ] -> Just LimitBuyOrder{lboReturnAddress, lboPolicy, lboQuantity, lboLovelace, lboAssetName}
  _ -> Nothing

matchRedeemer :: C.ScriptData
matchRedeemer = ScriptDataConstructor 1 [ScriptDataNumber 0]

cancelRedeemer :: C.ScriptData
cancelRedeemer = ScriptDataConstructor 0 [ScriptDataNumber 0]

-- TODO:
-- Cancel tx + test case
-- Txn metadata

scriptAddress :: C.AddressInEra C.BabbageEra
scriptAddress = maybe (error "") id $ C.deserialiseAddress (C.proxyToAsType Proxy) "addr1zyq0kyrml023kwjk8zr86d5gaxrt5w8lxnah8r6m6s4jp4g3r6dxnzml343sx8jweqn4vn3fz2kj8kgu9czghx0jrsyqqktyhv"
