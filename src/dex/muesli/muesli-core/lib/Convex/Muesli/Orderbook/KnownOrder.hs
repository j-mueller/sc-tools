{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
module Convex.Muesli.Orderbook.KnownOrder(
  KnownOrder(..),
  knownOrderHash,
  FromMetadataError(..),
  knownOrderFromMetadata,
  addOrderTxIn,
  addOrderTxOut,
  orderTxOut,
  extractLovelaceFee,
  validateKnownOrder
  ) where


import           Cardano.Api                       (Address, AssetId,
                                                    Quantity (..), ShelleyAddr,
                                                    TxIn (..))
import qualified Cardano.Api                       as C
import qualified Cardano.Api.Shelley               as C
import qualified Cardano.Binary                    as CBOR
import qualified Cardano.Ledger.Alonzo.Data        as Data
import qualified Cardano.Ledger.Babbage            as Babbage
import           Cardano.Ledger.Crypto             (StandardCrypto)
import           Cardano.Ledger.Keys               (KeyHash, KeyRole (Payment))
import           Cardano.Ledger.Shelley.Metadata   (Metadatum (..))
import           Control.Lens                      (over)
import           Convex.BuildTx                    (TxBuild)
import qualified Convex.BuildTx                    as BuildTx
import           Convex.Event                      (NewOutputEvent (..),
                                                    ScriptOutDataHash)
import qualified Convex.Lenses                     as L
import qualified Convex.Muesli.Orderbook.Constants as Constants
import           Convex.Muesli.Orderbook.Contract  (Order (..),
                                                    OrderAction (..),
                                                    OrderDatum (..))
import           Data.Map.Lazy                     (Map)
import qualified Data.Map.Lazy                     as Map
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Text
import           Data.Validation                   (Validation (..))
import           Data.Word                         (Word64)
import qualified Plutus.V1.Ledger.Api              as Plutus
import           Plutus.V1.Ledger.Crypto           (PubKeyHash (..))
import qualified Plutus.V1.Ledger.Value            as Value
import qualified PlutusTx.Prelude                  as PlutusTx
import           Text.Read                         (readMaybe)

data KnownOrder =
    KnownOrder
    { orderDatum         :: OrderDatum
    , orderAsk           :: (AssetId, Quantity)
    , orderSigner        :: KeyHash 'Payment StandardCrypto
    , orderReturnAddress :: Address ShelleyAddr
    }

instance Show KnownOrder where
  show KnownOrder{orderAsk} = "KnownOrder{orderAsk=" <> show orderAsk <> "}"

addOrderTxIn :: KnownOrder -> NewOutputEvent a -> TxBuild
addOrderTxIn order NewOutputEvent{neTransaction, neTxIx} =
  let txIn = TxIn neTransaction neTxIx
  in BuildTx.spendPlutusV1 txIn Constants.muesliScript' (orderDatum order) FullMatch

addOrderTxOut :: C.ProtocolParameters -> KnownOrder -> TxBuild
addOrderTxOut params order = over L.txOuts ((:) (orderTxOut params order))

orderTxOut :: C.ProtocolParameters -> KnownOrder -> C.TxOut C.CtxTx C.BabbageEra
orderTxOut params KnownOrder{orderAsk, orderReturnAddress} =
  let value = C.valueFromList [orderAsk]
      addr = C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) orderReturnAddress
  in BuildTx.setMinAdaDeposit (C.toLedgerPParams C.ShelleyBasedEraBabbage params) $ BuildTx.payToAddressTxOut addr value

mkOrder ::
  KeyHash 'Payment StandardCrypto ->
  Address ShelleyAddr ->
  AssetId ->
  Quantity ->
  KnownOrder
mkOrder orderSigner orderReturnAddress C.AdaAssetId q@(Quantity lovelace) =
  let orderDatum = OrderDatum $ Order
                    {oCreator = fromCardanoPaymentKeyHash orderSigner
                    , oBuyCurrency = Value.adaSymbol
                    , oBuyToken = Value.adaToken
                    , oBuyAmount = lovelace}
      orderAsk = (C.AdaAssetId, q)
  in KnownOrder{orderDatum, orderAsk, orderSigner, orderReturnAddress}
mkOrder orderSigner orderReturnAddress (C.AssetId policyId assetName) (Quantity lovelace) =
    let orderDatum = OrderDatum $ Order
                      { oCreator = fromCardanoPaymentKeyHash orderSigner
                      , oBuyCurrency = fromCardanoPolicyId policyId
                      , oBuyToken = fromCardanoAssetName assetName
                      , oBuyAmount = lovelace
                      }
        orderAsk = (C.AssetId policyId assetName, fromIntegral lovelace)
    in KnownOrder{orderDatum, orderAsk, orderSigner, orderReturnAddress}

fromCardanoPolicyId :: C.PolicyId -> Value.CurrencySymbol
fromCardanoPolicyId (C.PolicyId scriptHash) = Value.CurrencySymbol $ PlutusTx.toBuiltin (C.serialiseToRawBytes scriptHash)

fromCardanoAssetName :: C.AssetName -> Value.TokenName
fromCardanoAssetName (C.AssetName bs) = Value.TokenName $ PlutusTx.toBuiltin bs

fromCardanoPaymentKeyHash :: KeyHash 'Payment StandardCrypto -> PubKeyHash
fromCardanoPaymentKeyHash =
  PubKeyHash
  . PlutusTx.toBuiltin
  . C.serialiseToRawBytes
  . C.PaymentKeyHash

knownOrderHash :: KnownOrder -> ScriptOutDataHash
knownOrderHash = Data.hashData @(Babbage.BabbageEra StandardCrypto) . knownOrderData

knownOrderData :: KnownOrder -> Data.Data (Babbage.BabbageEra StandardCrypto)
knownOrderData = Data.Data . Plutus.builtinDataToData . Plutus.toBuiltinData . orderDatum

{-| Extract the return address of an order from the tx metadata
-}
extractAddress :: Map Word64 Metadatum -> Either FromMetadataError (C.Address C.ShelleyAddr)
extractAddress mp = do
  let _ADDRESS_KEY = 1000
  addrM <- maybe (Left $ KeyNotFound _ADDRESS_KEY) Right $ Map.lookup _ADDRESS_KEY mp
  case addrM of
    B bs -> case C.deserialiseFromRawBytes C.AsShelleyAddress bs of
      Nothing -> Left $ FailedToDecodeAddress
      Just k  -> pure k
    _    -> Left $ WrongTypeAtKey _ADDRESS_KEY

extractSigner :: Map Word64 Metadatum -> Either FromMetadataError (KeyHash 'Payment StandardCrypto)
extractSigner mp = do
  let _SIGNER_KEY = 1001
  addrM <- maybe (Left $ KeyNotFound _SIGNER_KEY) Right $ Map.lookup _SIGNER_KEY mp
  case addrM of
    S bs -> case textToKeyHash bs of
      Left _  -> Left $ FailedToDecodeSigner bs -- FIXME error type
      Right x -> pure x
    _    -> Left $ WrongTypeAtKey _SIGNER_KEY

data DecodeKeyHashError =
  Base16Err String
  | CBORErr CBOR.DecoderError
  | DeserialiseHashFailed String
  deriving Show

textToKeyHash :: Text -> Either DecodeKeyHashError (KeyHash 'Payment StandardCrypto)
textToKeyHash t =
  case C.deserialiseFromRawBytesHex (C.AsHash C.AsPaymentKey) (Text.encodeUtf8 t) of
    Left err                   -> Left (DeserialiseHashFailed $ show err)
    Right (C.PaymentKeyHash k) -> Right k

extractQuantity :: Map Word64 Metadatum -> Either FromMetadataError Quantity
extractQuantity mp = do
  let _QUANTITY_KEY = 1004
  case Map.lookup _QUANTITY_KEY mp of
    Just (S n) -> do
      case readMaybe (Text.unpack n) of
        Just n' -> Right $ Quantity n'
        Nothing -> Left $ QuantityFromStringFailed n
    Just (I n) -> Right $ Quantity n
    Just _     -> Left $ WrongTypeAtKey _QUANTITY_KEY
    Nothing    -> Left $ KeyNotFound _QUANTITY_KEY

extractLovelaceFee :: Map Word64 Metadatum -> Either FromMetadataError C.Lovelace
extractLovelaceFee mp = do
  let _FEE_KEY = 1005
  case Map.lookup _FEE_KEY mp of
    Just (I n) -> pure (C.Lovelace n)
    Just _     -> Left $ WrongTypeAtKey _FEE_KEY
    Nothing    -> Left $ KeyNotFound _FEE_KEY

extractAssetId :: Map Word64 Metadatum -> Either FromMetadataError AssetId
extractAssetId mp = do
  let _POLICY_KEY = 1002
      _TOKEN_NAME_KEY = 1003
  policyT <- case Map.lookup _POLICY_KEY mp of
    Just (S txt) -> pure txt
    Just _       -> Left $ WrongTypeAtKey _POLICY_KEY
    Nothing      -> Left $ KeyNotFound _POLICY_KEY
  tokenT <- case Map.lookup _TOKEN_NAME_KEY mp of
    Just (S txt) -> pure txt
    Just _       -> Left $ WrongTypeAtKey _TOKEN_NAME_KEY
    Nothing      -> Left $ KeyNotFound _TOKEN_NAME_KEY
  case (policyT, tokenT) of
    ("", "") -> pure C.AdaAssetId
    _ -> do
      policyId <- case C.deserialiseFromRawBytesHex C.AsPolicyId (Text.encodeUtf8 policyT) of
                    Left err -> Left $ FailedToDeserialisePolicyId policyT (show err)
                    Right x  -> pure x
      assetId <- Right $ C.AssetName $ Text.encodeUtf8 tokenT -- TODO: What to do if the token name is not a valid UTF 8 string?
        --case C.deserialiseFromRawBytesHex C.AsAssetName (Text.encodeUtf8 tokenT) of
                   -- Nothing -> Left $ FailedToDeserialiseTokenName tokenT
      return $ C.AssetId policyId assetId

knownOrderFromMetadata :: Map Word64 Metadatum -> Either FromMetadataError KnownOrder
knownOrderFromMetadata mp =
  mkOrder
    <$> extractSigner mp
    <*> extractAddress mp
    <*> extractAssetId mp
    <*> extractQuantity mp

data FromMetadataError
  = KeyNotFound Word64
  | WrongTypeAtKey Word64
  | FailedToDecodeAddress
  | FailedToDeserialisePolicyId Text String
  | FailedToDeserialiseTokenName Text
  | FailedToDecodeSigner Text
  | QuantityFromStringFailed Text
  deriving stock (Eq, Ord, Show)

validateKnownOrder :: KnownOrder -> NewOutputEvent a -> Validation [String] ()
validateKnownOrder knownOrder orderEvent =
  if knownOrderHash knownOrder == neDataHash orderEvent
    then pure ()
    else Failure ["hash mismatch"]
