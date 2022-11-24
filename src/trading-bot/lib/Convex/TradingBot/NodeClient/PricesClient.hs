{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-| Extract prices from muesli trades to CSV
-}
module Convex.TradingBot.NodeClient.PricesClient(
  pricesClient,
  PriceEventRow(..)
) where

import qualified Cardano.Api                   as C
import           Cardano.Api.Shelley           (AssetName, BlockInMode, BlockNo,
                                                CardanoMode, Env, Lovelace,
                                                NetworkId, PolicyId, Quantity,
                                                SlotNo, TxId)
import qualified Cardano.Api.Shelley           as C
import           Control.Lens                  (_2, makeLenses, view, (&), (.~),
                                                (^.))
import           Control.Monad                 (guard)
import           Control.Monad.IO.Class        (MonadIO (..), liftIO)
import           Control.Monad.Trans.Maybe     (runMaybeT)
import qualified Convex.Constants              as Constants
import           Convex.Event                  (Event (..), NewOutputEvent (..),
                                                OutputSpentEvent (..),
                                                ResolvedInputs (..),
                                                TxWithEvents (..), extract)
import qualified Convex.Lenses                 as L
import           Convex.MonadLog               (MonadLogKatipT (..))
import           Convex.Muesli.LP.BuildTx      (buyOrderFromScriptData)
import           Convex.Muesli.LP.Types        (BuyOrder (..), valueOf)
import           Convex.NodeClient.Fold        (CatchingUp (..), catchingUp,
                                                foldClient)
import           Convex.NodeClient.Resuming    (resumingClient)
import           Convex.NodeClient.Types       (PipelinedLedgerStateClient)
import           Convex.TradingBot.LPPoolEvent (OrderbookEvent (..))
import qualified Convex.TradingBot.LPPoolEvent as LPPoolEvent
import           Data.Csv                      (FromField (..), (.:))
import           Data.CSV.Export               (CSVRow (..), csvQuotationMark,
                                                defaultCSVConfig, writeCSVLine)
import           Data.Foldable                 (traverse_)
import           Data.Proxy                    (Proxy (..))
import qualified Data.Text                     as Text
import           GHC.IO.Handle                 (Handle)
import qualified Katip                         as K
import           Streaming.Cassava             (FromNamedRecord (..))


data ClientState =
  ClientState
    { _resolvedInputs :: !(ResolvedInputs OrderbookEvent)
    }

makeLenses ''ClientState

initialState :: ClientState
initialState = ClientState mempty

pricesClient :: Handle -> NetworkId -> K.LogEnv -> K.Namespace -> Env -> PipelinedLedgerStateClient
pricesClient handle networkId logEnv ns env =
  resumingClient [Constants.lessRecent] $ \_ ->
    foldClient
      initialState
      env
      (applyBlock handle networkId logEnv ns)

applyBlock :: Handle -> NetworkId -> K.LogEnv -> K.Namespace -> CatchingUp -> ClientState -> BlockInMode CardanoMode -> IO (Maybe ClientState)
applyBlock handle networkId logEnv initialNamespace c oldState block = K.runKatipContextT logEnv () initialNamespace $ runMonadLogKatipT $ runMaybeT $ do
  let (newEvents, newResolvedInputs) = extract (\e -> maybe Nothing (either (const Nothing) Just) . LPPoolEvent.extract e) (oldState ^. resolvedInputs) block
      newState = oldState & resolvedInputs .~ newResolvedInputs

  flip traverse_ newEvents $ \TxWithEvents{twEvents, twTx, twBlock} -> do
    let i = C.getTxId (C.getTxBody twTx)
        config = defaultCSVConfig{csvQuotationMark=Nothing}
    flip traverse_ twEvents $ \case
      ANewOutputEvent{} -> pure ()
      AnOutputSpentEvent OutputSpentEvent{oseDatum, oseTxOutput = NewOutputEvent{neSlot, neOutput}} -> do
        let vl = C.selectLovelace $ view (L._TxOut . _2 . L._TxOutValue) $ C.fromShelleyTxOut C.ShelleyBasedEraBabbage neOutput
            dt = C.fromAlonzoData oseDatum
        case buyOrderFromScriptData networkId vl dt of
          Nothing -> pure ()
          Just BuyOrder{buyCurrency, buyQuantity, buyPrice} -> do
            let row = PriceEventRow i neSlot twBlock (fst buyCurrency) (snd buyCurrency) buyQuantity (valueOf buyQuantity buyPrice)
            liftIO (writeCSVLine handle config row)

  guard (catchingUp c)
  pure newState

data PriceEventRow =
  PriceEventRow
    { peTx        :: TxId
    , peSlot      :: SlotNo
    , peBlockNo   :: BlockNo
    , pePolicyId  :: PolicyId
    , peAssetName :: AssetName
    , peQuantity  :: Quantity
    , peLovelace  :: Lovelace
    }

instance CSVRow PriceEventRow where
  headers _ = ["tx", "slot", "block", "policy", "asset", "quantity", "lovelace"]
  toRow PriceEventRow{peTx, peSlot=C.SlotNo slot, peBlockNo=C.BlockNo n, pePolicyId, peAssetName, peQuantity=C.Quantity q, peLovelace=C.Lovelace l} =
    [ C.serialiseToRawBytesHexText peTx
    , Text.pack (show slot)
    , Text.pack (show n)
    , C.serialiseToRawBytesHexText pePolicyId
    , C.serialiseToRawBytesHexText peAssetName
    , Text.pack (show q)
    , Text.pack (show l)
    ]

instance FromNamedRecord PriceEventRow where
  parseNamedRecord m =
    PriceEventRow
      <$> fmap unFromHex (m .: "tx")
      <*> fmap C.SlotNo (m .: "slot")
      <*> fmap C.BlockNo (m .: "block")
      <*> fmap unFromHex (m .: "policy")
      <*> fmap unFromHex (m .: "asset")
      <*> fmap C.Quantity (m .: "quantity")
      <*> fmap C.Lovelace (m .: "lovelace")

newtype FromHex a = FromHex{ unFromHex :: a }

instance C.SerialiseAsRawBytes a => FromField (FromHex a) where
  parseField bs = case C.deserialiseFromRawBytesHex (C.proxyToAsType Proxy) bs of
    Right a  -> pure (FromHex a)
    Left err -> fail $ "parseField failed: " <> show err
