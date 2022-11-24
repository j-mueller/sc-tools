{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-| Extract prices from muesli trades to CSV
-}
module Convex.TradingBot.NodeClient.PricesClient(
  pricesClient,
  PriceEventRow
) where

import qualified Cardano.Api                   as C
import           Cardano.Api.Shelley           (AssetName, BlockInMode,
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
import           Data.CSV.Export               (CSVRow (..), defaultCSVConfig,
                                                writeCSVLine)
import           Data.Foldable                 (traverse_)
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           GHC.IO.Handle                 (Handle)
import qualified Katip                         as K


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

  flip traverse_ newEvents $ \TxWithEvents{twEvents, twTx} -> do
    let i = C.getTxId (C.getTxBody twTx)
    flip traverse_ twEvents $ \case
      ANewOutputEvent{} -> pure ()
      AnOutputSpentEvent OutputSpentEvent{oseDatum, oseTxOutput = NewOutputEvent{neSlot, neOutput}} -> do
        let vl = C.selectLovelace $ view (L._TxOut . _2 . L._TxOutValue) $ C.fromShelleyTxOut C.ShelleyBasedEraBabbage neOutput
            dt = C.fromAlonzoData oseDatum
        case buyOrderFromScriptData networkId vl dt of
          Nothing -> pure ()
          Just BuyOrder{buyCurrency, buyQuantity, buyPrice} -> do
            let row = PriceEventRow i neSlot (fst buyCurrency) (snd buyCurrency) buyQuantity (valueOf buyQuantity buyPrice)
            liftIO (writeCSVLine handle defaultCSVConfig row)

  guard (catchingUp c)
  pure newState

data PriceEventRow =
  PriceEventRow
    { peTx        :: TxId
    , peSlot      :: SlotNo
    , pePolicyId  :: PolicyId
    , peAssetName :: AssetName
    , peQuantity  :: Quantity
    , peLovelace  :: Lovelace
    }

instance CSVRow PriceEventRow where
  headers _ = ["tx", "slot", "policy", "asset", "quantity", "lovelace"]
  toRow PriceEventRow{peTx, peSlot=C.SlotNo slot, pePolicyId, peAssetName, peQuantity=C.Quantity q, peLovelace=C.Lovelace l} =
    let C.AssetName assetName = peAssetName
    in  [ C.serialiseToRawBytesHexText peTx
        , Text.pack (show slot)
        , C.serialiseToRawBytesHexText pePolicyId
        , either (const $ C.serialiseToRawBytesHexText peAssetName) id (Text.decodeUtf8' assetName)
        , Text.pack (show q)
        , Text.pack (show l)
        ]
