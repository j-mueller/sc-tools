{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Working with fully resolved transactions
module Convex.ResolvedTx (
  ResolvedTx (..),
  resolveTxMockchain,

  -- * Visualising transactions
  dot,
  dotFile,
) where

import Cardano.Api qualified as C
import Cardano.Crypto.Hash (hashToTextAsHex)
import Cardano.Ledger.Credential qualified as Credential
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Shelley.API (Coin (..))
import Control.Lens (preview, view, _1, _2)
import Control.Monad.Reader (
  ReaderT,
  asks,
  lift,
  runReaderT,
 )
import Convex.Class (MonadMockchain, getTxById, utxoByTxIn)
import Convex.CardanoApi.Lenses (
  _ShelleyAddress,
  _TxOut,
  _TxOutValue,
 )
import Convex.CardanoApi.Lenses qualified as L
import Convex.Utils qualified as Utils
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  withObject,
  (.:),
 )
import Data.Aeson.Types (object, (.=))
import Data.Bifunctor (Bifunctor (second))
import Data.ByteString.Base16 qualified as Base16
import Data.Foldable (forM_, traverse_)
import Data.GraphViz.Attributes (bgColor)
import Data.GraphViz.Attributes qualified as A
import Data.GraphViz.Attributes.Colors.X11 qualified as Colors
import Data.GraphViz.Attributes.Complete qualified as A
import Data.GraphViz.Printing (DotCode, PrintDot (..))
import Data.GraphViz.Types qualified as GVT
import Data.GraphViz.Types.Generalised (DotGraph (..))
import Data.GraphViz.Types.Monadic qualified as GV
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import GHC.Generics (Generic)
import GHC.IsList (IsList (..))
import Control.Monad.Trans.Maybe (MaybeT(..))

{- | A transaction with fully resolved inputs.
To obtain a 'ResolvedTx' value see 'Convex.Blockfrost.resolveTx'
or 'resolveTxMockchain'
-}
data ResolvedTx
  = ResolvedTx
  { rtxTransaction :: C.Tx C.ConwayEra
  -- ^ The transaction
  , rtxInputs :: Map C.TxIn (C.TxOut C.CtxUTxO C.ConwayEra)
  -- ^ The set of spend, reference and collateral inputs of the transaction
  }
  deriving stock (Eq, Show, Generic)

{-| Produce a 'ResolvedTx' using the mockchain state
-}
resolveTxMockchain :: MonadMockchain C.ConwayEra m => C.TxId -> m (Maybe ResolvedTx)
resolveTxMockchain txI = runMaybeT $ do
  rtxTransaction <- MaybeT (getTxById txI)
  let (C.Tx (C.TxBody bodyContent) _witnesses) = rtxTransaction
  let reqTxIns = Utils.requiredTxIns bodyContent
  utxo <- utxoByTxIn reqTxIns
  pure ResolvedTx{rtxTransaction, rtxInputs = C.unUTxO utxo}

-- | The transaction's body content
txBodyContent :: ResolvedTx -> C.TxBodyContent C.ViewTx C.ConwayEra
txBodyContent ResolvedTx{rtxTransaction} =
  let (C.Tx (C.TxBody content) _witnesses) = rtxTransaction
   in content

txId :: ResolvedTx -> C.TxId
txId = C.getTxId . C.getTxBody . rtxTransaction

instance ToJSON ResolvedTx where
  toJSON ResolvedTx{rtxTransaction, rtxInputs} =
    object
      [ "transaction" .= C.serialiseToTextEnvelope Nothing rtxTransaction
      , "inputs" .= rtxInputs
      ]

instance FromJSON ResolvedTx where
  parseJSON = withObject "ResolvedTx" $ \obj ->
    ResolvedTx
      <$> (obj .: "transaction" >>= either (fail . show) pure . C.deserialiseFromTextEnvelope (C.proxyToAsType Proxy))
      <*> obj .: "inputs"

-- | A .dot (graphviz) representation of the transaction
dot :: [ResolvedTx] -> Text
dot = TL.toStrict . GVT.printDotGraph . dot' "resolved-transactions"

-- | Write the transaction graph to a .dot (graphviz) file
dotFile :: FilePath -> [ResolvedTx] -> IO ()
dotFile fp = TIO.writeFile fp . dot

data FullTxInput
  = RefInput C.TxIn
  | SpendInput C.TxIn
  | CollateralInput C.TxIn
  deriving stock (Eq, Ord, Show)

getTxIn :: FullTxInput -> C.TxIn
getTxIn = \case
  RefInput i -> i
  SpendInput i -> i
  CollateralInput i -> i

addressLabel :: (C.IsShelleyBasedEra era) => C.TxOut ctx era -> A.RecordField
addressLabel txo = case preview (_TxOut . _1 . _ShelleyAddress . _2) txo of
  Just (Credential.KeyHashObj (KeyHash has)) -> A.FieldLabel $ TL.fromStrict $ "pubkey " <> shortenHash56 (hashToTextAsHex has)
  Just (Credential.ScriptHashObj (ScriptHash has)) -> A.FieldLabel $ TL.fromStrict $ "script " <> shortenHash56 (hashToTextAsHex has)
  _ -> A.FieldLabel "(byron)"

fullTxOutputLabel :: (C.IsMaryBasedEra era) => C.TxIn -> C.TxOut ctx era -> [A.RecordField]
fullTxOutputLabel i txOut =
  [ A.FieldLabel $ TL.fromStrict $ shortenHash $ C.renderTxIn i
  , addressLabel txOut
  , valueLabel txOut
  ]

adaLabel :: Integer -> Text
adaLabel ada =
  let (n, k) = ada `divMod` 1_000_000
      (n2, k2) = k `divMod` 10
      (n3, _) = k2 `divMod` 10
   in "Ada: " <> Text.pack (show n) <> "." <> Text.pack (show n2) <> Text.pack (show n3)

valueLabel :: (C.IsMaryBasedEra era) => C.TxOut ctx era -> A.RecordField
valueLabel =
  let renderAsset C.AdaAssetId (C.Quantity n) = adaLabel n
      renderAsset (C.AssetId C.PolicyId{C.unPolicyId} (C.AssetName assetName)) (C.Quantity n) =
        let lbl = shortenHash56 (Text.pack $ filter ((/=) '"') $ show unPolicyId) <> "." <> Text.decodeUtf8 (Base16.encode assetName)
         in lbl <> ": " <> Text.pack (show n)
      renderValue = Text.unlines . fmap (uncurry renderAsset) . toList
   in A.FieldLabel . TL.fromStrict . renderValue . view (_TxOut . _2 . _TxOutValue)

{- | Replace the hash sign with an underscore. This is required so that 'TxId's can be used
as node identifiers in .dot
-}
replaceHash :: Text -> Text
replaceHash = Text.replace "#" "_"

{- | Shorten a 64 character hash value by taking only the first and last
four characters
-}
shortenHash :: Text -> Text
shortenHash t = Text.take 4 t <> "..." <> Text.drop 60 t

{- | Shorten a 56 character hash value by taking only the first and last
four characters
-}
shortenHash56 :: Text -> Text
shortenHash56 t = Text.take 4 t <> "..." <> Text.drop 52 t

instance GVT.PrintDot FullTxInput where
  unqtDot = \case
    RefInput txI -> mkTxInLabel txI
    SpendInput txI -> mkTxInLabel txI
    CollateralInput txI -> mkTxInLabel txI

mkTxInLabel :: C.TxIn -> DotCode
mkTxInLabel txI = unqtDot ("txin_" <> replaceHash (C.renderTxIn txI))

mkTxLabel :: C.TxId -> DotCode
mkTxLabel txid = unqtDot ("tx_" <> filter (/= '"') (show txid))

-- | Object that we display in the graph
data FullTxObject
  = -- | Body of the transaction
    FullTxBody C.TxId
  | -- | Transaction output
    FullTxOutput C.TxIn
  deriving stock (Eq, Ord, Show)

instance GVT.PrintDot FullTxObject where
  unqtDot = \case
    FullTxBody txi -> mkTxLabel txi
    FullTxOutput txI -> mkTxInLabel txI

dot' :: Text -> [ResolvedTx] -> DotGraph FullTxObject
dot' (TL.fromStrict -> nm) transactions = GV.digraph (GV.Str nm) $ do
  GV.graphAttrs [A.RankDir A.FromLeft]
  GV.nodeAttrs
    [ A.Shape A.Record
    , A.style A.filled
    , bgColor Colors.Gray93
    , A.Height 0.1
    ]
  -- add all tx outs first
  traverse_ (uncurry addTxOut) (Map.toList $ foldMap rtxInputs transactions <> foldMap (Map.fromList . fmap (second C.toCtxUTxOTxOut) . Utils.txnUtxos . rtxTransaction) transactions)
  -- add transactions
  -- add links
  forM_ transactions $ \ftx ->
    flip runReaderT ftx $ do
      addTxBody (rtxTransaction ftx)
      asks (Utils.spendInputs . txBodyContent) >>= traverse_ (addInput . SpendInput)
      asks (Utils.referenceInputs . txBodyContent) >>= traverse_ (addInput . RefInput)
      asks (Utils.collateralInputs . txBodyContent) >>= traverse_ (addInput . CollateralInput)
      asks (Utils.txnUtxos . rtxTransaction) >>= traverse_ (uncurry addOutput)

type GraphBuilder a = ReaderT ResolvedTx (GV.DotM FullTxObject) a

addInput :: FullTxInput -> GraphBuilder ()
addInput txI = do
  i <- asks txId
  lift $ do
    let ref = FullTxOutput (getTxIn txI)
        txt = case txI of
          RefInput{} -> "reference"
          SpendInput{} -> "spend"
          CollateralInput{} -> "collateral"
    GV.edge
      ref
      (FullTxBody i)
      [ A.textLabel txt
      ]

addOutput :: C.TxIn -> C.TxOut context era -> GraphBuilder ()
addOutput txI _ = do
  i <- asks txId
  lift $ do
    let ref = FullTxOutput txI
    GV.edge (FullTxBody i) ref []

addTxBody :: C.Tx C.ConwayEra -> GraphBuilder ()
addTxBody transaction = do
  let i = C.getTxId $ C.getTxBody transaction
      (C.Tx (C.TxBody content) _witnesses) = transaction
      withdrawals =
        case C.txWithdrawals content of
          C.TxWithdrawalsNone -> []
          C.TxWithdrawals _ w -> w
      Coin n = view L.txFee content
  let labels =
        [ A.FieldLabel "Transaction"
        , A.FieldLabel $ "Fee: " <> TL.fromStrict (adaLabel n)
        , A.FieldLabel $ TL.fromStrict $ C.serialiseToRawBytesHexText i
        ]
          <> fmap withdrawalLabel withdrawals
  lift $ GV.node (FullTxBody i) [A.Label $ A.RecordLabel labels]

withdrawalLabel :: (C.StakeAddress, Coin, C.BuildTxWith C.ViewTx (C.Witness C.WitCtxStake C.ConwayEra)) -> A.RecordField
withdrawalLabel (addr, Coin n, _) =
  A.FieldLabel $ "Withdrawal: " <> TL.fromStrict (C.serialiseToBech32 addr) <> " (" <> TL.fromStrict (adaLabel n) <> ")"

addTxOut :: (C.IsMaryBasedEra era) => C.TxIn -> C.TxOut ctx era -> GV.DotM FullTxObject ()
addTxOut txI txOut = do
  let ref = FullTxOutput txI
  GV.node
    ref
    [ A.Label $ A.RecordLabel (fullTxOutputLabel txI txOut)
    , A.style A.rounded
    ]
