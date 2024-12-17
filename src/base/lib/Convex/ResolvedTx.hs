{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Working with fully resolved transactions
-}
module Convex.ResolvedTx(
  ResolvedTx(..),
  -- * Visualising transactions
  dot,
  dotFile
) where

import qualified Cardano.Api                         as C
import           Cardano.Crypto.Hash                 (hashToTextAsHex)
import qualified Cardano.Ledger.Credential           as Credential
import           Cardano.Ledger.Hashes               (ScriptHash (..))
import           Cardano.Ledger.Keys                 (KeyHash (..))
import           Cardano.Ledger.Shelley.API          (Coin (..))
import           Control.Lens                        (_1, _2, preview, view)
import           Control.Monad.Reader                (MonadReader, ReaderT,
                                                      asks, lift, runReaderT)
import           Convex.CardanoApi.Lenses            (_ShelleyAddress, _TxOut,
                                                      _TxOutValue)
import qualified Convex.CardanoApi.Lenses            as L
import qualified Convex.Utils                        as Utils
import           Data.Aeson                          (FromJSON (..),
                                                      ToJSON (..), withObject,
                                                      (.:))
import           Data.Aeson.Types                    (object, (.=))
import qualified Data.ByteString.Base16              as Base16
import           Data.Foldable                       (forM_, traverse_)
import           Data.GraphViz.Attributes            (bgColor, filled, style)
import qualified Data.GraphViz.Attributes.Colors.X11 as Colors
import qualified Data.GraphViz.Attributes.Complete   as A
import           Data.GraphViz.Printing              (DotCode, PrintDot (..))
import qualified Data.GraphViz.Types                 as GVT
import           Data.GraphViz.Types.Generalised     (DotGraph (..))
import qualified Data.GraphViz.Types.Monadic         as GV
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (fromMaybe)
import           Data.Proxy                          (Proxy (..))
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Data.Text.Encoding                  as Text
import qualified Data.Text.IO                        as TIO
import qualified Data.Text.Lazy                      as TL
import           GHC.Generics                        (Generic)
import           GHC.IsList                          (IsList (..))

{-| A transaction with fully resolved inputs.
To obtain a 'ResolvedTx' value see 'Convex.Blockfrost.resolveTx'
-}
data ResolvedTx =
  ResolvedTx
    { rtxTransaction :: C.Tx C.ConwayEra -- ^ The transaction
    , rtxInputs      :: Map C.TxIn (C.TxOut C.CtxUTxO C.ConwayEra) -- ^ The set of spend, reference and collateral inputs of the transaction
    }
    deriving stock (Eq, Show, Generic)

{-| The transaction's body content
-}
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
      , "inputs"      .= rtxInputs
      ]

instance FromJSON ResolvedTx where
  parseJSON = withObject "ResolvedTx" $ \obj ->
    ResolvedTx
      <$> (obj .: "transaction" >>= either (fail . show) pure . C.deserialiseFromTextEnvelope (C.proxyToAsType Proxy))
      <*> obj .: "inputs"

{-| A .dot (graphviz) representation of the transaction
-}
dot :: [ResolvedTx] -> Text
dot = TL.toStrict . GVT.printDotGraph . dot' "resolved-transactions"

{-| Write the transaction graph to a .dot (graphviz) file
-}
dotFile :: FilePath -> [ResolvedTx] -> IO ()
dotFile fp = TIO.writeFile fp . dot

data FullTxInput =
  RefInput C.TxIn
  | SpendInput C.TxIn
  | CollateralInput C.TxIn
  deriving stock (Eq, Ord, Show)

getTxIn :: FullTxInput -> C.TxIn
getTxIn = \case
  RefInput i -> i
  SpendInput i -> i
  CollateralInput i -> i

addressLabel :: C.IsShelleyBasedEra era => C.TxOut ctx era -> A.RecordField
addressLabel txo = case preview (_TxOut . _1 . _ShelleyAddress . _2) txo of
  Just (Credential.KeyHashObj (KeyHash has))       -> A.FieldLabel $ TL.fromStrict $ "pubkey " <> shortenHash56 (hashToTextAsHex has)
  Just (Credential.ScriptHashObj (ScriptHash has)) -> A.FieldLabel $ TL.fromStrict $ "script " <> shortenHash56 (hashToTextAsHex has)
  _                                                -> A.FieldLabel "(byron)"

fullTxInputLabel :: (C.IsMaryBasedEra era) => FullTxInput -> C.TxOut C.CtxUTxO era -> [A.RecordField]
fullTxInputLabel in_ _out = case in_ of
  RefInput i ->
    [ A.FieldLabel $ TL.fromStrict $ shortenHash $ C.renderTxIn i
    , A.FieldLabel "Reference"
    , addressLabel _out
    , valueLabel _out
    ]
  SpendInput i ->
    [ A.FieldLabel $ TL.fromStrict $ shortenHash $ C.renderTxIn i
    , A.FieldLabel "Spend"
    , addressLabel _out
    , valueLabel _out
    ]
  CollateralInput i ->
    [ A.FieldLabel $ TL.fromStrict $ shortenHash $ C.renderTxIn i
    , A.FieldLabel "Collateral"
    , addressLabel _out
    , valueLabel _out
    ]

fullTxOutputLabel :: (C.IsMaryBasedEra era) => C.TxIn -> C.TxOut C.CtxTx era -> [A.RecordField]
fullTxOutputLabel i txOut =
  [ A.FieldLabel $ TL.fromStrict $ shortenHash $ C.renderTxIn i
  , addressLabel txOut
  , valueLabel txOut
  ]

adaLabel :: Integer -> Text
adaLabel ada =
  let (n, k) = ada `divMod` 1_000_000
      (n2, k2) = k `divMod` 10
      (n3, _)  = k2 `divMod` 10
  in "Ada: " <> Text.pack (show n) <> "." <> Text.pack (show n2) <> Text.pack (show n3)

valueLabel :: (C.IsMaryBasedEra era) => C.TxOut ctx era -> A.RecordField
valueLabel =
  let renderAsset C.AdaAssetId (C.Quantity n)                   = adaLabel n
      renderAsset (C.AssetId C.PolicyId{C.unPolicyId} (C.AssetName assetName)) (C.Quantity n) =
        let lbl = shortenHash56 (Text.pack $ filter ((/=) '"') $ show unPolicyId) <> "." <> Text.decodeUtf8 (Base16.encode assetName)
        in lbl <> ": " <> Text.pack (show n)
      renderValue = Text.unlines . fmap (uncurry renderAsset) . toList
  in A.FieldLabel . TL.fromStrict . renderValue . view (_TxOut . _2 . _TxOutValue)

{-| Replace the hash sign with an underscore. This is required so that 'TxId's can be used
as node identifiers in .dot
-}
replaceHash :: Text -> Text
replaceHash = Text.replace "#" "_"

{-| Shorten a 64 character hash value by taking only the first and last
four characters
-}
shortenHash :: Text -> Text
shortenHash t = Text.take 4 t <> "..." <> Text.drop 60 t

{-| Shorten a 56 character hash value by taking only the first and last
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

{-| Object that we display in the graph
-}
data FullTxObject =
  FullTxBody C.TxId -- ^ Body of the transaction
  | FullTxOutput C.TxIn -- ^ Transaction output
  deriving stock (Eq, Ord, Show)

instance GVT.PrintDot FullTxObject where
  unqtDot = \case
    FullTxBody txi   -> mkTxLabel txi
    FullTxOutput txI -> mkTxInLabel txI

dot' :: Text -> [ResolvedTx] -> DotGraph FullTxObject
dot' (TL.fromStrict -> nm) transactions = GV.digraph (GV.Str nm) $ do
  GV.graphAttrs [ A.RankDir A.FromLeft ]
  GV.nodeAttrs
    [ A.Shape A.Record
    , style filled
    , bgColor Colors.Gray93
    , A.Height 0.1
    ]
  forM_ transactions $ \ftx ->
    flip runReaderT ftx $ do
      addTxBody
      asks (Utils.spendInputs . txBodyContent) >>= traverse_ (addInput . SpendInput)
      asks (Utils.referenceInputs . txBodyContent) >>= traverse_ (addInput . RefInput)
      asks (Utils.collateralInputs . txBodyContent) >>= traverse_ (addInput . CollateralInput)
      asks (Utils.txnUtxos . rtxTransaction) >>= traverse_ (uncurry addTxOut)

type GraphBuilder a = ReaderT ResolvedTx (GV.DotM FullTxObject) a

{-| Resolve the transaction output
-}
lookupTxIn :: MonadReader ResolvedTx m => C.TxIn -> m (C.TxOut C.CtxUTxO C.ConwayEra)
lookupTxIn txI =
  asks (fromMaybe (error "lookupTxIn failed") . Map.lookup txI . rtxInputs)

addInput :: FullTxInput -> GraphBuilder ()
addInput txI = do
  i <- asks txId
  output <- lookupTxIn (getTxIn txI)
  lift $ do
    let ref = FullTxOutput (getTxIn txI)
    GV.node ref
      [ A.Label $ A.RecordLabel (fullTxInputLabel txI output)
      ]
    GV.edge ref (FullTxBody i) []

addTxBody :: GraphBuilder ()
addTxBody = do
  i <- asks txId
  Coin n <- asks (view L.txFee . txBodyContent)
  let labels =
        [ A.FieldLabel "Transaction"
        , A.FieldLabel $ "Fee: " <> TL.fromStrict (adaLabel n)
        , A.FieldLabel $ TL.fromStrict $ C.serialiseToRawBytesHexText i
        ]
  lift $ GV.node (FullTxBody i) [A.Label $ A.RecordLabel labels]

addTxOut :: (C.IsMaryBasedEra era) => C.TxIn -> C.TxOut C.CtxTx era -> GraphBuilder ()
addTxOut txI txOut = do
  i <- asks txId
  lift $ do
    let ref = FullTxOutput txI
    GV.node ref [A.Label $ A.RecordLabel (fullTxOutputLabel txI txOut)]
    GV.edge (FullTxBody i) ref []
