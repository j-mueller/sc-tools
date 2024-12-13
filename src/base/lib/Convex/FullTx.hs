{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Working with fully resolved transactions
-}
module Convex.FullTx(
  FullTx(..),
  -- * Visualising transactions
  dot,
  dotFile
) where

import qualified Cardano.Api                          as C
import           Control.Monad.Reader                (ReaderT, asks, lift,
                                                      runReaderT)
import qualified Convex.Utils                        as Utils
import           Data.Aeson                          (FromJSON (..),
                                                      ToJSON (..), withObject,
                                                      (.:))
import           Data.Aeson.Types                    (object, (.=))
import           Data.Foldable                       (traverse_)
import           Data.GraphViz.Attributes            (bgColor, filled, style)
import qualified Data.GraphViz.Attributes.Colors.X11 as Colors
import qualified Data.GraphViz.Attributes.Complete   as A
import           Data.GraphViz.Printing              (PrintDot (..))
import qualified Data.GraphViz.Types                 as GVT
import           Data.GraphViz.Types.Generalised     (DotGraph (..))
import qualified Data.GraphViz.Types.Monadic         as GV
import           Data.Map                            (Map)
import           Data.Proxy                          (Proxy (..))
import           Data.Text                           (Text)
import qualified Data.Text.IO                        as TIO
import qualified Data.Text.Lazy                      as TL
import           GHC.Generics                        (Generic)

{-| A transaction with fully resolved inputs.
To obtain a 'FullTx' value see 'Convex.Blockfrost.resolveFullTx'
-}
data FullTx =
  FullTx
    { ftxTransaction :: C.Tx C.ConwayEra -- ^ The transaction
    , ftxInputs      :: Map C.TxIn (C.TxOut C.CtxUTxO C.ConwayEra) -- ^ The set of spend, reference and collateral inputs of the transaction
    }
    deriving stock (Eq, Show, Generic)

{-| The transaction's body content
-}
txBodyContent :: FullTx -> C.TxBodyContent C.ViewTx C.ConwayEra
txBodyContent FullTx{ftxTransaction} =
  let (C.Tx (C.TxBody content) _witnesses) = ftxTransaction
  in content

instance ToJSON FullTx where
  toJSON FullTx{ftxTransaction, ftxInputs} =
    object
      [ "transaction" .= C.serialiseToTextEnvelope Nothing ftxTransaction
      , "inputs"      .= ftxInputs
      ]

instance FromJSON FullTx where
  parseJSON = withObject "FullTx" $ \obj ->
    FullTx
      <$> (obj .: "transaction" >>= either (fail . show) pure . C.deserialiseFromTextEnvelope (C.proxyToAsType Proxy))
      <*> obj .: "inputs"

{-| A .dot (graphviz) representation of the transaction
-}
dot :: FullTx -> Text
dot tx@FullTx{ftxTransaction} = TL.toStrict $ GVT.printDotGraph $ dot' (C.textShow $ C.getTxId $ C.getTxBody ftxTransaction) tx

{-| Write the transaction graph to a .dot (graphviz) file
-}
dotFile :: FilePath -> FullTx -> IO ()
dotFile fp = TIO.writeFile fp . dot

data FullTxInput =
  RefInput C.TxIn
  | SpendInput C.TxIn
  | CollateralInput C.TxIn
  deriving stock (Eq, Ord, Show)

fullTxInputLabel :: FullTxInput -> A.RecordField
fullTxInputLabel = \case
  RefInput i -> A.FieldLabel $ TL.fromStrict $ C.renderTxIn i
  SpendInput i -> A.FieldLabel $ TL.fromStrict $ C.renderTxIn i
  CollateralInput i -> A.FieldLabel $ TL.fromStrict $ C.renderTxIn i

instance GVT.PrintDot FullTxInput where
  unqtDot = \case
    RefInput txI -> unqtDot ("ref-" <> C.renderTxIn txI)
    SpendInput txI -> unqtDot ("spend-" <> C.renderTxIn txI)
    CollateralInput txI -> unqtDot ("collateral-" <> C.renderTxIn txI)

{-| Object that we display in the graph
-}
data FullTxObject =
  FtxInput FullTxInput
  | FullTxBody
  | FullTxOutput Integer
  deriving stock (Eq, Ord, Show)

instance GVT.PrintDot FullTxObject where
  unqtDot = \case
    FtxInput it      -> unqtDot it
    FullTxBody       -> unqtDot @String "txbody"
    FullTxOutput idx -> unqtDot ("output" <> show idx)

dot' :: Text -> FullTx -> DotGraph FullTxObject
dot' (TL.fromStrict -> nm) ftx = GV.digraph (GV.Str nm) $ do
  GV.graphAttrs [ A.RankDir A.FromLeft ]
  GV.nodeAttrs
    [ A.Shape A.Record
    , style filled
    , bgColor Colors.Gray93
    , A.Height 0.1
    ]
  flip runReaderT ftx $ do
    addTxBody
    asks (Utils.spendInputs . txBodyContent) >>= traverse_ (addInput . SpendInput)
    asks (Utils.referenceInputs . txBodyContent) >>= traverse_ (addInput . RefInput)
    asks (Utils.collateralInputs . txBodyContent) >>= traverse_ (addInput . CollateralInput)

type GraphBuilder a = ReaderT FullTx (GV.DotM FullTxObject) a

addInput :: FullTxInput -> GraphBuilder ()
addInput txI = do
  lift $ do
    let ref = FtxInput txI
    GV.node ref [A.Label $ A.RecordLabel [fullTxInputLabel txI]]
    GV.edge ref FullTxBody []

addTxBody :: GraphBuilder ()
addTxBody = lift $ GV.node FullTxBody [A.Label $ A.RecordLabel [A.FieldLabel "tx body"]]
