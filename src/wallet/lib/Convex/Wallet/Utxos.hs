{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
module Convex.Wallet.Utxos(
  UtxoState(..),
  _UtxoState,
  totalBalance,
  UtxoChange(..),
  outputsAdded,
  outputsRemoved,
  null,
  apply,
  inv,
  extract,
  describeChange
  ) where

import           Cardano.Api                   (AddressInEra, BabbageEra,
                                                Block (..), BlockInMode (..),
                                                CardanoMode, EraInMode (..),
                                                Tx (..), TxIn (..), TxIx (..),
                                                Value)
import qualified Cardano.Api                   as C
import           Cardano.Api.Shelley           (TxBody (..))
import qualified Cardano.Api.Shelley           as CS
import qualified Cardano.Ledger.Babbage.TxBody as Babbage.TxBody
import qualified Cardano.Ledger.BaseTypes      as CT
import qualified Cardano.Ledger.TxIn           as CT
import           Control.Lens                  (_1, _2, makeLenses, makePrisms,
                                                view)
import qualified Convex.Lenses                 as L
import           Data.Bifunctor                (Bifunctor (..))
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (mapMaybe)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Prelude                       hiding (null)

newtype UtxoState = UtxoState{ _utxos :: Map C.TxIn (C.TxOut C.CtxTx C.BabbageEra) }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

makePrisms ''UtxoState

totalBalance :: UtxoState -> Value
totalBalance = foldMap (view (L._TxOut . _2 . L._TxOutValue)) . _utxos

data UtxoChange =
  UtxoChange
    { _outputsAdded   :: Map C.TxIn (C.TxOut C.CtxTx C.BabbageEra)
    , _outputsRemoved :: Map C.TxIn (C.TxOut C.CtxTx C.BabbageEra)
    }

makeLenses ''UtxoChange
-- TODO: change '<>' so that @x <> invert x == mempty@ and @invert x <> x == mempty@

instance Semigroup UtxoChange where
  l <> r =
    UtxoChange
      { _outputsAdded   = _outputsAdded l <> _outputsAdded r
      , _outputsRemoved = _outputsRemoved l <> _outputsRemoved r
      }

instance Monoid UtxoChange where
  mempty = UtxoChange mempty mempty

null :: UtxoChange -> Bool
null UtxoChange{_outputsAdded, _outputsRemoved} = Map.null _outputsAdded && Map.null _outputsRemoved

{-| Describe the UtxoChange
-}
describeChange :: UtxoChange -> Text
describeChange UtxoChange{_outputsAdded, _outputsRemoved} =
  let tshow = Text.pack . show
  in tshow (Map.size _outputsAdded) <> " outputs added, " <> tshow (Map.size _outputsRemoved) <> " outputs removed"

{-| Change the 'UtxoState'
-}
apply :: UtxoState -> UtxoChange -> UtxoState
apply UtxoState{_utxos} UtxoChange{_outputsAdded, _outputsRemoved} =
  UtxoState $ (_utxos `Map.union` _outputsAdded) `Map.difference` _outputsRemoved

{-| Invert a 'UtxoChange' value
-}
inv :: UtxoChange -> UtxoChange
inv (UtxoChange added removed) = UtxoChange removed added

{-| Extract from a block the UTXO changes at the given address
-}
extract :: C.AddressInEra BabbageEra -> UtxoState -> BlockInMode CardanoMode -> UtxoChange
extract addr state = \case
  BlockInMode block BabbageEraInCardanoMode -> extractBabbage state addr block
  _                                         -> mempty

extractBabbage :: UtxoState -> C.AddressInEra BabbageEra -> Block BabbageEra -> UtxoChange
extractBabbage state addr (Block _blockHeader txns) = foldMap (extractBabbageTxn state addr) txns

extractBabbageTxn :: UtxoState -> AddressInEra BabbageEra -> C.Tx BabbageEra -> UtxoChange
extractBabbageTxn UtxoState{_utxos} addr (Tx txBody _) =
  let ShelleyTxBody _ txBody' _scripts scriptData _auxiliaryData _ = txBody
      Babbage.TxBody.TxBody{Babbage.TxBody.inputs} = txBody'
      txId = C.getTxId txBody

      allOuts = C.fromLedgerTxOuts C.ShelleyBasedEraBabbage txBody' scriptData

      checkInput :: TxIn -> Maybe (TxIn, C.TxOut C.CtxTx C.BabbageEra)
      checkInput txIn = fmap (txIn,) $ Map.lookup txIn _utxos

      checkOutput :: TxIx -> C.TxOut C.CtxTx C.BabbageEra -> Maybe (TxIn, C.TxOut C.CtxTx C.BabbageEra)
      checkOutput txIx_ txOut
        | view (L._TxOut . _1) txOut == addr = Just (TxIn txId txIx_, txOut)
        | otherwise = Nothing

      _outputsAdded =
        Map.fromList
        $ mapMaybe (uncurry checkOutput)
        $ (zip (TxIx <$> [0..]) allOuts)

      _outputsRemoved =
        Map.fromList
        $ mapMaybe checkInput
        $ fmap (uncurry TxIn . bimap CS.fromShelleyTxId txIx . (\(CT.TxIn i n) -> (i, n)))
        $ Set.toList inputs
  in UtxoChange{_outputsAdded, _outputsRemoved}

txIx :: CT.TxIx -> TxIx
txIx (CT.TxIx i) = TxIx (fromIntegral i)
