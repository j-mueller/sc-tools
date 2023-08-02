{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TupleSections      #-}
module Convex.Event(
    Extract,
    TxWithEvents(..),
    NewOutputEvent(..),
    OutputSpentEvent(..),
    Event(..),
    ScriptOutDataHash,
    ScriptOut,
    txIn,
    splitEvent,
    -- * Extraction
    ResolvedInputs(..),
    extract,
    convertScript,
    extractBabbageTxn',
    extractBabbageTxn
    ) where

import           Cardano.Api                        (BabbageEra, Block (..),
                                                     BlockHeader,
                                                     BlockInMode (..), BlockNo,
                                                     CardanoMode,
                                                     EraInMode (..), ScriptHash,
                                                     SlotNo, Tx (..), TxId,
                                                     TxIn (..), TxIx (..))
import qualified Cardano.Api                        as C
import           Cardano.Api.Shelley                (TxBody (..))
import qualified Cardano.Api.Shelley                as CS
import qualified Cardano.Ledger.Address             as Address
import qualified Cardano.Ledger.Alonzo.Scripts      as Scripts
import           Cardano.Ledger.Alonzo.Scripts.Data (Data, DataHash)
import           Cardano.Ledger.Alonzo.TxAuxData    (AlonzoTxAuxData (..))
import qualified Cardano.Ledger.Alonzo.TxWits       as TxWitness
import qualified Cardano.Ledger.Babbage             as Babbage
import qualified Cardano.Ledger.Babbage.TxBody      as Babbage.TxBody
import qualified Cardano.Ledger.Babbage.TxOut
import qualified Cardano.Ledger.BaseTypes           as CT
import qualified Cardano.Ledger.Binary
import qualified Cardano.Ledger.Credential          as Credential
import           Cardano.Ledger.Crypto              (StandardCrypto)
import qualified Cardano.Ledger.Era                 as Era
import           Cardano.Ledger.Keys                (KeyHash, KeyRole (Witness))
import           Cardano.Ledger.Shelley.TxAuxData   (Metadatum)
import           Cardano.Ledger.Shelley.TxBody      (witVKeyHash)
import qualified Cardano.Ledger.TxIn                as CT
import           Control.Monad.State.Strict         (MonadState, get, put,
                                                     runState)
import           Convex.Era                         (ERA)
import           Data.Bifunctor                     (Bifunctor (..))
import           Data.Foldable                      (foldl', toList)
import           Data.List                          (sortOn)
import           Data.List.NonEmpty                 (NonEmpty (..))
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Maybe                         (catMaybes, mapMaybe,
                                                     maybeToList)
import qualified Data.Set                           as Set
import           Data.Word                          (Word64)
import           GHC.Generics                       (Generic)
import           Ouroboros.Consensus.Shelley.Eras   (StandardBabbage)

type ScriptOutDataHash = DataHash (Era.EraCrypto ERA)

type Extract a = C.TxOut C.CtxTx C.BabbageEra -> ScriptHash -> Maybe a

data Event a =
  AnOutputSpentEvent !(OutputSpentEvent a)
  | ANewOutputEvent !(NewOutputEvent a)
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)

{-| A transaction annotated with events extracted from it.
-}
data TxWithEvents a =
  TxWithEvents
    { twTx     :: !(Tx BabbageEra)
    , twEvents :: !(NonEmpty (Event a))
    , twBlock  :: !BlockNo
    , twSlot   :: !SlotNo
    } deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)

splitEvent :: Event a -> Either (OutputSpentEvent a) (NewOutputEvent a)
splitEvent = \case
  AnOutputSpentEvent e -> Left e
  ANewOutputEvent e    -> Right e

data OutputSpentEvent a =
  OutputSpentEvent
      { oseTxIn       :: !TxIn
      , oseRedeemer   :: !(Data StandardBabbage)
      , oseDatum      :: !(Data StandardBabbage)
      , oseSpendingTx :: !TxId
      , oseTxOutput   :: !(NewOutputEvent a)
      } deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)

data NewOutputEvent a =
  NewOutputEvent
    { neTransaction :: !TxId
    , neEvent       :: !a
    , neTxIx        :: !TxIx
    , neOutput      :: !ScriptOut
    , neDatum       :: !(Maybe (Data ERA))
    , neBlockNo     :: !Integer
    , neSlot        :: !SlotNo
    , neScriptHash  :: !ScriptHash
    , neDataHash    :: !ScriptOutDataHash
    , neSigners     :: ![KeyHash 'Witness StandardCrypto]
    , neTxMetadata  :: !(Map Word64 Metadatum)
    } deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)

{-| The 'TxIn' of the new output
-}
txIn :: NewOutputEvent a -> TxIn
txIn NewOutputEvent{neTransaction, neTxIx} = TxIn neTransaction neTxIx

newtype ResolvedInputs a = ResolvedInputs (Map TxIn (NewOutputEvent a))
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

extract ::
  Extract a
  -> ResolvedInputs a -- ^ Resolved inputs
  -> BlockInMode CardanoMode -- ^ New block
  -> ([TxWithEvents a], ResolvedInputs a) -- ^ Defi events extracted from block
extract ex resolvedInputs = \case
  BlockInMode block BabbageEraInCardanoMode -> extractBabbageBlock ex resolvedInputs block
  _                                        -> ([], resolvedInputs)

extractBabbageBlock :: Extract a -> ResolvedInputs a -> Block BabbageEra -> ([TxWithEvents a], ResolvedInputs a)
extractBabbageBlock ex resolvedInputs (Block blockHeader txns) =
  first catMaybes $ flip runState resolvedInputs $ traverse (extractBabbageTxn ex blockHeader) txns

type ScriptOut = Babbage.TxBody.BabbageTxOut (Babbage.BabbageEra StandardCrypto)

extractBabbageTxn' :: ResolvedInputs a -> Extract a -> BlockHeader -> Tx BabbageEra -> ([TxWithEvents a], ResolvedInputs a)
extractBabbageTxn' resolvedInputs ex blockHeader tx =
  first maybeToList $ runState (extractBabbageTxn ex blockHeader tx) resolvedInputs

extractBabbageTxn :: forall a m. MonadState (ResolvedInputs a) m => Extract a -> BlockHeader -> Tx BabbageEra -> m (Maybe (TxWithEvents a))
extractBabbageTxn ex (C.BlockHeader slotNo _ twBlock@(C.BlockNo blockNo)) twTx@(Tx txBody keyWitnesses) = do
  ResolvedInputs resolvedInputs <- get
  let txId = C.getTxId txBody
      ShelleyTxBody _ txBody' _scripts scriptData auxiliaryData _ = txBody
      Babbage.TxBody.BabbageTxBody{Babbage.TxBody.btbOutputs, Babbage.TxBody.btbInputs} = txBody'
      TxWitness.TxDats' txDats = case scriptData of
        C.TxBodyScriptData C.ScriptDataInBabbageEra txDats' _ -> txDats'
        _                                                     -> mempty
      txReds = case scriptData of
        C.TxBodyScriptData C.ScriptDataInBabbageEra _ (TxWitness.Redeemers txReds') -> txReds'
        _                                                                          -> mempty

      capiTxOuts = C.fromLedgerTxOuts C.ShelleyBasedEraBabbage txBody' scriptData

      mapOutput :: TxIx -> (ScriptOut, C.TxOut C.CtxTx C.BabbageEra) -> Maybe (ScriptHash, ScriptOut, TxIx, ScriptOutDataHash, a)
      mapOutput ix (scriptOut@(Cardano.Ledger.Babbage.TxOut.BabbageTxOut address _value (Babbage.TxBody.DatumHash dataHash) _), capiOut) = case address of -- FIXME: Could also be Datum?
          Address.Addr _network paymentCredential _stakeReference -> case paymentCredential of
              Credential.ScriptHashObj hsh ->
                  let hsh' = CS.fromShelleyScriptHash hsh in
                  case ex capiOut hsh' of
                    Just a  -> Just (hsh', scriptOut, ix, dataHash, a)
                    Nothing -> Nothing
              _ -> Nothing
          _ -> Nothing
      mapOutput _ _ = Nothing

      relevantOutputs = mapMaybe (uncurry mapOutput) (zip (toEnum <$> [0..]) (fmap (first Cardano.Ledger.Binary.sizedValue) $ zip (toList btbOutputs) capiTxOuts))

      outputEvents :: [NewOutputEvent a]
      outputEvents = fmap mkEvent relevantOutputs where
        mkEvent (neScriptHash, neOutput, neTxIx, neDataHash, neEvent) =
            let neDatum = Map.lookup neDataHash txDats
            in NewOutputEvent
                { neTransaction = txId
                , neTxIx
                , neEvent
                , neOutput
                , neBlockNo = fromIntegral blockNo
                , neSlot = slotNo
                , neScriptHash
                , neDatum
                , neDataHash
                , neSigners = mapMaybe getKeyWitness keyWitnesses
                , neTxMetadata = maybe mempty (\(AlonzoTxAuxData meta _ _) -> meta) auxiliaryData
                }

      outputSpentEvents :: [OutputSpentEvent a]
      outputSpentEvents =
        -- there should always be a redeemer
        -- because we only look at inputs that spend script outputs
        fmap (\(idx, oseTxIn, oseTxOutput) -> maybe (error $ "outputSpentEvents: Redeemer or datum not found: " <> show idx) (\((oseRedeemer, _), oseDatum) -> OutputSpentEvent{oseTxIn, oseTxOutput, oseRedeemer, oseDatum, oseSpendingTx=txId}) $ ((,) <$> Map.lookup (TxWitness.RdmrPtr Scripts.Spend idx) txReds) <*> Map.lookup (neDataHash oseTxOutput) txDats)
        $ mapMaybe (\(idx, oseTxIn) -> fmap (idx, oseTxIn,) $ Map.lookup oseTxIn resolvedInputs)
        $ zip [0..]
        $ fmap (uncurry TxIn . first CS.fromShelleyTxId . second txIx . (\(CT.TxIn i n) -> (i, n)))
        $ sortOn id
        $ Set.toList btbInputs

      resolvedInputsDeleted = ResolvedInputs (foldl' (\inp OutputSpentEvent{oseTxIn} -> Map.delete oseTxIn inp) resolvedInputs outputSpentEvents)

      newResolvedInputs = ResolvedInputs $ Map.fromList $ fmap (\e@NewOutputEvent{neTransaction, neTxIx} -> (TxIn neTransaction neTxIx, e)) outputEvents
  put (newResolvedInputs <> resolvedInputsDeleted)
  let newEvents = fmap ANewOutputEvent outputEvents ++ fmap AnOutputSpentEvent outputSpentEvents
  case newEvents of
    []     -> pure Nothing
    (y:ys) -> return (Just TxWithEvents{twTx, twEvents = y :| ys, twSlot = slotNo, twBlock })

convertScript :: Scripts.Script ERA -> Maybe (C.Script C.PlutusScriptV1)
convertScript = \case
    Scripts.TimelockScript{}          -> Nothing
    Scripts.PlutusScript _language bs -> Just (C.PlutusScript C.PlutusScriptV1 (CS.PlutusScriptSerialised bs))

getKeyWitness :: C.KeyWitness BabbageEra -> Maybe (KeyHash 'Witness StandardCrypto)
getKeyWitness = \case
  CS.ShelleyKeyWitness _era withVKey -> Just $ witVKeyHash withVKey
  _                                  -> Nothing

txIx :: CT.TxIx -> TxIx
txIx (CT.TxIx i) = TxIx (fromIntegral i)
