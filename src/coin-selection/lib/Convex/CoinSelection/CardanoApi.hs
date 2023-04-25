
{-| Balancing functions from @cardano-api@, modified to use
the ledger's @PParams@ type instead of ProtocolParams from
cardano-api.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs     #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Convex.CoinSelection.CardanoApi(
  evaluateTransactionBalance,
  evaluateTransactionFee,
  evaluateTransactionExecutionUnits,
  calculateMinimumUTxO
) where

import qualified Cardano.Api                          as C
import qualified Cardano.Api.Shelley                  as C
import qualified Cardano.Ledger.Alonzo.Data           as Alonzo
import qualified Cardano.Ledger.Alonzo.Language       as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts        as Alonzo
import qualified Cardano.Ledger.Alonzo.Tools          as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness      as Alonzo
import qualified Cardano.Ledger.Babbage.PParams       as Babbage
import qualified Cardano.Ledger.Babbage.TxBody        as Babbage
import qualified Cardano.Ledger.Core                  as CLedger
import qualified Cardano.Ledger.Crypto                as CLedger
import qualified Cardano.Ledger.Keys                  as CLedger
import qualified Cardano.Ledger.Shelley.API.Wallet    as Shelley
import           Cardano.Slotting.EpochInfo.API       (EpochInfo,
                                                       hoistEpochInfo)
import           Cardano.Slotting.Time                (SystemStart)
import           Control.Monad.Except                 (runExcept)
import qualified Convex.Era
import qualified Data.Array                           as Array
import           Data.Bifunctor                       (bimap, first)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Set                             as Set (Set)
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Ouroboros.Consensus.HardFork.History as Consensus

-- | same as function defined in cardano-api but is specialized for Babbage era
--   and accepts a Ledger.PParams as argument instead of ProtocolParameters
evaluateTransactionBalance ::
  CLedger.PParams (C.ShelleyLedgerEra C.BabbageEra)
  -> Set C.PoolId
  -> C.UTxO C.BabbageEra
  -> C.TxBody C.BabbageEra
  -> C.TxOutValue C.BabbageEra
evaluateTransactionBalance pparams poolids utxo (C.ShelleyTxBody _ txbody _ _ _ _) =
  C.TxOutValue C.MultiAssetInBabbageEra
  $ C.fromMaryValue
  $ Shelley.evaluateTransactionBalance pparams (C.toLedgerUTxO C.ShelleyBasedEraBabbage utxo) isNewPool txbody

  where
    isNewPool :: CLedger.KeyHash CLedger.StakePool CLedger.StandardCrypto -> Bool
    isNewPool kh = C.StakePoolKeyHash kh `Set.notMember` poolids

-- | same as function defined in cardano-api but is specialized for Babbage era
--   and accepts a Ledger.PParams as argument instead of ProtocolParameters
evaluateTransactionFee ::
  CLedger.PParams (C.ShelleyLedgerEra C.BabbageEra)
  -> C.TxBody C.BabbageEra
  -> Word  -- ^ The number of Shelley key witnesses
  -> C.Lovelace
evaluateTransactionFee pparams txbody keywitcount  =
  case C.makeSignedTransaction [] txbody of
    C.ShelleyTx _ tx -> C.fromShelleyLovelace $ Shelley.evaluateTransactionFee pparams tx keywitcount

-- FIXME: Looks like this function is exposed by Cardano.Api in cardano-node@v1.36
toLedgerEpochInfo :: C.EraHistory mode -> EpochInfo (Either Text)
toLedgerEpochInfo (C.EraHistory _ interpreter) =
  hoistEpochInfo (first (Text.pack . show) . runExcept) $
    Consensus.interpreterToEpochInfo interpreter

-- | same as function defined in cardano-api but is specialized for Babbage era
--   and accepts a Ledger.PParams as argument instead of ProtocolParameters
evaluateTransactionExecutionUnits ::
  SystemStart
  -> C.EraHistory C.CardanoMode
  -> CLedger.PParams (C.ShelleyLedgerEra C.BabbageEra)
  -> C.UTxO C.BabbageEra
  -> C.TxBody C.BabbageEra
  -> Either C.TransactionValidityError
            (Map C.ScriptWitnessIndex (Either C.ScriptExecutionError C.ExecutionUnits))
evaluateTransactionExecutionUnits systemstart history pparams utxo txbody =
  -- considering babbageEra only
  case C.makeSignedTransaction [] txbody of
    C.ShelleyTx _ tx' -> evalBabbage tx'

  where
    evalBabbage tx =
     let costModelsArray = toAlonzoCostModelsArray pparams in
       case Alonzo.evaluateTransactionExecutionUnits
            pparams
            tx
            (C.toLedgerUTxO C.ShelleyBasedEraBabbage utxo)
            (toLedgerEpochInfo history)
            systemstart
            costModelsArray
        of Left err    -> Left (C.TransactionValidityTranslationError err)
           Right exmap -> Right (fromLedgerScriptExUnitsMap exmap)

    toAlonzoCostModelsArray
      :: CLedger.PParams (C.ShelleyLedgerEra C.BabbageEra)
      -> Array.Array Alonzo.Language Alonzo.CostModel
    toAlonzoCostModelsArray lparams =
     let Alonzo.CostModels cModels = Babbage._costmdls lparams in
       Array.array (minBound, maxBound) (Map.toList cModels)


    fromLedgerScriptExUnitsMap
      :: Map Alonzo.RdmrPtr (Either (Alonzo.TransactionScriptFailure CLedger.StandardCrypto) Alonzo.ExUnits)
      -> Map C.ScriptWitnessIndex (Either C.ScriptExecutionError C.ExecutionUnits)
    fromLedgerScriptExUnitsMap exmap =
      Map.fromList
      [ (C.fromAlonzoRdmrPtr rdmrptr,
          bimap fromAlonzoScriptExecutionError C.fromAlonzoExUnits exunitsOrFailure)
      | (rdmrptr, exunitsOrFailure) <- Map.toList exmap ]


    fromAlonzoScriptExecutionError :: Alonzo.TransactionScriptFailure CLedger.StandardCrypto
                                   -> C.ScriptExecutionError
    fromAlonzoScriptExecutionError failure =
      case failure of
        Alonzo.UnknownTxIn     txin -> C.ScriptErrorMissingTxIn txin'
                                         where txin' = C.fromShelleyTxIn txin
        Alonzo.InvalidTxIn     txin -> C.ScriptErrorTxInWithoutDatum txin'
                                         where txin' = C.fromShelleyTxIn txin
        Alonzo.MissingDatum      dh -> C.ScriptErrorWrongDatum (C.ScriptDataHash dh)
        Alonzo.ValidationFailedV1 err logs -> C.ScriptErrorEvaluationFailed err logs
        Alonzo.ValidationFailedV2 err logs -> C.ScriptErrorEvaluationFailed err logs
        Alonzo.IncompatibleBudget _ -> C.ScriptErrorExecutionUnitsOverflow

        -- This is only possible for spending scripts and occurs when
        -- we attempt to spend a key witnessed tx input with a Plutus
        -- script witness.
        Alonzo.RedeemerNotNeeded rdmrPtr sh ->
          C.ScriptErrorNotPlutusWitnessedTxIn
            (C.fromAlonzoRdmrPtr rdmrPtr)
            (C.fromShelleyScriptHash sh)
        Alonzo.RedeemerPointsToUnknownScriptHash rdmrPtr ->
          C.ScriptErrorRedeemerPointsToUnknownScriptHash $ C.fromAlonzoRdmrPtr rdmrPtr
        -- This should not occur while using cardano-cli because we zip together
        -- the Plutus script and the use site (txin, certificate etc). Therefore
        -- the redeemer pointer will always point to a Plutus script.
        Alonzo.MissingScript rdmrPtr resolveable -> C.ScriptErrorMissingScript rdmrPtr resolveable

        Alonzo.NoCostModelInLedgerState l -> C.ScriptErrorMissingCostModel l

calculateMinimumUTxO
  :: C.TxOut C.CtxTx C.BabbageEra
  -> CLedger.PParams (C.ShelleyLedgerEra C.BabbageEra)
  -> Either C.MinimumUTxOError C.Value
calculateMinimumUTxO txout pparams' =
  let lTxOut = toShelleyTxOutAny txout
      minUTxO = Shelley.evaluateMinLovelaceOutput pparams' lTxOut
      val = C.lovelaceToValue $ C.fromShelleyLovelace minUTxO
  in Right val

-- | A variant of 'toShelleyTxOutAny that is used only internally to this module
-- that works with a 'TxOut' in any context (including CtxTx) by ignoring
-- embedded datums (taking only their hash).
--
toShelleyTxOutAny :: C.TxOut C.CtxTx C.BabbageEra -> CLedger.TxOut (Convex.Era.ERA)
toShelleyTxOutAny (C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra value) txoutdata refScript) =
    let cEra = C.shelleyBasedToCardanoEra C.ShelleyBasedEraBabbage
    in Babbage.TxOut (C.toShelleyAddr addr) (C.toMaryValue value)
                    (toBabbageTxOutDatum' txoutdata) (C.refScriptToShelleyScript cEra refScript)

-- TODO: Consolidate with alonzo function and rename
toBabbageTxOutDatum'
  :: C.TxOutDatum C.CtxTx (C.BabbageEra) -> Babbage.Datum (Convex.Era.ERA)
toBabbageTxOutDatum'  C.TxOutDatumNone = Babbage.NoDatum
toBabbageTxOutDatum' (C.TxOutDatumHash _ (C.ScriptDataHash dh)) = Babbage.DatumHash dh
toBabbageTxOutDatum' (C.TxOutDatumInTx _ d) = let C.ScriptDataHash hsh = C.hashScriptData d in Babbage.DatumHash hsh
toBabbageTxOutDatum' (C.TxOutDatumInline _ sd) = scriptDataToInlineDatum sd

scriptDataToInlineDatum :: C.ScriptData -> Babbage.Datum ledgerera
scriptDataToInlineDatum = Babbage.Datum . Alonzo.dataToBinaryData . C.toAlonzoData
