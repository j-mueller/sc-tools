{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | Building cardano transactions from tx bodies
module Convex.CoinSelection (
  -- * Data types
  CoinSelectionError (..),
  AsCoinSelectionError (..),
  bodyError,
  TransactionSignatureCount (..),
  CSInputs (..),
  ERA,
  utxo,
  txBody,
  changeOutput,
  numWitnesses,

  -- * Balancing
  BalanceTxError (..),
  AsBalanceTxError (..),
  BalancingError (..),
  AsBalancingError (..),
  TxBalancingMessage (..),
  ChangeOutputPosition (..),
  balanceTransactionBody,
  balanceForWallet,
  balanceForWalletReturn,
  balanceTx,
  signForWallet,
  signBalancedTxBody,

  -- * Balance changes
  balanceChanges,
  spentTxIns,

  -- * Etc.
  prepCSInputs,
  keyWitnesses,
  publicKeyCredential,
  exactScriptExecutionError,
) where

import Cardano.Api qualified
import Cardano.Api.Experimental qualified as C.Experimental
import Cardano.Api.Ledger qualified as CLedger
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley (
  BuildTx,
  ConwayEra,
  EraHistory,
  PoolId,
  TxBodyContent,
  TxOut,
  UTxO (..),
 )
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Core (PParams (..), hkdKeyDepositL)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys qualified as Keys
import Cardano.Ledger.Shelley.API (
  Coin (..),
  Credential (..),
  KeyHash (..),
  KeyRole (..),
  PoolParams (..),
 )
import Cardano.Ledger.Shelley.Core (EraCrypto)
import Cardano.Ledger.Shelley.TxCert qualified as TxCert
import Cardano.Slotting.Time (SystemStart)
import Control.Lens (
  Prism',
  at,
  makeLensesFor,
  over,
  preview,
  prism',
  review,
  set,
  to,
  traversed,
  view,
  (%~),
  (&),
  (<>~),
  (<|),
  (?~),
  (^.),
  (^..),
  _1,
  _2,
  _3,
  (|>),
 )
import Control.Lens qualified as L
import Control.Lens.TH (makeClassyPrisms)
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Tracer (Tracer, natTracer, traceWith)
import Convex.BuildTx (
  TxBuilder,
  addCollateral,
  execBuildTx,
  setMinAdaDeposit,
  spendPublicKeyOutput,
 )
import Convex.BuildTx qualified as BuildTx
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (MonadBlockchain (..))
import Convex.UTxOCompatibility (
  UTxOCompatibility,
  compatibleWith,
  txCompatibility,
 )
import Convex.Utils (
  inAlonzo,
  inBabbage,
  inMary,
  mapError,
  requiredTxIns,
 )
import Convex.Utxos (
  BalanceChanges (..),
  UtxoSet (..),
 )
import Convex.Utxos qualified as Utxos
import Convex.Wallet (Wallet)
import Convex.Wallet qualified as Wallet
import Data.Aeson (FromJSON (..), Options (sumEncoding), SumEncoding (..), ToJSON (..))
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.Default (Default (..))
import Data.Function (on)
import Data.Functor.Identity (Identity)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (
  isNothing,
  listToMaybe,
  mapMaybe,
  maybeToList,
 )
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import GHC.IsList (IsList (fromList, toList))

type ERA = ConwayEra

{- Note [Change Output]

The balancing functions take a "change output" parameter. This is a @TxOut@ value that will
receive any Ada change that's leftover after balancing.

If the change output has a non-zero value (of any currency) then it will be included in the
final transaction regardless of the final balance of the transaction.

-}

-- | The expected number of signatures that will be attached to the transaction
newtype TransactionSignatureCount = TransactionSignatureCount {unTransactionSignatureCount :: Word}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, ToJSON, FromJSON, Enum)

instance Default TransactionSignatureCount where
  def = 1

-- | Inputs needed for coin selection
data CSInputs era
  = CSInputs
  { csiUtxo :: UTxO era
  -- ^ UTXOs that we need to know about
  , csiTxBody :: TxBodyContent BuildTx era
  -- ^ Tx body to balance
  , csiChangeOutput :: C.TxOut C.CtxTx era
  -- ^ Change output -- see Note [Change Output]
  , csiNumWitnesses :: TransactionSignatureCount
  -- ^ How many shelley witnesses there will be
  }

makeLensesFor
  [ ("csiUtxo", "utxo")
  , ("csiTxBody", "txBody")
  , ("csiChangeOutput", "changeOutput")
  , ("csiNumWitnesses", "numWitnesses")
  ]
  ''CSInputs

data CoinSelectionError
  = UnsupportedBalance (C.TxOutValue ERA)
  | BodyError Text
  | NotEnoughInputsFor {lovelaceRequired :: C.Quantity, lovelaceFound :: C.Quantity}
  | NotEnoughMixedOutputsFor {valuesNeeded :: C.Value, valueProvided :: C.Value, txBalance :: C.Value}
  | -- | The wallet utxo set is empty
    NoWalletUTxOs
  | -- | The transaction body needs a collateral input, but there are no inputs that hold nothing but Ada
    NoAdaOnlyUTxOsForCollateral
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''CoinSelectionError

bodyError :: C.TxBodyError -> CoinSelectionError
bodyError = BodyError . Text.pack . C.docToString . C.prettyError

-- Orphan instance, needed to allow full json from here on.
-- TODO: Check that automatic generation is compatible with cardano-api encoding.

$(deriveFromJSON defaultOptions{sumEncoding = TaggedObject{tagFieldName = "kind", contentsFieldName = "value"}} ''C.ScriptWitnessIndex)

-- | Balancing errors, including the *special* 'BalancingScriptExecutionErr'.
data BalancingError era
  = BalancingError Text
  | -- | A single type of balancing error is treated specially: the type with
    -- 'C.ScriptExecutionError's.
    -- TODO: I would like to retain the actual error structure, but this collides (quite massively)
    -- with the required JSON encoding / decoding.
    ScriptExecutionErr [(C.ScriptWitnessIndex, Text, [Text])]
  | CheckMinUtxoValueError (C.TxOut C.CtxTx era) C.Quantity
  | BalanceCheckError (BalancingError era)
  | ComputeBalanceChangeError
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''BalancingError

{- | This prism will either match on the exact error using the script witness index and an error
string, or just on being a script execution error. The latter is necessary, since in non-debug
mode, there is no error to match on and the prism should not fail in its purpose in non-debug
mode.
-}
exactScriptExecutionError :: forall e era. (AsBalancingError e era) => (Int, Text) -> Prism' e [(C.ScriptWitnessIndex, Text, [Text])]
exactScriptExecutionError (i, s) = prism' tobe frombe
 where
  tobe :: [(C.ScriptWitnessIndex, Text, [Text])] -> e
  tobe = review _ScriptExecutionErr
  frombe :: e -> Maybe [(C.ScriptWitnessIndex, Text, [Text])]
  frombe x = case preview _ScriptExecutionErr x of
    Nothing -> Nothing
    Just xs
      -- index exists, and either we have no logs, of the last entry is the error string to match
      -- on.
      | Just (_, _, logs) <- xs L.^? L.ix i
      , null logs || last logs == s ->
          Just xs
      -- We don't have any internal logs, but still a script error
      | null xs -> Just xs
      -- Not the correct error
      | otherwise -> Nothing

{- | Sort *most* balancing errors into 'BalancingError', but script execution errors into their own
data constructor.
-}
balancingError :: (MonadError (BalancingError era) m) => Either (C.TxBodyErrorAutoBalance era) a -> m a
balancingError = \case
  Right a -> pure a
  Left (C.TxBodyScriptExecutionError es) -> throwError $ ScriptExecutionErr (map extractErrorInfo es)
  Left err -> throwError . BalancingError $ asText err
 where
  asText = Text.pack . C.docToString . C.prettyError
  extractErrorInfo :: (C.ScriptWitnessIndex, C.ScriptExecutionError) -> (C.ScriptWitnessIndex, Text, [Text])
  extractErrorInfo (wix, C.ScriptErrorEvaluationFailed evalErr logs) = (wix, Text.pack $ show evalErr, logs)
  extractErrorInfo (wix, other) = (wix, Text.pack $ show other, [])

-- | Messages that are produced during coin selection and balancing
data TxBalancingMessage
  = SelectingCoins
  | -- | The plutus compatibility level applied to the wallet tx outputs
    CompatibilityLevel {compatibility :: !UTxOCompatibility, droppedTxIns :: !Int}
  | -- | Preparing to balance the transaction using the available wallet balance
    PrepareInputs {availableBalance :: C.Value, transactionBalance :: C.Value}
  | -- | Balancing a transaction body
    StartBalancing {numInputs :: !Int, numOutputs :: !Int}
  | -- | Execution units of the transaction, or error message in case of script error
    ExUnitsMap {exUnits :: [(C.ScriptWitnessIndex, Either String C.ExecutionUnits)]}
  | -- | The transaction fee
    Txfee {fee :: C.Quantity}
  | -- | The remaining balance (after paying the fee)
    TxRemainingBalance {remainingBalance :: C.Value}
  | -- | The transaction was not missing any assets
    NoAssetsMissing
  | -- | The transaction is missing some inputs, these will be covered from the wallet's UTxOs.
    MissingAssets C.Value
  | -- | The transaction is missing some Ada. The amount will be covered from the wallet's UTxOs.
    MissingLovelace C.Quantity
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data ChangeOutputPosition
  = LeadingChange
  | TrailingChange

-- | Perform transaction balancing with configurable change output position
balanceTransactionBody
  :: forall era m
   . (MonadError (BalancingError era) m, C.IsBabbageBasedEra era)
  => Tracer m TxBalancingMessage
  -> SystemStart
  -> EraHistory
  -> C.LedgerProtocolParameters era
  -> Set PoolId
  -> CSInputs era
  -> ChangeOutputPosition
  -> m (C.BalancedTxBody era, BalanceChanges)
balanceTransactionBody
  tracer
  systemStart
  eraHistory
  protocolParams
  stakePools
  CSInputs{csiUtxo, csiTxBody, csiChangeOutput, csiNumWitnesses = TransactionSignatureCount numWits}
  changePosition = inBabbage @era $ do
    let csiChangeLatestEraOutput = csiChangeOutput
        mkChangeOutputFor i = csiChangeLatestEraOutput & L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId ?~ i
        changeOutputSmall = mkChangeOutputFor 1
        changeOutputLarge = mkChangeOutputFor ((2 ^ (64 :: Integer)) - 1)

    traceWith tracer StartBalancing{numInputs = csiTxBody ^. L.txIns . to length, numOutputs = csiTxBody ^. L.txOuts . to length}

    -- Function to add change output based on position
    let addChangeOutput change txb = case changePosition of
          LeadingChange -> txb & prependTxOut change
          TrailingChange -> txb & appendTxOut change

    txbody0 <-
      balancingError . first C.TxBodyError $ C.createTransactionBody C.shelleyBasedEra $ csiTxBody & addChangeOutput changeOutputSmall

    exUnitsMap <-
      balancingError . first C.TxBodyErrorValidityInterval $
        C.evaluateTransactionExecutionUnits
          C.cardanoEra
          systemStart
          (C.toLedgerEpochInfo eraHistory)
          protocolParams
          csiUtxo
          txbody0

    traceWith tracer $ ExUnitsMap $ fmap (second (first (C.docToString . C.prettyError) . fmap snd)) $ Map.toList exUnitsMap

    exUnitsMap' <- balancingError $
      case Map.mapEither id exUnitsMap of
        (failures, exUnitsMap') ->
          handleExUnitsErrors C.ScriptValid failures $
            fmap snd exUnitsMap' -- TODO: should this take the script validity from csiTxBody?
    txbodycontent1 <- balancingError $ substituteExecutionUnits exUnitsMap' csiTxBody
    let txbodycontent1' = txbodycontent1 & set L.txFee (Coin (2 ^ (32 :: Integer) - 1)) & over L.txOuts (|> changeOutputLarge)

    -- append output instead of prepending
    txbody1 <- balancingError . first C.TxBodyError $ C.createTransactionBody C.shelleyBasedEra txbodycontent1'

    let !t_fee = C.calculateMinTxFee C.shelleyBasedEra (C.unLedgerProtocolParameters protocolParams) csiUtxo txbody1 numWits
    traceWith tracer Txfee{fee = C.lovelaceToQuantity t_fee}

    let txbodycontent2 = txbodycontent1 & set L.txFee t_fee & appendTxOut csiChangeLatestEraOutput
    txbody2 <- balancingError . first C.TxBodyError $ C.createTransactionBody C.shelleyBasedEra txbodycontent2

    let unregPoolStakeBalance = C.maryEraOnwardsConstraints @era C.maryBasedEra unregBalance protocolParams txbodycontent2

    let !balance = view L._TxOutValue (Cardano.Api.evaluateTransactionBalance C.shelleyBasedEra (C.unLedgerProtocolParameters protocolParams) stakePools unregPoolStakeBalance mempty csiUtxo txbody2)

    traceWith tracer TxRemainingBalance{remainingBalance = balance}

    mapM_ (\x -> checkMinUTxOValue x protocolParams) $ C.txOuts txbodycontent1

    -- debug "balanceTransactionBody: changeOutputBalance"
    changeOutputBalance <- case C.valueToLovelace balance of
      Just b -> do
        let op = csiChangeLatestEraOutput & L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId <>~ (Just $ C.lovelaceToQuantity b)
        balanceCheck protocolParams op
        pure op
      Nothing -> balancingError $ Left $ C.TxBodyErrorNonAdaAssetsUnbalanced balance

    let finalBodyContent =
          txbodycontent1
            & set L.txFee t_fee
            & addChangeOutput changeOutputBalance

    unsingnedTx <- balancingError . first C.TxBodyError $ C.Experimental.makeUnsignedTx (C.Experimental.babbageEraOnwardsToEra $ C.babbageBasedEra @era) finalBodyContent

    balances <- maybe (throwError ComputeBalanceChangeError) pure (balanceChanges csiUtxo finalBodyContent)

    let finalBody = C.BalancedTxBody finalBodyContent unsingnedTx changeOutputBalance t_fee
    return (finalBody, balances)

checkMinUTxOValue
  :: (C.IsShelleyBasedEra era, MonadError (BalancingError era) m)
  => C.TxOut C.CtxTx era
  -> C.LedgerProtocolParameters era
  -> m ()
checkMinUTxOValue txout@(C.TxOut _ v _ _) pparams' = do
  let minUTxO = C.calculateMinimumUTxO C.shelleyBasedEra txout (C.unLedgerProtocolParameters pparams')
  if C.txOutValueToLovelace v >= minUTxO
    then pure ()
    else throwError (CheckMinUtxoValueError txout $ C.lovelaceToQuantity minUTxO)

prependTxOut :: C.TxOut C.CtxTx era -> C.TxBodyContent C.BuildTx era -> C.TxBodyContent C.BuildTx era
prependTxOut out = over L.txOuts (out <|)

appendTxOut :: C.TxOut C.CtxTx era -> C.TxBodyContent C.BuildTx era -> C.TxBodyContent C.BuildTx era
appendTxOut out = over L.txOuts (|> out)

{- | Check that the output has a positive Ada balance greater than or equal to the minimum
UTxO requirement
-}
balanceCheck
  :: (C.IsShelleyBasedEra era, MonadError (BalancingError era) m)
  => C.LedgerProtocolParameters era
  -> C.TxOut C.CtxTx era
  -> m ()
balanceCheck pparams output@(C.TxOut _ (C.txOutValueToValue -> value) _ _) =
  let valueLovelace = C.selectLovelace value
   in if value == mempty
        then return ()
        else do
          when (valueLovelace < 0) (balancingError $ Left $ C.TxBodyErrorAdaBalanceNegative valueLovelace)
          checkMinUTxOValue output pparams

handleExUnitsErrors
  :: C.ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> Map C.ScriptWitnessIndex C.ScriptExecutionError
  -> Map C.ScriptWitnessIndex C.ExecutionUnits
  -> Either (C.TxBodyErrorAutoBalance era) (Map C.ScriptWitnessIndex C.ExecutionUnits)
handleExUnitsErrors C.ScriptValid failuresMap exUnitsMap =
  if null failures
    then Right exUnitsMap
    else Left (C.TxBodyScriptExecutionError failures)
 where
  failures :: [(C.ScriptWitnessIndex, C.ScriptExecutionError)]
  failures = Map.toList failuresMap
handleExUnitsErrors C.ScriptInvalid failuresMap exUnitsMap
  | null scriptFailures = Left C.TxBodyScriptBadScriptValidity
  | null nonScriptFailures = Right exUnitsMap
  | otherwise = Left (C.TxBodyScriptExecutionError nonScriptFailures)
 where
  nonScriptFailures :: [(C.ScriptWitnessIndex, C.ScriptExecutionError)]
  nonScriptFailures = filter (not . isScriptErrorEvaluationFailed) (Map.toList failuresMap)
  scriptFailures :: [(C.ScriptWitnessIndex, C.ScriptExecutionError)]
  scriptFailures = filter isScriptErrorEvaluationFailed (Map.toList failuresMap)
  isScriptErrorEvaluationFailed :: (C.ScriptWitnessIndex, C.ScriptExecutionError) -> Bool
  isScriptErrorEvaluationFailed (_, e) = case e of
    C.ScriptErrorEvaluationFailed _ _ -> True
    _ -> True

substituteExecutionUnits
  :: (C.IsShelleyBasedEra era)
  => Map C.ScriptWitnessIndex C.ExecutionUnits
  -> C.TxBodyContent BuildTx era
  -> Either (C.TxBodyErrorAutoBalance x) (C.TxBodyContent BuildTx era)
substituteExecutionUnits exUnitsMap =
  mapTxScriptWitnesses f
 where
  f
    :: C.ScriptWitnessIndex
    -> C.ScriptWitness witctx era
    -> Either (C.TxBodyErrorAutoBalance x) (C.ScriptWitness witctx era)
  f _ wit@C.SimpleScriptWitness{} = Right wit
  f idx (C.PlutusScriptWitness langInEra version script datum redeemer _) =
    case Map.lookup idx exUnitsMap of
      Nothing ->
        Left $ C.TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap idx exUnitsMap
      Just exunits -> Right $ C.PlutusScriptWitness langInEra version script datum redeemer exunits

-- | same behaviour as in Cardano.Api.TxBody.
mapTxScriptWitnesses
  :: forall era x
   . (C.IsShelleyBasedEra era)
  => ( forall witctx
        . C.ScriptWitnessIndex
       -> C.ScriptWitness witctx era
       -> Either (C.TxBodyErrorAutoBalance x) (C.ScriptWitness witctx era)
     )
  -> C.TxBodyContent BuildTx era
  -> Either (C.TxBodyErrorAutoBalance x) (TxBodyContent BuildTx era)
mapTxScriptWitnesses
  f
  txbodycontent@C.TxBodyContent
    { C.txIns
    , C.txWithdrawals
    , C.txCertificates
    , C.txMintValue
    } = do
    mappedTxIns <- mapScriptWitnessesTxIns txIns
    mappedWithdrawals <- mapScriptWitnessesWithdrawals txWithdrawals
    mappedMintedVals <- mapScriptWitnessesMinting txMintValue
    mappedTxCertificates <- mapScriptWitnessesCertificates txCertificates

    Right $
      txbodycontent
        { C.txIns = mappedTxIns
        , C.txMintValue = mappedMintedVals
        , C.txCertificates = mappedTxCertificates
        , C.txWithdrawals = mappedWithdrawals
        }
   where
    mapScriptWitnessesTxIns
      :: [(C.TxIn, C.BuildTxWith BuildTx (C.Witness C.WitCtxTxIn era))]
      -> Either (C.TxBodyErrorAutoBalance x) [(C.TxIn, C.BuildTxWith BuildTx (C.Witness C.WitCtxTxIn era))]
    mapScriptWitnessesTxIns txins =
      let mappedScriptWitnesses
            :: [ ( C.TxIn
                 , Either (C.TxBodyErrorAutoBalance x) (C.BuildTxWith BuildTx (C.Witness C.WitCtxTxIn era))
                 )
               ]
          mappedScriptWitnesses =
            [ (txin, C.BuildTxWith <$> wit')
            | -- The tx ins are indexed in the map order by txid
            (ix, (txin, C.BuildTxWith wit)) <- zip [0 ..] (orderTxIns txins)
            , let wit' = case wit of
                    C.KeyWitness{} -> Right wit
                    C.ScriptWitness ctx witness -> C.ScriptWitness ctx <$> witness'
                     where
                      witness' = f (C.ScriptWitnessIndexTxIn ix) witness
            ]
       in traverse
            ( \(txIn, eWitness) ->
                case eWitness of
                  Left e -> Left e
                  Right wit -> Right (txIn, wit)
            )
            mappedScriptWitnesses

    mapScriptWitnessesWithdrawals
      :: C.TxWithdrawals BuildTx era
      -> Either (C.TxBodyErrorAutoBalance x) (C.TxWithdrawals BuildTx era)
    mapScriptWitnessesWithdrawals C.TxWithdrawalsNone = Right C.TxWithdrawalsNone
    mapScriptWitnessesWithdrawals (C.TxWithdrawals supported withdrawals) =
      let mappedWithdrawals
            :: [ ( C.StakeAddress
                 , Coin
                 , Either (C.TxBodyErrorAutoBalance x) (C.BuildTxWith BuildTx (C.Witness C.WitCtxStake era))
                 )
               ]
          mappedWithdrawals =
            [ (addr, withdrawal, C.BuildTxWith <$> mappedWitness)
            | -- The withdrawals are indexed in the map order by stake credential
            (ix, (addr, withdrawal, C.BuildTxWith wit)) <- zip [0 ..] (orderStakeAddrs withdrawals)
            , let mappedWitness = adjustWitness (f (C.ScriptWitnessIndexWithdrawal ix)) wit
            ]
       in C.TxWithdrawals supported
            <$> traverse
              ( \(sAddr, ll, eWitness) ->
                  case eWitness of
                    Left e -> Left e
                    Right wit -> Right (sAddr, ll, wit)
              )
              mappedWithdrawals
     where
      adjustWitness
        :: (C.ScriptWitness witctx era -> Either (C.TxBodyErrorAutoBalance x) (C.ScriptWitness witctx era))
        -> C.Witness witctx era
        -> Either (C.TxBodyErrorAutoBalance x) (C.Witness witctx era)
      adjustWitness _ (C.KeyWitness ctx) = Right $ C.KeyWitness ctx
      adjustWitness g (C.ScriptWitness ctx witness') = C.ScriptWitness ctx <$> g witness'

    mapScriptWitnessesCertificates
      :: C.TxCertificates BuildTx era
      -> Either (C.TxBodyErrorAutoBalance x) (C.TxCertificates BuildTx era)
    mapScriptWitnessesCertificates C.TxCertificatesNone = Right C.TxCertificatesNone
    mapScriptWitnessesCertificates (C.TxCertificates supported certs (C.BuildTxWith witnesses)) =
      let mappedScriptWitnesses
            :: [(C.StakeCredential, Either (C.TxBodyErrorAutoBalance x) (C.Witness C.WitCtxStake era))]
          mappedScriptWitnesses =
            [ (stakecred, C.ScriptWitness ctx <$> witness')
            | -- The certs are indexed in list order
            (ix, cert) <- zip [0 ..] certs
            , stakecred <- maybeToList (C.shelleyBasedEraConstraints @era C.shelleyBasedEra selectStakeCredential cert)
            , C.ScriptWitness ctx witness <- maybeToList (List.lookup stakecred witnesses)
            , let witness' = f (C.ScriptWitnessIndexCertificate ix) witness
            ]
       in C.TxCertificates supported certs . C.BuildTxWith
            <$> traverse
              ( \(sCred, eScriptWitness) ->
                  case eScriptWitness of
                    Left e -> Left e
                    Right wit -> Right (sCred, wit)
              )
              mappedScriptWitnesses

    selectStakeCredential :: (EraCrypto (C.ShelleyLedgerEra era) ~ StandardCrypto) => C.Certificate era -> Maybe C.StakeCredential
    selectStakeCredential = \case
      C.ShelleyRelatedCertificate _era cert -> case cert of
        TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyRegCert k) -> Just (C.fromShelleyStakeCredential k)
        TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyUnRegCert k) -> Just (C.fromShelleyStakeCredential k)
        TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyDelegCert k _) -> Just (C.fromShelleyStakeCredential k)
        TxCert.ShelleyTxCertPool{} -> Nothing
        TxCert.ShelleyTxCertGenesisDeleg{} -> Nothing
        TxCert.ShelleyTxCertMir{} -> Nothing
      C.ConwayCertificate _era cert -> case cert of
        CLedger.ConwayTxCertDeleg (CLedger.ConwayRegCert k _) -> Just (C.fromShelleyStakeCredential k)
        CLedger.ConwayTxCertDeleg (CLedger.ConwayUnRegCert k _) -> Just (C.fromShelleyStakeCredential k)
        CLedger.ConwayTxCertDeleg (CLedger.ConwayDelegCert k _) -> Just (C.fromShelleyStakeCredential k)
        CLedger.ConwayTxCertDeleg (CLedger.ConwayRegDelegCert k _dlgte _deposit) -> Just (C.fromShelleyStakeCredential k)
        _ -> Nothing

    mapScriptWitnessesMinting
      :: C.TxMintValue BuildTx era
      -> Either (C.TxBodyErrorAutoBalance x) (C.TxMintValue BuildTx era)
    mapScriptWitnessesMinting C.TxMintNone = Right C.TxMintNone
    mapScriptWitnessesMinting (C.TxMintValue supported value (C.BuildTxWith witnesses)) =
      let mappedScriptWitnesses
            :: [(C.PolicyId, Either (C.TxBodyErrorAutoBalance x) (C.ScriptWitness C.WitCtxMint era))]
          mappedScriptWitnesses =
            [ (policyid, witness')
            | -- The minting policies are indexed in policy id order in the value
            let C.ValueNestedRep bundle = C.valueToNestedRep value
            , (ix, C.ValueNestedBundle policyid _) <- zip [0 ..] bundle
            , witness <- maybeToList (Map.lookup policyid witnesses)
            , let witness' = f (C.ScriptWitnessIndexMint ix) witness
            ]
       in do
            final <-
              traverse
                ( \(pid, eScriptWitness) ->
                    case eScriptWitness of
                      Left e -> Left e
                      Right wit -> Right (pid, wit)
                )
                mappedScriptWitnesses
            Right . C.TxMintValue supported value . C.BuildTxWith $
              Map.fromList final

{- | This relies on the TxId Ord instance being consistent with the
Ledger.TxId Ord instance via the toShelleyTxId conversion.
This is checked by prop_ord_distributive_TxId
-}
orderTxIns :: [(C.TxIn, v)] -> [(C.TxIn, v)]
orderTxIns = List.sortBy (compare `on` fst)

{- | This relies on the StakeAddress Ord instance being consistent with the
Shelley.RewardAcnt Ord instance via the toShelleyStakeAddr conversion.
This is checked by prop_ord_distributive_StakeAddress
-}
orderStakeAddrs :: [(C.StakeAddress, x, v)] -> [(C.StakeAddress, x, v)]
orderStakeAddrs = List.sortBy (compare `on` (\(k, _, _) -> k))

{- | Get the 'BalanceChanges' for a tx body. Returns 'Nothing' if
a UTXO couldnt be found
-}
balanceChanges :: (C.IsMaryBasedEra era) => UTxO era -> TxBodyContent BuildTx era -> Maybe BalanceChanges
balanceChanges (C.UTxO lookups) body = do
  let outputs = foldMap txOutChange (body ^. L.txOuts)
  inputs <- Utxos.invBalanceChange . foldMap (txOutChange . id) <$> traverse (\(txi, _) -> Map.lookup txi lookups) (body ^. L.txIns)
  pure (outputs <> inputs)

unregBalance :: forall era. (L.EraPParams (C.ShelleyLedgerEra era), EraCrypto (C.ShelleyLedgerEra era) ~ StandardCrypto) => C.LedgerProtocolParameters era -> TxBodyContent BuildTx era -> Map.Map C.StakeCredential Coin
unregBalance (C.unLedgerProtocolParameters -> PParams phkd) txbodycontent =
  let (deposit :: Coin) = view (hkdKeyDepositL @_ @Identity) phkd
      certs = txbodycontent ^. L.txCertificates
      toUnregCert :: C.Certificate era -> Maybe (C.StakeCredential, Coin)
      toUnregCert (C.ConwayCertificate _ (CLedger.ConwayTxCertDeleg (CLedger.ConwayUnRegCert cred (CLedger.SJust a)))) = Just (C.fromShelleyStakeCredential cred, a)
      toUnregCert (C.ConwayCertificate _ (CLedger.ConwayTxCertDeleg (CLedger.ConwayUnRegCert cred CLedger.SNothing))) = Just (C.fromShelleyStakeCredential cred, deposit)
      toUnregCert (C.ShelleyRelatedCertificate _ (CLedger.ShelleyTxCertDelegCert (CLedger.ShelleyUnRegCert cred))) = Just (C.fromShelleyStakeCredential cred, deposit)
      toUnregCert _ = Nothing
   in case certs of
        C.TxCertificatesNone -> mempty
        C.TxCertificates _ cs _ -> Map.fromList $ mapMaybe toUnregCert cs

txOutChange :: forall era ctx. (C.IsMaryBasedEra era) => TxOut ctx era -> BalanceChanges
txOutChange (view L._TxOut -> (fmap C.fromShelleyPaymentCredential . preview (inMary @era L._AddressInEra . L._Address . _2) -> Just addr, view L._TxOutValue -> value, _, _)) =
  BalanceChanges (Map.singleton addr value)
txOutChange _ = mempty

data BalanceTxError era
  = ACoinSelectionError CoinSelectionError
  | ABalancingError (BalancingError era)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''BalanceTxError

instance AsCoinSelectionError (BalanceTxError era) where
  _CoinSelectionError = _ACoinSelectionError . _CoinSelectionError

instance AsBalancingError (BalanceTxError era) era where
  __BalancingError = _ABalancingError . __BalancingError

{- | Balance the transaction using the given UTXOs and return address. This
calls 'balanceTransactionBody' after preparing all the required inputs.
-}
balanceTx
  :: forall era m a
   . (MonadBlockchain era m, MonadError (BalanceTxError era) m, C.IsBabbageBasedEra era)
  => Tracer m TxBalancingMessage
  -- ^ Label
  -> C.TxOut C.CtxTx era
  -- ^ Return output used for leftover funds. This output will be used for
  --   balancing, and it will be added to the transaction
  --   IF the funds locked in it (after balancing) are non zero.
  -> UtxoSet C.CtxUTxO a
  -- ^ Set of UTxOs that can be used to supply missing funds
  -> TxBuilder era
  -- ^ The unbalanced transaction body
  -> ChangeOutputPosition
  -- ^ The return output position
  -> m (C.BalancedTxBody era, BalanceChanges)
  -- ^ The balanced transaction body and the balance changes (per address)
balanceTx dbg returnUTxO0 walletUtxo txb changePosition = inBabbage @era $ do
  params <- queryProtocolParameters
  pools <- queryStakePools
  availableUTxOs <- checkCompatibilityLevel dbg txb walletUtxo
  -- compatibility level
  let txb0 = txb <> BuildTx.liftTxBodyEndo (set L.txProtocolParams (C.BuildTxWith (Just params)))
  -- TODO: Better error handling (better than 'fail')
  otherInputs <- lookupTxIns (requiredTxIns $ BuildTx.buildTx txb)
  let combinedTxIns =
        let UTxO w = availableUTxOs
            UTxO o = otherInputs
         in UTxO (Map.union w o)

  (finalBody, returnUTxO1) <- mapError ACoinSelectionError $ do
    bodyWithInputs <- addOwnInput txb0 walletUtxo
    (bodyWithCollat, collatUTxOs) <- setCollateral bodyWithInputs walletUtxo
    let walletTxInsMinusCollatUTxOs = Utxos.removeUtxos (Map.keysSet $ _utxos collatUTxOs) walletUtxo
    balancePositive (natTracer lift dbg) pools params combinedTxIns returnUTxO0 walletTxInsMinusCollatUTxOs collatUTxOs bodyWithCollat
  count <- requiredSignatureCount finalBody
  let csi = prepCSInputs count returnUTxO1 combinedTxIns finalBody
  start <- querySystemStart
  hist <- queryEraHistory
  mapError ABalancingError (balanceTransactionBody (natTracer lift dbg) start hist params pools csi changePosition)

{- | Check the compatibility level of the transaction body
  and remove any incompatible UTxOs from the UTxO set.
-}
checkCompatibilityLevel :: forall era m a. (Monad m, C.IsBabbageBasedEra era) => Tracer m TxBalancingMessage -> TxBuilder era -> UtxoSet C.CtxUTxO a -> m (UTxO era)
checkCompatibilityLevel tr txBuilder utxoSet@(UtxoSet w) = inBabbage @era $ do
  let txB = BuildTx.buildTx txBuilder
      compatibility = inBabbage @era $ txCompatibility txB
      utxoIn = Utxos.toApiUtxo utxoSet
      UTxO utxoOut = compatibleWith compatibility utxoIn
      droppedTxIns = Map.size w - Map.size utxoOut
  traceWith tr CompatibilityLevel{compatibility, droppedTxIns}
  pure (UTxO utxoOut)

-- | Balance the transaction using the wallet's funds, then sign it.
balanceForWallet
  :: forall era m a
   . (MonadBlockchain era m, MonadError (BalanceTxError era) m, C.IsBabbageBasedEra era)
  => Tracer m TxBalancingMessage
  -> Wallet
  -> UtxoSet C.CtxUTxO a
  -> TxBuilder era
  -> ChangeOutputPosition
  -> m (C.Tx era, BalanceChanges)
balanceForWallet dbg wallet walletUtxo txb changePosition = inBabbage @era $ do
  n <- queryNetworkId
  let walletAddress = Wallet.addressInEra n wallet
      txOut = L.emptyTxOut walletAddress
  balanceForWalletReturn dbg wallet walletUtxo txOut txb changePosition

-- | Balance the transaction using the wallet's funds and the provided return output, then sign it.
balanceForWalletReturn
  :: forall era m a
   . (MonadBlockchain era m, MonadError (BalanceTxError era) m, C.IsBabbageBasedEra era)
  => Tracer m TxBalancingMessage
  -> Wallet
  -> UtxoSet C.CtxUTxO a
  -> C.TxOut C.CtxTx era
  -> TxBuilder era
  -> ChangeOutputPosition
  -> m (C.Tx era, BalanceChanges)
balanceForWalletReturn dbg wallet walletUtxo returnOutput txb changePosition = inBabbage @era $ do
  first (signForWallet wallet) <$> balanceTx dbg returnOutput walletUtxo txb changePosition

-- | Sign a transaction with the wallet's key
signForWallet :: forall era. (C.IsBabbageBasedEra era) => Wallet -> C.BalancedTxBody era -> C.Tx era
signForWallet wallet = signBalancedTxBody [C.WitnessPaymentKey (Wallet.getWallet wallet)]

-- | Sign the tx body with the given keys
signBalancedTxBody :: forall era. (C.IsBabbageBasedEra era) => [C.ShelleyWitnessSigningKey] -> C.BalancedTxBody era -> C.Tx era
signBalancedTxBody witnesses (C.BalancedTxBody _ txbody _changeOutput _fee) =
  let era = C.babbageBasedEra @era
      era2 = C.Experimental.babbageEraOnwardsToEra era
   in C.Experimental.obtainCommonConstraints era2 $
        let wit = C.Experimental.makeKeyWitness era2 txbody <$> witnesses
         in C.ShelleyTx (C.babbageEraOnwardsToShelleyBasedEra era) (C.Experimental.signTx era2 [] wit txbody)

{- | If the transaction body has no inputs then we add one from the wallet's UTxO set.
  (we have to do this because 'C.evaluateTransactionBalance' fails on a tx body with
   no inputs)
  Throws an error if the transaction body has no inputs and the wallet UTxO set is empty.
-}
addOwnInput :: (MonadError CoinSelectionError m, C.IsShelleyBasedEra era) => TxBuilder era -> UtxoSet ctx a -> m (TxBuilder era)
addOwnInput builder allUtxos =
  let body = BuildTx.buildTx builder
      UtxoSet{_utxos} = Utxos.removeUtxos (spentTxIns body) allUtxos
   in if
        | not (List.null $ view L.txIns body) -> pure builder
        | not (Map.null _utxos) ->
            -- Select ada-only outputs if possible
            let availableUTxOs =
                  List.sortOn
                    ( length
                        . (\(C.InAnyCardanoEra _ (C.TxOut _ txOutValue _ _)) -> toList (C.txOutValueToValue txOutValue))
                        . fst
                        . snd
                    )
                    (Map.toList _utxos)
             in pure $ builder <> execBuildTx (spendPublicKeyOutput (fst $ head availableUTxOs))
        | otherwise -> throwError NoWalletUTxOs

{- | Add a collateral input. Throws a 'NoAdaOnlyUTxOsForCollateral' error if a collateral input is required,
  but no suitable input is provided in the wallet UTxO set.
  Additionally returns the collateral UTxOs that were used.
-}
setCollateral :: forall era m ctx a. (MonadError CoinSelectionError m, C.IsAlonzoBasedEra era) => TxBuilder era -> UtxoSet ctx a -> m (TxBuilder era, UtxoSet ctx a)
setCollateral builder (Utxos.onlyAda -> UtxoSet{_utxos}) =
  inAlonzo @era $
    let body = BuildTx.buildTx builder
        noScripts = not (runsScripts body)
        collateral = view (L.txInsCollateral . L.txInsCollateralTxIns) body
        hasCollateral = not $ List.null collateral
     in if noScripts
          then pure (builder, UtxoSet{_utxos = Map.empty}) -- no script witnesses in inputs.
          else
            if hasCollateral
              then do
                let collatUTxOs = Map.restrictKeys _utxos (Set.fromList collateral)
                pure (builder, UtxoSet{_utxos = collatUTxOs})
              else do
                -- select the output with the largest amount of Ada
                let outputWithLargestAda =
                      listToMaybe
                        $ List.sortOn
                          ( Down
                              . C.selectLovelace
                              . (\(C.InAnyCardanoEra _ (C.TxOut _ txOutValue _ _)) -> C.txOutValueToValue txOutValue)
                              . fst
                              . snd
                          )
                        $ Map.toList _utxos
                case outputWithLargestAda of
                  Nothing -> throwError NoAdaOnlyUTxOsForCollateral
                  Just kp@(k, _) -> pure (builder <> execBuildTx (addCollateral k), UtxoSet{_utxos = Map.fromList [kp]})

-- | Whether the transaction runs any plutus scripts
runsScripts :: forall era. (C.IsMaryBasedEra era) => TxBodyContent BuildTx era -> Bool
runsScripts body =
  inMary @era $
    let scriptIns = body ^.. (L.txIns . traversed . _2 . L._BuildTxWith . L._ScriptWitness)
        minting = body ^. (L.txMintValue . L._TxMintValue . _2)
        certificates = body ^.. (L.txCertificates . L._TxCertificates . _2 . traversed . _2 . L._ScriptWitness)
        withdrawals = body ^.. (L.txWithdrawals . L._TxWithdrawals . traversed . _3 . L._BuildTxWith . L._ScriptWitness)
     in not (null scriptIns && Map.null minting && null withdrawals && null certificates)

{- | Add inputs to ensure that the balance is strictly positive. After calling @balancePositive@
* The amount of Ada provided by the transaction's inputs minus (the amount of Ada produced by the transaction's outputs plus the change output) is greater than zero
* For all native tokens @t@, the amount of @t@ provided by the transaction's inputs minus (the amount of @t@ produced by the transaction's outputs plus the change output plus the delta of @t@ minted / burned) is equal to zero
-}
balancePositive
  :: forall ctx era m a
   . (MonadError CoinSelectionError m, C.IsMaryBasedEra era)
  => Tracer m TxBalancingMessage
  -> Set PoolId
  -> C.LedgerProtocolParameters era
  -> C.UTxO era
  -> C.TxOut C.CtxTx era
  -> UtxoSet ctx a
  -> UtxoSet ctx a
  -- ^ Collateral UTxOs, these are the UTxOs that were used as collateral inputs
  --  they will only be used for balancing in the case that the other UTxOs are insufficient.
  -> TxBuilder era
  -> m (TxBuilder era, C.TxOut C.CtxTx era)
balancePositive dbg poolIds ledgerPPs utxo_ returnUTxO0 walletUtxo collateralUTxOs txBuilder0 = inMary @era $ do
  let txBodyContent0 = BuildTx.buildTx txBuilder0
  txb <- either (throwError . bodyError) pure (C.createTransactionBody C.shelleyBasedEra txBodyContent0)
  let bal = C.evaluateTransactionBalance C.shelleyBasedEra (C.unLedgerProtocolParameters ledgerPPs) poolIds mempty mempty utxo_ txb & view L._TxOutValue
      available = Utxos.removeUtxos (spentTxIns txBodyContent0) walletUtxo

  -- minimum positive balance (in lovelace) that must be available to cover
  -- \* minimum deposit on the ada-only change output, if required, and
  -- \* transaction fee, incl. script fee if required
  -- we set it to rather large value to ensure that we can build a valid transaction.
  let threshold = negate (if runsScripts txBodyContent0 then 8_000_000 else 2_500_000)
      balance = bal & L._Value . at C.AdaAssetId %~ maybe (Just threshold) (Just . (+) threshold)

  traceWith
    dbg
    PrepareInputs
      { availableBalance = Utxos.totalBalance available
      , transactionBalance = balance
      }
  (txBuilder1, additionalBalance) <- addInputsForAssets dbg balance available collateralUTxOs txBuilder0

  let bal0 = balance <> additionalBalance
  let (returnUTxO1, _deposit) = addOutputForNonAdaAssets ledgerPPs returnUTxO0 bal0

  pure (txBuilder1, returnUTxO1)

{- | Examine the negative part of the transaction balance and select inputs from
the wallet's UTXO set to cover the assets required by it. If there are no
assets missing then no inputs will be added.
-}
addInputsForAssets
  :: (MonadError CoinSelectionError m)
  => Tracer m TxBalancingMessage
  -> C.Value
  -- ^ The balance of the transaction
  -> UtxoSet ctx a
  -- ^ UTxOs (excluding collateral) that we can spend to cover the negative part of the balance
  -> UtxoSet ctx a
  -- ^ collateral UTxOs that we can use to cover the negative part of the balance if there are no other UTxOs available
  -> TxBuilder era
  -- ^ Transaction body
  -> m (TxBuilder era, C.Value)
  -- ^ Transaction body with additional inputs and the total value of the additional inputs
addInputsForAssets dbg txBal availableUtxo collateralUtxo txBuilder =
  if
    | null (fst $ splitValue txBal) -> do
        traceWith dbg NoAssetsMissing
        return (txBuilder, mempty)
    | otherwise -> do
        let missingAssets = fmap (second abs) $ fst $ splitValue txBal
        traceWith dbg (MissingAssets $ fromList missingAssets)
        case Wallet.selectMixedInputsCovering availableUtxo missingAssets of
          Nothing ->
            let total = availableUtxo <> collateralUtxo
             in case Wallet.selectMixedInputsCovering total missingAssets of
                  Nothing -> throwError (NotEnoughMixedOutputsFor (fromList missingAssets) (Utxos.totalBalance total) txBal)
                  Just (total_, ins) -> pure (txBuilder <> BuildTx.liftTxBodyEndo (over L.txIns (<> fmap spendPubKeyTxIn ins)), total_)
          Just (total, ins) -> pure (txBuilder <> BuildTx.liftTxBodyEndo (over L.txIns (<> fmap spendPubKeyTxIn ins)), total)

{- | Examine the positive part of the transaction balance and add any non-Ada assets it contains
to the provided change output. If the positive part only contains Ada then the
change output is returned unmodified.
-}
addOutputForNonAdaAssets
  :: (C.IsMaryBasedEra era)
  => C.LedgerProtocolParameters era
  -- ^ Protocol parameters (for computing the minimum lovelace amount in the output)
  -> C.TxOut C.CtxTx era
  -- ^ Change output. Overflow non-Ada assets will be added to this output's value.
  -> C.Value
  -- ^ The balance of the transaction
  -> (C.TxOut C.CtxTx era, C.Quantity)
  -- ^ The modified change output and the lovelace portion of the change output's value. If no output was added then the amount will be 0.
addOutputForNonAdaAssets pparams returnUTxO (fromList . snd . splitValue -> positives)
  | isNothing (C.valueToLovelace positives) =
      let (vlWithoutAda :: C.Value) = positives & set (L._Value . at C.AdaAssetId) Nothing
          output =
            setMinAdaDeposit pparams $
              returnUTxO & L._TxOut . _2 . L._TxOutValue <>~ vlWithoutAda
       in (output, output ^. L._TxOut . _2 . L._TxOutValue . to (C.lovelaceToQuantity . C.selectLovelace))
  | otherwise = (returnUTxO, C.Quantity 0)

splitValue :: C.Value -> ([(C.AssetId, C.Quantity)], [(C.AssetId, C.Quantity)])
splitValue =
  let p (_, q) = q < 0
      f (_, q) = q /= 0
   in List.partition p . List.filter f . toList

{- | Take the tx body and produce a 'CSInputs' value for coin selection,
using the @MonadBlockchain@ effect to query any missing UTxO information.
-}
prepCSInputs
  :: (C.IsShelleyBasedEra era)
  => TransactionSignatureCount
  -> C.TxOut C.CtxTx era
  -- ^ Change address
  -> C.UTxO era
  -- ^ UTxOs that may be used for balancing
  -> TxBuilder era
  -- ^ Unbalanced transaction body
  -> CSInputs era
  -- ^ Inputs for coin balancing
prepCSInputs sigCount csiChangeAddress csiUtxo (BuildTx.buildTx -> csiTxBody) = do
  CSInputs csiUtxo csiTxBody csiChangeAddress sigCount

spentTxIns :: C.TxBodyContent v era -> Set C.TxIn
spentTxIns (view L.txIns -> inputs) =
  -- TODO: Include collateral etc. fields
  Set.fromList (fst <$> inputs)

lookupTxIns :: (MonadBlockchain era m) => Set C.TxIn -> m (C.UTxO era)
lookupTxIns = utxoByTxIn

keyWitnesses :: (MonadBlockchain era m, C.IsShelleyBasedEra era) => C.TxBodyContent v era -> m (Set (Keys.KeyHash 'Keys.Payment StandardCrypto))
keyWitnesses (requiredTxIns -> inputs) = do
  C.UTxO utxos <- utxoByTxIn inputs
  pure $ Set.fromList $ mapMaybe (publicKeyCredential . snd) $ Map.toList utxos

{- | The number of signatures required to spend the transaction's inputs
  and to satisfy the "extra key witnesses" constraint
  and required for certification.
-}
requiredSignatureCount :: forall era m. (MonadBlockchain era m, C.IsAlonzoBasedEra era) => TxBuilder era -> m TransactionSignatureCount
requiredSignatureCount txBuilder = inAlonzo @era $ do
  let content = BuildTx.buildTx txBuilder
  keyWits <- keyWitnesses content
  let hsh (C.PaymentKeyHash h) = h
      extraSigs = view (L.txExtraKeyWits . L._TxExtraKeyWitnesses) content
      allSigs = Set.union keyWits (Set.fromList $ fmap hsh extraSigs)
      certKeyWits = case view L.txCertificates content of
        C.TxCertificates _ cs _ -> mconcat $ getCertKeyWits <$> cs
        C.TxCertificatesNone -> Set.empty

      getCertKeyWits :: C.Certificate era -> Set (CertificateKeyWitness era)
      {- Certificates that don't require a witness:
      \* For a DCertregkey certificate, cwitness is not defined as stake key
      registrations do not require a witness. (See SL-D5)
      \* For a DCertmir certificate, cwitness is not defined as there is no
      single core node or genesis key that posts the certificate. (See SL-D5)
      -}
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyUnRegCert (KeyHashObj hash)))) =
        Set.singleton $ CertificateStakeKey hash
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyDelegCert (KeyHashObj hash) _))) =
        Set.singleton $ CertificateStakeKey hash
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertPool (TxCert.RegPool PoolParams{ppId, ppOwners}))) =
        {- The pool registration certificate tx requires witnesses from all owner
        stake addresses, as well as a witness for the pool key. (See SL-D1)
        -}
        Set.singleton (CertificateStakePoolKey ppId)
          <> Set.fromList (CertificateStakeKey <$> Set.toList ppOwners)
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertPool (TxCert.RetirePool hash _))) =
        Set.singleton (CertificateStakePoolKey hash)
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertGenesisDeleg (TxCert.GenesisDelegCert hash _ _))) =
        Set.singleton (CertificateGenesisKey hash)
      getCertKeyWits _ =
        Set.empty

  pure $ TransactionSignatureCount (fromIntegral $ Set.size allSigs + Set.size certKeyWits)

-- | Certificate key witness
data CertificateKeyWitness era
  = CertificateStakeKey (KeyHash Staking (EraCrypto (C.ShelleyLedgerEra era)))
  | CertificateStakePoolKey (KeyHash StakePool (EraCrypto (C.ShelleyLedgerEra era)))
  | CertificateGenesisKey (KeyHash Genesis (EraCrypto (C.ShelleyLedgerEra era)))
  deriving stock (Eq, Ord)

publicKeyCredential :: (C.IsShelleyBasedEra era) => C.TxOut v era -> Maybe (Keys.KeyHash 'Keys.Payment StandardCrypto)
publicKeyCredential = preview (L._TxOut . _1 . L._ShelleyAddress . _2 . L._ShelleyPaymentCredentialByKey)

spendPubKeyTxIn :: C.TxIn -> (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))
-- TODO: consolidate with Convex.BuildTx.spendPublicKeyOutput
spendPubKeyTxIn txIn = (txIn, C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending))
