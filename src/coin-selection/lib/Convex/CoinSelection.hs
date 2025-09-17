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
-- NOTE: We have an orphan instance for ScriptWitnessIndex, until (or if) upstream provides such an
-- instance.
{-# OPTIONS_GHC -Wno-orphans #-}

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

import Cardano.Api (
  BuildTx,
  ConwayEra,
  EraHistory,
  PoolId,
  TxBodyContent,
  TxOut,
  UTxO (..),
 )
import Cardano.Api qualified
import Cardano.Api qualified as C
import Cardano.Api.Extras (substituteExecutionUnits)
import Cardano.Api.Ledger qualified as CLedger
import Cardano.Api.Ledger qualified as L
import Cardano.Ledger.Core (PParams (..), hkdKeyDepositL)
import Cardano.Ledger.Keys qualified as Keys
import Cardano.Ledger.Shelley.API (
  Coin (..),
  KeyHash (..),
  KeyRole (..),
 )
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
import Control.Monad.Error.Lens (throwing, throwing_)
import Control.Monad.Except (MonadError (..))
import Control.Tracer (Tracer, traceWith)
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
import Data.Functor.Identity (Identity)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.Maybe (
  isNothing,
  listToMaybe,
  mapMaybe,
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

{- | Balancing errors, including the important 'ScriptExecutionErr'. "Important" in the sense that we
can write prisms such as 'exactScriptExecutionError' that allow for more fine-grained
pattern-matching on the on-chain error.
-}
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

{- | This prism will match on script execution errors using a predicate over the script
witness index, an error string, and internal logs. The prism matches when the predicate
returns True for at least one entry in the script execution error list OR if _all_ script
execution errors have no log messages. This is useful because no logs are generated in
production mode, so we can still detect that a script failed (for example, an attack was
prevented) from the error occurrence even without any log entries. In debug mode, we have
logs associated with these errors, allowing us to match on specific failures.
-}
exactScriptExecutionError :: forall e era. (AsBalancingError e era) => ((C.ScriptWitnessIndex, Text, [Text]) -> Bool) -> L.Prism' e [(C.ScriptWitnessIndex, Text, [Text])]
exactScriptExecutionError p = prism' tobe frombe
 where
  tobe :: [(C.ScriptWitnessIndex, Text, [Text])] -> e
  tobe = review _ScriptExecutionErr
  frombe :: e -> Maybe [(C.ScriptWitnessIndex, Text, [Text])]
  frombe x = case preview _ScriptExecutionErr x of
    Nothing -> Nothing
    Just xs ->
      if any p xs || all (null . L.view L._3) xs
        then Just xs
        else Nothing

{- | This convenience function takes @Left@s of type 'C.TxBodyErrorAutoBalance' and throws
'C.TxBodyScriptErecutionError's as 'ScriptExecutionErr', with extraaction of the error log, if
availalbe. Other @Left@s are thrown as 'BalancingError's.

This is a convenient bridge from cardano-api errors to errors where we can pattern-match on script
execution errors and have an error hierarchy.
-}
balancingError :: forall era err m a. (MonadError err m, AsBalancingError err era) => Either (C.TxBodyErrorAutoBalance era) a -> m a
balancingError = \case
  Right a -> pure a
  Left (C.TxBodyScriptExecutionError es) -> throwing _ScriptExecutionErr (map extractErrorInfo es)
  Left err -> throwing _BalancingError (asText err)
 where
  asText = Text.pack . C.docToString . C.prettyError
  extractErrorInfo :: (C.ScriptWitnessIndex, C.ScriptExecutionError) -> (C.ScriptWitnessIndex, Text, [Text])
  -- TODO: We could expose scriptWithContext and executionUnits as well.
  extractErrorInfo (wix, C.ScriptErrorEvaluationFailed C.DebugPlutusFailure{C.dpfEvaluationError, C.dpfExecutionLogs}) = (wix, Text.pack $ show dpfEvaluationError, dpfExecutionLogs)
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
  | -- | Execution units of the transaction
    ExUnitsMap {exUnits :: [(C.ScriptWitnessIndex, C.ExecutionUnits)]}
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
  :: forall era err m
   . ( MonadError err m
     , C.IsBabbageBasedEra era
     , AsBalancingError err era
     )
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

    let exUnitsMap =
          C.evaluateTransactionExecutionUnits
            C.cardanoEra
            systemStart
            (C.toLedgerEpochInfo eraHistory)
            protocolParams
            csiUtxo
            txbody0

    exUnitsMap' <- balancingError $
      case Map.mapEither id exUnitsMap of
        (failures, exUnitsMap') ->
          handleExUnitsErrors (C.txScriptValidityToScriptValidity $ C.txScriptValidity csiTxBody) failures $
            fmap snd exUnitsMap'

    traceWith tracer $ ExUnitsMap $ Map.toList exUnitsMap'

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

    finalTxBody <- balancingError . first C.TxBodyError $ C.createTransactionBody C.shelleyBasedEra finalBodyContent
    balances <- maybe (throwing_ _ComputeBalanceChangeError) pure (balanceChanges csiUtxo finalBodyContent)

    let finalBody = C.BalancedTxBody finalBodyContent finalTxBody changeOutputBalance t_fee
    return (finalBody, balances)

checkMinUTxOValue
  :: (C.IsShelleyBasedEra era, MonadError err m, AsBalancingError err era)
  => C.TxOut C.CtxTx era
  -> C.LedgerProtocolParameters era
  -> m ()
checkMinUTxOValue txout@(C.TxOut _ v _ _) pparams' = do
  let minUTxO = C.calculateMinimumUTxO C.shelleyBasedEra (C.unLedgerProtocolParameters pparams') txout
  if C.txOutValueToLovelace v >= minUTxO
    then pure ()
    else throwing _CheckMinUtxoValueError (txout, C.lovelaceToQuantity minUTxO)

prependTxOut :: C.TxOut C.CtxTx era -> C.TxBodyContent C.BuildTx era -> C.TxBodyContent C.BuildTx era
prependTxOut out = over L.txOuts (out <|)

appendTxOut :: C.TxOut C.CtxTx era -> C.TxBodyContent C.BuildTx era -> C.TxBodyContent C.BuildTx era
appendTxOut out = over L.txOuts (|> out)

{- | Check that the output has a positive Ada balance greater than or equal to the minimum
UTxO requirement
-}
balanceCheck
  :: (C.IsShelleyBasedEra era, MonadError err m, AsBalancingError err era)
  => C.LedgerProtocolParameters era
  -> C.TxOut C.CtxTx era
  -> m ()
balanceCheck pparams output@(C.TxOut _ (C.txOutValueToValue -> value) _ _) =
  let valueLovelace = C.selectLovelace value
   in if value == mempty
        then return ()
        else do
          when (valueLovelace < 0) (balancingError $ Left $ C.TxBodyErrorBalanceNegative valueLovelace mempty)
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
  failures = toList failuresMap
handleExUnitsErrors C.ScriptInvalid failuresMap exUnitsMap
  | null failuresMap = Left C.TxBodyScriptBadScriptValidity
  | otherwise = Right $ Map.map (\_ -> C.ExecutionUnits 0 0) failuresMap <> exUnitsMap

{- | Get the 'BalanceChanges' for a tx body. Returns 'Nothing' if
a UTXO couldnt be found
-}
balanceChanges :: (C.IsMaryBasedEra era) => UTxO era -> TxBodyContent BuildTx era -> Maybe BalanceChanges
balanceChanges (C.UTxO lookups) body = do
  let outputs = foldMap txOutChange (body ^. L.txOuts)
  inputs <- Utxos.invBalanceChange . foldMap (txOutChange . id) <$> traverse (\(txi, _) -> Map.lookup txi lookups) (body ^. L.txIns)
  pure (outputs <> inputs)

unregBalance :: forall era. (L.EraPParams (C.ShelleyLedgerEra era)) => C.LedgerProtocolParameters era -> TxBodyContent BuildTx era -> Map.Map C.StakeCredential Coin
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
        C.TxCertificates _ cs -> Map.fromList $ mapMaybe (toUnregCert . fst) $ OMap.toAscList cs

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
  :: forall era err m a
   . ( MonadBlockchain era m
     , MonadError err m
     , C.IsBabbageBasedEra era
     , AsBalancingError err era
     , AsCoinSelectionError err
     )
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

  (finalBody, returnUTxO1) <- do
    bodyWithInputs <- addOwnInput txb0 walletUtxo
    (bodyWithCollat, collatUTxOs) <- setCollateral bodyWithInputs walletUtxo
    let walletTxInsMinusCollatUTxOs = Utxos.removeUtxos (Map.keysSet $ _utxos collatUTxOs) walletUtxo
    balancePositive dbg pools params combinedTxIns returnUTxO0 walletTxInsMinusCollatUTxOs collatUTxOs bodyWithCollat
  count <- requiredSignatureCount finalBody
  let csi = prepCSInputs count returnUTxO1 combinedTxIns finalBody
  start <- querySystemStart
  hist <- queryEraHistory
  balanceTransactionBody dbg start hist params pools csi changePosition

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
  :: forall era err m a
   . (MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, AsBalancingError err era, AsCoinSelectionError err)
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
  :: forall era err m a
   . (MonadBlockchain era m, MonadError err m, C.IsBabbageBasedEra era, AsBalancingError err era, AsCoinSelectionError err)
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
  C.signShelleyTransaction C.shelleyBasedEra txbody witnesses

{- | If the transaction body has no inputs then we add one from the wallet's UTxO set.
  (we have to do this because 'C.evaluateTransactionBalance' fails on a tx body with
   no inputs)
  Throws an error if the transaction body has no inputs and the wallet UTxO set is empty.
-}
addOwnInput :: (MonadError err m, C.IsShelleyBasedEra era, AsCoinSelectionError err) => TxBuilder era -> UtxoSet ctx a -> m (TxBuilder era)
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
        | otherwise -> throwing_ _NoWalletUTxOs

{- | Add a collateral input. Throws a 'NoAdaOnlyUTxOsForCollateral' error if a collateral input is required,
  but no suitable input is provided in the wallet UTxO set.
  Additionally returns the collateral UTxOs that were used.
-}
setCollateral :: forall era err m ctx a. (MonadError err m, C.IsAlonzoBasedEra era, AsCoinSelectionError err) => TxBuilder era -> UtxoSet ctx a -> m (TxBuilder era, UtxoSet ctx a)
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
                  Nothing -> throwing_ _NoAdaOnlyUTxOsForCollateral
                  Just kp@(k, _) -> pure (builder <> execBuildTx (addCollateral k), UtxoSet{_utxos = Map.fromList [kp]})

-- | Whether the transaction runs any plutus scripts
runsScripts :: forall era. (C.IsMaryBasedEra era) => TxBodyContent BuildTx era -> Bool
runsScripts body =
  inMary @era $
    let scriptIns = body ^.. (L.txIns . traversed . _2 . L._BuildTxWith . L._ScriptWitness)
        minting = body ^. (L.txMintValue . L._TxMintValue)
        certificates = body ^. (L.txCertificates . L._TxCertificates . to OMap.toAscList)
        withdrawals = body ^.. (L.txWithdrawals . L._TxWithdrawals . traversed . _3 . L._BuildTxWith . L._ScriptWitness)
     in not (null scriptIns && Map.null minting && null withdrawals && null certificates)

{- | Add inputs to ensure that the balance is strictly positive. After calling @balancePositive@
* The amount of Ada provided by the transaction's inputs minus (the amount of Ada produced by the transaction's outputs plus the change output) is greater than zero
* For all native tokens @t@, the amount of @t@ provided by the transaction's inputs minus (the amount of @t@ produced by the transaction's outputs plus the change output plus the delta of @t@ minted / burned) is equal to zero
-}
balancePositive
  :: forall ctx era err m a
   . (MonadError err m, C.IsMaryBasedEra era, AsCoinSelectionError err)
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
  txb <- case C.createTransactionBody C.shelleyBasedEra txBodyContent0 of
    Left err -> throwing _BodyError (Text.pack . C.docToString . C.prettyError $ err)
    Right k -> pure k
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
  :: (MonadError err m, AsCoinSelectionError err)
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
                  Nothing -> throwing _NotEnoughMixedOutputsFor (fromList missingAssets, Utxos.totalBalance total, txBal)
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

keyWitnesses :: (MonadBlockchain era m, C.IsShelleyBasedEra era) => C.TxBodyContent v era -> m (Set (Keys.KeyHash 'Keys.Payment))
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
        C.TxCertificates _ cs -> mconcat $ getCertKeyWits . fst <$> OMap.toAscList cs
        C.TxCertificatesNone -> Set.empty

      getCertKeyWits :: C.Certificate era -> Set (KeyHash Witness)
      {- Certificates that don't require a witness:
      \* For a DCertregkey certificate, cwitness is not defined as stake key
      registrations do not require a witness. (See SL-D5)
      \* For a DCertmir certificate, cwitness is not defined as there is no
      single core node or genesis key that posts the certificate. (See SL-D5)
      -}
      getCertKeyWits (C.ShelleyRelatedCertificate _era b) =
        maybe Set.empty Set.singleton (TxCert.getVKeyWitnessShelleyTxCert b)
      getCertKeyWits (C.ConwayCertificate C.ConwayEraOnwardsConway b) =
        maybe Set.empty Set.singleton (getTxCertWitness (C.convert C.ConwayEraOnwardsConway) b)

  pure $ TransactionSignatureCount (fromIntegral $ Set.size allSigs + Set.size certKeyWits)

getTxCertWitness
  :: C.ShelleyBasedEra era
  -> L.TxCert (C.ShelleyLedgerEra era)
  -> Maybe (KeyHash Witness)
getTxCertWitness sbe ledgerCert = C.shelleyBasedEraConstraints sbe $
  case L.getVKeyWitnessTxCert ledgerCert of
    Just keyHash -> Just keyHash
    _ -> Nothing

-- | Certificate key witness
data CertificateKeyWitness era
  = CertificateStakeKey (KeyHash Staking)
  | CertificateStakePoolKey (KeyHash StakePool)
  | CertificateGenesisKey (KeyHash Genesis)
  deriving stock (Eq, Ord)

publicKeyCredential :: (C.IsShelleyBasedEra era) => C.TxOut v era -> Maybe (Keys.KeyHash 'Keys.Payment)
publicKeyCredential = preview (L._TxOut . _1 . L._ShelleyAddress . _2 . L._ShelleyPaymentCredentialByKey)

spendPubKeyTxIn :: C.TxIn -> (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))
-- TODO: consolidate with Convex.BuildTx.spendPublicKeyOutput
spendPubKeyTxIn txIn = (txIn, C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending))
