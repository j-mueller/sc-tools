{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Building cardano transactions from tx bodies
-}
module Convex.CoinSelection(
  -- * Data types
  CoinSelectionError(..),
  bodyError,
  TransactionSignatureCount(..),
  CSInputs(..),
  ERA,
  utxo,
  txBody,
  changeOutput,
  numWitnesses,
  -- * Balancing
  BalanceTxError(..),
  BalancingError(..),
  TxBalancingMessage(..),
  balanceTransactionBody,
  balanceForWallet,
  balanceForWalletReturn,
  balanceTx,
  signForWallet,
  -- * Balance changes
  balanceChanges,
  requiredTxIns,
  spentTxIns,
  -- * Etc.
  prepCSInputs,
  keyWitnesses,
  publicKeyCredential
  ) where

import qualified Cardano.Api                   as Cardano.Api
import           Cardano.Api.Shelley           (BabbageEra, BuildTx, EraHistory,
                                                PoolId, TxBodyContent, TxOut,
                                                UTxO (..))
import           Cardano.Ledger.Shelley.API    (KeyHash (..), KeyRole (..),
                                                Credential (..), Coin (..),
                                                PoolParams (..))
import           Cardano.Ledger.Shelley.Core   (EraCrypto)
import qualified Cardano.Api.Shelley           as C
import           Cardano.Ledger.Crypto         (StandardCrypto)
import qualified Cardano.Ledger.Keys           as Keys
import qualified Cardano.Ledger.Shelley.TxCert as TxCert
import           Cardano.Slotting.Time         (SystemStart)
import           Control.Lens                  (_1, _2, _3, at, makeLensesFor,
                                                over, preview, set, to,
                                                traversed, view, (%~), (&),
                                                (.~), (<>~), (?~), (^.), (^..),
                                                (|>))
import           Control.Monad                 (when)
import           Control.Monad.Except          (MonadError (..))
import           Control.Monad.Trans.Class     (MonadTrans (..))
import           Control.Tracer                (Tracer, natTracer, traceWith)
import           Convex.BuildTx                (addCollateral, buildTxWith,
                                                execBuildTx, setMinAdaDeposit,
                                                spendPublicKeyOutput)
import           Convex.Class                  (MonadBlockchain (..))
import qualified Convex.Lenses                 as L
import           Convex.Utils                  (mapError)
import           Convex.UTxOCompatibility      (UTxOCompatibility,
                                                compatibleWith, txCompatibility)
import           Convex.Utxos                  (BalanceChanges (..),
                                                UtxoSet (..))
import qualified Convex.Utxos                  as Utxos
import           Convex.Wallet                 (Wallet)
import qualified Convex.Wallet                 as Wallet
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Bifunctor                (Bifunctor (..))
import           Data.Default                  (Default (..))
import           Data.Function                 (on)
import qualified Data.List                     as List
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Maybe                    (isNothing, listToMaybe,
                                                mapMaybe, maybeToList)
import           Data.Ord                      (Down (..))
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           GHC.Generics                  (Generic)

type ERA = BabbageEra

{- Note [Change Output]

The balancing functions take a "change output" parameter. This is a @TxOut@ value that will
receive any Ada change that's leftover after balancing.

If the change output has a non-zero value (of any currency) then it will be included in the
final transaction regardless of the final balance of the transaction.

-}

-- | The expected number of signatures that will be attached to the transaction
newtype TransactionSignatureCount = TransactionSignatureCount{ unTransactionSignatureCount :: Word }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, ToJSON, FromJSON, Enum)

instance Default TransactionSignatureCount where
  def = 1

{-| Inputs needed for coin selection
-}
data CSInputs =
  CSInputs
    { csiUtxo         :: UTxO ERA -- ^ UTXOs that we need to know about
    , csiTxBody       :: TxBodyContent BuildTx ERA -- ^ Tx body to balance
    , csiChangeOutput :: C.TxOut C.CtxTx C.BabbageEra -- ^ Change output -- see Note [Change Output]
    , csiNumWitnesses :: TransactionSignatureCount -- ^ How many shelley witnesses there will be
    }

makeLensesFor
  [ ("csiUtxo", "utxo")
  , ("csiTxBody", "txBody")
  , ("csiChangeOutput", "changeOutput")
  , ("csiNumWitnesses", "numWitnesses")
  ] ''CSInputs

data CoinSelectionError =
  UnsupportedBalance (C.TxOutValue ERA)
  | BodyError Text
  | NotEnoughInputsFor{ lovelaceRequired :: C.Quantity, lovelaceFound :: C.Quantity }
  | NotEnoughMixedOutputsFor{ valuesNeeded :: C.Value, valueProvided :: C.Value, txBalance :: C.Value }
  | NoWalletUTxOs -- ^ The wallet utxo set is empty
  | NoAdaOnlyUTxOsForCollateral -- ^ The transaction body needs a collateral input, but there are no inputs that hold nothing but Ada
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

bodyError :: C.TxBodyError -> CoinSelectionError
bodyError = BodyError . Text.pack . C.docToString . C.prettyError

data BalancingError =
  BalancingError Text
  | CheckMinUtxoValueError (C.TxOut C.CtxTx BabbageEra) C.Quantity
  | BalanceCheckError BalancingError
  | ComputeBalanceChangeError
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

balancingError :: MonadError BalancingError m => Either (C.TxBodyErrorAutoBalance C.BabbageEra) a -> m a
balancingError = either (throwError . BalancingError . Text.pack . C.docToString . C.prettyError) pure

-- | Messages that are produced during coin selection and balancing
data TxBalancingMessage =
  SelectingCoins
  | CompatibilityLevel{ compatibility :: !UTxOCompatibility, droppedTxIns :: !Int } -- ^ The plutus compatibility level applied to the wallet tx outputs
  | PrepareInputs{availableBalance :: C.Value, transactionBalance :: C.Value } -- ^ Preparing to balance the transaction using the available wallet balance
  | StartBalancing{numInputs :: !Int, numOutputs :: !Int} -- ^ Balancing a transaction body
  | ExUnitsMap{ exUnits :: [(C.ScriptWitnessIndex, Either String C.ExecutionUnits)] } -- ^ Execution units of the transaction, or error message in case of script error
  | Txfee{ fee :: C.Quantity } -- ^ The transaction fee
  | TxRemainingBalance{ remainingBalance :: C.Value } -- ^ The remaining balance (after paying the fee)
  | NoAssetsMissing -- ^ The transaction was not missing any assets
  | MissingAssets C.Value -- ^ The transaction is missing some inputs, these will be covered from the wallet's UTxOs.
  | MissingLovelace C.Quantity -- ^ The transaction is missing some Ada. The amount will be covered from the wallet's UTxOs.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

{-| Perform transaction balancing
-}
balanceTransactionBody :: (MonadError BalancingError m) => Tracer m TxBalancingMessage -> SystemStart -> EraHistory -> C.LedgerProtocolParameters BabbageEra -> Set PoolId -> CSInputs -> m (C.BalancedTxBody ERA, BalanceChanges)
balanceTransactionBody tracer systemStart eraHistory protocolParams stakePools CSInputs{csiUtxo, csiTxBody, csiChangeOutput, csiNumWitnesses=TransactionSignatureCount numWits} = do
  let mkChangeOutputFor i = csiChangeOutput & L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId ?~ i
      changeOutputSmall = mkChangeOutputFor 1
      changeOutputLarge = mkChangeOutputFor ((2^(64 :: Integer)) - 1)

  traceWith tracer StartBalancing{numInputs = csiTxBody ^. L.txIns . to length, numOutputs = csiTxBody ^. L.txOuts . to length}

  -- append output instead of prepending
  txbody0 <-
    balancingError . first C.TxBodyError $ C.createAndValidateTransactionBody C.ShelleyBasedEraBabbage $ csiTxBody & appendTxOut changeOutputSmall

  exUnitsMap <- balancingError . first C.TxBodyErrorValidityInterval $
                C.evaluateTransactionExecutionUnits C.BabbageEra
                systemStart (C.toLedgerEpochInfo eraHistory)
                protocolParams
                csiUtxo
                txbody0

  traceWith tracer $ ExUnitsMap $ fmap (second (first (C.docToString . C.prettyError))) $ Map.toList exUnitsMap

  exUnitsMap' <- balancingError $
    case Map.mapEither id exUnitsMap of
      (failures, exUnitsMap') ->
        handleExUnitsErrors C.ScriptValid failures exUnitsMap' -- TODO: should this take the script validity from csiTxBody?

  txbodycontent1 <- balancingError $ substituteExecutionUnits exUnitsMap' csiTxBody
  let txbodycontent1' = txbodycontent1 & set L.txFee (Coin (2^(32 :: Integer) - 1)) & over L.txOuts (|> changeOutputLarge)

  -- append output instead of prepending
  txbody1 <- balancingError . first C.TxBodyError $ C.createAndValidateTransactionBody C.ShelleyBasedEraBabbage txbodycontent1'

  let !t_fee = C.calculateMinTxFee C.ShelleyBasedEraBabbage (C.unLedgerProtocolParameters protocolParams) csiUtxo txbody1 numWits
  traceWith tracer Txfee{fee = C.lovelaceToQuantity t_fee}

  let txbodycontent2 = txbodycontent1 & set L.txFee t_fee & appendTxOut csiChangeOutput
  txbody2 <- balancingError . first C.TxBodyError $ C.createAndValidateTransactionBody C.ShelleyBasedEraBabbage txbodycontent2

  -- TODO: If there are any stake pool unregistration certificates in the transaction
  -- then we need to provide a @Map StakeCredential Lovelace@ here.
  -- See https://github.com/input-output-hk/cardano-api/commit/d23f964d311282b1950b2fd840bcc57ae40a0998
  let unregPoolStakeBalance = mempty

  let !balance = view L._TxOutValue (Cardano.Api.evaluateTransactionBalance C.ShelleyBasedEraBabbage (C.unLedgerProtocolParameters protocolParams) stakePools unregPoolStakeBalance mempty csiUtxo txbody2)

  traceWith tracer TxRemainingBalance{remainingBalance = balance}

  mapM_ (`checkMinUTxOValue` protocolParams) $ C.txOuts txbodycontent1

  -- debug "balanceTransactionBody: changeOutputBalance"
  changeOutputBalance <- case C.valueToLovelace balance of
    Just b -> do
      let op = csiChangeOutput & L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId <>~ (Just $ C.lovelaceToQuantity b)
      balanceCheck protocolParams op
      pure op
    Nothing -> balancingError $ Left $ C.TxBodyErrorNonAdaAssetsUnbalanced balance

  let finalBodyContent =
        txbodycontent1
          & set L.txFee t_fee
          & appendTxOut changeOutputBalance

  txbody3 <- balancingError . first C.TxBodyError $ C.createAndValidateTransactionBody C.ShelleyBasedEraBabbage finalBodyContent

  balances <- maybe (throwError ComputeBalanceChangeError) pure (balanceChanges csiUtxo finalBodyContent)

  let mkBalancedBody b = C.BalancedTxBody finalBodyContent b changeOutputBalance t_fee
  return (mkBalancedBody txbody3, balances)

checkMinUTxOValue
  :: MonadError BalancingError m
  => C.TxOut C.CtxTx C.BabbageEra
  -> C.LedgerProtocolParameters BabbageEra
  -> m ()
checkMinUTxOValue txout@(C.TxOut _ v _ _) pparams' = do
  let minUTxO  = C.calculateMinimumUTxO C.ShelleyBasedEraBabbage txout (C.unLedgerProtocolParameters pparams')
  if C.txOutValueToLovelace v >= minUTxO
  then pure ()
  else throwError (CheckMinUtxoValueError txout $ C.lovelaceToQuantity minUTxO)

appendTxOut :: C.TxOut C.CtxTx C.BabbageEra -> C.TxBodyContent C.BuildTx ERA -> C.TxBodyContent C.BuildTx ERA
appendTxOut out = over L.txOuts (|> out)

{-| Check that the output has a positive Ada balance greater than or equal to the minimum
UTxO requirement
-}
balanceCheck :: MonadError BalancingError m => C.LedgerProtocolParameters BabbageEra -> C.TxOut C.CtxTx C.BabbageEra -> m ()
balanceCheck pparams output =
  let balance = view (L._TxOut . _2) output in
    if view L._TxOutValue balance == mempty
      then return ()
      else do
        when (C.txOutValueToLovelace balance < 0) (balancingError $ Left $ C.TxBodyErrorAdaBalanceNegative $ C.txOutValueToLovelace balance)
        checkMinUTxOValue output pparams

handleExUnitsErrors ::
     C.ScriptValidity -- ^ Mark script as expected to pass or fail validation
  -> Map C.ScriptWitnessIndex C.ScriptExecutionError
  -> Map C.ScriptWitnessIndex C.ExecutionUnits
  -> Either (C.TxBodyErrorAutoBalance C.BabbageEra) (Map C.ScriptWitnessIndex C.ExecutionUnits)
handleExUnitsErrors C.ScriptValid failuresMap exUnitsMap =
    if null failures
      then Right exUnitsMap
      else Left (C.TxBodyScriptExecutionError failures)
  where failures :: [(C.ScriptWitnessIndex, C.ScriptExecutionError)]
        failures = Map.toList failuresMap
handleExUnitsErrors C.ScriptInvalid failuresMap exUnitsMap
  | null scriptFailures = Left C.TxBodyScriptBadScriptValidity
  | null nonScriptFailures = Right exUnitsMap
  | otherwise = Left (C.TxBodyScriptExecutionError nonScriptFailures)
  where nonScriptFailures :: [(C.ScriptWitnessIndex, C.ScriptExecutionError)]
        nonScriptFailures = filter (not . isScriptErrorEvaluationFailed) (Map.toList failuresMap)
        scriptFailures :: [(C.ScriptWitnessIndex, C.ScriptExecutionError)]
        scriptFailures = filter isScriptErrorEvaluationFailed (Map.toList failuresMap)
        isScriptErrorEvaluationFailed :: (C.ScriptWitnessIndex, C.ScriptExecutionError) -> Bool
        isScriptErrorEvaluationFailed (_, e) = case e of
            C.ScriptErrorEvaluationFailed _ _ -> True
            _                                 -> True

substituteExecutionUnits :: Map C.ScriptWitnessIndex C.ExecutionUnits
                         -> C.TxBodyContent BuildTx C.BabbageEra
                         -> Either (C.TxBodyErrorAutoBalance x) (C.TxBodyContent BuildTx C.BabbageEra)
substituteExecutionUnits exUnitsMap =
    mapTxScriptWitnesses f
  where
    f :: C.ScriptWitnessIndex
      -> C.ScriptWitness witctx era
      -> Either (C.TxBodyErrorAutoBalance x) (C.ScriptWitness witctx era)
    f _   wit@C.SimpleScriptWitness{} = Right wit
    f idx (C.PlutusScriptWitness langInEra version script datum redeemer _) =
      case Map.lookup idx exUnitsMap of
        Nothing ->
          Left $ C.TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap idx exUnitsMap
        Just exunits -> Right $ C.PlutusScriptWitness langInEra version script datum redeemer exunits

-- | same behaviour as in Cardano.Api.TxBody.
mapTxScriptWitnesses
  :: forall x. (forall witctx. C.ScriptWitnessIndex
                   -> C.ScriptWitness witctx C.BabbageEra
                   -> Either (C.TxBodyErrorAutoBalance x) (C.ScriptWitness witctx C.BabbageEra))
  -> C.TxBodyContent BuildTx C.BabbageEra
  -> Either (C.TxBodyErrorAutoBalance x) (TxBodyContent BuildTx C.BabbageEra)
mapTxScriptWitnesses f txbodycontent@C.TxBodyContent {
                         C.txIns,
                         C.txWithdrawals,
                         C.txCertificates,
                         C.txMintValue
                       } = do
    mappedTxIns <- mapScriptWitnessesTxIns txIns
    mappedWithdrawals <- mapScriptWitnessesWithdrawals txWithdrawals
    mappedMintedVals <- mapScriptWitnessesMinting txMintValue
    mappedTxCertificates <- mapScriptWitnessesCertificates txCertificates

    Right $ txbodycontent
      { C.txIns = mappedTxIns
      , C.txMintValue = mappedMintedVals
      , C.txCertificates = mappedTxCertificates
      , C.txWithdrawals = mappedWithdrawals
      }
  where
    mapScriptWitnessesTxIns
      :: [(C.TxIn, C.BuildTxWith BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))]
      -> Either (C.TxBodyErrorAutoBalance x) [(C.TxIn, C.BuildTxWith BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))]
    mapScriptWitnessesTxIns txins  =
      let mappedScriptWitnesses
            :: [ ( C.TxIn
                 , Either (C.TxBodyErrorAutoBalance x) (C.BuildTxWith BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))
                 )
               ]
          mappedScriptWitnesses =
            [ (txin, C.BuildTxWith <$> wit')
              -- The tx ins are indexed in the map order by txid
            | (ix, (txin, C.BuildTxWith wit)) <- zip [0..] (orderTxIns txins)
            , let wit' = case wit of
                           C.KeyWitness{}              -> Right wit
                           C.ScriptWitness ctx witness -> C.ScriptWitness ctx <$> witness'
                             where
                               witness' = f (C.ScriptWitnessIndexTxIn ix) witness
            ]
      in traverse ( \(txIn, eWitness) ->
                      case eWitness of
                        Left e    -> Left e
                        Right wit -> Right (txIn, wit)
                  ) mappedScriptWitnesses

    mapScriptWitnessesWithdrawals
      :: C.TxWithdrawals BuildTx C.BabbageEra
      -> Either (C.TxBodyErrorAutoBalance x) (C.TxWithdrawals BuildTx C.BabbageEra)
    mapScriptWitnessesWithdrawals C.TxWithdrawalsNone = Right C.TxWithdrawalsNone
    mapScriptWitnessesWithdrawals (C.TxWithdrawals supported withdrawals) =
      let mappedWithdrawals
            :: [( C.StakeAddress
                , Coin
                , Either (C.TxBodyErrorAutoBalance x) (C.BuildTxWith BuildTx (C.Witness C.WitCtxStake C.BabbageEra))
                )]
          mappedWithdrawals =
              [ (addr, withdrawal, C.BuildTxWith <$> mappedWitness)
                -- The withdrawals are indexed in the map order by stake credential
              | (ix, (addr, withdrawal, C.BuildTxWith wit)) <- zip [0..] (orderStakeAddrs withdrawals)
              , let mappedWitness = adjustWitness (f (C.ScriptWitnessIndexWithdrawal ix)) wit
              ]
      in C.TxWithdrawals supported
         <$> traverse ( \(sAddr, ll, eWitness) ->
                          case eWitness of
                            Left e    -> Left e
                            Right wit -> Right (sAddr, ll, wit)
                      ) mappedWithdrawals
      where
        adjustWitness
          :: (C.ScriptWitness witctx C.BabbageEra -> Either (C.TxBodyErrorAutoBalance x) (C.ScriptWitness witctx C.BabbageEra))
          -> C.Witness witctx C.BabbageEra
          -> Either (C.TxBodyErrorAutoBalance x) (C.Witness witctx C.BabbageEra)
        adjustWitness _ (C.KeyWitness ctx) = Right $ C.KeyWitness ctx
        adjustWitness g (C.ScriptWitness ctx witness') = C.ScriptWitness ctx <$> g witness'

    mapScriptWitnessesCertificates
      :: C.TxCertificates BuildTx C.BabbageEra
      -> Either (C.TxBodyErrorAutoBalance x) (C.TxCertificates BuildTx C.BabbageEra)
    mapScriptWitnessesCertificates C.TxCertificatesNone = Right C.TxCertificatesNone
    mapScriptWitnessesCertificates (C.TxCertificates supported certs (C.BuildTxWith witnesses)) =
      let mappedScriptWitnesses
           :: [(C.StakeCredential, Either (C.TxBodyErrorAutoBalance x) (C.Witness C.WitCtxStake C.BabbageEra))]
          mappedScriptWitnesses =
              [ (stakecred, C.ScriptWitness ctx <$> witness')
                -- The certs are indexed in list order
              | (ix, cert) <- zip [0..] certs
              , stakecred <- maybeToList (selectStakeCredential cert)
              , C.ScriptWitness ctx witness <- maybeToList (Map.lookup stakecred witnesses)
              , let witness' = f (C.ScriptWitnessIndexCertificate ix) witness
              ]
      in C.TxCertificates supported certs . C.BuildTxWith . Map.fromList <$>
           traverse ( \(sCred, eScriptWitness) ->
                        case eScriptWitness of
                          Left e    -> Left e
                          Right wit -> Right (sCred, wit)
                    ) mappedScriptWitnesses

    selectStakeCredential :: C.Certificate BabbageEra -> Maybe C.StakeCredential
    selectStakeCredential = \case
      C.ShelleyRelatedCertificate _era cert -> case cert of
        TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyRegCert k) -> Just (C.fromShelleyStakeCredential k)
        TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyUnRegCert k) -> Just (C.fromShelleyStakeCredential k)
        TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyDelegCert k _) -> Just (C.fromShelleyStakeCredential k)
        TxCert.ShelleyTxCertPool{} -> Nothing
        TxCert.ShelleyTxCertGenesisDeleg{} -> Nothing
        TxCert.ShelleyTxCertMir{} -> Nothing
      C.ConwayCertificate{} -> Nothing

    mapScriptWitnessesMinting
      :: C.TxMintValue BuildTx C.BabbageEra
      -> Either (C.TxBodyErrorAutoBalance x) (C.TxMintValue BuildTx C.BabbageEra)
    mapScriptWitnessesMinting C.TxMintNone = Right C.TxMintNone
    mapScriptWitnessesMinting (C.TxMintValue supported value (C.BuildTxWith witnesses)) =
      let mappedScriptWitnesses
            :: [(C.PolicyId, Either (C.TxBodyErrorAutoBalance x) (C.ScriptWitness C.WitCtxMint C.BabbageEra))]
          mappedScriptWitnesses =
            [ (policyid, witness')
              -- The minting policies are indexed in policy id order in the value
            | let C.ValueNestedRep bundle = C.valueToNestedRep value
            , (ix, C.ValueNestedBundle policyid _) <- zip [0..] bundle
            , witness <- maybeToList (Map.lookup policyid witnesses)
            , let witness' = f (C.ScriptWitnessIndexMint ix) witness
            ]
      in do final <- traverse ( \(pid, eScriptWitness) ->
                                   case eScriptWitness of
                                     Left e    -> Left e
                                     Right wit -> Right (pid, wit)
                              ) mappedScriptWitnesses
            Right . C.TxMintValue supported value . C.BuildTxWith
              $ Map.fromList final

{-| This relies on the TxId Ord instance being consistent with the
Ledger.TxId Ord instance via the toShelleyTxId conversion.
This is checked by prop_ord_distributive_TxId
-}
orderTxIns :: [(C.TxIn, v)] -> [(C.TxIn, v)]
orderTxIns = List.sortBy (compare `on` fst)

{-| This relies on the StakeAddress Ord instance being consistent with the
Shelley.RewardAcnt Ord instance via the toShelleyStakeAddr conversion.
This is checked by prop_ord_distributive_StakeAddress
-}
orderStakeAddrs :: [(C.StakeAddress, x, v)] -> [(C.StakeAddress, x, v)]
orderStakeAddrs = List.sortBy (compare `on` (\(k, _, _) -> k))


{-| Get the 'BalanceChanges' for a tx body. Returns 'Nothing' if
a UTXO couldnt be found
-}
balanceChanges :: UTxO ERA -> TxBodyContent BuildTx ERA -> Maybe BalanceChanges
balanceChanges (C.UTxO lookups) body = do
  let outputs = foldMap txOutChange (body ^. L.txOuts)
  inputs <- Utxos.invBalanceChange . foldMap (txOutChange . id) <$> traverse (\(txi, _) -> Map.lookup txi lookups) (body ^. L.txIns)
  pure (outputs <> inputs)

txOutChange :: TxOut ctx C.BabbageEra -> BalanceChanges
txOutChange (view L._TxOut -> (fmap C.fromShelleyPaymentCredential . preview (L._AddressInEra . L._Address . _2) -> Just addr, view L._TxOutValue -> value, _, _)) =
  BalanceChanges (Map.singleton addr value)
txOutChange _ = mempty

data BalanceTxError =
  ACoinSelectionError CoinSelectionError
  | ABalancingError BalancingError
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{-| Balance the transaction using the given UTXOs and return address. This
calls 'balanceTransactionBody' after preparing all the required inputs.
-}
balanceTx ::
  (MonadBlockchain m, MonadError BalanceTxError m) =>

  -- | Label
  Tracer m TxBalancingMessage ->

  -- | Return output used for leftover funds. This output will be used for
  --   balancing, and it will be added to the transaction
  --   IF the funds locked in it (after balancing) are non zero.
  C.TxOut C.CtxTx C.BabbageEra ->

  -- | Set of UTxOs that can be used to supply missing funds
  UtxoSet C.CtxUTxO a ->

  -- | The unbalanced transaction body
  TxBodyContent BuildTx ERA ->

  -- | The balanced transaction body and the balance changes (per address)
  m (C.BalancedTxBody ERA, BalanceChanges)
balanceTx dbg returnUTxO0 walletUtxo txb = do
  params <- queryProtocolParameters
  pools <- queryStakePools
  availableUTxOs <- checkCompatibilityLevel dbg txb walletUtxo
  -- compatibility level
  let txb0 = txb & L.txProtocolParams .~ C.BuildTxWith (Just params)
  -- TODO: Better error handling (better than 'fail')
  otherInputs <- lookupTxIns (requiredTxIns txb)
  let combinedTxIns =
        let UTxO w = availableUTxOs
            UTxO o = otherInputs
        in UTxO (Map.union w o)

  (finalBody, returnUTxO1) <- mapError ACoinSelectionError $ do
    bodyWithInputs <- addOwnInput txb0 walletUtxo
    bodyWithCollat <- setCollateral bodyWithInputs walletUtxo
    balancePositive (natTracer lift dbg) pools params combinedTxIns returnUTxO0 walletUtxo bodyWithCollat
  count <- requiredSignatureCount finalBody
  csi <- prepCSInputs count returnUTxO1 combinedTxIns finalBody
  start <- querySystemStart
  hist <- queryEraHistory
  mapError ABalancingError (balanceTransactionBody (natTracer lift dbg) start hist params pools csi)

-- | Check the compatibility level of the transaction body
--   and remove any incompatible UTxOs from the UTxO set.
checkCompatibilityLevel :: Monad m => Tracer m TxBalancingMessage -> TxBodyContent BuildTx ERA -> UtxoSet C.CtxUTxO a -> m (UTxO BabbageEra)
checkCompatibilityLevel tr txB (UtxoSet w) = do
  let compatibility = txCompatibility txB
      utxoIn = UTxO (fmap fst w)
      UTxO utxoOut = compatibleWith compatibility utxoIn
      droppedTxIns = Map.size w - Map.size utxoOut
  traceWith tr CompatibilityLevel{compatibility, droppedTxIns}
  pure (UTxO utxoOut)

{-| Balance the transaction using the wallet's funds, then sign it.
-}
balanceForWallet :: (MonadBlockchain m, MonadError BalanceTxError m) => Tracer m TxBalancingMessage -> Wallet -> UtxoSet C.CtxUTxO a -> TxBodyContent BuildTx ERA -> m (C.Tx ERA, BalanceChanges)
balanceForWallet dbg wallet walletUtxo txb = do
  n <- networkId
  let walletAddress = Wallet.addressInEra n wallet
      txOut = L.emptyTxOut walletAddress
  balanceForWalletReturn dbg wallet walletUtxo txOut txb

{-| Balance the transaction using the wallet's funds and the provided return output, then sign it.
-}
balanceForWalletReturn :: (MonadBlockchain m, MonadError BalanceTxError m) => Tracer m TxBalancingMessage -> Wallet -> UtxoSet C.CtxUTxO a -> C.TxOut C.CtxTx C.BabbageEra -> TxBodyContent BuildTx ERA -> m (C.Tx ERA, BalanceChanges)
balanceForWalletReturn dbg wallet walletUtxo returnOutput txb = do
  first (signForWallet wallet) <$> balanceTx dbg returnOutput walletUtxo txb

{-| Sign a transaction with the wallet's key
-}
signForWallet :: Wallet -> C.BalancedTxBody ERA -> C.Tx ERA
signForWallet wallet (C.BalancedTxBody _ txbody _changeOutput _fee) =
  let wit = [C.makeShelleyKeyWitness C.ShelleyBasedEraBabbage txbody $ C.WitnessPaymentKey (Wallet.getWallet wallet)]
  in C.makeSignedTransaction wit txbody

-- | If the transaction body has no inputs then we add one from the wallet's UTxO set.
--   (we have to do this because 'C.evaluateTransactionBalance' fails on a tx body with
--    no inputs)
--   Throws an error if the transaction body has no inputs and the wallet UTxO set is empty.
addOwnInput :: MonadError CoinSelectionError m => TxBodyContent BuildTx ERA -> UtxoSet ctx a -> m (TxBodyContent BuildTx ERA)
addOwnInput body (Utxos.removeUtxos (spentTxIns body) -> UtxoSet{_utxos})
  | not (List.null $ view L.txIns body) = pure body
  | not (Map.null _utxos) =
      -- Select ada-only outputs if possible
      let availableUTxOs = List.sortOn (length . view (L._TxOut . _2 . L._TxOutValue . to C.valueToList) . fst . snd) (Map.toList _utxos)
      in pure $ buildTxWith (execBuildTx (spendPublicKeyOutput (fst $ head availableUTxOs))) body
  | otherwise = throwError NoWalletUTxOs

-- | Add a collateral input. Throws a 'NoAdaOnlyUTxOsForCollateral' error if a collateral input is required,
--   but no suitable input is provided in the wallet UTxO set.
setCollateral :: MonadError CoinSelectionError m => TxBodyContent BuildTx ERA -> UtxoSet ctx a -> m (TxBodyContent BuildTx ERA)
setCollateral body (Utxos.onlyAda -> UtxoSet{_utxos}) =
  let noScripts = not (runsScripts body)
      hasCollateral = not (view (L.txInsCollateral . L._TxInsCollateral . to List.null) body)
  in
    if noScripts || hasCollateral
      then pure body -- no script witnesses in inputs.
      else
        -- select the output with the largest amount of Ada
        case listToMaybe $ List.sortOn (Down . C.selectLovelace . view (L._TxOut . _2 . L._TxOutValue) . fst . snd) $ Map.toList _utxos of
          Nothing     -> throwError NoAdaOnlyUTxOsForCollateral
          Just (k, _) -> pure $ buildTxWith (execBuildTx (addCollateral k)) body

{-| Whether the transaction runs any plutus scripts
-}
runsScripts :: TxBodyContent BuildTx ERA -> Bool
runsScripts body =
  let scriptIns   = body ^.. (L.txIns . traversed . _2 . L._BuildTxWith . L._ScriptWitness)
      minting     = body ^. (L.txMintValue . L._TxMintValue . _2)
      certificates = body ^.. (L.txCertificates . L._TxCertificates . _2 . traversed . L._ScriptWitness)
      withdrawals  = body ^.. (L.txWithdrawals . L._TxWithdrawals . traversed . _3 . L._BuildTxWith . L._ScriptWitness)

  in
    not (null scriptIns && Map.null minting && null withdrawals && null certificates)

{-| Add inputs to ensure that the balance is strictly positive. After calling @balancePositive@
* The amount of Ada provided by the transaction's inputs minus (the amount of Ada produced by the transaction's outputs plus the change output) is greater than zero
* For all native tokens @t@, the amount of @t@ provided by the transaction's inputs minus (the amount of @t@ produced by the transaction's outputs plus the change output plus the delta of @t@ minted / burned) is equal to zero
-}
balancePositive :: MonadError CoinSelectionError m => Tracer m TxBalancingMessage -> Set PoolId -> C.LedgerProtocolParameters BabbageEra -> C.UTxO ERA -> C.TxOut C.CtxTx C.BabbageEra -> UtxoSet ctx a -> TxBodyContent BuildTx ERA -> m (TxBodyContent BuildTx ERA, C.TxOut C.CtxTx C.BabbageEra)
balancePositive dbg poolIds ledgerPPs utxo_ returnUTxO0 walletUtxo txBodyContent0 = do
  txb <- either (throwError . bodyError) pure (C.createAndValidateTransactionBody C.ShelleyBasedEraBabbage txBodyContent0)
  let bal = C.evaluateTransactionBalance C.ShelleyBasedEraBabbage (C.unLedgerProtocolParameters ledgerPPs) poolIds mempty mempty utxo_ txb & view L._TxOutValue
      available = Utxos.removeUtxos (spentTxIns txBodyContent0) walletUtxo

  -- minimum positive balance (in lovelace) that must be available to cover
  -- * minimum deposit on the ada-only change output, if required, and
  -- * transaction fee, incl. script fee if required
  -- we set it to rather large value to ensure that we can build a valid transaction.
  let threshold = negate (if runsScripts txBodyContent0 then 8_000_000 else 2_500_000)
      balance = bal & L._Value . at C.AdaAssetId %~ maybe (Just threshold) (Just . (+) threshold)

  traceWith dbg PrepareInputs
    { availableBalance   = Utxos.totalBalance available
    , transactionBalance = balance
    }
  (txBodyContent1, additionalBalance) <- addInputsForAssets dbg balance available txBodyContent0

  let bal0 = balance <> additionalBalance
  let (returnUTxO1, _deposit) = addOutputForNonAdaAssets ledgerPPs returnUTxO0 bal0

  pure (txBodyContent1, returnUTxO1)

{-| Examine the negative part of the transaction balance and select inputs from
the wallet's UTXO set to cover the assets required by it. If there are no
assets missing then no inputs will be added.
-}
addInputsForAssets ::
  MonadError CoinSelectionError m =>
  Tracer m TxBalancingMessage ->
  C.Value -> -- ^ The balance of the transaction
  UtxoSet ctx a -> -- ^ UTxOs that we can spend to cover the negative part of the balance
  TxBodyContent BuildTx ERA -> -- ^ Transaction body
  m (TxBodyContent BuildTx ERA, C.Value) -- ^ Transaction body with additional inputs and the total value of the additional inputs
addInputsForAssets dbg txBal availableUtxo txBodyContent
  | null (fst $ splitValue txBal) = do
      traceWith dbg NoAssetsMissing
      return (txBodyContent, mempty)
  | otherwise = do
      let missingAssets = fmap (second abs) $ fst $ splitValue txBal
      traceWith dbg (MissingAssets $ C.valueFromList missingAssets)
      case Wallet.selectMixedInputsCovering availableUtxo missingAssets of
        Nothing -> throwError (NotEnoughMixedOutputsFor (C.valueFromList missingAssets) (Utxos.totalBalance availableUtxo) txBal)
        Just (total, ins) -> pure (txBodyContent & over L.txIns (<> fmap spendPubKeyTxIn ins), total)

{-| Examine the positive part of the transaction balance and add any non-Ada assets it contains
to the provided change output. If the positive part only contains Ada then the
change output is returned unmodified.
-}
addOutputForNonAdaAssets ::
  C.LedgerProtocolParameters BabbageEra -> -- ^ Protocol parameters (for computing the minimum lovelace amount in the output)
  C.TxOut C.CtxTx C.BabbageEra -> -- ^ Change output. Overflow non-Ada assets will be added to this output's value.
  C.Value -> -- ^ The balance of the transaction
  (C.TxOut C.CtxTx C.BabbageEra, C.Quantity) -- ^ The modified change output and the lovelace portion of the change output's value. If no output was added then the amount will be 0.
addOutputForNonAdaAssets pparams returnUTxO (C.valueFromList . snd . splitValue -> positives)
  | isNothing (C.valueToLovelace positives) =
      let vlWithoutAda = positives & set (L._Value . at C.AdaAssetId) Nothing
          output =
            setMinAdaDeposit pparams
            $ returnUTxO & L._TxOut . _2 . L._TxOutValue <>~ vlWithoutAda
      in  (output, output ^. L._TxOut . _2 . L._TxOutValue . to (C.lovelaceToQuantity . C.selectLovelace))
  | otherwise = (returnUTxO, C.Quantity 0)

splitValue :: C.Value -> ([(C.AssetId, C.Quantity)], [(C.AssetId, C.Quantity)])
splitValue =
  let p (_, q) = q < 0
      f (_, q) = q /= 0
  in List.partition p . List.filter f . C.valueToList

{-| Take the tx body and produce a 'CSInputs' value for coin selection,
using the @MonadBlockchain@ effect to query any missing UTxO information.
-}
prepCSInputs ::
 MonadBlockchain m
  => TransactionSignatureCount
  -> C.TxOut C.CtxTx C.BabbageEra -- ^ Change address
  -> C.UTxO ERA -- ^ UTxOs that may be used for balancing
  -> C.TxBodyContent C.BuildTx C.BabbageEra -- ^ Unbalanced transaction body
  -> m CSInputs -- ^ Inputs for coin balancing
prepCSInputs sigCount csiChangeAddress csiUtxo csiTxBody = do
  CSInputs
    <$> pure csiUtxo
    <*> pure csiTxBody
    <*> pure csiChangeAddress
    <*> pure sigCount

spentTxIns :: C.TxBodyContent v C.BabbageEra -> Set C.TxIn
spentTxIns (view L.txIns -> inputs) =
  -- TODO: Include collateral etc. fields
  Set.fromList (fst <$> inputs)

requiredTxIns :: C.TxBodyContent v C.BabbageEra -> Set C.TxIn
requiredTxIns body =
  Set.fromList (fst <$> view L.txIns body)
  <> Set.fromList (view (L.txInsReference . L._TxInsReference) body)
  <> Set.fromList (view (L.txInsCollateral . L._TxInsCollateral) body)

lookupTxIns :: MonadBlockchain m => Set C.TxIn -> m (C.UTxO ERA)
lookupTxIns = utxoByTxIn

keyWitnesses :: MonadBlockchain m => C.TxBodyContent v C.BabbageEra -> m (Set (Keys.KeyHash 'Keys.Payment StandardCrypto))
keyWitnesses (requiredTxIns -> inputs) = do
  C.UTxO utxos <- utxoByTxIn inputs
  pure $ Set.fromList $ mapMaybe (publicKeyCredential . snd) $ Map.toList utxos

-- | The number of signatures required to spend the transaction's inputs
--   and to satisfy the "extra key witnesses" constraint
--   and required for certification.
requiredSignatureCount :: MonadBlockchain m => C.TxBodyContent C.BuildTx C.BabbageEra -> m TransactionSignatureCount
requiredSignatureCount content = do
  keyWits <- keyWitnesses content
  let hsh (C.PaymentKeyHash h) = h
      extraSigs = view (L.txExtraKeyWits . L._TxExtraKeyWitnesses) content
      allSigs = Set.union keyWits (Set.fromList $ fmap hsh extraSigs)
      certKeyWits = case view L.txCertificates content of
        C.TxCertificates _ cs _ -> mconcat $ getCertKeyWits <$> cs
        C.TxCertificatesNone -> Set.empty

      getCertKeyWits :: C.Certificate era -> Set (CertificateKeyWitness era)
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyRegCert (KeyHashObj hash)))) =
        Set.singleton $ CertificateStakeKey hash
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyRegCert _))) =
        Set.empty
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyUnRegCert (KeyHashObj hash)))) =
        Set.singleton $ CertificateStakeKey hash
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyUnRegCert _))) =
        Set.empty
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyDelegCert (KeyHashObj hash) poolHash))) =
        Set.fromList [CertificateStakeKey hash, CertificateStakePoolKey poolHash]
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertDelegCert (TxCert.ShelleyDelegCert _ hash))) =
        Set.singleton $ CertificateStakePoolKey hash
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertPool (TxCert.RegPool PoolParams{ppId, ppOwners}))) =
           Set.singleton (CertificateStakePoolKey ppId)
        <> Set.fromList (CertificateStakeKey <$> Set.toList ppOwners)
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertPool (TxCert.RetirePool hash _))) =
           Set.singleton (CertificateStakePoolKey hash)
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertGenesisDeleg _)) =
        error "Genesis key delegation certificate key witness count not supported"
      getCertKeyWits (C.ShelleyRelatedCertificate _ (TxCert.ShelleyTxCertMir _)) =
        error "MIR certificate key witness count not supported"
      getCertKeyWits _ =
        Set.empty

  pure $ TransactionSignatureCount (fromIntegral $ Set.size allSigs + Set.size certKeyWits)

{- | Certificate key witness
-}
data CertificateKeyWitness era =
    CertificateStakeKey (KeyHash Staking (EraCrypto (C.ShelleyLedgerEra era)))
  | CertificateStakePoolKey (KeyHash StakePool (EraCrypto (C.ShelleyLedgerEra era)))
  deriving stock (Eq, Ord)

publicKeyCredential :: C.TxOut v C.BabbageEra -> Maybe (Keys.KeyHash 'Keys.Payment StandardCrypto)
publicKeyCredential = preview (L._TxOut . _1 . L._ShelleyAddressInBabbageEra . _2 . L._ShelleyPaymentCredentialByKey)

spendPubKeyTxIn :: C.TxIn -> (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))
-- TODO: consolidate with Convex.BuildTx.spendPublicKeyOutput
spendPubKeyTxIn txIn = (txIn, C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending))
