{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Building cardano transactions from tx bodies
-}
module Convex.CoinSelection(
  -- * Data types
  CSInputs(..),
  ERA,
  utxo,
  txBody,
  changeAddress,
  numWitnesses,
  -- * Balancing
  BalancingError(..),
  balanceTransactionBody,
  balanceForWallet,
  -- * Etc.
  prepCSInputs
  ) where

import           Cardano.Api.Shelley      (AddressInEra, BabbageEra, BuildTx,
                                           TxBodyContent, UTxO (..))
import qualified Cardano.Api.Shelley      as C
import           Cardano.Ledger.Crypto    (StandardCrypto)
import qualified Cardano.Ledger.Keys      as Keys
import           Control.Lens             (_1, _2, makeLensesFor, over, preview,
                                           set, view, (&), (|>))
import           Convex.BuildTx           (spendPublicKeyOutput)
import qualified Convex.Lenses            as L
import           Convex.MockChain.Class   (MonadBlockchain (..),
                                           MonadBlockchainQuery (..))
import           Convex.MockChain.Wallets (Wallet, WalletUtxo (..))
import qualified Convex.MockChain.Wallets as Wallet
import           Convex.NodeParams        (NodeParams (..))
import           Data.Bifunctor           (Bifunctor (..))
import           Data.Function            (on)
import qualified Data.List                as List
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe               (mapMaybe, maybeToList)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set

type ERA = BabbageEra

{-| Inputs needed for coin selection
-}
data CSInputs =
  CSInputs
    { csiUtxo          :: UTxO ERA -- ^ UTXOs that we need to know about
    , csiTxBody        :: TxBodyContent BuildTx ERA -- ^ Tx body to balance
    , csiChangeAddress :: AddressInEra ERA -- ^ Change address
    , csiNumWitnesses  :: Word -- ^ How many shelley key witnesses there will be
    }

makeLensesFor
  [ ("csiUtxo", "utxo")
  , ("csiTxBody", "txBody")
  , ("csiChangeAddress", "changeAddress")
  , ("csiNumWitnesses", "numWitnesses")
  ] ''CSInputs

data CoinSelectionError =
  UnsupportedBalance (C.TxOutValue ERA)
  | BodyError C.TxBodyError
  | NotEnoughAdaOnlyOutputsFor C.Lovelace
  deriving Show

data BalancingError =
  BalancingError C.TxBodyErrorAutoBalance
  deriving Show

{-| Perform transaction balancing
-}
balanceTransactionBody :: NodeParams -> CSInputs -> Either BalancingError (C.BalancedTxBody ERA)
balanceTransactionBody NodeParams{npSystemStart, npEraHistory, npProtocolParameters, npStakePools} CSInputs{csiUtxo, csiTxBody, csiChangeAddress, csiNumWitnesses} = do

  let changeOutputSmall = C.TxOut csiChangeAddress (C.lovelaceToTxOutValue 1) C.TxOutDatumNone C.ReferenceScriptNone
      changeOutputLarge = C.TxOut csiChangeAddress (C.lovelaceToTxOutValue $ C.Lovelace (2^(64 :: Integer)) - 1) C.TxOutDatumNone C.ReferenceScriptNone
  -- append output instead of prepending
  txbody0 <-
    first (BalancingError . C.TxBodyError) $ C.makeTransactionBody $ csiTxBody & over L.txOuts (|> changeOutputSmall)

  exUnitsMap <- first (BalancingError . C.TxBodyErrorValidityInterval) $
                C.evaluateTransactionExecutionUnits
                C.BabbageEraInCardanoMode
                npSystemStart npEraHistory
                npProtocolParameters
                csiUtxo
                txbody0

  exUnitsMap' <- first BalancingError $
    case Map.mapEither id exUnitsMap of
      (failures, exUnitsMap') ->
        handleExUnitsErrors C.ScriptValid failures exUnitsMap'

  let txbodycontent1 = substituteExecutionUnits exUnitsMap' csiTxBody

  -- append output instead of prepending
  txbody1 <- first (BalancingError . C.TxBodyError)
              $ C.makeTransactionBody
              $ txbodycontent1
                  & set L.txFee (C.Lovelace (2^(32 :: Integer) - 1))
                  & over L.txOuts (|> changeOutputLarge)

  let !t_fee = C.evaluateTransactionFee npProtocolParameters txbody1 csiNumWitnesses 0

  txbody2 <- first (BalancingError . C.TxBodyError)
              $ C.makeTransactionBody
              $ txbodycontent1 & set L.txFee t_fee

  let !balance = C.evaluateTransactionBalance npProtocolParameters npStakePools csiUtxo txbody2

  mapM_ (`checkMinUTxOValue` npProtocolParameters) $ C.txOuts txbodycontent1

  case balance of
    C.TxOutAdaOnly _ _ -> balanceCheck npProtocolParameters csiChangeAddress balance
    C.TxOutValue _ v   ->
      case C.valueToLovelace v of
        -- FIXME: Support non Ada assets
        Nothing -> Left $ BalancingError $ C.TxBodyErrorNonAdaAssetsUnbalanced v
        Just _  -> balanceCheck npProtocolParameters csiChangeAddress balance

  let finalBodyContent =
        txbodycontent1
          & set L.txFee t_fee
          & over L.txOuts (accountForNoChange (C.TxOut csiChangeAddress balance C.TxOutDatumNone C.ReferenceScriptNone))

  txbody3 <- first (BalancingError . C.TxBodyError) $ C.makeTransactionBody finalBodyContent

  let mkBalancedBody b = C.BalancedTxBody b (C.TxOut csiChangeAddress balance C.TxOutDatumNone C.ReferenceScriptNone) t_fee
  return (mkBalancedBody txbody3)

checkMinUTxOValue
  :: C.TxOut C.CtxTx C.BabbageEra
  -> C.ProtocolParameters
  -> Either BalancingError ()
checkMinUTxOValue txout@(C.TxOut _ v _ _) pparams' = do
  minUTxO  <- first (BalancingError . C.TxBodyErrorMinUTxOMissingPParams)
                $ C.calculateMinimumUTxO C.ShelleyBasedEraBabbage txout pparams'
  if C.txOutValueToLovelace v >= C.selectLovelace minUTxO
  then Right ()
  else Left $ BalancingError $ C.TxBodyErrorMissingParamMinUTxO
  -- replace TxBodyErrMinUTxONotMet with TxBodyErrorMissingParamMinUTxO to avoid TxOutInAnyEra

accountForNoChange :: C.TxOut C.CtxTx C.BabbageEra -> [C.TxOut C.CtxTx C.BabbageEra] -> [C.TxOut C.CtxTx C.BabbageEra]
accountForNoChange change@(C.TxOut _ balance _ _) rest =
  case C.txOutValueToLovelace balance of
    C.Lovelace 0 -> rest
    _ ->
      -- checks if there already exists a txout with same address
      -- that contains only ada tokens and no datum hash. If it exists then add change
      -- instead of creating a new txout, i.e., rest ++ [change]
      updateRestWithChange change rest

balanceCheck :: C.ProtocolParameters -> AddressInEra BabbageEra -> C.TxOutValue C.BabbageEra -> Either BalancingError ()
balanceCheck pparams changeaddr balance
  | C.txOutValueToLovelace balance == 0 = return ()
  | C.txOutValueToLovelace balance < 0 =
      Left . BalancingError . C.TxBodyErrorAdaBalanceNegative $ C.txOutValueToLovelace balance
  | otherwise =
      case checkMinUTxOValue (C.TxOut changeaddr balance C.TxOutDatumNone C.ReferenceScriptNone) pparams of
        Left (BalancingError C.TxBodyErrorMissingParamMinUTxO) ->
          -- replace TxBodyErrMinUTxONotMet with TxBodyEmptyTxOuts to avoid TxOutInAnyEra
          Left $ BalancingError $ C.TxBodyError C.TxBodyEmptyTxOuts
        Left err -> Left err
        Right _ -> Right ()

updateRestWithChange :: C.TxOut C.CtxTx C.BabbageEra -> [C.TxOut C.CtxTx C.BabbageEra] -> [C.TxOut C.CtxTx C.BabbageEra]
updateRestWithChange change [] = [change]
updateRestWithChange change@(C.TxOut caddr cv _ _) (txout@(C.TxOut addr (C.TxOutAdaOnly e v) C.TxOutDatumNone _) : tl)
  | addr == caddr =
      (C.TxOut addr (C.TxOutAdaOnly e ((C.txOutValueToLovelace cv) <> v)) C.TxOutDatumNone C.ReferenceScriptNone) : tl
  | otherwise = txout : (updateRestWithChange change tl)

updateRestWithChange change@(C.TxOut caddr cv _ _) (txout@(C.TxOut addr (C.TxOutValue e v) C.TxOutDatumNone _) : tl)
  | addr == caddr =
    case C.valueToLovelace v of
      Nothing -> txout : (updateRestWithChange change tl)
      Just l ->
        (C.TxOut addr (C.TxOutValue e (C.lovelaceToValue $ (C.txOutValueToLovelace cv) <> l)) C.TxOutDatumNone C.ReferenceScriptNone) : tl

updateRestWithChange change (txout : tl) = txout : (updateRestWithChange change tl)

handleExUnitsErrors ::
     C.ScriptValidity -- ^ Mark script as expected to pass or fail validation
  -> Map C.ScriptWitnessIndex C.ScriptExecutionError
  -> Map C.ScriptWitnessIndex C.ExecutionUnits
  -> Either C.TxBodyErrorAutoBalance (Map C.ScriptWitnessIndex C.ExecutionUnits)
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
                         -> C.TxBodyContent C.BuildTx C.BabbageEra
                         -> C.TxBodyContent C.BuildTx C.BabbageEra
substituteExecutionUnits exUnitsMap =
    mapTxScriptWitnesses f
  where
    f :: C.ScriptWitnessIndex
      -> C.ScriptWitness witctx C.BabbageEra
      -> C.ScriptWitness witctx C.BabbageEra
    f _   wit@C.SimpleScriptWitness{} = wit
    f idx wit@(C.PlutusScriptWitness langInEra version script datum redeemer _) =
      case Map.lookup idx exUnitsMap of
        Nothing      -> wit
        Just exunits ->
          C.PlutusScriptWitness langInEra version script datum redeemer exunits

-- | same behaviour as in Cardano.Api.TxBody. However, we do not consider withwdrawals,
-- certificates as not required for the time being.
mapTxScriptWitnesses :: (forall witctx. C.ScriptWitnessIndex
                                     -> C.ScriptWitness witctx C.BabbageEra
                                     -> C.ScriptWitness witctx C.BabbageEra)
                     -> C.TxBodyContent C.BuildTx C.BabbageEra
                     -> C.TxBodyContent C.BuildTx C.BabbageEra
mapTxScriptWitnesses f txbodycontent@C.TxBodyContent {
                         C.txIns,
                         C.txMintValue
                       } =
    txbodycontent {
      C.txIns          = mapScriptWitnessesTxIns txIns
    , C.txMintValue    = mapScriptWitnessesMinting txMintValue
    }
  where
    mapScriptWitnessesTxIns
      :: [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))]
      -> [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))]
    mapScriptWitnessesTxIns txins =
        [ (txin, C.BuildTxWith wit')
          -- keep txins order
        | (ix, (txin, C.BuildTxWith wit)) <- zip [0..] $ List.sortBy (compare `on` fst) txins
        , let wit' = case wit of
                       C.KeyWitness{}              -> wit
                       C.ScriptWitness ctx witness -> C.ScriptWitness ctx witness'
                         where
                           witness' = f (C.ScriptWitnessIndexTxIn ix) witness
        ]

    mapScriptWitnessesMinting
      :: C.TxMintValue C.BuildTx C.BabbageEra
      -> C.TxMintValue C.BuildTx C.BabbageEra
    mapScriptWitnessesMinting  C.TxMintNone = C.TxMintNone
    mapScriptWitnessesMinting (C.TxMintValue supported v
                                           (C.BuildTxWith witnesses)) =
      C.TxMintValue supported v $ C.BuildTxWith $ Map.fromList
        [ (policyid, witness')
          -- The minting policies are indexed in policy id order in the value
        | let C.ValueNestedRep bundle = C.valueToNestedRep v
        , (ix, C.ValueNestedBundle policyid _) <- zip [0..] bundle
        , witness <- maybeToList (Map.lookup policyid witnesses)
        , let witness' = f (C.ScriptWitnessIndexMint ix) witness
        ]

{-| Balance the transaction using the wallet's funds, then sign it.
-}
balanceForWallet :: (MonadBlockchain m, MonadBlockchainQuery m, MonadFail m) => NodeParams -> Wallet -> TxBodyContent BuildTx ERA -> m (C.Tx ERA)
balanceForWallet nodeParams wallet txb = do
  -- TODO: Better error handling (better than 'fail')
  walletFunds <- utxoByAddress (Wallet.addressInEra wallet)
  -- spentTxIns_ <- utxoByTxIn (spentTxIns txb)
  let walletUtxo = Wallet.fromUtxos (npNetworkId nodeParams) wallet walletFunds
  -- TODO: MonadState WalletUtxo ?
  finalBody <- either (fail . show) pure (addMissingInputs nodeParams walletFunds walletUtxo (flip addOwnInput walletUtxo txb))
  csi <- prepCSInputs (Wallet.addressInEra wallet) walletFunds finalBody
  C.BalancedTxBody txbody _changeOutput _fee <- either (fail . show) pure (balanceTransactionBody nodeParams csi)
  let wit = [C.makeShelleyKeyWitness txbody $ C.WitnessPaymentKey  (Wallet.getWallet wallet)]
      stx = C.makeSignedTransaction wit txbody
  pure stx

addOwnInput :: TxBodyContent BuildTx ERA -> WalletUtxo -> TxBodyContent BuildTx ERA
addOwnInput body (Wallet.removeTxIns (spentTxIns body) -> WalletUtxo{wiAdaOnlyOutputs})
  | Map.null wiAdaOnlyOutputs = body
  | not (List.null $ view L.txIns body) = body
  | otherwise =
      spendPublicKeyOutput (fst $ head $ Map.toList wiAdaOnlyOutputs) body

{-| Add inputs to ensure that the balance is strictly positive
-}
addMissingInputs :: NodeParams -> C.UTxO ERA -> WalletUtxo -> TxBodyContent BuildTx ERA -> Either CoinSelectionError (TxBodyContent BuildTx ERA)
addMissingInputs NodeParams{npProtocolParameters, npStakePools} utxo_ walletUtxo txBodyContent = do
  txb <- first BodyError (C.makeTransactionBody txBodyContent)
  let bal = C.evaluateTransactionBalance npProtocolParameters npStakePools utxo_ txb
      available = Wallet.removeTxIns (spentTxIns txBodyContent) walletUtxo
  case bal of
        C.TxOutValue _ (C.valueToLovelace -> Just (C.Lovelace l))
          | l < 3_000_000 -> -- ensure a minimum positive balance of 3 Ada to cover any fees & change. TODO: Configurable
              let l' = C.Lovelace (3_000_000 - l) in
              case Wallet.selectAdaInputsCovering available l' of
                Nothing -> Left (NotEnoughAdaOnlyOutputsFor l')
                Just (_, ins) -> Right (txBodyContent & over L.txIns (<> fmap spendPubKeyTxIn ins))
          | otherwise -> Right txBodyContent
        other -> Left (UnsupportedBalance other)

{-| Take the tx body and produce a 'CSInputs' value for coin selection,
using the @MonadBlockchain@ effect to query any missing UTxO information.
-}
prepCSInputs ::
 MonadBlockchain m
  => C.AddressInEra ERA -- ^ Change address
  -> C.UTxO ERA -- ^ UTxOs that may be used for balancing
  -> C.TxBodyContent C.BuildTx C.BabbageEra -- ^ Unbalanced transaction body
  -> m CSInputs -- ^ Inputs for coin balancing
prepCSInputs csiChangeAddress csiUtxo csiTxBody =
  CSInputs
    <$> pure csiUtxo
    <*> pure csiTxBody
    <*> pure csiChangeAddress
    <*> fmap (fromIntegral . Set.size) (keyWitnesses csiTxBody)

spentTxIns :: C.TxBodyContent v C.BabbageEra -> Set C.TxIn
spentTxIns (view L.txIns -> inputs) =
  -- TODO: Include collateral etc. fields
  Set.fromList (fst <$> inputs)

-- lookupTxIns :: MonadBlockchain m => C.UTxO ERA -> Set C.TxIn -> m (C.UTxO ERA)
-- lookupTxIns (C.UTxO other) allTxIns = do
--   let missingTxIns = allTxIns `Set.difference` Map.keysSet other
--   C.UTxO rest <- utxoByTxIn missingTxIns
--   pure (C.UTxO $ rest `Map.union` other)

keyWitnesses :: MonadBlockchain m => C.TxBodyContent C.BuildTx C.BabbageEra -> m (Set (Keys.KeyHash 'Keys.Payment StandardCrypto))
keyWitnesses (view L.txIns -> inputs) = do
  C.UTxO utxos <- utxoByTxIn (Set.fromList $ fst <$> inputs)
  pure $ Set.fromList $ mapMaybe (publicKeyCredential . snd) $ Map.toList utxos

publicKeyCredential :: C.TxOut C.CtxUTxO C.BabbageEra -> Maybe (Keys.KeyHash 'Keys.Payment StandardCrypto)
publicKeyCredential = preview (L._TxOut . _1 . L._ShelleyAddressInBabbageEra . _2 . L._ShelleyPaymentCredentialByKey)

spendPubKeyTxIn :: C.TxIn -> (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))
-- TODO: consolidate with Convex.BuildTx.spendPublicKeyOutput
spendPubKeyTxIn txIn = (txIn, C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending))
