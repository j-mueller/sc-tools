{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DataKinds          #-}
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
  signForWallet,
  -- * Balance changes
  balanceChanges,
  -- * Etc.
  prepCSInputs
  ) where

import           Cardano.Api.Shelley   (AddressInEra, BabbageEra, BuildTx,
                                        CardanoMode, EraHistory, PoolId,
                                        ProtocolParameters, TxBodyContent,
                                        TxOut, UTxO (..))
import qualified Cardano.Api.Shelley   as C
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys   as Keys
import           Cardano.Slotting.Time (SystemStart)
import           Control.Lens          (_1, _2, makeLensesFor, over, preview,
                                        set, to, traversed, view, (&), (.~),
                                        (^.), (^..), (|>))
import           Convex.BuildTx        (addCollateral, setMinAdaDeposit,
                                        spendPublicKeyOutput)
import           Convex.Class          (MonadBlockchain (..))
import qualified Convex.Lenses         as L
import           Convex.Wallet         (Wallet)
import qualified Convex.Wallet         as Wallet
import           Convex.Wallet.Utxos   (BalanceChanges (..), UtxoState (..))
import qualified Convex.Wallet.Utxos   as Utxos
import           Data.Bifunctor        (Bifunctor (..))
import           Data.Function         (on)
import qualified Data.List             as List
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (isNothing, mapMaybe, maybeToList)
import           Data.Set              (Set)
import qualified Data.Set              as Set

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
  | NotEnoughMixedOutputsFor C.Value
  deriving Show

data BalancingError =
  BalancingError C.TxBodyErrorAutoBalance
  | CheckMinUtxoValueError (C.TxOut C.CtxTx BabbageEra) C.Lovelace
  | BalanceCheckError BalancingError
  | ComputeBalanceChangeError
  deriving Show

{-| Perform transaction balancing
-}
balanceTransactionBody :: SystemStart -> EraHistory CardanoMode -> ProtocolParameters -> Set PoolId -> CSInputs -> Either BalancingError (C.BalancedTxBody ERA, BalanceChanges)
balanceTransactionBody systemStart eraHistory protocolParams stakePools CSInputs{csiUtxo, csiTxBody, csiChangeAddress, csiNumWitnesses} = do

  let changeOutputSmall = C.TxOut csiChangeAddress (C.lovelaceToTxOutValue 1) C.TxOutDatumNone C.ReferenceScriptNone
      changeOutputLarge = C.TxOut csiChangeAddress (C.lovelaceToTxOutValue $ C.Lovelace (2^(64 :: Integer)) - 1) C.TxOutDatumNone C.ReferenceScriptNone
  -- append output instead of prepending
  txbody0 <-
    first (BalancingError . C.TxBodyError) $ C.makeTransactionBody $ csiTxBody & over L.txOuts (|> changeOutputSmall)

  exUnitsMap <- first (BalancingError . C.TxBodyErrorValidityInterval) $
                C.evaluateTransactionExecutionUnits
                C.BabbageEraInCardanoMode
                systemStart eraHistory
                protocolParams
                csiUtxo
                txbody0

  exUnitsMap' <- first BalancingError $
    case Map.mapEither id exUnitsMap of
      (failures, exUnitsMap') ->
        handleExUnitsErrors C.ScriptValid failures exUnitsMap' -- TODO: should this take the script validity from csiTxBody?

  let txbodycontent1 = substituteExecutionUnits exUnitsMap' csiTxBody

  -- append output instead of prepending
  txbody1 <- first (BalancingError . C.TxBodyError)
              $ C.makeTransactionBody
              $ txbodycontent1
                  & set L.txFee (C.Lovelace (2^(32 :: Integer) - 1))
                  & over L.txOuts (|> changeOutputLarge)

  let !t_fee = C.evaluateTransactionFee protocolParams txbody1 csiNumWitnesses 0

  txbody2 <- first (BalancingError . C.TxBodyError)
              $ C.makeTransactionBody
              $ txbodycontent1 & set L.txFee t_fee

  let !balance = C.evaluateTransactionBalance protocolParams stakePools csiUtxo txbody2

  mapM_ (`checkMinUTxOValue` protocolParams) $ C.txOuts txbodycontent1

  case balance of
    C.TxOutAdaOnly _ _ -> balanceCheck protocolParams csiChangeAddress balance
    C.TxOutValue _ v   ->
      case C.valueToLovelace v of
        -- FIXME: Support non Ada assets
        Nothing -> Left $ BalancingError $ C.TxBodyErrorNonAdaAssetsUnbalanced v
        Just _  -> balanceCheck protocolParams csiChangeAddress balance

  let finalBodyContent =
        txbodycontent1
          & set L.txFee t_fee
          & over L.txOuts (accountForNoChange (C.TxOut csiChangeAddress balance C.TxOutDatumNone C.ReferenceScriptNone))

  txbody3 <- first (BalancingError . C.TxBodyError) $ C.makeTransactionBody finalBodyContent

  balances <- maybe (Left ComputeBalanceChangeError) Right (balanceChanges csiUtxo finalBodyContent)

  let mkBalancedBody b = C.BalancedTxBody b (C.TxOut csiChangeAddress balance C.TxOutDatumNone C.ReferenceScriptNone) t_fee
  return (mkBalancedBody txbody3, balances)

checkMinUTxOValue
  :: C.TxOut C.CtxTx C.BabbageEra
  -> C.ProtocolParameters
  -> Either BalancingError ()
checkMinUTxOValue txout@(C.TxOut _ v _ _) pparams' = do
  minUTxO  <- first (BalancingError . C.TxBodyErrorMinUTxOMissingPParams)
                $ C.calculateMinimumUTxO C.ShelleyBasedEraBabbage txout pparams'
  if C.txOutValueToLovelace v >= C.selectLovelace minUTxO
  then Right ()
  else Left (CheckMinUtxoValueError txout (C.selectLovelace minUTxO))

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
      bimap BalanceCheckError (const ())
      $ (checkMinUTxOValue (C.TxOut changeaddr balance C.TxOutDatumNone C.ReferenceScriptNone) pparams)

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

{-| Balance the transaction using the wallet's funds, then sign it.
-}
balanceForWallet :: (MonadBlockchain m, MonadFail m) => Wallet -> UtxoState -> TxBodyContent BuildTx ERA -> m (C.Tx ERA, BalanceChanges)
balanceForWallet wallet walletUtxo txb = do
  n <- networkId
  params <- queryProtocolParameters
  pools <- queryStakePools
  let walletAddress = Wallet.addressInEra n wallet
      txb0 = txb & L.txProtocolParams .~ C.BuildTxWith (Just params)
  -- TODO: Better error handling (better than 'fail')
  otherInputs <- lookupTxIns (spentTxIns txb)
  let combinedTxIns =
        let UtxoState w = walletUtxo
            UTxO o = otherInputs
        in UTxO (Map.union w o)
  finalBody <- either (fail . show) pure (addMissingInputs pools params combinedTxIns walletAddress walletUtxo (flip setCollateral walletUtxo $ flip addOwnInput walletUtxo txb0))
  csi <- prepCSInputs walletAddress combinedTxIns finalBody
  start <- querySystemStart
  hist <- queryEraHistory
  first (signForWallet wallet) <$> either (fail . show) pure (balanceTransactionBody start hist params pools csi)

{-| Sign a transaction with the wallet's key
-}
signForWallet :: Wallet -> C.BalancedTxBody ERA -> C.Tx ERA
signForWallet wallet (C.BalancedTxBody txbody _changeOutput _fee) =
  let wit = [C.makeShelleyKeyWitness txbody $ C.WitnessPaymentKey  (Wallet.getWallet wallet)]
  in C.makeSignedTransaction wit txbody

addOwnInput :: TxBodyContent BuildTx ERA -> UtxoState -> TxBodyContent BuildTx ERA
addOwnInput body (Utxos.onlyAda . Utxos.removeUtxos (spentTxIns body) -> UtxoState{_utxos})
  | Map.null _utxos = body
  | not (List.null $ view L.txIns body) = body
  | otherwise =
      spendPublicKeyOutput (fst $ head $ Map.toList _utxos) body

setCollateral :: TxBodyContent BuildTx ERA -> UtxoState -> TxBodyContent BuildTx ERA
setCollateral body (Utxos.onlyAda . Utxos.removeUtxos (spentTxIns body) -> UtxoState{_utxos}) =
  if not (runsScripts body)
    then body -- no script witnesses in inputs.
    else
      case Map.lookupMax _utxos of
        Nothing     -> body -- TODO: Throw error
        Just (k, _) -> addCollateral k body

{-| Whether the transaction runs any plutus scripts
-}
runsScripts :: TxBodyContent BuildTx ERA -> Bool
runsScripts body =
  let scriptIns = body ^.. (L.txIns . traversed . _2 . L._BuildTxWith . L._ScriptWitness)
      minting   = body ^. (L.txMintValue . L._TxMintValue . _2)
  in not (null scriptIns && Map.null minting)

{-| Add inputs to ensure that the balance is strictly positive
-}
addMissingInputs :: Set PoolId -> ProtocolParameters -> C.UTxO ERA -> C.AddressInEra C.BabbageEra -> UtxoState -> TxBodyContent BuildTx ERA -> Either CoinSelectionError (TxBodyContent BuildTx ERA)
addMissingInputs poolIds protocolParameters utxo_ returnAddress walletUtxo txBodyContent = do
  txb <- first BodyError (C.makeTransactionBody txBodyContent)
  let bal = C.evaluateTransactionBalance protocolParameters poolIds utxo_ txb & view L._TxOutValue
      available = Utxos.removeUtxos (spentTxIns txBodyContent) walletUtxo

  (txBodyContent0, additionalBalance) <- addInputsForNonAdaAssets bal walletUtxo txBodyContent

  let bal0 = bal <> additionalBalance
  let (txBodyContent1, C.Lovelace deposit) = addOutputForNonAdaAssets protocolParameters returnAddress bal0 txBodyContent0

      -- minimum positive balance (in lovelace) that must be available to cover
      -- * minimum deposit on the ada-only change output, if required, and
      -- * transaction fee, incl. script fee if required
      -- we set it to rather large value to ensure that we can build a valid transaction.
  let threshold =
        if runsScripts txBodyContent
          then 8_000_000
          else 2_500_000
      C.Lovelace l = C.selectLovelace bal0
      missingLovelace = C.Lovelace (deposit + threshold - l)

  addAdaOnlyInputsFor missingLovelace available txBodyContent1

{-| Select inputs from the wallet's UTXO set to cover the given amount of lovelace.
Will only consider inputs that have no other assets besides Ada.
-}
addAdaOnlyInputsFor :: C.Lovelace -> UtxoState -> TxBodyContent BuildTx ERA -> Either CoinSelectionError (TxBodyContent BuildTx ERA)
addAdaOnlyInputsFor l availableUtxo txBodyContent =
  case Wallet.selectAdaInputsCovering availableUtxo l of
    Nothing -> Left (NotEnoughAdaOnlyOutputsFor l)
    Just (_, ins) -> Right (txBodyContent & over L.txIns (<> fmap spendPubKeyTxIn ins))

{-| Examine the negative part of the transaction balance and select inputs from
the wallet's UTXO set to cover the non-Ada assets required by it. If there are no
non-Ada asset then no inputs will be added.
-}
addInputsForNonAdaAssets ::
  C.Value ->
  UtxoState ->
  TxBodyContent BuildTx ERA ->
  Either CoinSelectionError (TxBodyContent BuildTx ERA, C.Value)
addInputsForNonAdaAssets (fst . splitValue -> negatives) availableUtxo txBodyContent
  | isNothing (C.valueToLovelace $ C.valueFromList negatives) = do
      let nativeAsset (C.AdaAssetId, _) = Nothing
          nativeAsset (C.AssetId p n, C.Quantity q) = Just (p, n, C.Quantity (abs q))
      case Wallet.selectMixedInputsCovering availableUtxo (mapMaybe nativeAsset negatives) of
        Nothing -> Left (NotEnoughMixedOutputsFor (C.valueFromList negatives))
        Just (total, ins) -> Right (txBodyContent & over L.txIns (<> fmap spendPubKeyTxIn ins), total)
  | otherwise = return (txBodyContent, mempty)

{-| Examine the positive part of the transaction balance and add an output for
any non-Ada asset it contains. If the positive part only contains Ada then no
output is added.
-}
addOutputForNonAdaAssets ::
  C.ProtocolParameters -> -- ^ Protocol parameters (for computing the minimum lovelace amount in the output)
  C.AddressInEra C.BabbageEra -> -- ^ Address of the newly created output
  C.Value -> -- ^ The balance of the transaction
  TxBodyContent BuildTx ERA -> -- ^ Transaction body
  (TxBodyContent BuildTx ERA, C.Lovelace) -- ^ The modified transaction body and the lovelace portion of the change output's value. If no output was added then the amount will be 0.
addOutputForNonAdaAssets pparams returnAddress (C.valueFromList . snd . splitValue -> positives) txBodyContent
  | isNothing (C.valueToLovelace positives) =
      let vlWithAda = positives <> C.lovelaceToValue 1_000_000 -- add a dummy ada value to make sure the Ada asset ID is considered in 'C.calculateMinimumUTxO'
          output =
            setMinAdaDeposit pparams
            $ C.TxOut returnAddress (C.TxOutValue C.MultiAssetInBabbageEra vlWithAda) C.TxOutDatumNone C.ReferenceScriptNone
      in (txBodyContent & over L.txOuts (|> output), output ^. L._TxOut . _2 . L._TxOutValue . to C.selectLovelace)
  | otherwise = (txBodyContent, C.Lovelace 0)

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
  => C.AddressInEra ERA -- ^ Change address
  -> C.UTxO ERA -- ^ UTxOs that may be used for balancing
  -> C.TxBodyContent C.BuildTx C.BabbageEra -- ^ Unbalanced transaction body
  -> m CSInputs -- ^ Inputs for coin balancing
prepCSInputs csiChangeAddress csiUtxo csiTxBody =
  CSInputs
    <$> pure csiUtxo
    <*> pure csiTxBody
    <*> pure csiChangeAddress
    <*> fmap (succ . fromIntegral . Set.size) (keyWitnesses csiTxBody)

spentTxIns :: C.TxBodyContent v C.BabbageEra -> Set C.TxIn
spentTxIns (view L.txIns -> inputs) =
  -- TODO: Include collateral etc. fields
  Set.fromList (fst <$> inputs)

lookupTxIns :: MonadBlockchain m => Set C.TxIn -> m (C.UTxO ERA)
lookupTxIns allTxIns = do
  C.UTxO rest <- utxoByTxIn allTxIns
  pure (C.UTxO rest)

keyWitnesses :: MonadBlockchain m => C.TxBodyContent C.BuildTx C.BabbageEra -> m (Set (Keys.KeyHash 'Keys.Payment StandardCrypto))
keyWitnesses (view L.txIns -> inputs) = do
  C.UTxO utxos <- utxoByTxIn (Set.fromList $ fst <$> inputs)
  pure $ Set.fromList $ mapMaybe (publicKeyCredential . snd) $ Map.toList utxos

publicKeyCredential :: C.TxOut C.CtxUTxO C.BabbageEra -> Maybe (Keys.KeyHash 'Keys.Payment StandardCrypto)
publicKeyCredential = preview (L._TxOut . _1 . L._ShelleyAddressInBabbageEra . _2 . L._ShelleyPaymentCredentialByKey)

spendPubKeyTxIn :: C.TxIn -> (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))
-- TODO: consolidate with Convex.BuildTx.spendPublicKeyOutput
spendPubKeyTxIn txIn = (txIn, C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending))
