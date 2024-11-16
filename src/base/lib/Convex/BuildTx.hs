{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
{-| Building transactions
-}
module Convex.BuildTx(
  -- * Tx Builder
  TxBuilder(..),
  liftTxBodyEndo,
  -- ** Looking at transaction inputs
  lookupIndexSpending,
  lookupIndexReference,
  lookupIndexMinted,
  lookupIndexWithdrawal,
  findIndexSpending,
  findIndexReference,
  findIndexMinted,
  findIndexWithdrawal,
  buildTx,
  buildTxWith,
  addBtx,
  -- * Effect
  MonadBuildTx(..),
  BuildTxT(..),
  runBuildTxT,
  runBuildTx,
  execBuildTxT,
  execBuildTx,
  execBuildTx',
  evalBuildTxT,

  -- * Building transactions
  addInputWithTxBody,
  addMintWithTxBody,
  addWithdrawalWithTxBody,
  addReference,
  addCollateral,
  addAuxScript,
  prependTxOut,
  addOutput,
  setScriptsValid,
  addRequiredSignature,

  -- ** Variations of @addInput@
  spendPublicKeyOutput,
  spendPlutus,
  spendPlutusRef,
  spendPlutusRefWithInlineDatum,
  spendPlutusRefWithoutInRef,
  spendPlutusRefWithoutInRefInlineDatum,
  spendPlutusInlineDatum,
  spendSimpleScript,

  -- ** Adding outputs
  payToAddress,
  payToAddressTxOut,
  payToPublicKey,
  payToScriptHash,
  payToScriptDatumHash,
  payToScriptInlineDatum,
  createRefScriptBase,
  createRefScriptNoDatum,
  createRefScriptDatumHash,
  createRefScriptInlineDatum,

  -- ** Staking
  addWithdrawal,
  addScriptWithdrawal,
  addWithdrawZeroPlutusV2InTransaction,
  addWithdrawZeroPlutusV2Reference,
  addCertificate,
  addShelleyStakeCredentialRegistrationCertificatePreConway,
  addShelleyStakeCredentialUnregistrationCertificatePreConway,
  addShelleyStakeCredentialRegistrationCertificateInConway,
  addShelleyStakeCredentialUnregistrationCertificateInConway,
  addConwayStakeCredentialRegistrationCertificate,
  addConwayStakeCredentialDelegationCertificate,
  addConwayStakeCredentialRegistrationAndDelegationCertificate,
  addConwayStakeCredentialUnRegistrationCertificate,
  addStakeWitness,
  addStakeScriptWitness,
  addStakeWitnessWithTxBody,

  -- ** Minting and burning tokens
  mintPlutus,
  mintPlutusRef,
  mintSimpleScriptAssets,

  -- ** Constructing script witness
  buildScriptWitness,
  buildRefScriptWitness,

  -- ** Utilities
  assetValue,

  -- * Minimum Ada deposit
  minAdaDeposit,
  setMinAdaDeposit,
  setMinAdaDepositAll,

  -- * Others
  simpleScriptInShelleyEra,
  mkTxOutValue
  ) where

import qualified Cardano.Api.Ledger             as Ledger
import           Cardano.Api.Shelley            (Hash, HashableScriptData,
                                                 NetworkId, PaymentKey,
                                                 PlutusScript, PlutusScriptV2,
                                                 ScriptHash, WitCtxTxIn)
import qualified Cardano.Api.Shelley            as C
import qualified Cardano.Ledger.Conway.TxCert   as ConwayTxCert (Delegatee (..))
import qualified Cardano.Ledger.Shelley.TxCert  as TxCert
import           Control.Lens                   (_1, _2, at, mapped, over, set,
                                                 view, (&))
import qualified Control.Lens                   as L
import           Control.Monad.Except           (MonadError (..))
import           Control.Monad.Reader.Class     (MonadReader (..))
import qualified Control.Monad.State            as LazyState
import           Control.Monad.State.Class      (MonadState (..))
import qualified Control.Monad.State.Strict     as StrictState
import           Control.Monad.Trans.Class      (MonadTrans (..))
import           Control.Monad.Trans.Except     (ExceptT)
import           Control.Monad.Trans.Writer.CPS (WriterT, execWriterT,
                                                 runWriterT)
import           Control.Monad.Writer.Class     (MonadWriter (..))
import qualified Convex.CardanoApi.Lenses       as L
import           Convex.Class                   (MonadBlockchain (..),
                                                 MonadBlockchainCardanoNodeT,
                                                 MonadDatumQuery (queryDatumFromHash),
                                                 MonadMockchain (..))
import           Convex.Eon                     (IsShelleyToBabbageEra (shelleyToBabbageEra))
import           Convex.MonadLog                (MonadLog (..), MonadLogIgnoreT,
                                                 MonadLogKatipT)
import           Convex.Scripts                 (toHashableScriptData)
import           Convex.Utils                   (inAlonzo, inBabbage, inMary)
import           Data.Foldable                  (traverse_)
import           Data.Functor.Identity          (Identity (..))
import           Data.List                      (nub)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust)
import qualified Data.Set                       as Set
import           GHC.IsList                     (IsList (fromList))
import qualified PlutusLedgerApi.V1             as Plutus

type TxBody era = C.TxBodyContent C.BuildTx era

simpleScriptInShelleyEra :: forall era. C.IsShelleyBasedEra era => C.ScriptLanguageInEra C.SimpleScript' era
simpleScriptInShelleyEra = case C.shelleyBasedEra @era of
  C.ShelleyBasedEraShelley -> C.SimpleScriptInShelley
  C.ShelleyBasedEraAllegra -> C.SimpleScriptInAllegra
  C.ShelleyBasedEraMary    -> C.SimpleScriptInMary
  C.ShelleyBasedEraAlonzo  -> C.SimpleScriptInAlonzo
  C.ShelleyBasedEraBabbage -> C.SimpleScriptInBabbage
  C.ShelleyBasedEraConway  -> C.SimpleScriptInConway

mkTxOutValue :: forall era. C.IsMaryBasedEra era => C.Value -> C.TxOutValue era
mkTxOutValue val =
  C.maryEraOnwardsConstraints @era C.maryBasedEra $ C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toMaryValue val


{-| Look up the index of the @TxIn@ in the list of spending inputs
-}
lookupIndexSpending :: C.TxIn -> TxBody era -> Maybe Int
lookupIndexSpending txi = Map.lookupIndex txi . Map.fromList . (fmap (view L._BuildTxWith) <$>) . view L.txIns

{-| Look up the index of the @TxIn@ in the list of spending inputs. Throws an error if the @TxIn@ is not present.
-}
findIndexSpending :: C.TxIn -> TxBody era -> Int
findIndexSpending txi = fromJust . lookupIndexSpending txi

{-| Look up the index of the @TxIn@ in the list of reference inputs
-}
lookupIndexReference :: C.IsBabbageBasedEra era => C.TxIn -> TxBody era -> Maybe Int
lookupIndexReference txi = Set.lookupIndex txi . Set.fromList . view (L.txInsReference . L._TxInsReferenceIso)

{-| Look up the index of the @TxIn@ in the list of reference inputs. Throws an error if the @TxIn@ is not present.
-}
findIndexReference :: C.IsBabbageBasedEra era => C.TxIn -> TxBody era -> Int
findIndexReference txi = fromJust . lookupIndexReference txi

{-| Look up the index of the @PolicyId@ in the transaction mint.
Note: cardano-api represents a value as a @Map AssetId Quantity@, this is different than the on-chain representation
which is @Map CurrencySymbol (Map TokenName Quantity).
Here, we want to get the index into the on-chain map, but instead index into the cardano-api @Map CurrencySymbol Witness@.
These two indexes should be the same by construction, but it is possible to violate this invariant when building a tx.
-}
lookupIndexMinted :: C.IsMaryBasedEra era => C.PolicyId -> TxBody era -> Maybe Int
lookupIndexMinted policy = Map.lookupIndex policy . view (L.txMintValue . L._TxMintValue .  _2)

{-| Look up the index of the @PolicyId@ in the transaction mint. Throws an error if the @PolicyId@ is not present.
-}
findIndexMinted :: C.IsMaryBasedEra era => C.PolicyId -> TxBody era -> Int
findIndexMinted policy = fromJust . lookupIndexMinted policy

{-| Look up the index of the @StakeAddress@ in the list of withdrawals.
-}
lookupIndexWithdrawal :: C.IsShelleyBasedEra era => C.StakeAddress -> TxBody era -> Maybe Int
lookupIndexWithdrawal stakeAddress = Set.lookupIndex stakeAddress . Set.fromList . fmap (view _1) . view (L.txWithdrawals . L._TxWithdrawals)

{-| Look up the index of the @StakeAddress@ in the list of withdrawals. Throws an error if the @StakeAddress@ is not present.
-}
findIndexWithdrawal :: C.IsShelleyBasedEra era => C.StakeAddress -> TxBody era -> Int
findIndexWithdrawal stakeAddress = fromJust . lookupIndexWithdrawal stakeAddress


{-|
A function that modifies the final @TxBodyContent@, after seeing the @TxBodyContent@ of
the entire finished transaction (lazily).

Note that the result of @unTxBuilder txBody@ must not completely force the @txBody@,
or refer to itself circularly. For example, using this to construct a redeemer that contains the whole
@TransactionInputs@ map is going to loop forever.
-}
newtype TxBuilder era = TxBuilder{ unTxBuilder :: TxBody era -> TxBody era -> TxBody era }

{-| Construct the final @TxBodyContent@
-}
buildTx :: C.IsShelleyBasedEra era => TxBuilder era -> TxBody era
buildTx txb = buildTxWith txb L.emptyTx

-- | The 'TxBuilder' that modifies the tx body without looking at the final result
liftTxBodyEndo :: (TxBody era -> TxBody era) -> TxBuilder era
liftTxBodyEndo f = TxBuilder (const f)

{-| Construct the final @TxBodyContent@ from the provided @TxBodyContent@
-}
buildTxWith :: TxBuilder era -> TxBody era -> TxBody era
buildTxWith TxBuilder{unTxBuilder} initial =
  let result = unTxBuilder result initial
  in result

instance Semigroup (TxBuilder era) where
    -- note that the order here is reversed, compared to @Data.Monoid.Endo@.
    -- This is so that @addBtx a >> addBtx b@ will result in a transaction
    -- where @a@ has been applied before @b@.
  (TxBuilder l) <> (TxBuilder r) = TxBuilder $ \k -> r k . l k

instance Monoid (TxBuilder era) where
  mempty = TxBuilder $ const id

{-| An effect that collects @TxBuilder@ values for building
cardano transactions
-}
class Monad m => MonadBuildTx era m | m -> era where
  -- | Add a @TxBuilder@
  addTxBuilder :: TxBuilder era -> m ()

  default addTxBuilder :: (MonadTrans t, m ~ t n, MonadBuildTx era n) => TxBuilder era -> m ()
  addTxBuilder = lift . addTxBuilder

instance MonadBuildTx era m => MonadBuildTx era (ExceptT e m)
instance MonadBuildTx era m => MonadBuildTx era (StrictState.StateT e m)
instance MonadBuildTx era m => MonadBuildTx era (LazyState.StateT e m)
instance (Monoid w, MonadBuildTx era m) => MonadBuildTx era (WriterT w m)
instance MonadBuildTx era m => MonadBuildTx era (MonadBlockchainCardanoNodeT era m)
instance MonadBuildTx era m => MonadBuildTx era (MonadLogIgnoreT m)
instance MonadBuildTx era m => MonadBuildTx era (MonadLogKatipT m)

addBtx :: MonadBuildTx era m => (TxBody era -> TxBody era) -> m ()
addBtx = addTxBuilder . TxBuilder . const

{-| Monad transformer for the @MonadBuildTx@ effect
-}
newtype BuildTxT era m a = BuildTxT{unBuildTxT :: WriterT (TxBuilder era) m a }
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadState s, MonadError e)

instance MonadTrans (BuildTxT era) where
  lift = BuildTxT . lift

instance Monad m => MonadBuildTx era (BuildTxT era m) where
  addTxBuilder = BuildTxT . tell

instance MonadBlockchain era m => MonadBlockchain era (BuildTxT era m)
instance MonadMockchain era m => MonadMockchain era (BuildTxT era m)

instance MonadDatumQuery m => MonadDatumQuery (BuildTxT era m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance MonadLog m => MonadLog (BuildTxT era m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'
  logDebug' = lift . logDebug'

{-| Run the @BuildTxT@ monad transformer
-}
runBuildTxT :: BuildTxT era m a -> m (a, TxBuilder era)
runBuildTxT = runWriterT . unBuildTxT

{-| Run the @BuildTxT@ monad transformer, returning the @TxBuild@ part only
-}
execBuildTxT :: Monad m => BuildTxT era m a -> m (TxBuilder era)
execBuildTxT = execWriterT . unBuildTxT

{-| Run the @BuildTxT@ monad transformer, returnin only the result
-}
evalBuildTxT :: Monad m => BuildTxT era m a -> m a
evalBuildTxT = fmap fst . runWriterT . unBuildTxT

runBuildTx :: BuildTxT era Identity a -> (a, TxBuilder era)
runBuildTx = runIdentity . runBuildTxT

execBuildTx :: BuildTxT era Identity a -> TxBuilder era
execBuildTx = runIdentity . execBuildTxT

{-| Run the @BuildTx@ action and produce a transaction body
-}
execBuildTx' :: C.IsShelleyBasedEra era => BuildTxT era Identity a -> TxBody era
execBuildTx' = buildTx . runIdentity . execBuildTxT

{-| These functions allow to build the witness for an input/asset/withdrawal
by accessing the final @TxBodyContent@. To avoid an infinite loop when
constructing the witness, make sure that the witness does not depend on
itself. For example: @addInputWithTxBody txi (\body -> find (\in -> in == txi) (txInputs body))@
is going to loop.
-}
addInputWithTxBody :: MonadBuildTx era m => C.TxIn -> (TxBody era -> C.Witness WitCtxTxIn era) -> m ()
addInputWithTxBody txIn f = addTxBuilder (TxBuilder $ \body -> over L.txIns ((txIn, C.BuildTxWith $ f body) :))

addMintWithTxBody :: (MonadBuildTx era m, C.IsMaryBasedEra era) => C.PolicyId -> C.AssetName -> C.Quantity -> (TxBody era -> C.ScriptWitness C.WitCtxMint era) -> m ()
addMintWithTxBody policy assetName quantity f =
  let v = assetValue (C.unPolicyId policy) assetName quantity
  in addTxBuilder (TxBuilder $ \body -> over (L.txMintValue . L._TxMintValue) (over _1 (<> v) . over _2 (Map.insert policy (f body))))

addWithdrawalWithTxBody :: (MonadBuildTx era m, C.IsShelleyBasedEra era) => C.StakeAddress -> C.Quantity -> (TxBody era -> C.Witness C.WitCtxStake era) -> m ()
addWithdrawalWithTxBody address amount f =
  addTxBuilder (TxBuilder $ \body -> over (L.txWithdrawals . L._TxWithdrawals) ((address, C.quantityToLovelace amount, C.BuildTxWith $ f body) :))

{-| Add a stake witness to the transaction.
TODO We should probably remove this as the `addStakeScriptWitness` is more useful.
-}
addStakeWitness ::
  ( MonadBuildTx era m
  , C.IsShelleyBasedEra era
  )
  => C.StakeCredential
  -> C.Witness C.WitCtxStake era
  -> m ()
addStakeWitness credential witness =
  addBtx (over (L.txCertificates . L._TxCertificates . _2) ((:) (credential, witness)))

-- mintPlutus :: forall redeemer lang era m. (Plutus.ToData redeemer, MonadBuildTx era m, C.HasScriptLanguageInEra lang era, C.IsAlonzoBasedEra era, C.IsPlutusScriptLanguage lang) => PlutusScript lang -> redeemer -> C.AssetName -> C.Quantity -> m ()
{-| Add a stake script witness to the transaction.
-}
addStakeScriptWitness ::
  ( MonadBuildTx era m
  , Plutus.ToData redeemer
  , C.IsShelleyBasedEra era
  , C.IsPlutusScriptLanguage lang
  , C.HasScriptLanguageInEra lang era
  )
  => C.StakeCredential
  -> C.PlutusScript lang
  -> redeemer
  -> m ()
addStakeScriptWitness credential script redeemer = do
  let scriptWitness = buildScriptWitness script C.NoScriptDatumForStake redeemer
  let witness = C.ScriptWitness C.ScriptWitnessForStakeAddr scriptWitness
  addBtx (over (L.txCertificates . L._TxCertificates . _2) ((:) (credential, witness)))

{- | Like @addStakeWitness@ but uses a function that takes a @TxBody@ to build the witness.
TODO Give an example of why this is useful. We should just remove it.
-}
addStakeWitnessWithTxBody :: (MonadBuildTx era m, C.IsShelleyBasedEra era) => C.StakeCredential -> (TxBody era -> C.Witness C.WitCtxStake era) -> m ()
addStakeWitnessWithTxBody credential buildWitness =
  addTxBuilder (TxBuilder $ \body -> over (L.txCertificates . L._TxCertificates . _2) ((:) (credential, buildWitness body)))

{-| Spend an output locked by a public key
-}
spendPublicKeyOutput :: MonadBuildTx era m => C.TxIn -> m ()
spendPublicKeyOutput txIn = do
  let wit = C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending)
  addBtx (over L.txIns ((txIn, wit) :))

{-| Utility function to build a script witness
-}
buildScriptWitness :: forall era lang redeemer witctx.
  (Plutus.ToData redeemer, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang) =>
  C.PlutusScript lang ->
  C.ScriptDatum witctx ->
  redeemer ->
  C.ScriptWitness witctx era
buildScriptWitness script datum redeemer =
  C.PlutusScriptWitness
    C.scriptLanguageInEra
    C.plutusScriptVersion
    (C.PScript script)
    datum
    (toHashableScriptData redeemer)
    (C.ExecutionUnits 0 0)

{-| Utility function to build a reference script witness
-}
buildRefScriptWitness ::
  (Plutus.ToData redeemer, C.HasScriptLanguageInEra lang era) =>
  C.TxIn ->
  C.PlutusScriptVersion lang ->
  C.ScriptDatum witctx ->
  redeemer ->
  C.ScriptWitness witctx era
buildRefScriptWitness refTxIn scrVer datum redeemer =
  C.PlutusScriptWitness
    C.scriptLanguageInEra
    scrVer
    (C.PReferenceScript refTxIn Nothing)
    datum
    (toHashableScriptData redeemer)
    (C.ExecutionUnits 0 0)

spendPlutus :: forall datum redeemer era lang m.
  (MonadBuildTx era m, Plutus.ToData datum, Plutus.ToData redeemer, C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> PlutusScript lang -> datum -> redeemer -> m ()
spendPlutus txIn s (toHashableScriptData -> dat) red =
  let wit = C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending $ buildScriptWitness s (C.ScriptDatumForTxIn $ Just dat) red
  in setScriptsValid >> addBtx (over L.txIns ((txIn, wit) :))

{-| Spend an output locked by a Plutus V2 validator with an inline datum
-}
spendPlutusInlineDatum :: forall redeemer lang era m. (MonadBuildTx era m, Plutus.ToData redeemer, C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> PlutusScript lang -> redeemer -> m ()
spendPlutusInlineDatum txIn s red =
  let wit = C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending $ buildScriptWitness s C.InlineScriptDatum red
  in setScriptsValid >> addBtx (over L.txIns ((txIn, wit) :))

{-| Spend an output locked by a Plutus V2 validator using the redeemer provided. The redeemer
can depend on the index of the @TxIn@ in the inputs of the final transaction.
-}
spendPlutusRefBase :: forall redeemer lang era m. (MonadBuildTx era m, Plutus.ToData redeemer, C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra lang era)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> C.ScriptDatum C.WitCtxTxIn -> (Int -> redeemer) -> m ()
spendPlutusRefBase txIn refTxIn scrVer dat red =
  let wit txBody = C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending $ buildRefScriptWitness refTxIn scrVer dat (red $ findIndexSpending txIn txBody)
  in setScriptsValid >> addTxBuilder (TxBuilder $ \body -> over L.txIns ((txIn, wit body) :))

{-| Spend an output locked by a Plutus V2 validator using the redeemer
-}
spendPlutusRefBaseWithInRef :: forall redeemer lang era m. (MonadBuildTx era m, Plutus.ToData redeemer, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra lang era)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> C.ScriptDatum C.WitCtxTxIn -> redeemer -> m ()
spendPlutusRefBaseWithInRef txIn refTxIn scrVer dat red = inBabbage @era $ spendPlutusRefBase txIn refTxIn scrVer dat (const red) >> addReference refTxIn

spendPlutusRef :: forall datum redeemer lang era m. (MonadBuildTx era m, Plutus.ToData datum, Plutus.ToData redeemer, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra lang era)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> datum -> redeemer -> m ()
spendPlutusRef txIn refTxIn scrVer (toHashableScriptData -> dat) = spendPlutusRefBaseWithInRef txIn refTxIn scrVer (C.ScriptDatumForTxIn $ Just dat)

{-| same as spendPlutusV2Ref but considers inline datum at the spent utxo
-}
spendPlutusRefWithInlineDatum :: forall redeemer lang era m.
  (MonadBuildTx era m, Plutus.ToData redeemer, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra lang era)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> redeemer -> m ()
spendPlutusRefWithInlineDatum txIn refTxIn scrVer = spendPlutusRefBaseWithInRef txIn refTxIn scrVer C.InlineScriptDatum

{-| same as spendPlutusV2Ref but does not add the reference script in the reference input list
This is to cover the case whereby the reference script utxo is expected to be consumed in the same tx.
-}
spendPlutusRefWithoutInRef :: forall datum redeemer lang era m.
  (MonadBuildTx era m, Plutus.ToData datum, Plutus.ToData redeemer, C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra lang era)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> datum -> redeemer -> m ()
spendPlutusRefWithoutInRef txIn refTxIn scrVer (toHashableScriptData -> dat) red = spendPlutusRefBase txIn refTxIn scrVer (C.ScriptDatumForTxIn $ Just dat) (const red)

{-| same as spendPlutusV2RefWithoutInRef but considers inline datum at the spent utxo
-}
spendPlutusRefWithoutInRefInlineDatum :: forall redeemer lang era m.
  (MonadBuildTx era m, Plutus.ToData redeemer, C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra lang era)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> redeemer -> m ()
spendPlutusRefWithoutInRefInlineDatum txIn refTxIn scrVer red = spendPlutusRefBase txIn refTxIn scrVer C.InlineScriptDatum (const red)

mintPlutus :: forall redeemer lang era m.
  ( Plutus.ToData redeemer
  , MonadBuildTx era m
  , C.HasScriptLanguageInEra lang era
  , C.IsAlonzoBasedEra era
  , C.IsPlutusScriptLanguage lang
  )
  => PlutusScript lang
  -> redeemer
  -> C.AssetName
  -> C.Quantity
  -> m ()
mintPlutus script red assetName quantity =
  let sh = C.hashScript (C.PlutusScript C.plutusScriptVersion script)
      v = assetValue sh assetName quantity
      policyId = C.PolicyId sh
      wit      = buildScriptWitness @era script C.NoScriptDatumForMint red
  in
    inAlonzo @era $
    setScriptsValid >> addBtx (over (L.txMintValue . L._TxMintValue) (over _1 (<> v) . over _2 (Map.insert policyId wit)))

{-| A value containing the given amount of the native asset
-}
assetValue :: ScriptHash -> C.AssetName -> C.Quantity -> C.Value
assetValue hsh assetName quantity =
  fromList [(C.AssetId (C.PolicyId hsh) assetName, quantity)]

mintPlutusRef :: forall redeemer lang era m.
  ( Plutus.ToData redeemer
  , MonadBuildTx era m
  , C.HasScriptLanguageInEra lang era
  , C.IsBabbageBasedEra era
  )
  => C.TxIn
  -> C.PlutusScriptVersion lang
  -> C.ScriptHash
  -> redeemer
  -> C.AssetName
  -> C.Quantity
  -> m ()
mintPlutusRef refTxIn scrVer sh red assetName quantity = inBabbage @era $
  let v = assetValue sh assetName quantity
      wit = buildRefScriptWitness refTxIn scrVer C.NoScriptDatumForMint red
      policyId = C.PolicyId sh
  in  setScriptsValid
      >> addBtx (over (L.txMintValue . L._TxMintValue) (over _1 (<> v) . over _2 (Map.insert policyId wit)))
      >> addReference refTxIn

mintSimpleScriptAssets :: forall era m. (MonadBuildTx era m, C.IsMaryBasedEra era) => C.SimpleScript -> [(C.AssetName, C.Quantity)] -> m ()
mintSimpleScriptAssets sscript assets = inMary @era $
  let wit = C.SimpleScriptWitness simpleScriptInShelleyEra (C.SScript sscript)
      policyId = C.scriptPolicyId . C.SimpleScript $ sscript
  in traverse_ (\(an,q) -> addMintWithTxBody policyId an q (const wit)) assets

spendSimpleScript :: (MonadBuildTx era m, C.IsShelleyBasedEra era) => C.TxIn -> C.SimpleScript -> m ()
spendSimpleScript txIn sscript =
  let wit = C.SimpleScriptWitness simpleScriptInShelleyEra (C.SScript sscript)
  in addBtx (over L.txIns ((txIn, C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending wit) :))

addCollateral :: (MonadBuildTx era m, C.IsAlonzoBasedEra era) => C.TxIn -> m ()
addCollateral i = addBtx $ over (L.txInsCollateral . L._TxInsCollateralIso) (i :)

addReference :: (MonadBuildTx era m, C.IsBabbageBasedEra era) => C.TxIn -> m ()
addReference i = addBtx $ over (L.txInsReference . L._TxInsReferenceIso) (i :)

addAuxScript :: (MonadBuildTx era m, C.IsAllegraBasedEra era) => C.ScriptInEra era -> m ()
addAuxScript s = addBtx (over (L.txAuxScripts . L._TxAuxScripts) (s :))

payToAddressTxOut :: C.IsMaryBasedEra era => C.AddressInEra era -> C.Value -> C.TxOut C.CtxTx era
payToAddressTxOut addr vl =
  C.TxOut
  addr
  (mkTxOutValue vl)
  C.TxOutDatumNone
  C.ReferenceScriptNone

payToAddress :: (MonadBuildTx era m, C.IsMaryBasedEra era) => C.AddressInEra era -> C.Value -> m ()
payToAddress addr vl = addBtx $ over L.txOuts (payToAddressTxOut addr vl :)

payToPublicKey :: (MonadBuildTx era m, C.IsMaryBasedEra era) => NetworkId -> Hash PaymentKey -> C.Value -> m ()
payToPublicKey network pk vl =
  let val = mkTxOutValue vl
      addr = C.makeShelleyAddressInEra (C.maryEraOnwardsToShelleyBasedEra C.maryBasedEra) network (C.PaymentCredentialByKey pk) C.NoStakeAddress
      txo = C.TxOut addr val C.TxOutDatumNone C.ReferenceScriptNone
  in addOutput txo

payToScriptHash :: forall era m. (MonadBuildTx era m, C.IsAlonzoBasedEra era) => NetworkId -> ScriptHash -> HashableScriptData -> C.StakeAddressReference -> C.Value -> m ()
payToScriptHash network script datum stakeAddress vl = inAlonzo @era $
  let val = mkTxOutValue vl
      addr = C.makeShelleyAddressInEra C.shelleyBasedEra network (C.PaymentCredentialByScript script) stakeAddress
      dat = C.TxOutDatumInTx C.alonzoBasedEra datum
      txo = C.TxOut addr val dat C.ReferenceScriptNone
  in addOutput txo

payToScriptDatumHash :: forall a lang era m.
  (MonadBuildTx era m, Plutus.ToData a, C.IsAlonzoBasedEra era)
  => NetworkId -> C.Script lang -> a -> C.StakeAddressReference -> C.Value -> m ()
payToScriptDatumHash network s datum stakeRef vl =
  let sh = C.hashScript s
      dt = toHashableScriptData datum
  in payToScriptHash network sh dt stakeRef vl

createRefScriptBase :: forall lang era m. (MonadBuildTx era m, C.IsBabbageBasedEra era, C.IsScriptLanguage lang)
  => C.AddressInEra era -> C.Script lang -> C.TxOutDatum C.CtxTx era -> C.Value -> m ()
createRefScriptBase addr script dat vl = inBabbage @era $
  let refScript = C.ReferenceScript C.babbageBasedEra $ C.ScriptInAnyLang C.scriptLanguage script
      txo = C.TxOut addr (mkTxOutValue vl) dat refScript
  in addOutput txo

createRefScriptNoDatum :: (MonadBuildTx era m, C.IsBabbageBasedEra era, C.IsScriptLanguage lang) => C.AddressInEra era -> C.Script lang -> C.Value -> m ()
createRefScriptNoDatum addr script = createRefScriptBase addr script C.TxOutDatumNone

{-| same as createRefScriptBase but also specify an inline datum -}
createRefScriptInlineDatum :: forall a lang era m. (MonadBuildTx era m, Plutus.ToData a, C.IsBabbageBasedEra era, C.IsScriptLanguage lang)
  => C.AddressInEra era -> C.Script lang -> a -> C.Value -> m ()
createRefScriptInlineDatum addr script datum vl =
  let dat = C.TxOutDatumInline C.babbageBasedEra (toHashableScriptData datum)
  in createRefScriptBase addr script dat vl

{-| same as createRefScriptBase but also specify a datum -}
createRefScriptDatumHash :: forall a lang era m. (MonadBuildTx era m, Plutus.ToData a, C.IsBabbageBasedEra era, C.IsScriptLanguage lang)
  => C.AddressInEra era -> C.Script lang -> a -> C.Value -> m ()
createRefScriptDatumHash addr script datum vl =
  let dat = C.TxOutDatumInTx (C.babbageEraOnwardsToAlonzoEraOnwards C.babbageBasedEra) (toHashableScriptData datum)
  in createRefScriptBase addr script dat vl

payToScriptInlineDatum :: forall a era m. (MonadBuildTx era m, Plutus.ToData a, C.IsBabbageBasedEra era) => NetworkId -> C.ScriptHash -> a -> C.StakeAddressReference -> C.Value -> m ()
payToScriptInlineDatum network sh datum stakeRef vl = inBabbage @era $
  let val = mkTxOutValue vl
      addr = C.makeShelleyAddressInEra C.shelleyBasedEra network (C.PaymentCredentialByScript sh) stakeRef
      dat = C.TxOutDatumInline C.babbageBasedEra (toHashableScriptData datum)
      txo = C.TxOut addr val dat C.ReferenceScriptNone
  in addOutput txo
-- TODO: Functions for building outputs (Output -> Output)

setScriptsValid :: C.IsAlonzoBasedEra era => MonadBuildTx era m => m ()
setScriptsValid = addBtx $ set L.txScriptValidity (C.TxScriptValidity C.alonzoBasedEra C.ScriptValid)

{-| Set the Ada component in an output's value to at least the amount needed to cover the
minimum UTxO deposit for this output
-}
setMinAdaDeposit :: C.IsMaryBasedEra era => C.LedgerProtocolParameters era -> C.TxOut C.CtxTx era -> C.TxOut C.CtxTx era
setMinAdaDeposit params txOut =
  let minUtxo = minAdaDeposit params txOut
  in txOut & over (L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId) (maybe (Just minUtxo) (Just . max minUtxo))

{-| Calculate the minimum amount of Ada that must be locked in the given UTxO to
satisfy the ledger's minimum Ada constraint.
-}
minAdaDeposit :: C.IsMaryBasedEra era => C.LedgerProtocolParameters era -> C.TxOut C.CtxTx era -> C.Quantity
minAdaDeposit (C.LedgerProtocolParameters params) txOut =
  let minAdaValue = C.Quantity 3_000_000
      txo = txOut
            -- set the Ada value to a dummy amount to ensure that it is not 0 (if it was 0, the size of the output
            -- would be smaller, causing 'calculateMinimumUTxO' to compute an amount that is a little too small)
            & over (L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId) (maybe (Just minAdaValue) (Just . max minAdaValue))
  in C.lovelaceToQuantity $ C.calculateMinimumUTxO (C.maryEraOnwardsToShelleyBasedEra C.maryBasedEra) txo params

{-| Apply 'setMinAdaDeposit' to all outputs
-}
setMinAdaDepositAll :: (MonadBuildTx era m, C.IsMaryBasedEra era) => C.LedgerProtocolParameters era -> m ()
setMinAdaDepositAll params = addBtx $ over (L.txOuts . mapped) (setMinAdaDeposit params)

{-| Add a public key hash to the list of required signatures.
-}
addRequiredSignature :: (MonadBuildTx era m, C.IsAlonzoBasedEra era) => Hash PaymentKey -> m ()
addRequiredSignature sig =
  addBtx $ over (L.txExtraKeyWits . L._TxExtraKeyWitnesses) (nub . (:) sig)

{-| Add a transaction output to the start of the list of transaction outputs.
-}
prependTxOut :: MonadBuildTx era m => C.TxOut C.CtxTx era -> m ()
prependTxOut txOut = addBtx (over L.txOuts ((:) txOut))

{-| Add a transaction output to the end of the list of transaction outputs.
-}
addOutput :: MonadBuildTx era m => C.TxOut C.CtxTx era -> m ()
addOutput txOut = addBtx (over (L.txOuts . L.reversed) (txOut :))

{-| Add a stake rewards withdrawal to the transaction
-}
addWithdrawal :: (MonadBuildTx era m, C.IsShelleyBasedEra era) => C.StakeAddress -> C.Quantity -> C.Witness C.WitCtxStake era -> m ()
addWithdrawal address amount witness =
  let withdrawal = (address, C.quantityToLovelace amount, C.BuildTxWith witness)
  in addBtx (over (L.txWithdrawals . L._TxWithdrawals) ((:) withdrawal))

{- Like `addWithdrawal` but the stake address is built from the supplied script hash. This is an utility to make withdrawals guarded by
scripts easier to trigger
-}
addScriptWithdrawal :: (MonadBlockchain era m, MonadBuildTx era m, C.IsShelleyBasedEra era) => ScriptHash -> C.Quantity -> C.ScriptWitness C.WitCtxStake era -> m ()
addScriptWithdrawal sh quantity witness = do
  n <- queryNetworkId
  let addr = C.StakeAddress (C.toShelleyNetwork n) $ C.toShelleyStakeCredential $ C.StakeCredentialByScript sh
      wit  = C.ScriptWitness C.ScriptWitnessForStakeAddr witness
  addWithdrawal addr quantity wit

{-| Add a withdrawal of 0 Lovelace from the rewards account locked by the given Plutus V2 script.
Includes the script in the transaction.
-}
addWithdrawZeroPlutusV2InTransaction ::
  ( MonadBlockchain era m
  , MonadBuildTx era m
  , C.HasScriptLanguageInEra PlutusScriptV2 era
  , Plutus.ToData redeemer
  , C.IsShelleyBasedEra era
  )
  => PlutusScript PlutusScriptV2
  -> redeemer
  -> m ()
addWithdrawZeroPlutusV2InTransaction script redeemer = do
  let sh = C.hashScript $ C.PlutusScript C.PlutusScriptV2 script
  addScriptWithdrawal sh 0 $ buildScriptWitness script C.NoScriptDatumForStake redeemer

{-| Add a withdrawal of 0 Lovelace from the rewards account locked by the given Plutus V2 script.
The script is provided as a reference input.
-}
addWithdrawZeroPlutusV2Reference
  :: (MonadBlockchain era m, MonadBuildTx era m, Plutus.ToData redeemer, C.HasScriptLanguageInEra PlutusScriptV2 era, C.IsShelleyBasedEra era)
  => C.TxIn -> ScriptHash -> redeemer -> m ()
addWithdrawZeroPlutusV2Reference refTxIn script redeemer = addScriptWithdrawal script 0 $ buildRefScriptWitness refTxIn C.PlutusScriptV2 C.NoScriptDatumForStake redeemer

{-| Add a certificate (stake delegation, stake pool registration, etc)
to the transaction
-}
addCertificate :: (MonadBuildTx era m, C.IsShelleyBasedEra era) => C.Certificate era -> m ()
addCertificate cert =
  addBtx (over (L.txCertificates . L._TxCertificates . _1) ((:) cert))

{-| Add a 'C.StakeCredential' registration as a ShelleyRelatedCerticate to the transaction in a pre-Conway era.
-}
addShelleyStakeCredentialRegistrationCertificatePreConway :: forall era m.
  ( IsShelleyToBabbageEra era
  , MonadBuildTx era m
  )
  => C.StakeCredential
  -> m ()
addShelleyStakeCredentialRegistrationCertificatePreConway stakeCred = do
  let cert = C.makeStakeAddressRegistrationCertificate $ C.StakeAddrRegistrationPreConway shelleyToBabbageEra stakeCred
  addCertificate cert

{-| Add a 'C.StakeCredential' deregistration as a ShelleyRelatedCerticate to the transaction in a pre-Conway era.
-}
addShelleyStakeCredentialUnregistrationCertificatePreConway :: forall era m.
  ( IsShelleyToBabbageEra era
  , MonadBuildTx era m
  )
  => C.StakeCredential
  -> m ()
addShelleyStakeCredentialUnregistrationCertificatePreConway stakeCred = do
  let cert = C.makeStakeAddressUnregistrationCertificate $ C.StakeAddrRegistrationPreConway shelleyToBabbageEra stakeCred
  addCertificate cert

{-| Add a 'C.StakeCredential' registration as a ShelleyRelatedCerticate to the transaction in Conway era.
-}
addShelleyStakeCredentialRegistrationCertificateInConway :: forall era m.
  ( MonadBuildTx era m
  , C.IsConwayBasedEra era
  )
  => C.StakeCredential
  -> m ()
addShelleyStakeCredentialRegistrationCertificateInConway stakeCred = do
  C.conwayEraOnwardsConstraints @era C.conwayBasedEra $
    addCertificate $ C.ConwayCertificate C.conwayBasedEra $ TxCert.RegTxCert $ C.toShelleyStakeCredential stakeCred

{-| Add a 'C.StakeCredential' deregistration as a ShelleyRelatedCerticate to the transaction in Conway era.
-}
addShelleyStakeCredentialUnregistrationCertificateInConway :: forall era m.
  ( MonadBuildTx era m
  , C.IsConwayBasedEra era
  )
  => C.StakeCredential
  -> m ()
addShelleyStakeCredentialUnregistrationCertificateInConway stakeCred = do
  C.conwayEraOnwardsConstraints @era C.conwayBasedEra $
    addCertificate $ C.ConwayCertificate C.conwayBasedEra $ TxCert.UnRegTxCert $ C.toShelleyStakeCredential stakeCred

{-| Add a 'C.StakeCredential' registration as a ConwayCerticate to the transaction.
-}
addConwayStakeCredentialRegistrationCertificate :: forall era m.
  ( C.IsConwayBasedEra era
  , MonadBuildTx era m
  )
  => C.StakeCredential
  -> Ledger.Coin
  -- ^ Deposit, when present, must match the expected deposit amount specified by `ppKeyDepositL` in the protocol parameters.
  -> m ()
addConwayStakeCredentialRegistrationCertificate stakeCred deposit = do
  addCertificate $ C.makeStakeAddressRegistrationCertificate $ C.StakeAddrRegistrationConway C.conwayBasedEra deposit stakeCred

{-| Delegate to some delegatee in a ConwayCerticate to the transaction.
-}
addConwayStakeCredentialDelegationCertificate :: forall era m.
  ( C.IsConwayBasedEra era
  , MonadBuildTx era m
  )
  => C.StakeCredential
  -> ConwayTxCert.Delegatee (Ledger.EraCrypto (C.ShelleyLedgerEra era))
  -> m ()
addConwayStakeCredentialDelegationCertificate stakeCred delegatee = do
  let cert =
        C.makeStakeAddressDelegationCertificate $
          C.StakeDelegationRequirementsConwayOnwards C.conwayBasedEra stakeCred delegatee
  addCertificate cert

{-| Register a 'C.StakeCredential' and delegate to some delegatee in a single ConwayCerticate to the transaction.
-}
addConwayStakeCredentialRegistrationAndDelegationCertificate :: forall era m.
  ( C.IsConwayBasedEra era
  , MonadBuildTx era m
  )
  => C.StakeCredential
  -> ConwayTxCert.Delegatee (Ledger.EraCrypto (C.ShelleyLedgerEra era))
  -> Ledger.Coin
  -- Deposit is required and must match the expected deposit amount specified by `ppKeyDepositL` in the protocol parameters.
  -> m ()
addConwayStakeCredentialRegistrationAndDelegationCertificate stakeCred delegatee deposit = do
  let cert = C.makeStakeAddressAndDRepDelegationCertificate C.conwayBasedEra stakeCred delegatee deposit
  addCertificate cert

{-| Add a 'C.StakeCredential' as a ConwayEra and onwards deregistration certificate to the transaction.
-}
addConwayStakeCredentialUnRegistrationCertificate :: forall era m.
  ( C.IsConwayBasedEra era
  , MonadBuildTx era m
  )
  => C.StakeCredential
  -> Ledger.Coin
  -- ^ Deposit, if present, must match the amount that was left as a deposit upon stake credential registration.
  -> m ()
addConwayStakeCredentialUnRegistrationCertificate stakeCred deposit = do
  let cert = C.makeStakeAddressUnregistrationCertificate $ C.StakeAddrRegistrationConway C.conwayBasedEra deposit stakeCred
  addCertificate cert

