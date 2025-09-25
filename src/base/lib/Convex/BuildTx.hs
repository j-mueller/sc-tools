{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Building transactions
module Convex.BuildTx (
  -- * Tx Builder
  TxBuilder (..),
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
  MonadBuildTx (..),
  BuildTxT (..),
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
  spendPlutusWithRedeemerFn,
  spendPlutusRef,
  spendPlutusRefWithRedeemerFn,
  spendPlutusRefBase,
  spendPlutusRefBaseWithRedeemerFn,
  spendPlutusRefBaseWithInRef,
  spendPlutusRefBaseWithInRefWithRedeemerFn,
  spendPlutusRefWithInlineDatum,
  spendPlutusRefWithInlineDatumWithRedeemerFn,
  spendPlutusInlineDatum,
  spendPlutusInlineDatumWithRedeemerFn,
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
  addScriptWithdrawalWithRedeemerFn,
  addWithdrawZeroPlutusV2InTransaction,
  addWithdrawZeroPlutusV2Reference,
  addCertificate,
  mkConwayStakeCredentialRegistrationCertificate,
  mkConwayStakeCredentialDelegationCertificate,
  mkConwayStakeCredentialRegistrationAndDelegationCertificate,
  mkConwayStakeCredentialUnRegistrationCertificate,
  addStakeScriptWitness,
  addStakeScriptWitnessWithRedeemerFn,
  addStakeScriptWitnessRef,
  addStakeScriptWitnessRefWithRedeemerFn,
  addStakeWitnessWithTxBody,

  -- ** Minting and burning tokens
  mintPlutus,
  mintPlutusWithRedeemerFn,
  mintPlutusRef,
  mintPlutusRefWithRedeemerFn,
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
  mkTxOutValue,
) where

import Cardano.Api (
  Hash,
  HashableScriptData,
  NetworkId,
  PaymentKey,
  PlutusScript,
  PlutusScriptV2,
  ScriptHash,
  WitCtxTxIn,
 )
import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Conway.TxCert qualified as ConwayTxCert (Delegatee (..))
import Control.Lens (
  at,
  mapped,
  over,
  set,
  view,
  (&),
  _1,
  _2,
 )
import Control.Lens qualified as L
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State qualified as LazyState
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.State.Strict qualified as StrictState
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Writer.CPS (
  WriterT,
  execWriterT,
  runWriterT,
 )
import Control.Monad.Writer.Class (MonadWriter (..))
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (
  MonadBlockchain (..),
  MonadBlockchainCardanoNodeT,
  MonadDatumQuery (queryDatumFromHash),
  MonadMockchain (..),
 )
import Convex.MonadLog (
  MonadLog (..),
  MonadLogIgnoreT,
  MonadLogKatipT,
 )
import Convex.Scripts (toHashableScriptData)
import Convex.Utils (inAlonzo, inBabbage, inMary)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity (..))
import Data.List (nub)
import Data.Map qualified as Map
import Data.Map.Ordered.Strict qualified as OMap
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import GHC.IsList (IsList (fromList))
import PlutusLedgerApi.V1 qualified as Plutus

type TxBody era = C.TxBodyContent C.BuildTx era

simpleScriptInShelleyEra :: forall era. (C.IsShelleyBasedEra era) => C.ScriptLanguageInEra C.SimpleScript' era
simpleScriptInShelleyEra = case C.shelleyBasedEra @era of
  C.ShelleyBasedEraShelley -> C.SimpleScriptInShelley
  C.ShelleyBasedEraAllegra -> C.SimpleScriptInAllegra
  C.ShelleyBasedEraMary -> C.SimpleScriptInMary
  C.ShelleyBasedEraAlonzo -> C.SimpleScriptInAlonzo
  C.ShelleyBasedEraBabbage -> C.SimpleScriptInBabbage
  C.ShelleyBasedEraConway -> C.SimpleScriptInConway

mkTxOutValue :: forall era. (C.IsMaryBasedEra era) => C.Value -> C.TxOutValue era
mkTxOutValue val =
  C.maryEraOnwardsConstraints @era C.maryBasedEra $ C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toMaryValue val

-- | Look up the index of the @TxIn@ in the list of spending inputs
lookupIndexSpending :: C.TxIn -> TxBody era -> Maybe Int
lookupIndexSpending txi = Map.lookupIndex txi . Map.fromList . (fmap (view L._BuildTxWith) <$>) . view L.txIns

-- | Look up the index of the @TxIn@ in the list of spending inputs. Throws an error if the @TxIn@ is not present.
findIndexSpending :: C.TxIn -> TxBody era -> Int
findIndexSpending txi = fromJust . lookupIndexSpending txi

-- | Look up the index of the @TxIn@ in the list of reference inputs
lookupIndexReference :: (C.IsBabbageBasedEra era) => C.TxIn -> TxBody era -> Maybe Int
lookupIndexReference txi = Set.lookupIndex txi . Set.fromList . fst . view (L.txInsReference . L._TxInsReferenceIso)

-- | Look up the index of the @TxIn@ in the list of reference inputs. Throws an error if the @TxIn@ is not present.
findIndexReference :: (C.IsBabbageBasedEra era) => C.TxIn -> TxBody era -> Int
findIndexReference txi = fromJust . lookupIndexReference txi

{- | Look up the index of the @PolicyId@ in the transaction mint.
Note: cardano-api represents a value as a @Map AssetId Quantity@, this is different than the on-chain representation
which is @Map CurrencySymbol (Map TokenName Quantity).
Here, we want to get the index into the on-chain map, but instead index into the cardano-api @Map CurrencySymbol Witness@.
These two indexes should be the same by construction, but it is possible to violate this invariant when building a tx.
-}
lookupIndexMinted :: (C.IsMaryBasedEra era) => C.PolicyId -> TxBody era -> Maybe Int
lookupIndexMinted policy = Map.lookupIndex policy . view (L.txMintValue . L._TxMintValue)

-- | Look up the index of the @PolicyId@ in the transaction mint. Throws an error if the @PolicyId@ is not present.
findIndexMinted :: (C.IsMaryBasedEra era) => C.PolicyId -> TxBody era -> Int
findIndexMinted policy = fromJust . lookupIndexMinted policy

-- | Look up the index of the @StakeAddress@ in the list of withdrawals.
lookupIndexWithdrawal :: (C.IsShelleyBasedEra era) => C.StakeAddress -> TxBody era -> Maybe Int
lookupIndexWithdrawal stakeAddress = Set.lookupIndex stakeAddress . Set.fromList . fmap (view _1) . view (L.txWithdrawals . L._TxWithdrawals)

-- | Look up the index of the @StakeAddress@ in the list of withdrawals. Throws an error if the @StakeAddress@ is not present.
findIndexWithdrawal :: (C.IsShelleyBasedEra era) => C.StakeAddress -> TxBody era -> Int
findIndexWithdrawal stakeAddress = fromJust . lookupIndexWithdrawal stakeAddress

{- |
A function that modifies the final @TxBodyContent@, after seeing the @TxBodyContent@ of
the entire finished transaction (lazily).

Note that the result of @unTxBuilder txBody@ must not completely force the @txBody@,
or refer to itself circularly. For example, using this to construct a redeemer that contains the whole
@TransactionInputs@ map is going to loop forever.
-}
newtype TxBuilder era = TxBuilder {unTxBuilder :: TxBody era -> TxBody era -> TxBody era}

-- | Construct the final @TxBodyContent@
buildTx :: (C.IsShelleyBasedEra era) => TxBuilder era -> TxBody era
buildTx txb = buildTxWith txb L.emptyTx

-- | The 'TxBuilder' that modifies the tx body without looking at the final result
liftTxBodyEndo :: (TxBody era -> TxBody era) -> TxBuilder era
liftTxBodyEndo f = TxBuilder (const f)

-- | Construct the final @TxBodyContent@ from the provided @TxBodyContent@
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

{- | An effect that collects @TxBuilder@ values for building
cardano transactions
-}
class (Monad m) => MonadBuildTx era m | m -> era where
  -- | Add a @TxBuilder@
  addTxBuilder :: TxBuilder era -> m ()
  default addTxBuilder :: (MonadTrans t, m ~ t n, MonadBuildTx era n) => TxBuilder era -> m ()
  addTxBuilder = lift . addTxBuilder

instance (MonadBuildTx era m) => MonadBuildTx era (ExceptT e m)
instance (MonadBuildTx era m) => MonadBuildTx era (ReaderT e m)
instance (MonadBuildTx era m) => MonadBuildTx era (StrictState.StateT e m)
instance (MonadBuildTx era m) => MonadBuildTx era (LazyState.StateT e m)
instance (Monoid w, MonadBuildTx era m) => MonadBuildTx era (WriterT w m)
instance (MonadBuildTx era m) => MonadBuildTx era (MonadBlockchainCardanoNodeT era m)
instance (MonadBuildTx era m) => MonadBuildTx era (MonadLogIgnoreT m)
instance (MonadBuildTx era m) => MonadBuildTx era (MonadLogKatipT m)

addBtx :: (MonadBuildTx era m) => (TxBody era -> TxBody era) -> m ()
addBtx = addTxBuilder . TxBuilder . const

-- | Monad transformer for the @MonadBuildTx@ effect
newtype BuildTxT era m a = BuildTxT {unBuildTxT :: WriterT (TxBuilder era) m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadState s, MonadError e)

instance MonadTrans (BuildTxT era) where
  lift = BuildTxT . lift

instance (Monad m) => MonadBuildTx era (BuildTxT era m) where
  addTxBuilder = BuildTxT . tell

instance (MonadBlockchain era m) => MonadBlockchain era (BuildTxT era m)
instance (MonadMockchain era m) => MonadMockchain era (BuildTxT era m)

instance (MonadDatumQuery m) => MonadDatumQuery (BuildTxT era m) where
  queryDatumFromHash = lift . queryDatumFromHash

instance (MonadLog m) => MonadLog (BuildTxT era m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'
  logDebug' = lift . logDebug'

-- | Run the @BuildTxT@ monad transformer
runBuildTxT :: BuildTxT era m a -> m (a, TxBuilder era)
runBuildTxT = runWriterT . unBuildTxT

-- | Run the @BuildTxT@ monad transformer, returning the @TxBuild@ part only
execBuildTxT :: (Monad m) => BuildTxT era m a -> m (TxBuilder era)
execBuildTxT = execWriterT . unBuildTxT

-- | Run the @BuildTxT@ monad transformer, returnin only the result
evalBuildTxT :: (Monad m) => BuildTxT era m a -> m a
evalBuildTxT = fmap fst . runWriterT . unBuildTxT

runBuildTx :: BuildTxT era Identity a -> (a, TxBuilder era)
runBuildTx = runIdentity . runBuildTxT

execBuildTx :: BuildTxT era Identity a -> TxBuilder era
execBuildTx = runIdentity . execBuildTxT

-- | Run the @BuildTx@ action and produce a transaction body
execBuildTx' :: (C.IsShelleyBasedEra era) => BuildTxT era Identity a -> TxBody era
execBuildTx' = buildTx . runIdentity . execBuildTxT

{- | These functions allow to build the witness for an input/asset/withdrawal
by accessing the final @TxBodyContent@. To avoid an infinite loop when
constructing the witness, make sure that the witness does not depend on
itself. For example: @addInputWithTxBody txi (\body -> find (\in -> in == txi) (txInputs body))@
is going to loop.
-}
addInputWithTxBody :: (MonadBuildTx era m) => C.TxIn -> (TxBody era -> C.Witness WitCtxTxIn era) -> m ()
addInputWithTxBody txIn f = addTxBuilder (TxBuilder $ \body -> over L.txIns ((txIn, C.BuildTxWith $ f body) :))

mintTxBodyL
  :: (Functor f, C.IsMaryBasedEra era)
  => C.PolicyId
  -> C.AssetName
  -> C.BuildTxWith C.BuildTx (C.ScriptWitness C.WitCtxMint era)
  -> (C.Quantity -> f C.Quantity)
  -> TxBody era
  -> f (TxBody era)
mintTxBodyL policy assetName wit =
  -- NOTE: witness is not overriden everytime we call mintTxBodyL. Should we override or keep the first?
  L.txMintValue . L._TxMintValue . at policy . L.anon (mempty, wit) (Map.null . fst) . _1 . at assetName . L.non 0

addMintWithTxBody :: (MonadBuildTx era m, C.IsMaryBasedEra era) => C.PolicyId -> C.AssetName -> C.Quantity -> (TxBody era -> C.ScriptWitness C.WitCtxMint era) -> m ()
addMintWithTxBody policy assetName quantity f =
  let wit body = C.BuildTxWith $ f body
   in addTxBuilder
        ( TxBuilder $ \body ->
            over
              (mintTxBodyL policy assetName (wit body))
              (+ quantity)
        )

addWithdrawalWithTxBody :: (MonadBuildTx era m, C.IsShelleyBasedEra era) => C.StakeAddress -> C.Quantity -> (TxBody era -> C.Witness C.WitCtxStake era) -> m ()
addWithdrawalWithTxBody address amount f =
  addTxBuilder (TxBuilder $ \body -> over (L.txWithdrawals . L._TxWithdrawals) ((address, C.quantityToLovelace amount, C.BuildTxWith $ f body) :))

{- | Like 'addStakeScriptWitness', but allows specifying a redeemer
based on the 'C.TxBodyContent'.

Warning: The redeemer function receives the current transaction body
to compute the redeemer. This can create infinite loops if the
redeemer depends on parts of the transaction that are modified by
adding this witness itself.
-}
addStakeScriptWitnessWithRedeemerFn
  :: forall era lang m redeemer
   . ( MonadBuildTx era m
     , Plutus.ToData redeemer
     , C.IsShelleyBasedEra era
     , C.IsPlutusScriptLanguage lang
     , C.HasScriptLanguageInEra lang era
     )
  => C.Certificate era
  -> C.StakeCredential
  -> C.PlutusScript lang
  -> (C.TxBodyContent C.BuildTx era -> redeemer)
  -> m ()
addStakeScriptWitnessWithRedeemerFn certificate credential script redFn = do
  let scriptWitness txBody = buildScriptWitness script C.NoScriptDatumForStake (redFn txBody)
      witness txBody = C.ScriptWitness C.ScriptWitnessForStakeAddr $ scriptWitness txBody
  addBtx (\body -> over (L.txCertificates . L._TxCertificates) (OMap.>| (certificate, C.BuildTxWith (Just (credential, witness body)))) body)

-- | Add a stake script witness to the transaction.
addStakeScriptWitness
  :: forall era lang m redeemer
   . ( MonadBuildTx era m
     , Plutus.ToData redeemer
     , C.IsShelleyBasedEra era
     , C.IsPlutusScriptLanguage lang
     , C.HasScriptLanguageInEra lang era
     )
  => C.Certificate era
  -> C.StakeCredential
  -> C.PlutusScript lang
  -> redeemer
  -> m ()
addStakeScriptWitness certificate credential script red = addStakeScriptWitnessWithRedeemerFn certificate credential script (const red)

{- | Like 'addStakeScriptWitnessRef', but allows specifying a redeemer
based on the 'C.TxBodyContent'.

Warning: The redeemer function receives the current transaction body
to compute the redeemer. This can create infinite loops if the
redeemer depends on parts of the transaction that are modified by
adding this witness itself.
-}
addStakeScriptWitnessRefWithRedeemerFn
  :: forall era lang redeemer m
   . ( MonadBuildTx era m
     , Plutus.ToData redeemer
     , C.IsShelleyBasedEra era
     , C.HasScriptLanguageInEra lang era
     , C.IsPlutusScriptLanguage lang
     )
  => C.Certificate era
  -> C.StakeCredential
  -> C.TxIn
  -> C.PlutusScriptVersion lang
  -> (C.TxBodyContent C.BuildTx era -> redeemer)
  -> m ()
addStakeScriptWitnessRefWithRedeemerFn certificate credential txIn plutusScriptVersion redFn = do
  let scriptWitness txBody = buildRefScriptWitness @era txIn plutusScriptVersion C.NoScriptDatumForStake (redFn txBody)
      witness txBody = C.ScriptWitness C.ScriptWitnessForStakeAddr $ scriptWitness txBody
  addBtx (\body -> over (L.txCertificates . L._TxCertificates) (OMap.>| (certificate, C.BuildTxWith (Just (credential, witness body)))) body)

-- | Add a stake script reference witness to the transaction.
addStakeScriptWitnessRef
  :: forall era lang redeemer m
   . ( MonadBuildTx era m
     , Plutus.ToData redeemer
     , C.IsShelleyBasedEra era
     , C.HasScriptLanguageInEra lang era
     , C.IsPlutusScriptLanguage lang
     )
  => C.Certificate era
  -> C.StakeCredential
  -> C.TxIn
  -> C.PlutusScriptVersion lang
  -> redeemer
  -> m ()
addStakeScriptWitnessRef certificate credential txIn plutusScriptVersion red = addStakeScriptWitnessRefWithRedeemerFn certificate credential txIn plutusScriptVersion (const red)

{- | Like @addStakeWitness@ but uses a function that takes a @TxBody@ to build the witness.
TODO Give an example of why this is useful. We should just remove it.
-}
addStakeWitnessWithTxBody :: (MonadBuildTx era m, C.IsShelleyBasedEra era) => C.Certificate era -> C.StakeCredential -> (TxBody era -> C.Witness C.WitCtxStake era) -> m ()
addStakeWitnessWithTxBody certificate credential buildWitness =
  addTxBuilder (TxBuilder $ \body -> over (L.txCertificates . L._TxCertificates) (OMap.>| (certificate, C.BuildTxWith (Just (credential, buildWitness body)))))

-- | Spend an output locked by a public key
spendPublicKeyOutput :: (MonadBuildTx era m) => C.TxIn -> m ()
spendPublicKeyOutput txIn = do
  let wit = C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending)
  addBtx (over L.txIns ((txIn, wit) :))

-- | Utility function to build a script witness
buildScriptWitness
  :: forall era lang redeemer witctx
   . (Plutus.ToData redeemer, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.PlutusScript lang
  -> C.ScriptDatum witctx
  -> redeemer
  -> C.ScriptWitness witctx era
buildScriptWitness script datum redeemer =
  C.PlutusScriptWitness
    C.scriptLanguageInEra
    C.plutusScriptVersion
    (C.PScript script)
    datum
    (toHashableScriptData redeemer)
    (C.ExecutionUnits 0 0)

-- | Utility function to build a reference script witness
buildRefScriptWitness
  :: forall era lang redeemer witctx
   . (Plutus.ToData redeemer, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn
  -> C.PlutusScriptVersion lang
  -> C.ScriptDatum witctx
  -> redeemer
  -> C.ScriptWitness witctx era
buildRefScriptWitness refTxIn scrVer datum redeemer =
  C.PlutusScriptWitness
    C.scriptLanguageInEra
    scrVer
    (C.PReferenceScript refTxIn)
    datum
    (toHashableScriptData redeemer)
    (C.ExecutionUnits 0 0)

{- | Like 'spendPlutus', but allows specifying a redeemer
based on the 'C.TxBodyContent'.

Warning: The redeemer function receives the current transaction body
to compute the redeemer. This can create infinite loops if the
redeemer depends on parts of the transaction that are modified by
spending this input.
-}
spendPlutusWithRedeemerFn
  :: forall datum redeemer era lang m
   . (MonadBuildTx era m, Plutus.ToData datum, Plutus.ToData redeemer, C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> PlutusScript lang -> datum -> (C.TxBodyContent C.BuildTx era -> redeemer) -> m ()
spendPlutusWithRedeemerFn txIn s (toHashableScriptData -> dat) redFn =
  let wit txBody = C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending $ buildScriptWitness s (C.ScriptDatumForTxIn $ Just dat) (redFn txBody)
   in setScriptsValid >> addBtx (\body -> over L.txIns ((txIn, wit body) :) body)

-- | Spend an output locked by a Plutus V2 validator
spendPlutus
  :: forall datum redeemer era lang m
   . (MonadBuildTx era m, Plutus.ToData datum, Plutus.ToData redeemer, C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> PlutusScript lang -> datum -> redeemer -> m ()
spendPlutus txIn s dat red = spendPlutusWithRedeemerFn txIn s dat (const red)

{- | Like 'spendPlutusInlineDatum', but allows specifying a redeemer
based on the 'C.TxBodyContent'.

Warning: The redeemer function receives the current transaction body
to compute the redeemer. This can create infinite loops if the
redeemer depends on parts of the transaction that are modified by
spending this input
-}
spendPlutusInlineDatumWithRedeemerFn
  :: forall redeemer era lang m
   . (MonadBuildTx era m, Plutus.ToData redeemer, C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> PlutusScript lang -> (C.TxBodyContent C.BuildTx era -> redeemer) -> m ()
spendPlutusInlineDatumWithRedeemerFn txIn s redFn =
  let wit txBody = C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending $ buildScriptWitness s C.InlineScriptDatum (redFn txBody)
   in setScriptsValid >> addBtx (\body -> over L.txIns ((txIn, wit body) :) body)

-- | Spend an output locked by a Plutus V2 validator with an inline datum
spendPlutusInlineDatum
  :: forall redeemer era lang m
   . (MonadBuildTx era m, Plutus.ToData redeemer, C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> PlutusScript lang -> redeemer -> m ()
spendPlutusInlineDatum txIn s red = spendPlutusInlineDatumWithRedeemerFn txIn s (const red)

{- | Like 'spendPlutusRefBase', but allows specifying a redeemer
based on the 'C.TxBodyContent'.

Warning: The redeemer function receives the current transaction body
to compute the redeemer. This can create infinite loops if the
redeemer depends on parts of the transaction that are modified by
spending this input.
-}
spendPlutusRefBaseWithRedeemerFn
  :: forall redeemer era lang m
   . (MonadBuildTx era m, Plutus.ToData redeemer, C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> C.ScriptDatum C.WitCtxTxIn -> (C.TxBodyContent C.BuildTx era -> redeemer) -> m ()
spendPlutusRefBaseWithRedeemerFn txIn refTxIn scrVer dat redFn =
  let wit txBody = C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending $ buildRefScriptWitness refTxIn scrVer dat (redFn txBody)
   in setScriptsValid >> addTxBuilder (TxBuilder $ \body -> over L.txIns ((txIn, wit body) :))

-- | Spend an output locked by a Plutus V2 validator using the redeemer provided.
spendPlutusRefBase
  :: forall redeemer era lang m
   . (MonadBuildTx era m, Plutus.ToData redeemer, C.IsAlonzoBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> C.ScriptDatum WitCtxTxIn -> redeemer -> m ()
spendPlutusRefBase txIn refTxIn scrVer dat red = spendPlutusRefBaseWithRedeemerFn txIn refTxIn scrVer dat (const red)

{- | Like 'spendPlutusRefBaseWithInRef', but allows specifying a redeemer
based on the 'C.TxBodyContent'.

Warning: The redeemer function receives the current transaction body
to compute the redeemer. This can create infinite loops if the
redeemer depends on parts of the transaction that are modified by
spending this input.
-}
spendPlutusRefBaseWithInRefWithRedeemerFn
  :: forall redeemer era lang m
   . (MonadBuildTx era m, Plutus.ToData redeemer, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> C.ScriptDatum C.WitCtxTxIn -> (C.TxBodyContent C.BuildTx era -> redeemer) -> m ()
spendPlutusRefBaseWithInRefWithRedeemerFn txIn refTxIn scrVer dat redFn = inBabbage @era $ spendPlutusRefBaseWithRedeemerFn txIn refTxIn scrVer dat redFn >> addReference refTxIn

-- | Spend an output locked by a Plutus V2 validator using the redeemer
spendPlutusRefBaseWithInRef
  :: forall redeemer era lang m
   . (MonadBuildTx era m, Plutus.ToData redeemer, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> C.ScriptDatum WitCtxTxIn -> redeemer -> m ()
spendPlutusRefBaseWithInRef txIn refTxIn scrVer dat red = spendPlutusRefBaseWithInRefWithRedeemerFn txIn refTxIn scrVer dat (const red)

{- | Like 'spendPlutusRef', but allows specifying a redeemer
based on the 'C.TxBodyContent'.

Warning: The redeemer function receives the current transaction body
to compute the redeemer. This can create infinite loops if the
redeemer depends on parts of the transaction that are modified by
spending this input.
-}
spendPlutusRefWithRedeemerFn
  :: forall datum redeemer era lang m
   . (MonadBuildTx era m, Plutus.ToData datum, Plutus.ToData redeemer, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> datum -> (C.TxBodyContent C.BuildTx era -> redeemer) -> m ()
spendPlutusRefWithRedeemerFn txIn refTxIn scrVer (toHashableScriptData -> dat) = spendPlutusRefBaseWithInRefWithRedeemerFn txIn refTxIn scrVer (C.ScriptDatumForTxIn $ Just dat)

spendPlutusRef
  :: forall datum redeemer era lang m
   . (MonadBuildTx era m, Plutus.ToData datum, Plutus.ToData redeemer, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> datum -> redeemer -> m ()
spendPlutusRef txIn refTxIn scrVer dat red = spendPlutusRefWithRedeemerFn txIn refTxIn scrVer dat (const red)

{- | Like 'spendPlutusRefWithInlineDatumWithRedeemerFn', but allows specifying a redeemer
based on the 'C.TxBodyContent'.

Warning: The redeemer function receives the current transaction body
to compute the redeemer. This can create infinite loops if the
redeemer depends on parts of the transaction that are modified by
spending this input.
-}
spendPlutusRefWithInlineDatumWithRedeemerFn
  :: forall redeemer era lang m
   . (MonadBuildTx era m, Plutus.ToData redeemer, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> (C.TxBodyContent C.BuildTx era -> redeemer) -> m ()
spendPlutusRefWithInlineDatumWithRedeemerFn txIn refTxIn scrVer = spendPlutusRefBaseWithInRefWithRedeemerFn txIn refTxIn scrVer C.InlineScriptDatum

-- | same as 'spendPlutusRef' but considers inline datum at the spent utxo
spendPlutusRefWithInlineDatum
  :: forall redeemer era lang m
   . (MonadBuildTx era m, Plutus.ToData redeemer, C.IsBabbageBasedEra era, C.HasScriptLanguageInEra lang era, C.IsPlutusScriptLanguage lang)
  => C.TxIn -> C.TxIn -> C.PlutusScriptVersion lang -> redeemer -> m ()
spendPlutusRefWithInlineDatum txIn refTxIn scrVer red = spendPlutusRefWithInlineDatumWithRedeemerFn txIn refTxIn scrVer (const red)

{- | Like 'mintPlutus', but allows specifying a redeemer
based on the 'C.TxBodyContent'.

Warning: The redeemer function receives the current transaction body
to compute the redeemer. This can create infinite loops if the
redeemer depends on parts of the transaction that are modified by
this minting.
-}
mintPlutusWithRedeemerFn
  :: forall redeemer era lang m
   . ( Plutus.ToData redeemer
     , MonadBuildTx era m
     , C.HasScriptLanguageInEra lang era
     , C.IsAlonzoBasedEra era
     , C.IsPlutusScriptLanguage lang
     )
  => PlutusScript lang
  -> (C.TxBodyContent C.BuildTx era -> redeemer)
  -> C.AssetName
  -> C.Quantity
  -> m ()
mintPlutusWithRedeemerFn script redFn assetName quantity =
  let sh = C.hashScript (C.PlutusScript C.plutusScriptVersion script)
      policyId = C.PolicyId sh
      wit txBody = C.BuildTxWith $ buildScriptWitness @era script C.NoScriptDatumForMint (redFn txBody)
   in inAlonzo @era $
        setScriptsValid
          >> addBtx
            ( over
                (\qFn body -> mintTxBodyL policyId assetName (wit body) qFn body)
                (+ quantity)
            )

mintPlutus
  :: forall redeemer era lang m
   . ( Plutus.ToData redeemer
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
mintPlutus script red = mintPlutusWithRedeemerFn script (const red)

-- | A value containing the given amount of the native asset
assetValue :: ScriptHash -> C.AssetName -> C.Quantity -> C.Value
assetValue hsh assetName quantity =
  fromList [(C.AssetId (C.PolicyId hsh) assetName, quantity)]

{- | Like 'mintPlutusRef', but allows specifying a redeemer
based on the 'C.TxBodyContent'.

Warning: The redeemer function receives the current transaction body
to compute the redeemer. This can create infinite loops if the
redeemer depends on parts of the transaction that are modified by
this minting.
-}
mintPlutusRefWithRedeemerFn
  :: forall redeemer era lang m
   . ( Plutus.ToData redeemer
     , MonadBuildTx era m
     , C.HasScriptLanguageInEra lang era
     , C.IsBabbageBasedEra era
     , C.IsPlutusScriptLanguage lang
     )
  => C.TxIn
  -> C.PlutusScriptVersion lang
  -> C.ScriptHash
  -> (C.TxBodyContent C.BuildTx era -> redeemer)
  -> C.AssetName
  -> C.Quantity
  -> m ()
mintPlutusRefWithRedeemerFn refTxIn scrVer sh redFn assetName quantity =
  inBabbage @era $
    let wit txBody = C.BuildTxWith $ buildRefScriptWitness refTxIn scrVer C.NoScriptDatumForMint (redFn txBody)
        policyId = C.PolicyId sh
     in setScriptsValid
          >> addBtx
            ( over
                (\qFn body -> mintTxBodyL policyId assetName (wit body) qFn body)
                (+ quantity)
            )
          >> addReference refTxIn

mintPlutusRef
  :: forall redeemer era lang m
   . ( Plutus.ToData redeemer
     , MonadBuildTx era m
     , C.HasScriptLanguageInEra lang era
     , C.IsBabbageBasedEra era
     , C.IsPlutusScriptLanguage lang
     )
  => C.TxIn
  -> C.PlutusScriptVersion lang
  -> C.ScriptHash
  -> redeemer
  -> C.AssetName
  -> C.Quantity
  -> m ()
mintPlutusRef refTxIn scrVer sh red = mintPlutusRefWithRedeemerFn refTxIn scrVer sh (const red)

mintSimpleScriptAssets :: forall era m. (MonadBuildTx era m, C.IsMaryBasedEra era) => C.SimpleScript -> [(C.AssetName, C.Quantity)] -> m ()
mintSimpleScriptAssets sscript assets =
  inMary @era $
    let wit = C.SimpleScriptWitness simpleScriptInShelleyEra (C.SScript sscript)
        policyId = C.scriptPolicyId . C.SimpleScript $ sscript
     in traverse_ (\(an, q) -> addMintWithTxBody policyId an q (const wit)) assets

spendSimpleScript :: (MonadBuildTx era m, C.IsShelleyBasedEra era) => C.TxIn -> C.SimpleScript -> m ()
spendSimpleScript txIn sscript =
  let wit = C.SimpleScriptWitness simpleScriptInShelleyEra (C.SScript sscript)
   in addBtx (over L.txIns ((txIn, C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending wit) :))

addCollateral :: (MonadBuildTx era m, C.IsAlonzoBasedEra era) => C.TxIn -> m ()
addCollateral i = addBtx $ over (L.txInsCollateral . L._TxInsCollateralIso) (i :)

addReference :: (MonadBuildTx era m, C.IsBabbageBasedEra era) => C.TxIn -> m ()
addReference i = addBtx $ over (L.txInsReference . L._TxInsReferenceIso . L._1) (i :)

addAuxScript :: (MonadBuildTx era m, C.IsAllegraBasedEra era) => C.ScriptInEra era -> m ()
addAuxScript s = addBtx (over (L.txAuxScripts . L._TxAuxScripts) (s :))

payToAddressTxOut :: (C.IsMaryBasedEra era) => C.AddressInEra era -> C.Value -> C.TxOut C.CtxTx era
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
      addr = C.makeShelleyAddressInEra (C.convert C.maryBasedEra) network (C.PaymentCredentialByKey pk) C.NoStakeAddress
      txo = C.TxOut addr val C.TxOutDatumNone C.ReferenceScriptNone
   in addOutput txo

payToScriptHash :: forall era m. (MonadBuildTx era m, C.IsBabbageBasedEra era) => NetworkId -> ScriptHash -> HashableScriptData -> C.StakeAddressReference -> C.Value -> m ()
payToScriptHash network script datum stakeAddress vl =
  inAlonzo @era $
    let val = mkTxOutValue vl
        addr = C.makeShelleyAddressInEra C.shelleyBasedEra network (C.PaymentCredentialByScript script) stakeAddress
        dat = C.TxOutDatumHash C.alonzoBasedEra (C.hashScriptDataBytes datum)
        txo = C.TxOut addr val dat C.ReferenceScriptNone
     in addOutput txo

payToScriptDatumHash
  :: forall a lang era m
   . (MonadBuildTx era m, Plutus.ToData a, C.IsBabbageBasedEra era)
  => NetworkId -> C.Script lang -> a -> C.StakeAddressReference -> C.Value -> m ()
payToScriptDatumHash network s datum stakeRef vl =
  let sh = C.hashScript s
      dt = toHashableScriptData datum
   in payToScriptHash network sh dt stakeRef vl

createRefScriptBase
  :: forall lang era m
   . (MonadBuildTx era m, C.IsBabbageBasedEra era, C.IsScriptLanguage lang)
  => C.AddressInEra era -> C.Script lang -> C.TxOutDatum C.CtxTx era -> C.Value -> m ()
createRefScriptBase addr script dat vl =
  inBabbage @era $
    let refScript = C.ReferenceScript C.babbageBasedEra $ C.ScriptInAnyLang C.scriptLanguage script
        txo = C.TxOut addr (mkTxOutValue vl) dat refScript
     in addOutput txo

createRefScriptNoDatum :: (MonadBuildTx era m, C.IsBabbageBasedEra era, C.IsScriptLanguage lang) => C.AddressInEra era -> C.Script lang -> C.Value -> m ()
createRefScriptNoDatum addr script = createRefScriptBase addr script C.TxOutDatumNone

-- | same as createRefScriptBase but also specify an inline datum
createRefScriptInlineDatum
  :: forall a lang era m
   . (MonadBuildTx era m, Plutus.ToData a, C.IsBabbageBasedEra era, C.IsScriptLanguage lang)
  => C.AddressInEra era -> C.Script lang -> a -> C.Value -> m ()
createRefScriptInlineDatum addr script datum vl =
  let dat = C.TxOutDatumInline C.babbageBasedEra (toHashableScriptData datum)
   in createRefScriptBase addr script dat vl

-- | same as createRefScriptBase but also specify a datum
createRefScriptDatumHash
  :: forall a lang era m
   . (MonadBuildTx era m, Plutus.ToData a, C.IsBabbageBasedEra era, C.IsScriptLanguage lang)
  => C.AddressInEra era -> C.Script lang -> a -> C.Value -> m ()
createRefScriptDatumHash addr script datum vl =
  let dat = C.TxOutDatumInline C.babbageBasedEra (toHashableScriptData datum)
   in createRefScriptBase addr script dat vl

payToScriptInlineDatum :: forall a era m. (MonadBuildTx era m, Plutus.ToData a, C.IsBabbageBasedEra era) => NetworkId -> C.ScriptHash -> a -> C.StakeAddressReference -> C.Value -> m ()
payToScriptInlineDatum network sh datum stakeRef vl =
  inBabbage @era $
    let val = mkTxOutValue vl
        addr = C.makeShelleyAddressInEra C.shelleyBasedEra network (C.PaymentCredentialByScript sh) stakeRef
        dat = C.TxOutDatumInline C.babbageBasedEra (toHashableScriptData datum)
        txo = C.TxOut addr val dat C.ReferenceScriptNone
     in addOutput txo

-- TODO: Functions for building outputs (Output -> Output)

setScriptsValid :: (C.IsAlonzoBasedEra era) => (MonadBuildTx era m) => m ()
setScriptsValid = addBtx $ set L.txScriptValidity (C.TxScriptValidity C.alonzoBasedEra C.ScriptValid)

{- | Set the Ada component in an output's value to at least the amount needed to cover the
minimum UTxO deposit for this output
-}
setMinAdaDeposit :: (C.IsMaryBasedEra era) => C.LedgerProtocolParameters era -> C.TxOut C.CtxTx era -> C.TxOut C.CtxTx era
setMinAdaDeposit params txOut =
  let minUtxo = minAdaDeposit params txOut
   in txOut & over (L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId) (maybe (Just minUtxo) (Just . max minUtxo))

{- | Calculate the minimum amount of Ada that must be locked in the given UTxO to
satisfy the ledger's minimum Ada constraint.
-}
minAdaDeposit :: (C.IsMaryBasedEra era) => C.LedgerProtocolParameters era -> C.TxOut C.CtxTx era -> C.Quantity
minAdaDeposit (C.LedgerProtocolParameters params) txOut =
  let minAdaValue = C.Quantity 3_000_000
      txo =
        txOut
          -- set the Ada value to a dummy amount to ensure that it is not 0 (if it was 0, the size of the output
          -- would be smaller, causing 'calculateMinimumUTxO' to compute an amount that is a little too small)
          & over (L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId) (maybe (Just minAdaValue) (Just . max minAdaValue))
   in C.lovelaceToQuantity $ C.calculateMinimumUTxO (C.convert C.maryBasedEra) params txo

-- | Apply 'setMinAdaDeposit' to all outputs
setMinAdaDepositAll :: (MonadBuildTx era m, C.IsMaryBasedEra era) => C.LedgerProtocolParameters era -> m ()
setMinAdaDepositAll params = addBtx $ over (L.txOuts . mapped) (setMinAdaDeposit params)

-- | Add a public key hash to the list of required signatures.
addRequiredSignature :: (MonadBuildTx era m, C.IsAlonzoBasedEra era) => Hash PaymentKey -> m ()
addRequiredSignature sig =
  addBtx $ over (L.txExtraKeyWits . L._TxExtraKeyWitnesses) (nub . (:) sig)

-- | Add a transaction output to the start of the list of transaction outputs.
prependTxOut :: (MonadBuildTx era m) => C.TxOut C.CtxTx era -> m ()
prependTxOut txOut = addBtx (over L.txOuts ((:) txOut))

-- | Add a transaction output to the end of the list of transaction outputs.
addOutput :: (MonadBuildTx era m) => C.TxOut C.CtxTx era -> m ()
addOutput txOut = addBtx (over (L.txOuts . L.reversed) (txOut :))

-- | Add a stake rewards withdrawal to the transaction
addWithdrawal :: (MonadBuildTx era m, C.IsShelleyBasedEra era) => C.StakeAddress -> C.Quantity -> C.Witness C.WitCtxStake era -> m ()
addWithdrawal address amount witness =
  let withdrawal = (address, C.quantityToLovelace amount, C.BuildTxWith witness)
   in addBtx (over (L.txWithdrawals . L._TxWithdrawals) ((:) withdrawal))

{- | Like `addWithdrawal` but the stake address is built from the supplied script hash. This is an utility to make withdrawals guarded by
scripts easier to trigger
-}
addScriptWithdrawal :: (MonadBlockchain era m, MonadBuildTx era m, C.IsShelleyBasedEra era) => ScriptHash -> C.Quantity -> C.ScriptWitness C.WitCtxStake era -> m ()
addScriptWithdrawal sh quantity witness = do
  n <- queryNetworkId
  let addr = C.StakeAddress (C.toShelleyNetwork n) $ C.toShelleyStakeCredential $ C.StakeCredentialByScript sh
      wit = C.ScriptWitness C.ScriptWitnessForStakeAddr witness
  addWithdrawal addr quantity wit

{- | Like 'addScriptWithdrawal', but takes a script rather than a script
hash and allows specifying a redeemer based on the 'C.TxBodyContent'.

Warning: The redeemer function receives the current transaction body
to compute the redeemer. This can create infinite loops if the
redeemer depends on parts of the transaction that are modified by
this withdrawal itself.
-}
addScriptWithdrawalWithRedeemerFn
  :: ( MonadBlockchain era m
     , MonadBuildTx era m
     , Plutus.ToData redeemer
     , C.IsShelleyBasedEra era
     , C.IsPlutusScriptLanguage lang
     , C.HasScriptLanguageInEra lang era
     )
  => C.PlutusScript lang -> C.Quantity -> (C.TxBodyContent C.BuildTx era -> redeemer) -> m ()
addScriptWithdrawalWithRedeemerFn script quantity redFn = do
  n <- queryNetworkId
  let sh = C.hashScript $ C.PlutusScript C.plutusScriptVersion script
  let addr = C.StakeAddress (C.toShelleyNetwork n) $ C.toShelleyStakeCredential $ C.StakeCredentialByScript sh
      wit txBody = C.ScriptWitness C.ScriptWitnessForStakeAddr $ buildScriptWitness script C.NoScriptDatumForStake (redFn txBody)
  addWithdrawalWithTxBody addr quantity wit

{- | Add a withdrawal of 0 Lovelace from the rewards account locked by the given Plutus V2 script.
Includes the script in the transaction.
-}
addWithdrawZeroPlutusV2InTransaction
  :: ( MonadBlockchain era m
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

{- | Add a withdrawal of 0 Lovelace from the rewards account locked by the given Plutus V2 script.
The script is provided as a reference input.
-}
addWithdrawZeroPlutusV2Reference
  :: (MonadBlockchain era m, MonadBuildTx era m, Plutus.ToData redeemer, C.HasScriptLanguageInEra PlutusScriptV2 era, C.IsShelleyBasedEra era)
  => C.TxIn -> ScriptHash -> redeemer -> m ()
addWithdrawZeroPlutusV2Reference refTxIn script redeemer = addScriptWithdrawal script 0 $ buildRefScriptWitness refTxIn C.PlutusScriptV2 C.NoScriptDatumForStake redeemer

{- | Add a certificate (stake delegation, stake pool registration, etc)
to the transaction
-}
addCertificate :: (MonadBuildTx era m, C.IsShelleyBasedEra era) => C.Certificate era -> m ()
addCertificate cert =
  let witness = (,C.KeyWitness C.KeyWitnessForStakeAddr) <$> C.selectStakeCredentialWitness cert
   in addBtx (over (L.txCertificates . L._TxCertificates) ((cert, C.BuildTxWith witness) OMap.|<))

-- | Create a 'C.StakeCredential' registration as a ConwayCertificate to the transaction.
mkConwayStakeCredentialRegistrationCertificate
  :: forall era m
   . ( C.IsConwayBasedEra era
     , MonadBlockchain era m
     )
  => C.StakeCredential
  -> m (C.Certificate era)
mkConwayStakeCredentialRegistrationCertificate stakeCred = do
  deposit <-
    C.conwayEraOnwardsConstraints @era C.conwayBasedEra $
      view (Ledger.ppKeyDepositL @(C.ShelleyLedgerEra era)) . C.unLedgerProtocolParameters <$> queryProtocolParameters
  pure $ C.makeStakeAddressRegistrationCertificate $ C.StakeAddrRegistrationConway C.conwayBasedEra deposit stakeCred

-- | Create a certificate for delegation to some delegatee in a ConwayCertificate to the transaction.
mkConwayStakeCredentialDelegationCertificate
  :: forall era
   . (C.IsConwayBasedEra era)
  => C.StakeCredential
  -> ConwayTxCert.Delegatee
  -> C.Certificate era
mkConwayStakeCredentialDelegationCertificate stakeCred =
  C.makeStakeAddressDelegationCertificate
    . C.StakeDelegationRequirementsConwayOnwards C.conwayBasedEra stakeCred

-- | Create a 'C.StakeCredential' and delegate to some delegatee in a single ConwayCertificate to the transaction.
mkConwayStakeCredentialRegistrationAndDelegationCertificate
  :: forall era m
   . ( C.IsConwayBasedEra era
     , MonadBlockchain era m
     )
  => C.StakeCredential
  -> ConwayTxCert.Delegatee
  -> m (C.Certificate era)
mkConwayStakeCredentialRegistrationAndDelegationCertificate stakeCred delegatee = do
  deposit <-
    C.conwayEraOnwardsConstraints @era C.conwayBasedEra $
      view (Ledger.ppKeyDepositL @(C.ShelleyLedgerEra era)) . C.unLedgerProtocolParameters <$> queryProtocolParameters
  let cert = C.makeStakeAddressAndDRepDelegationCertificate C.conwayBasedEra stakeCred delegatee deposit
  pure cert

-- | Add a 'C.StakeCredential' as a ConwayEra and onwards deregistration certificate to the transaction.
mkConwayStakeCredentialUnRegistrationCertificate
  :: forall era
   . (C.IsConwayBasedEra era)
  => C.StakeCredential
  -> Ledger.Coin
  -- ^ Deposit, if present, must match the amount that was left as a deposit upon stake credential registration.
  -> C.Certificate era
mkConwayStakeCredentialUnRegistrationCertificate stakeCred deposit =
  C.makeStakeAddressUnregistrationCertificate $ C.StakeAddrRegistrationConway C.conwayBasedEra deposit stakeCred
