{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-| Building transactions
-}
module Convex.BuildTx(
  -- * Tx Builder
  TxBuilder(..),
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
  spendPlutusV1,
  spendPlutusV2,
  spendPlutusV2Ref,
  spendPlutusV2RefWithInlineDatum,
  spendPlutusV2RefWithoutInRef,
  spendPlutusV2RefWithoutInRefInlineDatum,
  spendPlutusV2InlineDatum,

  -- ** Adding outputs
  payToAddress,
  payToAddressTxOut,
  payToPublicKey,
  payToScriptHash,
  payToPlutusV1,
  payToPlutusV2,
  payToPlutusV2InlineDatum,
  payToPlutusV2Inline,
  payToPlutusV2InlineWithInlineDatum,
  payToPlutusV2InlineWithDatum,

  -- ** Staking
  addWithdrawal,
  addScriptWithdrawal,
  addWithdrawZeroPlutusV2InTransaction,
  addWithdrawZeroPlutusV2Reference,
  addCertificate,
  addStakeCredentialCertificate,
  addStakeWitness,

  -- ** Minting and burning tokens
  mintPlutusV1,
  mintPlutusV2,
  mintPlutusV2Ref,

  -- ** Constructing script witness
  buildV1ScriptWitness,
  buildV2ScriptWitness,
  buildV2RefScriptWitness,

  -- ** Utilities
  assetValue,

  -- * Minimum Ada deposit
  minAdaDeposit,
  setMinAdaDeposit,
  setMinAdaDepositAll
  ) where

import           Cardano.Api.Shelley           (Hash, HashableScriptData,
                                                NetworkId, PaymentKey,
                                                PlutusScript, PlutusScriptV1,
                                                PlutusScriptV2, ScriptHash,
                                                WitCtxTxIn)
import qualified Cardano.Api.Shelley           as C
import qualified Cardano.Ledger.Shelley.TxCert as TxCert
import           Control.Lens                  (_1, _2, at, mapped, over, set,
                                                view, (&))
import qualified Control.Lens                  as L
import           Control.Monad.Except          (MonadError (..))
import           Control.Monad.Reader.Class    (MonadReader (..))
import qualified Control.Monad.State           as LazyState
import           Control.Monad.State.Class     (MonadState (..))
import qualified Control.Monad.State.Strict    as StrictState
import           Control.Monad.Trans.Class     (MonadTrans (..))
import           Control.Monad.Trans.Except    (ExceptT)
import           Control.Monad.Writer          (WriterT, execWriterT,
                                                runWriterT)
import           Control.Monad.Writer.Class    (MonadWriter (..))
import           Convex.Class                  (MonadBlockchain (..),
                                                MonadBlockchainCardanoNodeT,
                                                MonadMockchain (..))
import qualified Convex.Lenses                 as L
import           Convex.MonadLog               (MonadLog (..), MonadLogIgnoreT,
                                                MonadLogKatipT)
import           Convex.Scripts                (toHashableScriptData)
import           Data.Functor.Identity         (Identity (..))
import           Data.List                     (nub)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe, fromJust)
import qualified Data.Set                      as Set
import qualified PlutusLedgerApi.V1            as Plutus

type TxBody = C.TxBodyContent C.BuildTx C.BabbageEra

{-| Look up the index of the @TxIn@ in the list of spending inputs
-}
lookupIndexSpending :: C.TxIn -> TxBody -> Maybe Int
lookupIndexSpending txi = Map.lookupIndex txi . Map.fromList . (fmap (view L._BuildTxWith) <$>) . view L.txIns

{-| Look up the index of the @TxIn@ in the list of spending inputs. Throws an error if the @TxIn@ is not present.
-}
findIndexSpending :: C.TxIn -> TxBody -> Int
findIndexSpending txi = fromJust . lookupIndexSpending txi

{-| Look up the index of the @TxIn@ in the list of reference inputs
-}
lookupIndexReference :: C.TxIn -> TxBody -> Maybe Int
lookupIndexReference txi = Set.lookupIndex txi . Set.fromList . view (L.txInsReference . L._TxInsReference)

{-| Look up the index of the @TxIn@ in the list of reference inputs. Throws an error if the @TxIn@ is not present.
-}
findIndexReference :: C.TxIn -> TxBody -> Int
findIndexReference txi = fromJust . lookupIndexReference txi

{-| Look up the index of the @PolicyId@ in the transaction mint.
Note: cardano-api represents a value as a @Map AssetId Quantity@, this is different than the on-chain representation
which is @Map CurrencySymbol (Map TokenName Quantity).
Here, we want to get the index into the on-chain map, but instead index into the cardano-api @Map CurrencySymbol Witness@.
These two indexes should be the same by construction, but it is possible to violate this invariant when building a tx.
-}
lookupIndexMinted :: C.PolicyId -> TxBody -> Maybe Int
lookupIndexMinted policy = Map.lookupIndex policy . view (L.txMintValue . L._TxMintValue .  _2)

{-| Look up the index of the @PolicyId@ in the transaction mint. Throws an error if the @PolicyId@ is not present.
-}
findIndexMinted :: C.PolicyId -> TxBody -> Int
findIndexMinted policy = fromJust . lookupIndexMinted policy

{-| Look up the index of the @StakeAddress@ in the list of withdrawals.
-}
lookupIndexWithdrawal :: C.StakeAddress -> TxBody -> Maybe Int
lookupIndexWithdrawal stakeAddress = Set.lookupIndex stakeAddress . Set.fromList . fmap (view _1) . view (L.txWithdrawals . L._TxWithdrawals)

{-| Look up the index of the @StakeAddress@ in the list of withdrawals. Throws an error if the @StakeAddress@ is not present.
-}
findIndexWithdrawal :: C.StakeAddress -> TxBody -> Int
findIndexWithdrawal stakeAddress = fromJust . lookupIndexWithdrawal stakeAddress


{-|
A function that modifies the final @TxBodyContent@, after seeing the @TxBodyContent@ of
the entire finished transaction (lazily).

Note that the result of @unTxBuilder txBody@ must not completely force the @txBody@,
or refer to itself circularly. For example, using this to construct a redeemer that contains the whole
@TransactionInputs@ map is going to loop forever.
-}
newtype TxBuilder = TxBuilder{ unTxBuilder :: TxBody -> TxBody -> TxBody }

{-| Construct the final @TxBodyContent@
-}
buildTx :: TxBuilder -> TxBody
buildTx txb = buildTxWith txb L.emptyTx

{-| Construct the final @TxBodyContent@ from the provided @TxBodyContent@
-}
buildTxWith :: TxBuilder -> TxBody -> TxBody
buildTxWith TxBuilder{unTxBuilder} initial =
  let result = unTxBuilder result initial
  in result

instance Semigroup TxBuilder where
    -- note that the order here is reversed, compared to @Data.Monoid.Endo@.
    -- This is so that @addBtx a >> addBtx b@ will result in a transaction
    -- where @a@ has been applied before @b@.
  (TxBuilder l) <> (TxBuilder r) = TxBuilder $ \k -> (r k . l k)

instance Monoid TxBuilder where
  mempty = TxBuilder $ const id

{-| An effect that collects @TxBuilder@ values for building
cardano transactions
-}
class Monad m => MonadBuildTx m where
  -- | Add a @TxBuilder@
  addTxBuilder :: TxBuilder -> m ()

instance MonadBuildTx m => MonadBuildTx (ExceptT e m) where
  addTxBuilder = lift . addTxBuilder

instance MonadBuildTx m => MonadBuildTx (StrictState.StateT e m) where
  addTxBuilder = lift . addTxBuilder

instance MonadBuildTx m => MonadBuildTx (LazyState.StateT e m) where
  addTxBuilder = lift . addTxBuilder

instance (Monoid w, MonadBuildTx m) => MonadBuildTx (WriterT w m) where
  addTxBuilder = lift . addTxBuilder

instance MonadBuildTx m => MonadBuildTx (MonadBlockchainCardanoNodeT e m) where
  addTxBuilder = lift . addTxBuilder

instance MonadBuildTx m => MonadBuildTx (MonadLogIgnoreT m) where
  addTxBuilder = lift . addTxBuilder

instance MonadBuildTx m => MonadBuildTx (MonadLogKatipT m) where
  addTxBuilder = lift . addTxBuilder

addBtx :: MonadBuildTx m => (TxBody -> TxBody) -> m ()
addBtx = addTxBuilder . TxBuilder . const

{-| Monad transformer for the @MonadBuildTx@ effect
-}
newtype BuildTxT m a = BuildTxT{unBuildTxT :: WriterT TxBuilder m a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans BuildTxT where
  lift = BuildTxT . lift

instance Monad m => MonadBuildTx (BuildTxT m) where
  addTxBuilder = BuildTxT . tell

instance MonadError e m => MonadError e (BuildTxT m) where
  throwError = lift . throwError
  catchError m action = BuildTxT (unBuildTxT $ catchError m action)

instance MonadReader e m => MonadReader e (BuildTxT m) where
  ask = lift ask
  local f = BuildTxT . local f . unBuildTxT

instance MonadBlockchain m => MonadBlockchain (BuildTxT m) where
  sendTx = lift . sendTx
  utxoByTxIn = lift . utxoByTxIn
  queryProtocolParameters = lift queryProtocolParameters
  queryStakeAddresses creds = lift . queryStakeAddresses creds
  queryStakePools = lift queryStakePools
  querySystemStart = lift querySystemStart
  queryEraHistory = lift queryEraHistory
  querySlotNo = lift querySlotNo
  networkId = lift networkId

instance MonadMockchain m => MonadMockchain (BuildTxT m) where
  modifySlot = lift . modifySlot
  modifyUtxo = lift . modifyUtxo
  resolveDatumHash = lift . resolveDatumHash

instance MonadState s m => MonadState s (BuildTxT m) where
  state = lift . state

instance MonadLog m => MonadLog (BuildTxT m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'
  logDebug' = lift . logDebug'

{-| Run the @BuildTxT@ monad transformer
-}
runBuildTxT :: BuildTxT m a -> m (a, TxBuilder)
runBuildTxT = runWriterT . unBuildTxT

{-| Run the @BuildTxT@ monad transformer, returning the @TxBuild@ part only
-}
execBuildTxT :: Monad m => BuildTxT m a -> m TxBuilder
execBuildTxT = execWriterT . unBuildTxT

{-| Run the @BuildTxT@ monad transformer, returnin only the result
-}
evalBuildTxT :: Monad m => BuildTxT m a -> m a
evalBuildTxT = fmap fst . runWriterT . unBuildTxT

runBuildTx :: BuildTxT Identity a -> (a, TxBuilder)
runBuildTx = runIdentity . runBuildTxT

execBuildTx :: BuildTxT Identity a -> TxBuilder
execBuildTx = runIdentity . execBuildTxT

{-| Run the @BuildTx@ action and produce a transaction body
-}
execBuildTx' :: BuildTxT Identity a -> TxBody
execBuildTx' = buildTx . runIdentity . execBuildTxT

{-| These functions allow to build the witness for an input/asset/withdrawal
by accessing the final @TxBodyContent@. To avoid an infinite loop when
constructing the witness, make sure that the witness does not depend on
itself. For example: @addInputWithTxBody txi (\body -> find (\in -> in == txi) (txInputs body))@
is going to loop.
-}
addInputWithTxBody :: MonadBuildTx m => C.TxIn -> (TxBody -> C.Witness WitCtxTxIn C.BabbageEra) -> m ()
addInputWithTxBody txIn f = addTxBuilder (TxBuilder $ \body -> (over L.txIns ((txIn, C.BuildTxWith $ f body) :)))

addMintWithTxBody :: MonadBuildTx m => C.PolicyId -> C.AssetName -> C.Quantity -> (TxBody -> C.ScriptWitness C.WitCtxMint C.BabbageEra) -> m ()
addMintWithTxBody policy assetName quantity f =
  let v = assetValue (C.unPolicyId policy) assetName quantity
  in addTxBuilder (TxBuilder $ \body -> (over (L.txMintValue . L._TxMintValue) (over _1 (<> v) . over _2 (Map.insert policy (f body)))))

addWithdrawalWithTxBody ::  MonadBuildTx m => C.StakeAddress -> C.Quantity -> (TxBody -> C.Witness C.WitCtxStake C.BabbageEra) -> m ()
addWithdrawalWithTxBody address amount f =
  addTxBuilder (TxBuilder $ \body -> (over (L.txWithdrawals . L._TxWithdrawals) ((address, C.quantityToLovelace amount, C.BuildTxWith $ f body) :)))

{-| Spend an output locked by a public key
-}
spendPublicKeyOutput :: MonadBuildTx m => C.TxIn -> m ()
spendPublicKeyOutput txIn = do
  let wit = C.BuildTxWith (C.KeyWitness (C.KeyWitnessForSpending))
  addBtx (over L.txIns ((txIn, wit) :))

{-| Utility function to build a v1 script witness
-}
buildV1ScriptWitness :: forall redeemer witctx . Plutus.ToData redeemer =>
  C.PlutusScript C.PlutusScriptV1 ->
  C.ScriptDatum witctx ->
  redeemer ->
  C.ScriptWitness witctx C.BabbageEra
buildV1ScriptWitness script datum redeemer =
  C.PlutusScriptWitness
    C.PlutusScriptV1InBabbage
    C.PlutusScriptV1
    (C.PScript script)
    datum
    (toHashableScriptData redeemer)
    (C.ExecutionUnits 0 0)

{-| Utility function to build a v2 script witness
-}
buildV2ScriptWitness :: forall redeemer witctx . Plutus.ToData redeemer =>
  C.PlutusScript C.PlutusScriptV2 ->
  C.ScriptDatum witctx ->
  redeemer ->
  C.ScriptWitness witctx C.BabbageEra
buildV2ScriptWitness script datum redeemer =
  C.PlutusScriptWitness
    C.PlutusScriptV2InBabbage
    C.PlutusScriptV2
    (C.PScript script)
    datum
    (toHashableScriptData redeemer)
    (C.ExecutionUnits 0 0)

{-| Utility function to build a reference script witness
-}
buildV2RefScriptWitness :: forall redeemer witctx . Plutus.ToData redeemer =>
  C.TxIn ->
  Maybe C.ScriptHash ->
  C.ScriptDatum witctx ->
  redeemer ->
  C.ScriptWitness witctx C.BabbageEra
buildV2RefScriptWitness refTxIn mbSh datum redeemer =
  C.PlutusScriptWitness
    C.PlutusScriptV2InBabbage
    C.PlutusScriptV2
    (C.PReferenceScript refTxIn mbSh)
    datum
    (toHashableScriptData redeemer)
    (C.ExecutionUnits 0 0)

spendPlutusV1 :: forall datum redeemer m. (MonadBuildTx m, Plutus.ToData datum, Plutus.ToData redeemer) => C.TxIn -> PlutusScript PlutusScriptV1 -> datum -> redeemer -> m ()
spendPlutusV1 txIn s (toHashableScriptData -> dat) red =
  let wit = C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending $ buildV1ScriptWitness s (C.ScriptDatumForTxIn dat) red
  in setScriptsValid >> addBtx (over L.txIns ((txIn, wit) :))

spendPlutusV2 :: forall datum redeemer m. (MonadBuildTx m, Plutus.ToData datum, Plutus.ToData redeemer) => C.TxIn -> PlutusScript PlutusScriptV2 -> datum -> redeemer -> m ()
spendPlutusV2 txIn s (toHashableScriptData -> dat) red =
  let wit = C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending $ buildV2ScriptWitness s (C.ScriptDatumForTxIn dat) red
  in setScriptsValid >> addBtx (over L.txIns ((txIn, wit) :))

{-| Spend an output locked by a Plutus V2 validator with an inline datum
-}
spendPlutusV2InlineDatum :: forall redeemer m. (MonadBuildTx m, Plutus.ToData redeemer) => C.TxIn -> PlutusScript PlutusScriptV2 -> redeemer -> m ()
spendPlutusV2InlineDatum txIn s red =
  let wit = C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending $ buildV2ScriptWitness s C.InlineScriptDatum red
  in setScriptsValid >> addBtx (over L.txIns ((txIn, wit) :))

{-| Spend an output locked by a Plutus V2 validator using the redeemer provided. The redeemer
can depend on the index of the @TxIn@ in the inputs of the final transaction.
-}
spendPlutusV2RefBase :: forall redeemer m. (MonadBuildTx m, Plutus.ToData redeemer) => C.TxIn -> C.TxIn -> Maybe C.ScriptHash -> C.ScriptDatum C.WitCtxTxIn -> (Int -> redeemer) -> m ()
spendPlutusV2RefBase txIn refTxIn sh dat red =
  let wit txBody = C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending $ buildV2RefScriptWitness refTxIn sh dat (red $ findIndexSpending txIn txBody)
  in setScriptsValid >> addTxBuilder (TxBuilder $ \body -> over L.txIns ((txIn, wit body) :))

{-| Spend an output locked by a Plutus V2 validator using the redeemer
-}
spendPlutusV2RefBaseWithInRef :: forall redeemer m. (MonadBuildTx m, Plutus.ToData redeemer) => C.TxIn -> C.TxIn -> Maybe C.ScriptHash -> C.ScriptDatum C.WitCtxTxIn -> redeemer -> m ()
spendPlutusV2RefBaseWithInRef txIn refTxIn sh dat red = spendPlutusV2RefBase txIn refTxIn sh dat (const red) >> addReference refTxIn

spendPlutusV2Ref :: forall datum redeemer m. (MonadBuildTx m, Plutus.ToData datum, Plutus.ToData redeemer) => C.TxIn -> C.TxIn -> Maybe C.ScriptHash -> datum -> redeemer -> m ()
spendPlutusV2Ref txIn refTxIn sh (toHashableScriptData -> dat) red = spendPlutusV2RefBaseWithInRef txIn refTxIn sh (C.ScriptDatumForTxIn dat) red

{-| same as spendPlutusV2Ref but considers inline datum at the spent utxo
-}
spendPlutusV2RefWithInlineDatum :: forall redeemer m. (MonadBuildTx m, Plutus.ToData redeemer) => C.TxIn -> C.TxIn -> Maybe C.ScriptHash -> redeemer -> m ()
spendPlutusV2RefWithInlineDatum txIn refTxIn sh red = spendPlutusV2RefBaseWithInRef txIn refTxIn sh C.InlineScriptDatum red

{-| same as spendPlutusV2Ref but does not add the reference script in the reference input list
This is to cover the case whereby the reference script utxo is expected to be consumed in the same tx.
-}
spendPlutusV2RefWithoutInRef :: forall datum redeemer m. (MonadBuildTx m, Plutus.ToData datum, Plutus.ToData redeemer) => C.TxIn -> C.TxIn -> Maybe C.ScriptHash -> datum -> redeemer -> m ()
spendPlutusV2RefWithoutInRef txIn refTxIn sh (toHashableScriptData -> dat) red = spendPlutusV2RefBase txIn refTxIn sh (C.ScriptDatumForTxIn dat) (const red)

{-| same as spendPlutusV2RefWithoutInRef but considers inline datum at the spent utxo
-}
spendPlutusV2RefWithoutInRefInlineDatum :: forall redeemer m. (MonadBuildTx m, Plutus.ToData redeemer) => C.TxIn -> C.TxIn -> Maybe C.ScriptHash -> redeemer -> m ()
spendPlutusV2RefWithoutInRefInlineDatum txIn refTxIn sh red = spendPlutusV2RefBase txIn refTxIn sh C.InlineScriptDatum (const red)

mintPlutusV1 :: forall redeemer m. (Plutus.ToData redeemer, MonadBuildTx m) => PlutusScript PlutusScriptV1 -> redeemer -> C.AssetName -> C.Quantity -> m ()
mintPlutusV1 script red assetName quantity =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV1 script)
      v = assetValue sh assetName quantity
      policyId = C.PolicyId sh
      wit      = buildV1ScriptWitness script C.NoScriptDatumForMint red
  in  setScriptsValid >> addBtx (over (L.txMintValue . L._TxMintValue) (over _1 (<> v) . over _2 (Map.insert policyId wit)))

{-| A value containing the given amount of the native asset
-}
assetValue :: ScriptHash -> C.AssetName -> C.Quantity -> C.Value
assetValue hsh assetName quantity =
  C.valueFromList [(C.AssetId (C.PolicyId hsh) assetName, quantity)]

mintPlutusV2 :: forall redeemer m. (Plutus.ToData redeemer, MonadBuildTx m) => PlutusScript PlutusScriptV2 -> redeemer -> C.AssetName -> C.Quantity -> m ()
mintPlutusV2 script red assetName quantity =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
      v = assetValue sh assetName quantity
      policyId = C.PolicyId sh
      wit      = buildV2ScriptWitness script C.NoScriptDatumForMint red
  in  setScriptsValid >> addBtx (over (L.txMintValue . L._TxMintValue) (over _1 (<> v) . over _2 (Map.insert policyId wit)))

mintPlutusV2Ref :: forall redeemer m. (Plutus.ToData redeemer, MonadBuildTx m) => C.TxIn -> C.ScriptHash -> redeemer -> C.AssetName -> C.Quantity -> m ()
mintPlutusV2Ref refTxIn sh red assetName quantity =
  let v = assetValue sh assetName quantity
      wit = buildV2RefScriptWitness refTxIn (Just sh) C.NoScriptDatumForMint red
      policyId = C.PolicyId sh
  in  setScriptsValid
      >> addBtx (over (L.txMintValue . L._TxMintValue) (over _1 (<> v) . over _2 (Map.insert policyId wit)))
      >> addReference refTxIn

addCollateral :: MonadBuildTx m => C.TxIn -> m ()
addCollateral i = addBtx $ over (L.txInsCollateral . L._TxInsCollateral) ((:) i)

addReference :: MonadBuildTx m => C.TxIn -> m ()
addReference i = addBtx $ over (L.txInsReference . L._TxInsReference) ((:) i)

addAuxScript :: MonadBuildTx m => C.ScriptInEra C.BabbageEra -> m ()
addAuxScript s = addBtx (over (L.txAuxScripts . L._TxAuxScripts) ((:) s))

payToAddressTxOut :: C.AddressInEra C.BabbageEra -> C.Value -> C.TxOut C.CtxTx C.BabbageEra
payToAddressTxOut addr vl = C.TxOut addr (C.TxOutValueShelleyBased C.ShelleyBasedEraBabbage $ C.toMaryValue vl) C.TxOutDatumNone C.ReferenceScriptNone

payToAddress :: MonadBuildTx m => C.AddressInEra C.BabbageEra -> C.Value -> m ()
payToAddress addr vl = addBtx $ over L.txOuts ((:) (payToAddressTxOut addr vl))

payToPublicKey :: MonadBuildTx m => NetworkId -> Hash PaymentKey -> C.Value -> m ()
payToPublicKey network pk vl =
  let val = C.TxOutValueShelleyBased C.ShelleyBasedEraBabbage $ C.toMaryValue vl
      addr = C.makeShelleyAddressInEra C.ShelleyBasedEraBabbage network (C.PaymentCredentialByKey pk) C.NoStakeAddress
      txo = C.TxOut addr val C.TxOutDatumNone C.ReferenceScriptNone
  in prependTxOut txo

payToScriptHash :: MonadBuildTx m => NetworkId -> ScriptHash -> HashableScriptData -> C.StakeAddressReference -> C.Value -> m ()
payToScriptHash network script datum stakeAddress vl =
  let val = C.TxOutValueShelleyBased C.ShelleyBasedEraBabbage $ C.toMaryValue vl
      addr = C.makeShelleyAddressInEra C.ShelleyBasedEraBabbage network (C.PaymentCredentialByScript script) stakeAddress
      dat = C.TxOutDatumInTx C.AlonzoEraOnwardsBabbage datum
      txo = C.TxOut addr val dat C.ReferenceScriptNone
  in prependTxOut txo

payToPlutusV1 :: forall a m. (MonadBuildTx m, Plutus.ToData a) => NetworkId -> PlutusScript PlutusScriptV1 -> a -> C.StakeAddressReference -> C.Value -> m ()
payToPlutusV1 network s datum stakeRef vl =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV1 s)
      dt = toHashableScriptData datum
  in payToScriptHash network sh dt stakeRef vl

payToPlutusV2 :: forall a m. (MonadBuildTx m, Plutus.ToData a) => NetworkId -> PlutusScript PlutusScriptV2 -> a -> C.StakeAddressReference -> C.Value -> m ()
payToPlutusV2 network s datum stakeRef vl =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV2 s)
      dt = toHashableScriptData datum
  in payToScriptHash network sh dt stakeRef vl

payToPlutusV2InlineBase :: MonadBuildTx m => C.AddressInEra C.BabbageEra -> C.PlutusScript C.PlutusScriptV2 -> C.TxOutDatum C.CtxTx C.BabbageEra -> C.Value -> m ()
payToPlutusV2InlineBase addr script dat vl =
  let refScript = C.ReferenceScript C.BabbageEraOnwardsBabbage (C.toScriptInAnyLang $ C.PlutusScript C.PlutusScriptV2 script)
      txo = C.TxOut addr (C.TxOutValueShelleyBased C.ShelleyBasedEraBabbage $ C.toMaryValue vl) dat refScript
  in prependTxOut txo

payToPlutusV2Inline :: MonadBuildTx m => C.AddressInEra C.BabbageEra -> PlutusScript PlutusScriptV2 -> C.Value -> m ()
payToPlutusV2Inline addr script vl = payToPlutusV2InlineBase addr script C.TxOutDatumNone vl

{-| same as payToPlutusV2Inline but also specify an inline datum -}
payToPlutusV2InlineWithInlineDatum :: forall a m. (MonadBuildTx m, Plutus.ToData a) => C.AddressInEra C.BabbageEra -> C.PlutusScript C.PlutusScriptV2 -> a -> C.Value -> m ()
payToPlutusV2InlineWithInlineDatum addr script datum vl =
  let dat = C.TxOutDatumInline C.BabbageEraOnwardsBabbage (toHashableScriptData datum)
  in payToPlutusV2InlineBase addr script dat vl

{-| same as payToPlutusV2Inline but also specify a datum -}
payToPlutusV2InlineWithDatum :: forall a m. (MonadBuildTx m, Plutus.ToData a) => C.AddressInEra C.BabbageEra -> C.PlutusScript C.PlutusScriptV2 -> a -> C.Value -> m ()
payToPlutusV2InlineWithDatum addr script datum vl =
  let dat = C.TxOutDatumInTx C.AlonzoEraOnwardsBabbage (toHashableScriptData datum)
  in payToPlutusV2InlineBase addr script dat vl

payToPlutusV2InlineDatum :: forall a m. (MonadBuildTx m, Plutus.ToData a) => NetworkId -> PlutusScript PlutusScriptV2 -> a -> C.StakeAddressReference -> C.Value -> m ()
payToPlutusV2InlineDatum network script datum stakeRef vl =
  let val = C.TxOutValueShelleyBased C.ShelleyBasedEraBabbage $ C.toMaryValue vl
      sh  = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
      addr = C.makeShelleyAddressInEra C.ShelleyBasedEraBabbage network (C.PaymentCredentialByScript sh) stakeRef
      dat = C.TxOutDatumInline C.BabbageEraOnwardsBabbage (toHashableScriptData datum)
      txo = C.TxOut addr val dat C.ReferenceScriptNone
  in prependTxOut txo
-- TODO: Functions for building outputs (Output -> Output)

setScriptsValid :: MonadBuildTx m => m ()
setScriptsValid = addBtx $ set L.txScriptValidity (C.TxScriptValidity C.AlonzoEraOnwardsBabbage C.ScriptValid)

{-| Set the Ada component in an output's value to at least the amount needed to cover the
minimum UTxO deposit for this output
-}
setMinAdaDeposit :: C.LedgerProtocolParameters C.BabbageEra -> C.TxOut C.CtxTx C.BabbageEra -> C.TxOut C.CtxTx C.BabbageEra
setMinAdaDeposit params txOut =
  let minUtxo = minAdaDeposit params txOut
  in txOut & over (L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId) (maybe (Just minUtxo) (Just . max minUtxo))

{-| Calculate the minimum amount of Ada that must be locked in the given UTxO to
satisfy the ledger's minimum Ada constraint.
-}
minAdaDeposit :: C.LedgerProtocolParameters C.BabbageEra -> C.TxOut C.CtxTx C.BabbageEra -> C.Quantity
minAdaDeposit (C.LedgerProtocolParameters params) txOut =
  let minAdaValue = C.Quantity 3_000_000
      txo = txOut
            -- set the Ada value to a dummy amount to ensure that it is not 0 (if it was 0, the size of the output
            -- would be smaller, causing 'calculateMinimumUTxO' to compute an amount that is a little too small)
            & over (L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId) (maybe (Just minAdaValue) (Just . max minAdaValue))
  in fromMaybe (C.Quantity 0) $ do
        let l = C.calculateMinimumUTxO C.ShelleyBasedEraBabbage txo params
        pure (C.lovelaceToQuantity l)

{-| Apply 'setMinAdaDeposit' to all outputs
-}
setMinAdaDepositAll :: MonadBuildTx m => C.LedgerProtocolParameters C.BabbageEra -> m ()
setMinAdaDepositAll params = addBtx $ over (L.txOuts . mapped) (setMinAdaDeposit params)

{-| Add a public key hash to the list of required signatures.
-}
addRequiredSignature :: MonadBuildTx m => Hash PaymentKey -> m ()
addRequiredSignature sig =
  addBtx $ over (L.txExtraKeyWits . L._TxExtraKeyWitnesses) (nub . (:) sig)

{-| Add a transaction output to the start of the list of transaction outputs.
-}
prependTxOut :: MonadBuildTx m => C.TxOut C.CtxTx C.BabbageEra -> m ()
prependTxOut txOut = addBtx (over L.txOuts ((:) txOut))

{-| Add a transaction output to the end of the list of transaction outputs.
-}
addOutput :: MonadBuildTx m => C.TxOut C.CtxTx C.BabbageEra -> m ()
addOutput txOut = addBtx (over (L.txOuts . L.reversed) ((:) txOut))

{-| Add a stake rewards withdrawal to the transaction
-}
addWithdrawal :: MonadBuildTx m => C.StakeAddress -> C.Quantity -> C.Witness C.WitCtxStake C.BabbageEra -> m ()
addWithdrawal address amount witness =
  let withdrawal = (address, C.quantityToLovelace amount, C.BuildTxWith witness)
  in addBtx (over (L.txWithdrawals . L._TxWithdrawals) ((:) withdrawal))

{- Like `addWithdrawal` but the stake address is built from the supplied script hash. This is an utility to make withdrawals guarded by
scripts easier to trigger
-}
addScriptWithdrawal :: (MonadBlockchain m, MonadBuildTx m) => ScriptHash -> C.Quantity -> C.ScriptWitness C.WitCtxStake C.BabbageEra -> m ()
addScriptWithdrawal sh quantity witness = do
  n <- networkId
  let addr = C.StakeAddress (C.toShelleyNetwork n) $ C.toShelleyStakeCredential $ C.StakeCredentialByScript sh
      wit  = C.ScriptWitness C.ScriptWitnessForStakeAddr witness
  addWithdrawal addr quantity wit

{-| Add a withdrawal of 0 Lovelace from the rewards account locked by the given Plutus V2 script.
Includes the script in the transaction.
-}
addWithdrawZeroPlutusV2InTransaction :: (MonadBlockchain m, MonadBuildTx m, Plutus.ToData redeemer) => PlutusScript PlutusScriptV2 -> redeemer -> m ()
addWithdrawZeroPlutusV2InTransaction script redeemer = do
  let sh = C.hashScript $ C.PlutusScript C.PlutusScriptV2 script
  addScriptWithdrawal sh 0 $ buildV2ScriptWitness script C.NoScriptDatumForStake redeemer

{-| Add a withdrawal of 0 Lovelace from the rewards account locked by the given Plutus V2 script.
The script is provided as a reference input.
-}
addWithdrawZeroPlutusV2Reference :: (MonadBlockchain m, MonadBuildTx m, Plutus.ToData redeemer) => C.TxIn -> ScriptHash -> redeemer -> m ()
addWithdrawZeroPlutusV2Reference refTxIn script redeemer = addScriptWithdrawal script 0 $ buildV2RefScriptWitness refTxIn (Just script) C.NoScriptDatumForStake redeemer

{-| Add a certificate (stake delegation, stake pool registration, etc)
to the transaction
-}
addCertificate :: MonadBuildTx m => C.Certificate C.BabbageEra -> m ()
addCertificate cert =
  addBtx (over (L.txCertificates . L._TxCertificates . _1) ((:) cert))

{-| Add a 'C.StakeCredential' as a certificate to the transaction
-}
addStakeCredentialCertificate :: MonadBuildTx m => C.StakeCredential -> m ()
addStakeCredentialCertificate =
  addCertificate . C.ShelleyRelatedCertificate C.ShelleyToBabbageEraBabbage . TxCert.RegTxCert . C.toShelleyStakeCredential

{-| Add a stake witness to the transaction
-}
addStakeWitness :: MonadBuildTx m => C.StakeCredential -> C.Witness C.WitCtxStake C.BabbageEra -> m ()
addStakeWitness credential witness =
  addBtx (set (L.txCertificates . L._TxCertificates . _2 . at credential) (Just witness))
