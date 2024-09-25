{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Minimal mockchain
-}
module Convex.MockChain(
  -- * State of the mockchain
  ERA,
  MockChainState(..),
  InitialUTXOs,
  initialState,
  initialStateFor,
  genesisUTxO,
  env,
  poolState,
  transactions,
  failedTransactions,
  utxoSet,
  datums,
  walletUtxo,
  fromLedgerUTxO,
  -- * Transaction validation
  ExUnitsError(..),
  _Phase1Error,
  _Phase2Error,
  ValidationError(..),
  _VExUnits,
  _PredicateFailures,
  _ApplyTxFailure,
  getTxExUnits,
  evaluateTx,
  applyTransaction,
  -- * Plutus scripts
  PlutusWithContext(..),
  fullyAppliedScript,
  -- * Mockchain implementation
  MockchainError(..),
  MockchainT(..),
  Mockchain,
  runMockchainT,
  runMockchain,
  runMockchain0,
  evalMockchainT,
  evalMockchain,
  evalMockchain0,
  execMockchain,
  execMockchainT,
  execMockchain0,

  -- ** MockchainIO
  MockchainIO,
  runMockchainIO,
  runMockchain0IO,
  runMockchain0IOWith,
  evalMockchainIO,
  evalMockchain0IO,
  execMockchainIO,
  execMockchain0IO
  ) where

import           Cardano.Api.Shelley                   (AddressInEra, ConwayEra,
                                                        Hash (StakePoolKeyHash),
                                                        ShelleyLedgerEra,
                                                        SlotNo, Tx,
                                                        TxBody (ShelleyTxBody))
import qualified Cardano.Api.Shelley                   as C
import           Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError,
                                                        collectPlutusScriptsWithContext,
                                                        evalPlutusScripts)
import           Cardano.Ledger.Alonzo.TxWits          (unTxDats)
import           Cardano.Ledger.Babbage.Tx             (IsValid (..))
import           Cardano.Ledger.BaseTypes              (Globals (systemStart),
                                                        epochInfo, getVersion,
                                                        pvMajor)
import           Cardano.Ledger.Conway                 (Conway)
import           Cardano.Ledger.Conway.Tx              (AlonzoTx (..))
import qualified Cardano.Ledger.Core                   as Core
import           Cardano.Ledger.Crypto                 (StandardCrypto)
import           Cardano.Ledger.Plutus.Evaluate        (PlutusWithContext (..),
                                                        ScriptResult (..))
import           Cardano.Ledger.Plutus.Language        (LegacyPlutusArgs (..),
                                                        PlutusArgs (PlutusV1Args, PlutusV2Args, PlutusV3Args),
                                                        PlutusScriptContext,
                                                        unPlutusBinary)
import qualified Cardano.Ledger.Plutus.Language        as Plutus.Language
import           Cardano.Ledger.Shelley.API            (AccountState (..),
                                                        Coin (..),
                                                        LedgerEnv (..),
                                                        UTxO (..), UtxoEnv (..),
                                                        Validated,
                                                        initialFundsPseudoTxIn)
import qualified Cardano.Ledger.Shelley.API
import           Cardano.Ledger.Shelley.LedgerState    (LedgerState (..),
                                                        UTxOState (..),
                                                        certDStateL,
                                                        delegations,
                                                        lsCertStateL, rewards,
                                                        smartUTxOState)
import           Cardano.Ledger.UMap                   (RDPair (..),
                                                        domRestrictedMap,
                                                        fromCompact)
import qualified Cardano.Ledger.Val                    as Val
import           Control.Lens                          (_1, _3, over, to, view,
                                                        (%=), (&), (.~), (^.))
import           Control.Monad                         (forM)
import           Control.Monad.Except                  (ExceptT,
                                                        MonadError (throwError),
                                                        runExceptT)
import           Control.Monad.IO.Class                (MonadIO)
import           Control.Monad.Primitive               (PrimMonad (..))
import           Control.Monad.Reader                  (MonadReader, ReaderT,
                                                        ask, asks, local,
                                                        runReaderT)
import           Control.Monad.State                   (MonadState, StateT, get,
                                                        gets, put, runStateT)
import           Control.Monad.Trans.Class             (MonadTrans (..))
import qualified Convex.CardanoApi.Lenses              as L
import           Convex.Class                          (ExUnitsError (..),
                                                        MockChainState (..),
                                                        MonadBlockchain (..),
                                                        MonadDatumQuery (..),
                                                        MonadMockchain (..),
                                                        MonadUtxoQuery (..),
                                                        SendTxFailed (..),
                                                        ValidationError (..),
                                                        _ApplyTxFailure,
                                                        _Phase1Error,
                                                        _Phase2Error,
                                                        _PredicateFailures,
                                                        _VExUnits, datums, env,
                                                        failedTransactions,
                                                        modifyUtxo, poolState,
                                                        transactions)
import           Convex.Constants                      (ERA)
import           Convex.MockChain.Defaults             ()
import qualified Convex.MockChain.Defaults             as Defaults
import           Convex.MonadLog                       (MonadLog (..))
import           Convex.NodeParams                     (NodeParams (..))
import           Convex.Utils                          (slotToUtcTime)
import           Convex.Utxos                          (UtxoSet (..),
                                                        fromApiUtxo,
                                                        onlyCredential,
                                                        onlyCredentials)
import           Convex.Wallet                         (Wallet, addressInEra,
                                                        paymentCredential)
import           Data.Bifunctor                        (Bifunctor (..))
import           Data.Default                          (Default (def))
import           Data.Foldable                         (for_, traverse_)
import           Data.Functor.Identity                 (Identity (..))
import qualified Data.Map                              as Map
import qualified Data.Set                              as Set
import           Ouroboros.Consensus.Shelley.Eras      (EraCrypto)
import qualified PlutusCore                            as PLC
import           PlutusLedgerApi.Common                (PlutusLedgerLanguage (..),
                                                        mkTermToEvaluate)
import qualified PlutusLedgerApi.Common                as Plutus
import qualified PlutusLedgerApi.V3                    as PV3
import qualified UntypedPlutusCore                     as UPLC

{-| Apply the plutus script to all its arguments and return a plutus
program
-}
fullyAppliedScript :: NodeParams -> PlutusWithContext StandardCrypto -> Either String (UPLC.Program UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
fullyAppliedScript params pwc@PlutusWithContext{pwcScript} = do
  let plutus = either id Plutus.Language.plutusFromRunnable pwcScript
      binScript = Plutus.Language.plutusBinary plutus
      pv = Plutus.MajorProtocolVersion $ getVersion $ pvMajor (Defaults.protVer params)
  let -- pArgs = getPlutusArgs pwcArgs
      lng =
        case Plutus.Language.plutusLanguage plutus of
          Plutus.Language.PlutusV1 -> PlutusV1
          Plutus.Language.PlutusV2 -> PlutusV2
          Plutus.Language.PlutusV3 -> PlutusV3
      pArgs = getPlutusArgs pwc
  scriptForEval <- first show $ Plutus.deserialiseScript lng pv (unPlutusBinary binScript)
  appliedTerm <- first show $ mkTermToEvaluate lng pv scriptForEval pArgs
  pure $ UPLC.Program () PLC.latestVersion appliedTerm

getPlutusArgs :: PlutusWithContext StandardCrypto -> [Plutus.Data]
getPlutusArgs pwc =
  withRunnablePlutusWithContext
    pwc
    (const [])
    plutusArgs

withRunnablePlutusWithContext ::
  PlutusWithContext c ->
  -- | Handle the decoder failure
  (PV3.EvaluationError -> a) ->
  (forall l. Plutus.Language.PlutusLanguage l => Plutus.Language.PlutusRunnable l -> PlutusArgs l -> a) ->
  a
withRunnablePlutusWithContext PlutusWithContext {pwcProtocolVersion, pwcScript, pwcArgs} onError f =
  case pwcScript of
    Right pr -> f pr pwcArgs
    Left plutus ->
      case Plutus.Language.decodePlutusRunnable pwcProtocolVersion plutus of
        Right pr -> f pr pwcArgs
        Left err -> onError (PV3.CodecError err)

plutusArgs :: forall l. Plutus.Language.PlutusLanguage l => Plutus.Language.PlutusRunnable l -> PlutusArgs l -> [PV3.Data]
plutusArgs _runnable args = case Plutus.Language.isLanguage @l of
  Plutus.Language.SPlutusV1 -> getPlutusArgsV1 args
  Plutus.Language.SPlutusV2 -> getPlutusArgsV2 args
  Plutus.Language.SPlutusV3 -> getPlutusArgsV3 args

getPlutusArgsV1 :: PlutusArgs Plutus.Language.PlutusV1 -> [Plutus.Data]
getPlutusArgsV1 = \case
  PlutusV1Args args -> legacyPlutusArgsToData args

getPlutusArgsV2 :: PlutusArgs Plutus.Language.PlutusV2 -> [Plutus.Data]
getPlutusArgsV2 = \case
  PlutusV2Args args -> legacyPlutusArgsToData args

getPlutusArgsV3 :: PlutusArgs Plutus.Language.PlutusV3 -> [Plutus.Data]
getPlutusArgsV3 = \case
  PlutusV3Args context -> [PV3.toData context]

legacyPlutusArgsToData :: Plutus.ToData (PlutusScriptContext l) => LegacyPlutusArgs l -> [Plutus.Data]
legacyPlutusArgsToData = \case
  LegacyPlutusArgs2 redeemer scriptContext -> [redeemer, PV3.toData scriptContext]
  LegacyPlutusArgs3 datum redeemer scriptContext -> [datum, redeemer, PV3.toData scriptContext]

initialState :: NodeParams -> MockChainState
initialState params = initialStateFor params []

genesisUTxO ::
  forall era capiEra.
  (EraCrypto era ~ StandardCrypto, Core.EraTxOut era) =>
  [(AddressInEra capiEra, Coin)] ->
  UTxO era
genesisUTxO utxos =
  UTxO $
    Map.fromList
      [ (txIn, txOut)
        | (C.toShelleyAddr -> addr, amount) <- utxos,
          let txIn = initialFundsPseudoTxIn addr
              txOut = Core.mkBasicTxOut addr (Val.inject amount)
      ]

type InitialUTXOs = [(Wallet, Coin)]

{-| Initialise the 'MockChainState' with a list of UTxOs
-}
initialStateFor ::
  NodeParams ->
  InitialUTXOs -> -- List of UTXOs at each wallet's address. Can have multiple entries per wallet.
  MockChainState
initialStateFor params@NodeParams{npNetworkId} utxos =
  let utxo = genesisUTxO @ERA @C.ConwayEra (fmap (first (addressInEra npNetworkId)) utxos)
  in MockChainState
      { mcsEnv =
          LedgerEnv
            { ledgerSlotNo = 0
            , ledgerIx = minBound
            , ledgerPp = Defaults.pParams params
            , ledgerAccount = AccountState (Coin 0) (Coin 0)
            }
      , mcsPoolState = LedgerState
          { lsUTxOState = smartUTxOState (Defaults.pParams params) utxo (Coin 0) (Coin 0) def (Coin 0)
          , lsCertState = def
          }
      , mcsTransactions = []
      , mcsFailedTransactions = []
      , mcsDatums = Map.empty
      }

utxoEnv :: NodeParams -> SlotNo -> UtxoEnv ERA
utxoEnv params slotNo = UtxoEnv slotNo (Defaults.pParams params) def

{-| Compute the exunits of a transaction
-}
getTxExUnits ::
  NodeParams ->
  UTxO ERA ->
  C.Tx C.ConwayEra ->
  Either ExUnitsError (Map.Map C.ScriptWitnessIndex C.ExecutionUnits)
getTxExUnits NodeParams{npSystemStart, npEraHistory, npProtocolParameters} utxo (C.getTxBody -> tx) =
  case C.evaluateTransactionExecutionUnits C.ConwayEra npSystemStart (C.toLedgerEpochInfo npEraHistory) npProtocolParameters (fromLedgerUTxO C.ShelleyBasedEraConway utxo) tx of
    Left e      -> Left (Phase1Error e)
    Right rdmrs -> traverse (either (Left . Phase2Error) (Right . snd)) rdmrs

applyTransaction :: NodeParams -> MockChainState -> C.Tx C.ConwayEra -> Either ValidationError (MockChainState, Validated (Core.Tx ERA))
applyTransaction params state tx'@(C.ShelleyTx _era tx) = do
  let currentSlot = state ^. env . L.slot
      utxoState_ = state ^. poolState . L.utxoState
      utxo = utxoState_ ^. L._UTxOState (C.unLedgerProtocolParameters $ npProtocolParameters params) . _1
  (vtx, scripts) <- first PredicateFailures (constructValidated (Defaults.globals params) (utxoEnv params currentSlot) utxoState_ tx)
  result <- applyTx params state vtx scripts

  -- Not sure if this step is needed.
  _ <- first VExUnits (getTxExUnits params utxo tx')

  pure result

{-| Evaluate a transaction, returning all of its script contexts.
-}
evaluateTx :: NodeParams -> SlotNo -> UTxO ERA -> C.Tx C.ConwayEra -> Either ValidationError [PlutusWithContext StandardCrypto]
evaluateTx params slotNo utxo (C.ShelleyTx _ tx) = do
    (vtx, scripts) <- first PredicateFailures (constructValidated (Defaults.globals params) (utxoEnv params slotNo) (lsUTxOState (mcsPoolState state)) tx)
    _ <- applyTx params state vtx scripts
    pure scripts
  where
    state =
      initialState params
        & env . L.slot .~ slotNo
        & poolState . L.utxoState . L._UTxOState (C.unLedgerProtocolParameters $ npProtocolParameters params) . _1 .~ utxo

-- | Construct a 'ValidatedTx' from a 'Core.Tx' by setting the `IsValid`
-- flag.
--
-- Note that this simply constructs the transaction; it does not validate
-- anything other than the scripts. Thus the resulting transaction may be
-- completely invalid.
--
-- Copied from cardano-ledger as it was removed there
-- in https://github.com/input-output-hk/cardano-ledger/commit/721adb55b39885847562437a6fe7e998f8e48c03
constructValidated ::
  forall m.
  ( MonadError [CollectError Conway] m
  ) =>
  Globals ->
  UtxoEnv Conway ->
  UTxOState Conway ->
  Core.Tx Conway ->
  m (AlonzoTx Conway, [PlutusWithContext StandardCrypto])
constructValidated globals (UtxoEnv _ pp _) st tx =
  case collectPlutusScriptsWithContext ei sysS pp tx utxo of
    Left errs -> throwError errs
    Right sLst ->
      let scriptEvalResult = evalPlutusScripts @(EraCrypto Conway) sLst
          vTx =
            AlonzoTx
              (body tx)
              (wits tx) -- (getField @"wits" tx)
              (IsValid (lift_ scriptEvalResult))
              (auxiliaryData tx) -- (getField @"auxiliaryData" tx)
       in pure (vTx, sLst)
  where
    utxo = utxosUtxo st
    sysS = systemStart globals
    ei = epochInfo globals
    lift_ (Passes _)  = True
    lift_ (Fails _ _) = False

applyTx ::
  NodeParams ->
  MockChainState ->
  Core.Tx ERA ->
  [PlutusWithContext StandardCrypto] ->
  Either ValidationError (MockChainState, Validated (Core.Tx ERA))
applyTx params oldState@MockChainState{mcsEnv, mcsPoolState} tx context = do
  (newMempool, vtx) <- first ApplyTxFailure (Cardano.Ledger.Shelley.API.applyTx (Defaults.globals params) mcsEnv mcsPoolState tx)
  return (oldState & poolState .~ newMempool & over transactions ((vtx,context) :) , vtx)

newtype MockchainT m a = MockchainT (ReaderT NodeParams (StateT MockChainState (ExceptT MockchainError m)) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLog)

instance PrimMonad m => PrimMonad (MockchainT m) where
  type PrimState (MockchainT m) = PrimState m
  {-# INLINEABLE primitive #-}
  primitive f = lift (primitive f)

instance MonadTrans MockchainT where
  lift = MockchainT . lift . lift . lift

instance Monad m => MonadReader NodeParams (MockchainT m) where
  ask = MockchainT ask
  local f (MockchainT m) = MockchainT $ local f m

instance Monad m => MonadState MockChainState (MockchainT m) where
  get = MockchainT $ lift get
  put = MockchainT . lift . put

data MockchainError =
  FailWith String
  deriving (Show)

instance Monad m => MonadFail (MockchainT m) where
  fail = MockchainT . throwError . FailWith

instance Monad m => MonadBlockchain (MockchainT m) where
  sendTx tx = MockchainT $ do
    nps <- ask
    addDatumHashes tx
    st <- get
    case applyTransaction nps st tx of
      Left err       -> do
        failedTransactions %= ((:) (tx, err))
        return $ Left $ SendTxFailed $ show err
      Right (st', _) ->
        let C.Tx body _ = tx
        in put st' >> return (Right $ C.getTxId body)
  utxoByTxIn txIns = MockchainT $ do
    nps <- ask
    C.UTxO mp <- gets (view $ poolState . L.utxoState . L._UTxOState (Defaults.pParams nps) . _1 . to (fromLedgerUTxO C.ShelleyBasedEraConway))
    let mp' = Map.restrictKeys mp txIns
    pure (C.UTxO mp')
  queryProtocolParameters = MockchainT (asks npProtocolParameters)
  queryStakeAddresses creds nid = MockchainT $ do
    dState <- gets (view $ poolState . lsCertStateL . certDStateL)
    let
      creds' = toLedgerStakeCredentials creds
      rewards' = domRestrictedMap creds' (rewards dState)
      delegations' = domRestrictedMap creds' (delegations dState)
      rewardsMap =
        Map.fromList
          $ bimap
              fromLedgerStakeAddress
              (C.lovelaceToQuantity . fromCompact . rdReward) <$> Map.toList rewards'
      delegationsMap =
        Map.fromList
          $ bimap
              fromLedgerStakeAddress
              StakePoolKeyHash <$> Map.toList delegations'
    pure (rewardsMap, delegationsMap)
    where
      toLedgerStakeCredentials creds' = Set.fromList $ C.toShelleyStakeCredential <$> Set.toList creds'
      fromLedgerStakeAddress = C.makeStakeAddress nid . C.fromShelleyStakeCredential
  queryStakePools = MockchainT (asks npStakePools)
  networkId = MockchainT (asks npNetworkId)
  querySystemStart = MockchainT (asks npSystemStart)
  queryEraHistory = MockchainT (asks npEraHistory)
  querySlotNo = MockchainT $ do
    st <- get
    NodeParams{npSystemStart, npEraHistory, npSlotLength} <- ask
    let slotNo = st ^. env . L.slot
    utime <- either (throwError . FailWith) pure (slotToUtcTime npEraHistory npSystemStart slotNo)
    return (slotNo, npSlotLength, utime)

instance Monad m => MonadMockchain (MockchainT m) where
  modifyMockChainState f = MockchainT $ get >>= \st -> let (s, a) = f st in put s >> pure a
  askNodeParams = ask

instance Monad m => MonadUtxoQuery (MockchainT m) where
  utxosByPaymentCredentials cred = do
    UtxoSet utxos <- fmap (onlyCredentials cred) utxoSet
    let
      resolveDatum :: C.TxOutDatum C.CtxUTxO era -> MockchainT m (Maybe C.HashableScriptData)
      resolveDatum C.TxOutDatumNone         = pure Nothing
      resolveDatum (C.TxOutDatumHash _ dh)  = queryDatumFromHash dh
      resolveDatum (C.TxOutDatumInline _ d) = pure $ Just d

    resolvedUtxos <- forM utxos $ \(txOut@(C.InAnyCardanoEra _ (C.TxOut _ _ d _)), _) -> do
      resolvedDatum <- resolveDatum d
      pure (txOut, resolvedDatum)
    pure $ UtxoSet resolvedUtxos

instance Monad m => MonadDatumQuery (MockchainT m) where
  queryDatumFromHash dh = MockchainT (gets (Map.lookup dh . view datums))

{-| Add all datums from the transaction to the map of known datums
-}
addDatumHashes :: MonadState MockChainState m => Tx ConwayEra -> m ()
addDatumHashes (C.Tx (ShelleyTxBody C.ShelleyBasedEraConway txBody _scripts scriptData _auxData _) _witnesses) = do
  let txOuts = C.fromLedgerTxOuts C.ShelleyBasedEraConway txBody scriptData

  let insertHashableScriptData hashableScriptData =
        datums %= Map.insert (C.hashScriptDataBytes hashableScriptData) hashableScriptData

  for_ txOuts $ \(view (L._TxOut . _3) -> txDat) -> case txDat of
    C.TxOutDatumInline _ dat -> insertHashableScriptData dat
    _                        -> pure ()

  case scriptData of
    C.TxBodyScriptData _ (unTxDats -> txDats) _redeemers -> do
      traverse_ (insertHashableScriptData . C.fromAlonzoData) txDats
    _ -> pure ()

{-| All transaction outputs
-}
utxoSet :: MonadMockchain m => m (UtxoSet C.CtxUTxO ())
utxoSet =
  let f utxos = (utxos, fromApiUtxo () $ C.inAnyCardanoEra C.cardanoEra $ fromLedgerUTxO C.ShelleyBasedEraConway utxos)
  in modifyUtxo f

{-| The wallet's transaction outputs on the mockchain
-}
walletUtxo :: MonadMockchain m => Wallet -> m (UtxoSet C.CtxUTxO ())
walletUtxo wallet = do
  fmap (onlyCredential (paymentCredential wallet)) utxoSet

{-| Run the 'MockchainT' action with the @NodeParams@ from an initial state
-}
runMockchainT :: MockchainT m a -> NodeParams -> MockChainState -> m (Either MockchainError (a, MockChainState))
runMockchainT (MockchainT action) nps state =
  runExceptT (runStateT (runReaderT action nps) state)

type Mockchain a = MockchainT Identity a

runMockchain :: Mockchain a -> NodeParams -> MockChainState -> Either MockchainError (a, MockChainState)
runMockchain action nps = runIdentity . runMockchainT action nps

{-| Run the mockchain action with an initial distribution, using the default node parameters
-}
runMockchain0 :: InitialUTXOs -> Mockchain a -> Either MockchainError (a, MockChainState)
runMockchain0 dist = runMockchain0With dist Defaults.nodeParams

{-| Run the mockchain action with an initial distribution and a given set of node params
-}
runMockchain0With :: InitialUTXOs -> NodeParams -> Mockchain a -> Either MockchainError (a, MockChainState)
runMockchain0With dist params action = runMockchain action params (initialStateFor params dist)

evalMockchainT :: Functor m => MockchainT m a -> NodeParams -> MockChainState -> m (Either MockchainError a)
evalMockchainT action nps = fmap (fmap fst) . runMockchainT action nps

evalMockchain :: Mockchain a -> NodeParams -> MockChainState -> Either MockchainError a
evalMockchain action nps = runIdentity . evalMockchainT action nps

evalMockchain0 :: InitialUTXOs -> Mockchain a -> Either MockchainError a
evalMockchain0 dist action = evalMockchain action Defaults.nodeParams (initialStateFor Defaults.nodeParams dist)

execMockchainT :: Functor m => MockchainT m a -> NodeParams -> MockChainState -> m (Either MockchainError MockChainState)
execMockchainT action nps = fmap (fmap snd) . runMockchainT action nps

execMockchain :: Mockchain a -> NodeParams -> MockChainState -> Either MockchainError MockChainState
execMockchain action nps = runIdentity . execMockchainT action nps

execMockchain0 :: InitialUTXOs -> Mockchain a -> Either MockchainError MockChainState
execMockchain0 dist action = execMockchain action Defaults.nodeParams (initialStateFor Defaults.nodeParams dist)

type MockchainIO a = MockchainT IO a

runMockchainIO :: MockchainIO a -> NodeParams -> MockChainState -> IO (Either MockchainError (a, MockChainState))
runMockchainIO action nps = runMockchainT action nps

{-| Run the mockchain IO action with an initial distribution, using the default node parameters
-}
runMockchain0IO :: InitialUTXOs -> MockchainIO a -> IO (Either MockchainError (a, MockChainState))
runMockchain0IO dist = runMockchain0IOWith dist Defaults.nodeParams

{-| Run the mockchain IO action with an initial distribution and a given set of node params
-}
runMockchain0IOWith :: InitialUTXOs -> NodeParams -> MockchainIO a -> IO (Either MockchainError (a, MockChainState))
runMockchain0IOWith dist params action = runMockchainIO action params (initialStateFor params dist)

evalMockchainIO :: MockchainIO a -> NodeParams -> MockChainState -> IO (Either MockchainError a)
evalMockchainIO action nps = evalMockchainT action nps

evalMockchain0IO :: InitialUTXOs -> MockchainIO a -> IO (Either MockchainError a)
evalMockchain0IO dist action = evalMockchainIO action Defaults.nodeParams (initialStateFor Defaults.nodeParams dist)

execMockchainIO :: MockchainIO a -> NodeParams -> MockChainState -> IO (Either MockchainError MockChainState)
execMockchainIO action nps = execMockchainT action nps

execMockchain0IO :: InitialUTXOs -> MockchainIO a -> IO (Either MockchainError MockChainState)
execMockchain0IO dist action = execMockchainIO action Defaults.nodeParams (initialStateFor Defaults.nodeParams dist)

-- not exported by cardano-api 1.35.3 (though it seems like it's exported in 1.36)
fromLedgerUTxO :: ShelleyLedgerEra era ~ ledgerera
               => EraCrypto ledgerera ~ StandardCrypto
               => C.ShelleyBasedEra era
               -> UTxO ledgerera
               -> C.UTxO era
fromLedgerUTxO era (UTxO utxo) =
  C.UTxO
  . Map.fromList
  . map (bimap C.fromShelleyTxIn (C.fromShelleyTxOut era))
  . Map.toList
  $ utxo
