{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
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
  ScriptContext,
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

import           Cardano.Api.Shelley                   (AddressInEra,
                                                        BabbageEra,
                                                        Hash (StakePoolKeyHash),
                                                        ScriptData,
                                                        ShelleyLedgerEra,
                                                        SlotNo, Tx,
                                                        TxBody (ShelleyTxBody),
                                                        makeStakeAddress)
import qualified Cardano.Api.Shelley                   as Cardano.Api
import           Cardano.Ledger.Alonzo.Data            (Data)
import qualified Cardano.Ledger.Alonzo.Data            as Ledger
import           Cardano.Ledger.Alonzo.Language        (Language (..))
import           Cardano.Ledger.Alonzo.PlutusScriptApi (CollectError,
                                                        collectTwoPhaseScriptInputs,
                                                        evalScripts)
import           Cardano.Ledger.Alonzo.Scripts         (CostModel, CostModels,
                                                        ExUnits, Script,
                                                        unCostModels)
import           Cardano.Ledger.Alonzo.Tools           (TransactionScriptFailure)
import qualified Cardano.Ledger.Alonzo.Tools
import           Cardano.Ledger.Alonzo.Tx              (ValidatedTx (..))
import           Cardano.Ledger.Alonzo.TxInfo          (ExtendedUTxO,
                                                        ScriptResult (..),
                                                        TranslationError)
import qualified Cardano.Ledger.Alonzo.TxInfo          as Ledger
import           Cardano.Ledger.Alonzo.TxWitness       (RdmrPtr, unTxDats)
import qualified Cardano.Ledger.Alonzo.TxWitness       as Alonzo
import           Cardano.Ledger.Babbage.PParams        (PParams' (..))
import           Cardano.Ledger.Babbage.Tx             (IsValid (..))
import           Cardano.Ledger.BaseTypes              (Globals (systemStart),
                                                        ProtVer, epochInfo)
import qualified Cardano.Ledger.Core                   as Core
import           Cardano.Ledger.Crypto                 (StandardCrypto)
import           Cardano.Ledger.Era                    (Crypto, Era,
                                                        ValidateScript)
import           Cardano.Ledger.Shelley.API            (AccountState (..),
                                                        ApplyTxError, Coin (..),
                                                        GenDelegs (..),
                                                        LedgerEnv (..),
                                                        MempoolEnv,
                                                        MempoolState, TxIn,
                                                        UTxO (..), UtxoEnv (..),
                                                        Validated,
                                                        initialFundsPseudoTxIn)
import qualified Cardano.Ledger.Shelley.API
import           Cardano.Ledger.Shelley.Constraints    (UsesTxOut (..))
import           Cardano.Ledger.Shelley.LedgerState    (DPState (..),
                                                        LedgerState (..),
                                                        UTxOState (..), rewards,
                                                        smartUTxOState)
import           Cardano.Ledger.Shelley.TxBody         (DCert, Wdrl)
import           Cardano.Ledger.ShelleyMA.Timelocks    (ValidityInterval)
import qualified Cardano.Ledger.Val                    as Val
import           Control.Lens                          (_1, _3, over, set, to,
                                                        view, (%=), (&), (.~),
                                                        (^.))
import           Control.Lens.TH                       (makeLensesFor,
                                                        makePrisms)
import           Control.Monad.Except                  (ExceptT,
                                                        MonadError (throwError),
                                                        runExceptT)
import           Control.Monad.IO.Class                (MonadIO)
import           Control.Monad.Reader                  (ReaderT, ask, asks,
                                                        runReaderT)
import           Control.Monad.State                   (MonadState, StateT, get,
                                                        gets, modify, put,
                                                        runStateT)
import           Control.Monad.Trans.Class             (MonadTrans (..))
import           Convex.Class                          (MonadBlockchain (..),
                                                        MonadMockchain (..),
                                                        SendTxFailed (..))
import           Convex.Era                            (ERA)
import qualified Convex.Lenses                         as L
import           Convex.MockChain.Defaults             ()
import qualified Convex.MockChain.Defaults             as Defaults
import           Convex.MonadLog                       (MonadLog (..))
import           Convex.NodeParams                     (NodeParams (..))
import           Convex.Utils                          (slotToUtcTime)
import           Convex.Utxos                          (UtxoSet (..),
                                                        fromApiUtxo,
                                                        onlyCredential)
import           Convex.Wallet                         (Wallet, addressInEra,
                                                        paymentCredential)
import           Data.Array                            (array)
import           Data.Bifunctor                        (Bifunctor (..))
import           Data.ByteString.Short                 (ShortByteString)
import           Data.Default                          (Default (def))
import           Data.Foldable                         (for_, traverse_)
import           Data.Functor.Identity                 (Identity (..))
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Proxy                            (Proxy (..))
import           Data.Sequence.Strict                  (StrictSeq)
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Data.UMap                             (delView, rewView, (⋪))
import           GHC.Records                           (HasField (..))
import           Plutus.ApiCommon                      (mkTermToEvaluate)
import qualified Plutus.ApiCommon                      as Plutus
import qualified PlutusCore                            as PLC
import qualified UntypedPlutusCore                     as UPLC

{-| All the information needed to evaluate a Plutus script: The script itself, the
script language, redeemers and datums, execution units required, and the cost model.
-}
type ScriptContext era = (ShortByteString, Language, [Data era], ExUnits, CostModel)

    -- let pv = Ledger.transProtocolVersion (_protocolVersion pp)
    --     pArgs = Ledger.getPlutusData <$> arguments
    -- appliedTerm <- left show $ mkTermToEvaluate Plutus.PlutusV2 pv script pArgs
    -- pure $ UPLC.Program () (PLC.defaultVersion ()) appliedTerm

{-| Apply the plutus script to all its arguments and return a plutus
program
-}
fullyAppliedScript :: NodeParams -> ScriptContext ERA -> Either String (UPLC.Program UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
fullyAppliedScript params (script, lang, arguments, _, _) = do
  let pv = Ledger.transProtocolVersion (_protocolVersion $ Defaults.pParams params)
      pArgs = Ledger.getPlutusData <$> arguments
      lng = case lang of
              PlutusV1 -> Plutus.PlutusV1
              PlutusV2 -> Plutus.PlutusV2
  appliedTerm <- first show $ mkTermToEvaluate lng pv script pArgs
  pure $ UPLC.Program () (PLC.defaultVersion ()) appliedTerm

data ExUnitsError =
  Phase1Error (TranslationError StandardCrypto)
  | Phase2Error (TransactionScriptFailure StandardCrypto)
  deriving (Eq, Show)

makePrisms ''ExUnitsError

data ValidationError =
  VExUnits ExUnitsError
  | PredicateFailures [CollectError (Crypto ERA)]
  | ApplyTxFailure (ApplyTxError ERA)
  deriving (Eq, Show)

makePrisms ''ValidationError

{-| State of the mockchain
-}
data MockChainState =
  MockChainState
    { mcsEnv                :: MempoolEnv ERA
    , mcsPoolState          :: MempoolState ERA
    , mcsTransactions       :: [(Validated (Core.Tx ERA), [ScriptContext ERA])] -- ^ Transactions that were submitted to the mockchain and validated
    , mcsFailedTransactions :: [(Tx BabbageEra, ValidationError)] -- ^ Transactions that were submitted to the mockchain, but failed with a validation error
    , mcsDatums             :: Map (Hash ScriptData) ScriptData
    }

makeLensesFor
  [ ("mcsEnv", "env")
  , ("mcsPoolState", "poolState")
  , ("mcsTransactions", "transactions")
  , ("mcsFailedTransactions", "failedTransactions")
  , ("mcsDatums", "datums")
  ] ''MockChainState

initialState :: NodeParams -> MockChainState
initialState params = initialStateFor params []

genesisUTxO ::
  forall era capiEra.
  (Era era, UsesTxOut era, Crypto era ~ StandardCrypto) =>
  [(AddressInEra capiEra, Coin)] ->
  UTxO era
genesisUTxO utxos =
  UTxO $
    Map.fromList
      [ (txIn, txOut)
        | (Cardano.Api.toShelleyAddr -> addr, amount) <- utxos,
          let txIn = initialFundsPseudoTxIn addr
              txOut = makeTxOut (Proxy @era) addr (Val.inject amount)
      ]

type InitialUTXOs = [(Wallet, Coin)]

{-| Initialise the 'MockChainState' with a list of UTxOs
-}
initialStateFor ::
  NodeParams ->
  InitialUTXOs -> -- List of UTXOs at each wallet's address. Can have multiple entries per wallet.
  MockChainState
initialStateFor params@NodeParams{npNetworkId} utxos =
  let utxo = genesisUTxO @ERA @Cardano.Api.BabbageEra (fmap (first (addressInEra npNetworkId)) utxos)
  in MockChainState
      { mcsEnv =
          LedgerEnv
            { ledgerSlotNo = 0
            , ledgerIx = minBound
            , ledgerPp = Defaults.pParams params
            , ledgerAccount = AccountState (Coin 0) (Coin 0)
            }
      , mcsPoolState = LedgerState
          { lsUTxOState = smartUTxOState utxo (Coin 0) (Coin 0) def
          , lsDPState = DPState def def
          }
      , mcsTransactions = []
      , mcsFailedTransactions = []
      , mcsDatums = Map.empty
      }

utxoEnv :: NodeParams -> SlotNo -> UtxoEnv ERA
utxoEnv params slotNo = UtxoEnv slotNo (Defaults.pParams params) mempty (GenDelegs mempty)

{-| Compute the exunits of a transaction
-}
getTxExUnits ::
  NodeParams ->
  UTxO ERA ->
  Cardano.Api.Tx Cardano.Api.BabbageEra ->
  Either ExUnitsError (Map.Map RdmrPtr ExUnits)
getTxExUnits nodeParams utxo (Cardano.Api.ShelleyTx _ tx) =
  let pParams = Defaults.pParams nodeParams
      globals = Defaults.globals nodeParams
      ei = epochInfo globals
      ss = systemStart globals
      costmdls = array (minBound, maxBound) . Map.toList $ unCostModels $ getField @"_costmdls" pParams
  in
    case Cardano.Ledger.Alonzo.Tools.evaluateTransactionExecutionUnits pParams tx utxo ei ss costmdls of
      Left e      -> Left (Phase1Error e)
      Right rdmrs -> traverse (either (Left . Phase2Error) Right) rdmrs

applyTransaction :: NodeParams -> MockChainState -> Cardano.Api.Tx Cardano.Api.BabbageEra -> Either ValidationError (MockChainState, Validated (Core.Tx ERA))
applyTransaction params state tx'@(Cardano.Api.ShelleyTx _era tx) = do
  let currentSlot = state ^. env . L.slot
      utxoState_ = state ^. poolState . L.utxoState
      utxo = utxoState_ ^. L._UTxOState . _1
  (vtx, scripts) <- first PredicateFailures (constructValidated (Defaults.globals params) (utxoEnv params currentSlot) utxoState_ tx)
  result <- applyTx params state vtx scripts

  -- Not sure if this step is needed.
  _ <- first VExUnits (getTxExUnits params utxo tx')

  pure result

{-| Evaluate a transaction, returning all of its script contexts.
-}
evaluateTx :: NodeParams -> SlotNo -> UTxO ERA -> Cardano.Api.Tx Cardano.Api.BabbageEra -> Either ValidationError [ScriptContext ERA]
evaluateTx params slotNo utxo (Cardano.Api.ShelleyTx _ tx) = do
    (vtx, scripts) <- first PredicateFailures (constructValidated (Defaults.globals params) (utxoEnv params slotNo) (lsUTxOState (mcsPoolState state)) tx)
    _ <- applyTx params state vtx scripts
    pure scripts
  where
    state =
      initialState params
        & env . L.slot .~ slotNo
        & poolState . L.utxoState . L._UTxOState . _1 .~ utxo

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
  forall era m.
  ( MonadError [CollectError (Crypto era)] m,
    Core.Script era ~ Script era,
    Core.Witnesses era ~ Alonzo.TxWitness era,
    ValidateScript era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_costmdls" (Core.PParams era) CostModels,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "vldt" (Core.TxBody era) ValidityInterval,
    ExtendedUTxO era
  ) =>
  Globals ->
  UtxoEnv era ->
  UTxOState era ->
  Core.Tx era ->
  m (ValidatedTx era, [ScriptContext era])
constructValidated globals (UtxoEnv _ pp _ _) st tx =
  case collectTwoPhaseScriptInputs ei sysS pp tx utxo of
    Left errs -> throwError errs
    Right sLst ->
      let scriptEvalResult = evalScripts @era (getField @"_protocolVersion" pp) tx sLst
          vTx =
            ValidatedTx
              (getField @"body" tx)
              (getField @"wits" tx)
              (IsValid (lift_ scriptEvalResult))
              (getField @"auxiliaryData" tx)
       in pure (vTx, sLst)
  where
    utxo = _utxo st
    sysS = systemStart globals
    ei = epochInfo globals
    lift_ (Passes _)  = True
    lift_ (Fails _ _) = False

applyTx ::
  NodeParams ->
  MockChainState ->
  Core.Tx ERA ->
  [ScriptContext ERA] ->
  Either ValidationError (MockChainState, Validated (Core.Tx ERA))
applyTx params oldState@MockChainState{mcsEnv, mcsPoolState} tx context = do
  (newMempool, vtx) <- first ApplyTxFailure (Cardano.Ledger.Shelley.API.applyTx (Defaults.globals params) mcsEnv mcsPoolState tx)
  return (oldState & poolState .~ newMempool & over transactions ((:) (vtx, context)), vtx)

newtype MockchainT m a = MockchainT (ReaderT NodeParams (StateT MockChainState (ExceptT MockchainError m)) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLog)

instance MonadTrans MockchainT where
  lift = MockchainT . lift . lift . lift

data MockchainError =
  FailWith String
  deriving (Eq, Show)

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
        let Cardano.Api.Tx body _ = tx
        in put st' >> return (Right $ Cardano.Api.getTxId body)
  utxoByTxIn txIns = MockchainT $ do
    Cardano.Api.UTxO mp <- gets (view $ poolState . L.utxoState . L._UTxOState . _1 . to (fromLedgerUTxO Cardano.Api.ShelleyBasedEraBabbage))
    let mp' = Map.restrictKeys mp txIns
    pure (Cardano.Api.UTxO mp')
  queryProtocolParameters = MockchainT ((,) <$> asks npProtocolParameters <*> asks npLedgerParams)
  queryStakeAddresses creds nid = MockchainT $ do
    dState <- gets (dpsDState . lsDPState . view poolState)
    let
      rewards' = toLedgerStakeCredentials creds ⋪ rewards dState
      rewardsMap =
        Map.fromList
          $ bimap
              fromLedgerStakeAddress
              Cardano.Api.fromShelleyLovelace <$> Map.toList (rewView rewards')
      poolMap =
        Map.fromList
          $ bimap
              fromLedgerStakeAddress
              StakePoolKeyHash <$> Map.toList (delView rewards')
    pure (rewardsMap, poolMap)
    where
      toLedgerStakeCredentials creds' = Set.fromList $ Cardano.Api.toShelleyStakeCredential <$> Set.toList creds'
      fromLedgerStakeAddress = makeStakeAddress nid . Cardano.Api.fromShelleyStakeCredential
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
  modifySlot f = MockchainT $ do
    s <- gets (view $ env . L.slot)
    let (s', a) = f s
    modify (set (env . L.slot) s')
    pure a
  modifyUtxo f = MockchainT $ do
    u <- gets (view $ poolState . L.utxoState . L._UTxOState . _1)
    let (u', a) = f u
    modify (set (poolState . L.utxoState . L._UTxOState . _1) u')
    pure a
  resolveDatumHash k = MockchainT (gets (Map.lookup k . view datums))


{-| Add all datums from the transaction to the map of known datums
-}
addDatumHashes :: MonadState MockChainState m => Tx BabbageEra -> m ()
addDatumHashes (Cardano.Api.Tx (ShelleyTxBody Cardano.Api.ShelleyBasedEraBabbage txBody _scripts scriptData _auxData _) _witnesses) = do
  let txOuts = Cardano.Api.fromLedgerTxOuts Cardano.Api.ShelleyBasedEraBabbage txBody scriptData

  let insertHashableScriptData scriptDat =
        datums %= Map.insert (Cardano.Api.hashScriptData scriptDat) scriptDat

  for_ txOuts $ \(view (L._TxOut . _3) -> txDat) -> case txDat of
    Cardano.Api.TxOutDatumInline _ dat -> insertHashableScriptData dat
    _                                  -> pure ()

  case scriptData of
    Cardano.Api.TxBodyScriptData Cardano.Api.ScriptDataInBabbageEra (unTxDats -> txDats) _redeemers -> do
      traverse_ (insertHashableScriptData . Cardano.Api.fromAlonzoData) txDats
    _ -> pure ()

{-| All transaction outputs
-}
utxoSet :: MonadMockchain m => m (UtxoSet Cardano.Api.CtxUTxO ())
utxoSet =
  let f (utxos) = (utxos, fromApiUtxo $ fromLedgerUTxO Cardano.Api.ShelleyBasedEraBabbage utxos)
  in modifyUtxo f

{-| The wallet's transaction outputs on the mockchain
-}
walletUtxo :: MonadMockchain m => Wallet -> m (UtxoSet Cardano.Api.CtxUTxO ())
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
               => Crypto ledgerera ~ StandardCrypto
               => Cardano.Api.ShelleyBasedEra era
               -> UTxO ledgerera
               -> Cardano.Api.UTxO era
fromLedgerUTxO era (UTxO utxo) =
  Cardano.Api.UTxO
  . Map.fromList
  . map (bimap Cardano.Api.fromShelleyTxIn (Cardano.Api.fromShelleyTxOut era))
  . Map.toList
  $ utxo
