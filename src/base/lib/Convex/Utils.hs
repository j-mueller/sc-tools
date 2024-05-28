{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
{-| Conversion functions and other conveniences
-}
module Convex.Utils(
  scriptFromCbor,
  unsafeScriptFromCbor,
  scriptAddress,
  -- * Plutus V1
  scriptFromCborV1,
  unsafeScriptFromCborV1,
  scriptAddressV1,
  -- * Serialised transactions
  txFromCbor,
  unsafeTxFromCbor,

  -- * Dealing with errors
  liftResult,
  liftEither,
  mapError,
  failOnLeft,
  failOnLeftLog,
  failOnError,

  -- * Reading key files
  readSigningKeyFromFile,
  readVerificationKeyFromFile,
  readStakingKeyFromFile,

  -- * Etc.
  extractTx,
  txnUtxos,
  slotToUtcTime,
  utcTimeToSlot,
  utcTimeToSlotUnsafe,
  utcTimeToPosixTime,
  posixTimeToSlot,
  posixTimeToSlotUnsafe,
  toShelleyPaymentCredential
) where

import           Cardano.Api                              (BabbageEra,
                                                           BlockInMode (..),
                                                           NetworkId,
                                                           PaymentCredential (..),
                                                           PlutusScript,
                                                           PlutusScriptV1,
                                                           PlutusScriptV2,
                                                           SlotNo, Tx, TxIn)
import qualified Cardano.Api.Shelley                      as C
import qualified Cardano.Ledger.Credential                as Shelley
import           Cardano.Ledger.Crypto                    (StandardCrypto)
import           Cardano.Slotting.EpochInfo.API           (EpochInfo,
                                                           epochInfoSlotToUTCTime,
                                                           hoistEpochInfo)
import qualified Cardano.Slotting.Time                    as Time
import           Control.Monad                            (void, when)
import           Control.Monad.Except                     (MonadError,
                                                           runExcept)
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Control.Monad.Result                     (ResultT, throwError)
import qualified Control.Monad.Result                     as Result
import           Control.Monad.Trans.Except               (ExceptT, runExceptT)
import           Convex.MonadLog                          (MonadLog, logWarnS)
import           Convex.PlutusLedger                      (transPOSIXTime,
                                                           unTransPOSIXTime)
import           Data.Aeson                               (Result (..),
                                                           fromJSON, object,
                                                           (.=))
import           Data.Bifunctor                           (Bifunctor (..))
import           Data.Foldable                            (traverse_)
import           Data.Function                            ((&))
import           Data.Proxy                               (Proxy (..))
import           Data.Set                                 (Set)
import qualified Data.Set                                 as Set
import qualified Data.Text.IO                             as Text
import           Data.Time.Clock                          (NominalDiffTime,
                                                           UTCTime)
import           Data.Time.Clock.POSIX                    (posixSecondsToUTCTime,
                                                           utcTimeToPOSIXSeconds)
import qualified Ouroboros.Consensus.HardFork.History     as Consensus
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import qualified PlutusLedgerApi.V1                       as PV1
import           System.Exit                              (exitFailure)

scriptFromCborV1 :: String -> Either String (PlutusScript PlutusScriptV1)
scriptFromCborV1 cbor = do
  let vl = object ["type" .= s "PlutusScriptV1", "description" .= s "", "cborHex" .= cbor]
  textEnvelope <- fromJSON vl & (\case { Error err -> Left (show err); Success e -> Right e })
  C.deserialiseFromTextEnvelope (C.proxyToAsType $ Proxy @(PlutusScript PlutusScriptV1)) textEnvelope & first show

unsafeScriptFromCborV1 :: String -> PlutusScript PlutusScriptV1
unsafeScriptFromCborV1 = either error id . scriptFromCborV1

{-| Script address without staking key
-}
scriptAddressV1 :: NetworkId -> PlutusScript PlutusScriptV1 -> C.AddressInEra C.BabbageEra
scriptAddressV1 network script =
  let hash = C.hashScript (C.PlutusScript C.PlutusScriptV1 script)
  in C.makeShelleyAddressInEra C.ShelleyBasedEraBabbage network (C.PaymentCredentialByScript hash) C.NoStakeAddress

scriptFromCbor :: String -> Either String (PlutusScript PlutusScriptV2)
scriptFromCbor cbor = do
  let vl = object ["type" .= s "PlutusScriptV2", "description" .= s "", "cborHex" .= cbor]
  textEnvelope <- fromJSON vl & (\case { Error err -> Left (show err); Success e -> Right e })
  C.deserialiseFromTextEnvelope (C.proxyToAsType $ Proxy @(PlutusScript PlutusScriptV2)) textEnvelope & first show

txFromCbor :: String -> Either String (Tx BabbageEra)
txFromCbor cbor = do
  let vl = object ["type" .= s "Tx BabbageEra", "description" .= s "", "cborHex" .= cbor]
  textEnvelope <- fromJSON vl & (\case { Error err -> Left (show err); Success e -> Right e })
  C.deserialiseFromTextEnvelope (C.proxyToAsType $ Proxy @(Tx BabbageEra)) textEnvelope & first show

unsafeScriptFromCbor :: String -> PlutusScript PlutusScriptV2
unsafeScriptFromCbor = either error id . scriptFromCbor

unsafeTxFromCbor :: String -> Tx BabbageEra
unsafeTxFromCbor = either error id . txFromCbor

{-| Script address without staking key
-}
scriptAddress :: NetworkId -> PlutusScript PlutusScriptV2 -> C.AddressInEra C.BabbageEra
scriptAddress network script =
  let hash = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
  in C.makeShelleyAddressInEra C.ShelleyBasedEraBabbage network (C.PaymentCredentialByScript hash) C.NoStakeAddress

s :: String -> String
s = id

{-| Search for interesting transactions in a block and serialise them to JSON files
-}
extractTx :: forall m. MonadIO m => Set C.TxId -> BlockInMode -> m ()
extractTx txIds =
  let extractTx' :: C.Tx C.BabbageEra -> m ()
      extractTx' tx@(C.Tx txBody _) = do
        let txi = C.getTxId txBody
        when (txi `Set.member` txIds) $
          void $ liftIO $ C.writeFileTextEnvelope (C.File $ show txi <> ".json") Nothing tx
  in \case
    BlockInMode C.BabbageEra (C.Block _ txns) ->
      traverse_ extractTx' txns
    _                                                    -> pure ()

{-| The UTxOs produced by the transaction
-}
txnUtxos :: Tx era -> [(TxIn, C.TxOut C.CtxTx era)]
txnUtxos tx =
  let C.TxBody C.TxBodyContent{C.txOuts} = C.getTxBody tx
      txi  = C.getTxId (C.getTxBody tx)
  in zip (C.TxIn txi . C.TxIx <$> [0..]) txOuts

{-| Convert a slot number to UTC time
-}
slotToUtcTime :: C.EraHistory -> C.SystemStart -> SlotNo -> Either String UTCTime
slotToUtcTime (toLedgerEpochInfo -> info) systemStart slot = epochInfoSlotToUTCTime info systemStart slot

{-| Convert a UTC time to slot no. Returns the time spent and time left in this slot.
-}
utcTimeToSlot :: C.EraHistory -> C.SystemStart -> UTCTime -> Either String (SlotNo, NominalDiffTime, NominalDiffTime)
utcTimeToSlot (C.EraHistory interpreter) systemStart t = first show $
  Qry.interpretQuery interpreter (Qry.wallclockToSlot (Time.toRelativeTime systemStart t))

utcTimeToPosixTime :: UTCTime -> PV1.POSIXTime
utcTimeToPosixTime =  transPOSIXTime . utcTimeToPOSIXSeconds

{-| Convert a 'PV1.POSIXTime' to slot no. Returns the time spent and time left in this slot.
-}
posixTimeToSlot :: C.EraHistory -> C.SystemStart -> PV1.POSIXTime -> Either String (SlotNo, NominalDiffTime, NominalDiffTime)
posixTimeToSlot eraHistory systemStart (posixSecondsToUTCTime . unTransPOSIXTime -> utcTime) =
  utcTimeToSlot eraHistory systemStart utcTime

{-| Convert a UTC time to slot no. Returns the time spent and time left in this slot.
Extends the interpreter range to infinity before running the query, ignoring
any future hard forks. This avoids horizon errors for times that are in the future.
It may still fail for times that are in the past (before the beginning of the horizin)
-}
utcTimeToSlotUnsafe :: C.EraHistory -> C.SystemStart -> UTCTime -> Either String (SlotNo, NominalDiffTime, NominalDiffTime)
utcTimeToSlotUnsafe (C.EraHistory interpreter) systemStart t = first show $
  Qry.interpretQuery (Qry.unsafeExtendSafeZone interpreter) (Qry.wallclockToSlot (Time.toRelativeTime systemStart t))

{-| Convert a 'PV1.POSIXTime' to slot no. Returns the time spent and time left in this slot.
-}
posixTimeToSlotUnsafe :: C.EraHistory -> C.SystemStart -> PV1.POSIXTime -> Either String (SlotNo, NominalDiffTime, NominalDiffTime)
posixTimeToSlotUnsafe eraHistory systemStart (posixSecondsToUTCTime . unTransPOSIXTime -> utcTime) =
  utcTimeToSlotUnsafe eraHistory systemStart utcTime

-- FIXME: Looks like this function is exposed by Cardano.Api in cardano-node@v1.36
toLedgerEpochInfo :: C.EraHistory -> EpochInfo (Either String)
toLedgerEpochInfo (C.EraHistory interpreter) =
  hoistEpochInfo (first show . runExcept) $
    Consensus.interpreterToEpochInfo interpreter

liftResult :: (MonadError e m) => (String -> e) -> ResultT m a -> m a
liftResult f action = Result.runResultT action >>= either (throwError . f) pure . Result.toEither

liftEither :: (MonadError e m) => (ee -> e) -> m (Either ee a) -> m a
liftEither f action = action >>= either (throwError . f) pure

mapError :: (MonadError e m) => (ee -> e) -> ExceptT ee m a -> m a
mapError f action = runExceptT action >>= either (throwError . f) pure

failOnLeft :: MonadIO m => (e -> String) -> Either e a -> m a
failOnLeft f = \case
  Left err -> liftIO $ do
    putStrLn (f err)
    exitFailure
  Right x -> pure x

failOnLeftLog :: (MonadLog m, MonadIO m) => (e -> String) -> Either e a -> m a
failOnLeftLog f = \case
  Left err -> do
    logWarnS (f err)
    liftIO exitFailure
  Right x -> pure x

{-| Call @fail@ if there is an error
-}
failOnError :: (MonadFail m, Show e) => ExceptT e m a -> m a
failOnError action = runExceptT action >>= either (fail . show) pure

{-| Read a serialised signing key from a file
-}
readSigningKeyFromFile :: FilePath -> IO (C.SigningKey C.PaymentKey)
readSigningKeyFromFile = readKeyFromFile Proxy

{-| Read a serialised verification key from a file. Try bech32 encoding first, then text envelope (JSON)
-}
readVerificationKeyFromFile :: FilePath -> IO (C.VerificationKey C.PaymentKey)
readVerificationKeyFromFile = readKeyFromFile Proxy

{-| Read a serialised signing key from a file
-}
readStakingKeyFromFile :: FilePath -> IO (C.VerificationKey C.StakeKey)
readStakingKeyFromFile = readKeyFromFile Proxy

{-| Read a serialised key from a file. Try bech32 encoding first, then text envelope (JSON)
-}
readKeyFromFile :: (C.SerialiseAsBech32 key, C.HasTextEnvelope key) => Proxy key -> FilePath -> IO key
readKeyFromFile p source = do
  txt <- Text.readFile source
  case C.deserialiseFromBech32 (C.proxyToAsType p) txt of
    Left err1 -> C.readFileTextEnvelope (C.proxyToAsType p) (C.File source) >>= \case
      Left err2 -> fail ("readKeyFromFile: Failed to read " <> source <> ". Errors: " <> show err1 <> ", " <> show err2)
      Right k -> pure k
    Right k  -> pure k

toShelleyPaymentCredential :: PaymentCredential -> Shelley.PaymentCredential StandardCrypto
toShelleyPaymentCredential (PaymentCredentialByKey (C.PaymentKeyHash kh)) =
    Shelley.KeyHashObj kh
toShelleyPaymentCredential (PaymentCredentialByScript sh) =
    Shelley.ScriptHashObj (C.toShelleyScriptHash sh)
