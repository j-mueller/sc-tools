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
  -- * Etc.
  extractTx,
  txnUtxos,
  slotToUtcTime,
  utcTimeToSlot,
  utcTimeToSlotUnsafe
) where

import           Cardano.Api                              (BabbageEra,
                                                           Block (..),
                                                           BlockInMode (..),
                                                           CardanoMode,
                                                           NetworkId,
                                                           PlutusScript,
                                                           PlutusScriptV1,
                                                           PlutusScriptV2,
                                                           SlotNo, Tx, TxIn)
import qualified Cardano.Api.Shelley                      as C
import           Cardano.Slotting.EpochInfo.API           (EpochInfo,
                                                           epochInfoSlotToUTCTime,
                                                           hoistEpochInfo)
import qualified Cardano.Slotting.Time                    as Time
import           Control.Monad                            (void, when)
import           Control.Monad.Except                     (runExcept)
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Data.Aeson                               (Result (..),
                                                           fromJSON, object,
                                                           (.=))
import           Data.Bifunctor                           (Bifunctor (..))
import           Data.Either                              (fromRight)
import           Data.Foldable                            (traverse_)
import           Data.Function                            ((&))
import           Data.Proxy                               (Proxy (..))
import           Data.Set                                 (Set)
import qualified Data.Set                                 as Set
import           Data.Time.Clock                          (NominalDiffTime,
                                                           UTCTime)
import qualified Ouroboros.Consensus.HardFork.History     as Consensus
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry

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
  in C.makeShelleyAddressInEra network (C.PaymentCredentialByScript hash) C.NoStakeAddress

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
  in C.makeShelleyAddressInEra network (C.PaymentCredentialByScript hash) C.NoStakeAddress

s :: String -> String
s = id

{-| Search for interesting transactions in a block and serialise them to JSON files
-}
extractTx :: forall m. MonadIO m => Set C.TxId -> BlockInMode CardanoMode -> m ()
extractTx txIds =
  let extractTx' :: C.Tx C.BabbageEra -> m ()
      extractTx' tx@(C.Tx txBody _) = do
        let txi = C.getTxId txBody
        when (txi `Set.member` txIds) $
          void $ liftIO $ C.writeFileTextEnvelope (show txi <> ".json") Nothing tx
  in \case
    BlockInMode (Block _ txns) C.BabbageEraInCardanoMode ->
      traverse_ extractTx' txns
    _                                                    -> pure ()

{-| The UTxOs produced by the transaction
-}
txnUtxos :: Tx era -> [(TxIn, C.TxOut C.CtxTx era)]
txnUtxos tx =
  let C.TxBody C.TxBodyContent{C.txOuts} = C.getTxBody tx
      txi  = C.getTxId (C.getTxBody tx)
  in take (length txOuts) (zip (C.TxIn txi . C.TxIx <$> [0..]) txOuts)

{-| Convert a slot number to UTC time
-}
slotToUtcTime :: C.EraHistory mode -> C.SystemStart -> SlotNo -> Either String UTCTime
slotToUtcTime (toLedgerEpochInfo -> info) systemStart slot = epochInfoSlotToUTCTime info systemStart slot

{-| Convert a UTC time to slot no. Returns the time spent and time left in this slot.
-}
utcTimeToSlot :: C.EraHistory mode -> C.SystemStart -> UTCTime -> Either String (SlotNo, NominalDiffTime, NominalDiffTime)
utcTimeToSlot (C.EraHistory _ interpreter) systemStart t = first show $
  Qry.interpretQuery interpreter (Qry.wallclockToSlot (Time.toRelativeTime systemStart t))

{-| Convert a UTC time to slot no. Returns the time spent and time left in this slot.
Extends the interpreter range to infinity before running the query (ignoring
any future hard forks)
-}
utcTimeToSlotUnsafe :: C.EraHistory mode -> C.SystemStart -> UTCTime -> (SlotNo, NominalDiffTime, NominalDiffTime)
utcTimeToSlotUnsafe (C.EraHistory _ interpreter) systemStart t = fromRight (error "utcTimeToSlotUnsafe: interpretQuery failed unexpectedly") $
  Qry.interpretQuery (Qry.unsafeExtendSafeZone interpreter) (Qry.wallclockToSlot (Time.toRelativeTime systemStart t))

-- FIXME: Looks like this function is exposed by Cardano.Api in cardano-node@v1.36
toLedgerEpochInfo :: C.EraHistory mode -> EpochInfo (Either String)
toLedgerEpochInfo (C.EraHistory _ interpreter) =
  hoistEpochInfo (first show . runExcept) $
    Consensus.interpreterToEpochInfo interpreter
