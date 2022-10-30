{-# LANGUAGE DataKinds #-}
{-| Building transactions
-}
module Convex.BuildTx(
  -- * Building transactions
  spendPublicKeyOutput,
  payToAddress,
  payToPublicKey,
  payToScriptHash,
  payToPlutusV1,
  payToPlutusV2,
  spendPlutusV1,
  spendPlutusV2,
  addCollateral
  ) where

import           Cardano.Api.Shelley  (Hash, NetworkId, PaymentKey,
                                       PlutusScript, PlutusScriptV1,
                                       PlutusScriptV2, ScriptData, ScriptHash)
import qualified Cardano.Api.Shelley  as C
import           Control.Lens         (over, set)
import qualified Convex.Lenses        as L
import qualified Plutus.V1.Ledger.Api as Plutus

{-| Spend an output locked by a public key
-}
spendPublicKeyOutput :: C.TxIn -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
spendPublicKeyOutput txIn =
  let wit = C.BuildTxWith (C.KeyWitness (C.KeyWitnessForSpending))
  in over L.txIns ((txIn, wit) :)

spendPlutusV1 :: forall datum redeemer. (Plutus.ToData datum, Plutus.ToData redeemer) => C.TxIn -> PlutusScript PlutusScriptV1 -> datum -> redeemer -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
spendPlutusV1 txIn s d r =
  let dat = C.fromPlutusData (Plutus.toData d)
      red = C.fromPlutusData (Plutus.toData r)
      wit = C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1 (C.PScript s) (C.ScriptDatumForTxIn dat) red (C.ExecutionUnits 0 0)
      wit' = C.BuildTxWith (C.ScriptWitness C.ScriptWitnessForSpending wit)
  in over L.txIns ((txIn, wit') :)
      . set L.txScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptValid)

spendPlutusV2 :: forall datum redeemer. (Plutus.ToData datum, Plutus.ToData redeemer) => C.TxIn -> PlutusScript PlutusScriptV2 -> datum -> redeemer -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
spendPlutusV2 txIn s d r =
  let dat = C.fromPlutusData (Plutus.toData d)
      red = C.fromPlutusData (Plutus.toData r)
      wit = C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 (C.PScript s) (C.ScriptDatumForTxIn dat) red (C.ExecutionUnits 0 0)
      wit' = C.BuildTxWith (C.ScriptWitness C.ScriptWitnessForSpending wit)
  in over L.txIns ((txIn, wit') :)

addCollateral :: C.TxIn -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
addCollateral i = over (L.txInsCollateral . L._TxInsCollateral) ((:) i)

payToAddress :: C.AddressInEra C.BabbageEra -> C.Value -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
payToAddress addr vl =
  let txo = C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra vl) C.TxOutDatumNone C.ReferenceScriptNone
  in over L.txOuts ((:) txo)

payToPublicKey :: NetworkId -> Hash PaymentKey -> C.Value -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
payToPublicKey network pk vl =
  let val = C.TxOutValue C.MultiAssetInBabbageEra vl
      addr = C.makeShelleyAddressInEra network (C.PaymentCredentialByKey pk) C.NoStakeAddress
      txo = C.TxOut addr val C.TxOutDatumNone C.ReferenceScriptNone
  in over L.txOuts ((:) txo)

payToScriptHash :: NetworkId -> ScriptHash -> ScriptData -> C.Value -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
payToScriptHash network script datum vl =
  let val = C.TxOutValue C.MultiAssetInBabbageEra vl
      addr = C.makeShelleyAddressInEra network (C.PaymentCredentialByScript script) C.NoStakeAddress
      dat = C.TxOutDatumInTx C.ScriptDataInBabbageEra datum
      txo = C.TxOut addr val dat C.ReferenceScriptNone
  in over L.txOuts ((:) txo)

payToPlutusV1 :: forall a. Plutus.ToData a => NetworkId -> PlutusScript PlutusScriptV1 -> a -> C.Value -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
payToPlutusV1 network s datum vl =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV1 s)
      dt = C.fromPlutusData (Plutus.toData datum)
  in payToScriptHash network sh dt vl

payToPlutusV2 :: forall a. Plutus.ToData a => NetworkId -> PlutusScript PlutusScriptV2 -> a -> C.Value -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
payToPlutusV2 network s datum vl =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV2 s)
      dt = C.fromPlutusData (Plutus.toData datum)
  in payToScriptHash network sh dt vl
