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
  mintPlutusV1,
  mintPlutusV2,
  addCollateral,
  assetValue
  ) where

import           Cardano.Api.Shelley  (Hash, NetworkId, PaymentKey,
                                       PlutusScript, PlutusScriptV1,
                                       PlutusScriptV2, ScriptData, ScriptHash)
import qualified Cardano.Api.Shelley  as C
import           Control.Lens         (_1, _2, over, set)
import qualified Convex.Lenses        as L
import qualified Data.Map             as Map
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
  in over L.txIns ((txIn, wit') :) . setScriptsValid

spendPlutusV2 :: forall datum redeemer. (Plutus.ToData datum, Plutus.ToData redeemer) => C.TxIn -> PlutusScript PlutusScriptV2 -> datum -> redeemer -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
spendPlutusV2 txIn s d r =
  let dat = C.fromPlutusData (Plutus.toData d)
      red = C.fromPlutusData (Plutus.toData r)
      wit = C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 (C.PScript s) (C.ScriptDatumForTxIn dat) red (C.ExecutionUnits 0 0)
      wit' = C.BuildTxWith (C.ScriptWitness C.ScriptWitnessForSpending wit)
  in over L.txIns ((txIn, wit') :) . setScriptsValid

mintPlutusV1 :: forall redeemer. (Plutus.ToData redeemer) => PlutusScript PlutusScriptV1 -> redeemer -> C.AssetName -> C.Quantity -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
mintPlutusV1 script redeemer assetName quantity =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV1 script)
      red = C.fromPlutusData (Plutus.toData redeemer)
      v = assetValue sh assetName quantity
      policyId = C.PolicyId sh
      wit      = C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1 (C.PScript script) (C.NoScriptDatumForMint) red (C.ExecutionUnits 0 0)
  in over (L.txMintValue . L._TxMintValue) (over _1 (<> v) . over _2 (Map.insert policyId wit))
      . setScriptsValid

{-| A value containing the given amount of the native asset
-}
assetValue :: ScriptHash -> C.AssetName -> C.Quantity -> C.Value
assetValue hsh assetName quantity =
  C.valueFromList [(C.AssetId (C.PolicyId hsh) assetName, quantity)]

mintPlutusV2 :: forall redeemer. (Plutus.ToData redeemer) => PlutusScript PlutusScriptV2 -> redeemer -> C.AssetName -> C.Quantity -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
mintPlutusV2 script redeemer assetName quantity =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
      red = C.fromPlutusData (Plutus.toData redeemer)
      v = assetValue sh assetName quantity
      policyId = C.PolicyId sh
      wit      = C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 (C.PScript script) (C.NoScriptDatumForMint) red (C.ExecutionUnits 0 0)
  in over (L.txMintValue . L._TxMintValue) (over _1 (<> v) . over _2 (Map.insert policyId wit))
      . setScriptsValid

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

setScriptsValid :: C.TxBodyContent v C.BabbageEra -> C.TxBodyContent v C.BabbageEra
setScriptsValid = set L.txScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptValid)
