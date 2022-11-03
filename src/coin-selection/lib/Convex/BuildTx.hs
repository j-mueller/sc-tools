{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-| Building transactions
-}
module Convex.BuildTx(
  TxBuild,
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
  assetValue,
  -- * Minimum Ada deposit
  setMinAdaDeposit,
  setMinAdaDepositAll
  ) where

import           Cardano.Api.Shelley  (Hash, NetworkId, PaymentKey,
                                       PlutusScript, PlutusScriptV1,
                                       PlutusScriptV2, ScriptData, ScriptHash)
import qualified Cardano.Api.Shelley  as C
import           Control.Lens         (_1, _2, at, mapped, over, set, (&))
import qualified Convex.Lenses        as L
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import qualified Plutus.V1.Ledger.Api as Plutus

type TxBuild = C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra

{-| Spend an output locked by a public key
-}
spendPublicKeyOutput :: C.TxIn -> TxBuild
spendPublicKeyOutput txIn =
  let wit = C.BuildTxWith (C.KeyWitness (C.KeyWitnessForSpending))
  in over L.txIns ((txIn, wit) :)

spendPlutusV1 :: forall datum redeemer. (Plutus.ToData datum, Plutus.ToData redeemer) => C.TxIn -> PlutusScript PlutusScriptV1 -> datum -> redeemer -> TxBuild
spendPlutusV1 txIn s d r =
  let dat = C.fromPlutusData (Plutus.toData d)
      red = C.fromPlutusData (Plutus.toData r)
      wit = C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1 (C.PScript s) (C.ScriptDatumForTxIn dat) red (C.ExecutionUnits 0 0)
      wit' = C.BuildTxWith (C.ScriptWitness C.ScriptWitnessForSpending wit)
  in over L.txIns ((txIn, wit') :) . setScriptsValid

spendPlutusV2 :: forall datum redeemer. (Plutus.ToData datum, Plutus.ToData redeemer) => C.TxIn -> PlutusScript PlutusScriptV2 -> datum -> redeemer -> TxBuild
spendPlutusV2 txIn s d r =
  let dat = C.fromPlutusData (Plutus.toData d)
      red = C.fromPlutusData (Plutus.toData r)
      wit = C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 (C.PScript s) (C.ScriptDatumForTxIn dat) red (C.ExecutionUnits 0 0)
      wit' = C.BuildTxWith (C.ScriptWitness C.ScriptWitnessForSpending wit)
  in over L.txIns ((txIn, wit') :) . setScriptsValid

mintPlutusV1 :: forall redeemer. (Plutus.ToData redeemer) => PlutusScript PlutusScriptV1 -> redeemer -> C.AssetName -> C.Quantity -> TxBuild
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

mintPlutusV2 :: forall redeemer. (Plutus.ToData redeemer) => PlutusScript PlutusScriptV2 -> redeemer -> C.AssetName -> C.Quantity -> TxBuild
mintPlutusV2 script redeemer assetName quantity =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
      red = C.fromPlutusData (Plutus.toData redeemer)
      v = assetValue sh assetName quantity
      policyId = C.PolicyId sh
      wit      = C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 (C.PScript script) (C.NoScriptDatumForMint) red (C.ExecutionUnits 0 0)
  in over (L.txMintValue . L._TxMintValue) (over _1 (<> v) . over _2 (Map.insert policyId wit))
      . setScriptsValid

addCollateral :: C.TxIn -> TxBuild
addCollateral i = over (L.txInsCollateral . L._TxInsCollateral) ((:) i)

payToAddress :: C.AddressInEra C.BabbageEra -> C.Value -> TxBuild
payToAddress addr vl =
  let txo = C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra vl) C.TxOutDatumNone C.ReferenceScriptNone
  in over L.txOuts ((:) txo)

payToPublicKey :: NetworkId -> Hash PaymentKey -> C.Value -> TxBuild
payToPublicKey network pk vl =
  let val = C.TxOutValue C.MultiAssetInBabbageEra vl
      addr = C.makeShelleyAddressInEra network (C.PaymentCredentialByKey pk) C.NoStakeAddress
      txo = C.TxOut addr val C.TxOutDatumNone C.ReferenceScriptNone
  in over L.txOuts ((:) txo)

payToScriptHash :: NetworkId -> ScriptHash -> ScriptData -> C.Value -> TxBuild
payToScriptHash network script datum vl =
  let val = C.TxOutValue C.MultiAssetInBabbageEra vl
      addr = C.makeShelleyAddressInEra network (C.PaymentCredentialByScript script) C.NoStakeAddress
      dat = C.TxOutDatumInTx C.ScriptDataInBabbageEra datum
      txo = C.TxOut addr val dat C.ReferenceScriptNone
  in over L.txOuts ((:) txo)

payToPlutusV1 :: forall a. Plutus.ToData a => NetworkId -> PlutusScript PlutusScriptV1 -> a -> C.Value -> TxBuild
payToPlutusV1 network s datum vl =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV1 s)
      dt = C.fromPlutusData (Plutus.toData datum)
  in payToScriptHash network sh dt vl

payToPlutusV2 :: forall a. Plutus.ToData a => NetworkId -> PlutusScript PlutusScriptV2 -> a -> C.Value -> TxBuild
payToPlutusV2 network s datum vl =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV2 s)
      dt = C.fromPlutusData (Plutus.toData datum)
  in payToScriptHash network sh dt vl

setScriptsValid :: C.TxBodyContent v C.BabbageEra -> C.TxBodyContent v C.BabbageEra
setScriptsValid = set L.txScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptValid)

{-| Set the Ada component in an output's value to at least the amount needed to cover the
minimum UTxO deposit for this output
-}
setMinAdaDeposit :: C.ProtocolParameters -> C.TxOut C.CtxTx C.BabbageEra -> C.TxOut C.CtxTx C.BabbageEra
setMinAdaDeposit params txOut =
  let txo = txOut
              -- set the Ada value to a dummy amount to ensure that it is not 0 (if it was 0, the size of the output
              -- would be smaller, causing 'calculateMinimumUTxO' to compute an amount that is a little too small)
              & over (L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId) (maybe (Just $ C.Quantity 3_000_000) Just)
      minUtxo = fromMaybe (C.Quantity 0) $ do
        k <- either (const Nothing) pure (C.calculateMinimumUTxO C.ShelleyBasedEraBabbage txo params)
        C.Lovelace l <- C.valueToLovelace k
        pure (C.Quantity l)
  in txOut & over (L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId) (maybe (Just minUtxo) (Just . max minUtxo))

{-| Apply 'setMinAdaDeposit' to all outputs
-}
setMinAdaDepositAll :: C.ProtocolParameters -> TxBuild
setMinAdaDepositAll params = over (L.txOuts . mapped) (setMinAdaDeposit params)
