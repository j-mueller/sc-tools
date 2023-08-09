{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Building transactions
-}
module Convex.BuildTx(
  TxBuild,
  -- * Building transactions
  spendPublicKeyOutput,
  payToAddress,
  payToAddressTxOut,
  payToPublicKey,
  payToScriptHash,
  payToPlutusV1,
  payToPlutusV2,
  spendPlutusV1,
  spendPlutusV2,
  spendPlutusV2Ref,
  mintPlutusV1,
  mintPlutusV2,
  payToPlutusV2Inline,
  addReference,
  addCollateral,
  addAuxScript,
  assetValue,
  setScriptsValid,
  -- * Minimum Ada deposit
  minAdaDeposit,
  setMinAdaDeposit,
  setMinAdaDepositAll
  ) where

import           Cardano.Api.Shelley (Hash, HashableScriptData, NetworkId,
                                      PaymentKey, PlutusScript, PlutusScriptV1,
                                      PlutusScriptV2, ScriptHash)
import qualified Cardano.Api.Shelley as C
import           Control.Lens        (_1, _2, at, mapped, over, set, (&))
import qualified Convex.Lenses       as L
import           Convex.Scripts      (toHashableScriptData)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import qualified PlutusLedgerApi.V1  as Plutus

type TxBuild = C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra

{-| Spend an output locked by a public key
-}
spendPublicKeyOutput :: C.TxIn -> TxBuild
spendPublicKeyOutput txIn =
  let wit = C.BuildTxWith (C.KeyWitness (C.KeyWitnessForSpending))
  in over L.txIns ((txIn, wit) :)

spendPlutusV1 :: forall datum redeemer. (Plutus.ToData datum, Plutus.ToData redeemer) => C.TxIn -> PlutusScript PlutusScriptV1 -> datum -> redeemer -> TxBuild
spendPlutusV1 txIn s (toHashableScriptData -> dat) (toHashableScriptData -> red) =
  let wit = C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1 (C.PScript s) (C.ScriptDatumForTxIn dat) red (C.ExecutionUnits 0 0)
      wit' = C.BuildTxWith (C.ScriptWitness C.ScriptWitnessForSpending wit)
  in over L.txIns ((txIn, wit') :) . setScriptsValid

spendPlutusV2 :: forall datum redeemer. (Plutus.ToData datum, Plutus.ToData redeemer) => C.TxIn -> PlutusScript PlutusScriptV2 -> datum -> redeemer -> TxBuild
spendPlutusV2 txIn s (toHashableScriptData -> dat) (toHashableScriptData -> red) =
  let wit = C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 (C.PScript s) (C.ScriptDatumForTxIn dat) red (C.ExecutionUnits 0 0)
      wit' = C.BuildTxWith (C.ScriptWitness C.ScriptWitnessForSpending wit)
  in over L.txIns ((txIn, wit') :) . setScriptsValid

spendPlutusV2Ref :: forall datum redeemer. (Plutus.ToData datum, Plutus.ToData redeemer) => C.TxIn -> C.TxIn -> Maybe C.ScriptHash -> datum -> redeemer -> TxBuild
spendPlutusV2Ref txIn refTxIn sh (toHashableScriptData -> dat) (toHashableScriptData -> red) =
  let wit = C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 (C.PReferenceScript refTxIn sh) (C.ScriptDatumForTxIn dat) red (C.ExecutionUnits 0 0)
      wit' = C.BuildTxWith (C.ScriptWitness C.ScriptWitnessForSpending wit)
  in over L.txIns ((txIn, wit') :) . setScriptsValid . addReference refTxIn

mintPlutusV1 :: forall redeemer. (Plutus.ToData redeemer) => PlutusScript PlutusScriptV1 -> redeemer -> C.AssetName -> C.Quantity -> TxBuild
mintPlutusV1 script (toHashableScriptData -> red) assetName quantity =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV1 script)
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
mintPlutusV2 script (toHashableScriptData -> red) assetName quantity =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
      v = assetValue sh assetName quantity
      policyId = C.PolicyId sh
      wit      = C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 (C.PScript script) (C.NoScriptDatumForMint) red (C.ExecutionUnits 0 0)
  in over (L.txMintValue . L._TxMintValue) (over _1 (<> v) . over _2 (Map.insert policyId wit))
      . setScriptsValid

addCollateral :: C.TxIn -> TxBuild
addCollateral i = over (L.txInsCollateral . L._TxInsCollateral) ((:) i)

addReference :: C.TxIn -> TxBuild
addReference i = over (L.txInsReference . L._TxInsReference) ((:) i)

addAuxScript :: C.ScriptInEra C.BabbageEra -> TxBuild
addAuxScript s = over (L.txAuxScripts . L._TxAuxScripts) ((:) s)

payToAddressTxOut :: C.AddressInEra C.BabbageEra -> C.Value -> C.TxOut C.CtxTx C.BabbageEra
payToAddressTxOut addr vl = C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra vl) C.TxOutDatumNone C.ReferenceScriptNone

payToAddress :: C.AddressInEra C.BabbageEra -> C.Value -> TxBuild
payToAddress addr vl = over L.txOuts ((:) (payToAddressTxOut addr vl))

payToPublicKey :: NetworkId -> Hash PaymentKey -> C.Value -> TxBuild
payToPublicKey network pk vl =
  let val = C.TxOutValue C.MultiAssetInBabbageEra vl
      addr = C.makeShelleyAddressInEra network (C.PaymentCredentialByKey pk) C.NoStakeAddress
      txo = C.TxOut addr val C.TxOutDatumNone C.ReferenceScriptNone
  in over L.txOuts ((:) txo)

payToScriptHash :: NetworkId -> ScriptHash -> HashableScriptData -> C.StakeAddressReference -> C.Value -> TxBuild
payToScriptHash network script datum stakeAddress vl =
  let val = C.TxOutValue C.MultiAssetInBabbageEra vl
      addr = C.makeShelleyAddressInEra network (C.PaymentCredentialByScript script) stakeAddress
      dat = C.TxOutDatumInTx C.ScriptDataInBabbageEra datum
      txo = C.TxOut addr val dat C.ReferenceScriptNone
  in over L.txOuts ((:) txo)

payToPlutusV1 :: forall a. Plutus.ToData a => NetworkId -> PlutusScript PlutusScriptV1 -> a -> C.StakeAddressReference -> C.Value -> TxBuild
payToPlutusV1 network s datum stakeRef vl =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV1 s)
      dt = toHashableScriptData datum
  in payToScriptHash network sh dt stakeRef vl

payToPlutusV2 :: forall a. Plutus.ToData a => NetworkId -> PlutusScript PlutusScriptV2 -> a -> C.StakeAddressReference -> C.Value -> TxBuild
payToPlutusV2 network s datum stakeRef vl =
  let sh = C.hashScript (C.PlutusScript C.PlutusScriptV2 s)
      dt = toHashableScriptData datum
  in payToScriptHash network sh dt stakeRef vl

payToPlutusV2Inline :: C.AddressInEra C.BabbageEra -> PlutusScript PlutusScriptV2 -> C.Value -> TxBuild
payToPlutusV2Inline addr script vl =
  let txo = C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra vl) C.TxOutDatumNone (C.ReferenceScript C.ReferenceTxInsScriptsInlineDatumsInBabbageEra (C.toScriptInAnyLang $ C.PlutusScript C.PlutusScriptV2 script))
  in over L.txOuts ((:) txo)

-- TODO: Functions for building outputs (Output -> Output)

setScriptsValid :: C.TxBodyContent v C.BabbageEra -> C.TxBodyContent v C.BabbageEra
setScriptsValid = set L.txScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptValid)

{-| Set the Ada component in an output's value to at least the amount needed to cover the
minimum UTxO deposit for this output
-}
setMinAdaDeposit :: C.BundledProtocolParameters C.BabbageEra -> C.TxOut C.CtxTx C.BabbageEra -> C.TxOut C.CtxTx C.BabbageEra
setMinAdaDeposit params txOut =
  let minUtxo = minAdaDeposit params txOut
  in txOut & over (L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId) (maybe (Just minUtxo) (Just . max minUtxo))

minAdaDeposit :: C.BundledProtocolParameters C.BabbageEra -> C.TxOut C.CtxTx C.BabbageEra -> C.Quantity
minAdaDeposit params txOut =
  let txo = txOut
              -- set the Ada value to a dummy amount to ensure that it is not 0 (if it was 0, the size of the output
              -- would be smaller, causing 'calculateMinimumUTxO' to compute an amount that is a little too small)
              & over (L._TxOut . _2 . L._TxOutValue . L._Value . at C.AdaAssetId) (maybe (Just $ C.Quantity 3_000_000) Just)
  in fromMaybe (C.Quantity 0) $ do
        let C.Lovelace l = C.calculateMinimumUTxO C.ShelleyBasedEraBabbage txo params
        pure (C.Quantity l)

{-| Apply 'setMinAdaDeposit' to all outputs
-}
setMinAdaDepositAll :: C.BundledProtocolParameters C.BabbageEra -> TxBuild
setMinAdaDepositAll params = over (L.txOuts . mapped) (setMinAdaDeposit params)
