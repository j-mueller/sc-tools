# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

* `convex-base`
  - `Convex.PlutusLedger.transAddressInEra`
  - `Convex.Utxos.singleton`
  - `Convex.Utxos`: Added redeemer and ex units to `RemoveUtxoEvent`
  - `Convex.Lenses`: Added `_TxOutDatumInline`, `_TxOutDatumInTx`, `_ScriptData`
* `convex-devnet`: Some haddocks in `Convex.Devnet.NodeQueries`
* `convex-mockchain`: 
  - `Convex.MockChain`: Support for profiling plutus scripts. `evaluateTx` returns the script contexts for a transaction. These can be turned into a fully applied script with `fullyAppliedScript`

## [0.0.1] - 2023-04-26

* Initial release
