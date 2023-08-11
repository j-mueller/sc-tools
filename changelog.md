# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

* Update `queryTip` and `queryTipSlotNo` in `Convex.Devnet.NodeQueries` to also return `SlotLength`.
* Rename `toUtxoTx` to `fromUtxoTx` in `Convex.Utxos`
* Add generic return type to `withCardanoNode` and `withCardanoNodeDevnet` in `Convex.Devnet.CardanoNode`.
* Update dependencies to `cardano-api-8.8.0.0` and `cardano-node-8.1.1`. Simplified `cabal.project`
* `Convex.BuildTx`: Include stake reference in `payToScriptHash`, `payToPlutusV1` and `payToPlutusV2`

### Added

* `convex-base`
  - `Convex.PlutusLedger`: `transAddressInEra`, prisms and isos for the `Interval` type
  - `Convex.Utxos.singleton`
  - `Convex.Utxos`: Added redeemer and ex units to `RemoveUtxoEvent`
  - `Convex.Lenses`: Added `_TxOutDatumInline`, `_TxOutDatumInTx`, `_ScriptData`, `_PlutusScriptWitness`, `_TxExtraKeyWitnesses`, prisms for validity intervals, `_TxOutDatumHash`
* `convex-devnet`: Some haddocks in `Convex.Devnet.NodeQueries`
* `convex-mockchain`:
  - `Convex.MockChain`: Support for profiling plutus scripts. `evaluateTx` returns the script contexts for a transaction. These can be turned into a fully applied script with `fullyAppliedScript`.
  - `Convex.MockChain`: Export `fromLedgerUTxO`
  - `Convex.MockChain`: `MonadTrans` instance for `MockchainT`, export constructor
* Add `querySlotNo` to `MonadBlockchain` typeclass and update both blockchain and mockchain implementations.
* Add `utcTimeToPosixTime` in `Convex.Utils`.
* Considering explicit error type `MonadBlockchainError` for `MonadBlockchainCardanoNodeT` to enable proper error handling by caller.

### Deleted

* Deleted the `trading-bot` and `muesli` packages.
* Deleted `spendPlutusV1Ref` and `spendPlutusV2Ref` as they don't make sense.

## [0.0.1] - 2023-04-26

* Initial release
