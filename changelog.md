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
* `Convex.CoinSelection`:
  - Export `requiredTxIns`, `spentTxIns`
  - Fix a bug in coin selection that added too much output value for non-Ada assets
  - Add a "change output" parameter to the coin selection functions
* Set `slotLength` in `Convex.MockChain.Defaults` to 1 second (it was set to 1000 seconds by accident)
* Change base monad of `mockchainSucceeds` to `IO`
* Change `_PlutusScriptWitness` in `Convex.Lenses` to `_PlutusScriptWitnessV1`
* Increased the amount of UTxOs generated for wallets in the devnet from 1 to 10.
* Relax the context type of `_TxOutDatumInline` from `CtxTx` to `ctx` (any context type)
* Added a field for `(LocalNodeConnectInfo CardanoMode, Env)` to `RunningNode` in `convex-devnet`

### Added

* `convex-base`
  - `Convex.PlutusLedger`: `transAddressInEra`, prisms and isos for the `Interval` type
  - `Convex.Utxos.singleton`
  - `Convex.Utxos`: Added redeemer and ex units to `RemoveUtxoEvent`
  - `Convex.Lenses`: Added `_TxOutDatumInline`, `_TxOutDatumInTx`, `_ScriptData`, `_PlutusScriptWitness`, `_TxExtraKeyWitnesses`, prisms for validity intervals, `_TxOutDatumHash`
* `convex-devnet`:
  - Some haddocks in `Convex.Devnet.NodeQueries`
  - Added `Convex.Devnet.NodeQueries.queryUTxOWhole`
* `convex-mockchain`:
  - `Convex.MockChain`: Support for profiling plutus scripts. `evaluateTx` returns the script contexts for a transaction. These can be turned into a fully applied script with `fullyAppliedScript`.
  - `Convex.MockChain`: Export `fromLedgerUTxO`
  - `Convex.MockChain`: `MonadTrans` instance for `MockchainT`, export constructor
  - `Convex.MockChain`: Add `MockchainIO` type
* `convex-node-client`: Add `Convex.NodeClient.WaitForTxnClient`
* `Convex.BuildTx`:
  - Add a monadic (writer) interface for building transactions
  - Add `addRequiredSignature`, `prependTxOut`, `payToPlutusV2InlineDatum`, `spendPlutusV2InlineDatum` functions
  - Add `spendPlutusV2RefWithInlineDatum`, `spendPlutusV2RefWithoutInRef` and `spendPlutusV2RefWithoutInRefInlineDatum` functions
  - Add `payToPlutusV2InlineWithDatum` and `payToPlutusV2InlineWithInlineDatum` functions
* Add `querySlotNo` to `MonadBlockchain` typeclass and update both blockchain and mockchain implementations.
* Add `utcTimeToPosixTime`, `toShelleyPaymentCredential` in `Convex.Utils`.
* Considering explicit error type `MonadBlockchainError` for `MonadBlockchainCardanoNodeT` to enable proper error handling by caller.
* `convex-wallet`:
  - Add HTTP server
  - Add `Convex.Wallet.Operator` for managing credentials
* `convex-coin-selection`:
  - Add `Convex.Query` for UTxO queries, add convex-wallet backend for operator UTxOs
* Add `_PlutusScriptWitnessV2` to `Convex.Lenses`



### Deleted

* Deleted the `trading-bot` and `muesli` packages.
* Deleted `spendPlutusV1Ref` as it does not make sense.

## [0.0.1] - 2023-04-26

* Initial release
