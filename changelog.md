# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changes

* Bump `cardano-api`, `cardano-node`
* Updated the [conway genesis configuration file](node-config/mainnet/mainnet-conway-genesis.json) for mainnet
* When selecting public-key UTxOs during coin selection, outputs that are incompatible with PlutusV1 scripts are excluded.
* Fixed a bug in coin selection where the wallet's mixed inputs were not considered for a `TxBodyContent` with zero inputs

### Added

- `Eq` and `Ord` instances for `Operator`
- `Convex.BuildTx`: Added the option to look at all of a transaction's inputs when building a transaction

### Deleted

## [0.3.0.0]

### Changes

- Move `Convex.BuildTx` from `convex-coin-selection` to `convex-base`
- Added a `Tracer m TxBalancingMessage` argument to the coin selection functions. This prints out useful information about decisions taken during coin selection and balancing. Instantiate with `mempty` to ignore the messages.
- Export `Convex.Wallet.MockWallet.w4`
- The balancing algorithm now correctly calculates the number of required signatures when computing the transaction fee
- Deleted spurious `runQuery'` log messages
- The constraints for most of the functions in `Convex.CoinSelection` have changed from `MonadFail m` to `MonadError BalanceTxError m`, allowing for better error handling
- Relaxed the `MonadError` instance of `MonadBlockchainCardanoNodeT` by removing the `MonadError e m` constraint; fixed the implementation of `catchError`
- `Convex.BuildTx`: Ensure that at least 3 Ada is present when computing minimum UTxO value in `minAdaDeposit`.

### Added

- More fine-grained functions in `Convex.Wallet.Operator` and `Convex.Query` signing & balancing transactions
- JSON instances for `CoinSelectionError` and `BalancingError`
- Add `Convex.Devnet.CardanoNode.withCardanoNodeDevnetConfig` and associated types for modifications to the genesis config files. Includes `allowLargeTransactions` for large transactions.
- MonadLog instance for `MockchainT`
- API documentation published on github pages (https://j-mueller.github.io/sc-tools/)
- A mockchain test that shows how to increase the maximum tx size
- `Convex.MockChain.Utils.runMockchainPropWith` to run a mockchain action in a quickcheck property with custom node params and initial distribution

### Deleted

- `Convex.Era`, `Convex.Measure`, `Convex.Event` modules

## [0.2.0.0]

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
  - `Convex.PlutusLedger`: `transAddressInEra`, prisms and isos for the `Interval` type, `transValue`
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
  - `Convex.MockChain`: Add `resolveDatumHash` function, giving access to all datums that were previously seen
  - `Convex.NodeParams`: Add lenses for `ProtocolParameters` type from `cardano-api`
  - `Convex.MockChain`: Add `runMockchain0IOWith` to allow easy manipulation of the protocol parameters for emulator tests, incl. `mockchainSucceedsWith` and `mockchainFailsWith` in `Convex.MockChain.Utils`
* `convex-node-client`: Add `Convex.NodeClient.WaitForTxnClient`
* `Convex.BuildTx`:
  - Add a monadic (writer) interface for building transactions
  - Add `addRequiredSignature`, `prependTxOut`, `payToPlutusV2InlineDatum`, `spendPlutusV2InlineDatum` functions
  - Add `spendPlutusV2RefWithInlineDatum`, `spendPlutusV2RefWithoutInRef` and `spendPlutusV2RefWithoutInRefInlineDatum` functions
  - Add `payToPlutusV2InlineWithDatum` and `payToPlutusV2InlineWithInlineDatum` functions
  - Add `mintPlutusV2Ref` function
* Add `querySlotNo` to `MonadBlockchain` typeclass and update both blockchain and mockchain implementations.
* Add `utcTimeToPosixTime`, `toShelleyPaymentCredential` in `Convex.Utils`.
* Considering explicit error type `MonadBlockchainError` for `MonadBlockchainCardanoNodeT` to enable proper error handling by caller.
* `convex-wallet`:
  - Add HTTP server
  - Add `Convex.Wallet.Operator` for managing credentials
* `convex-coin-selection`:
  - Add `Convex.Query` for UTxO queries, add convex-wallet backend for operator UTxOs
  - Add `utxosByPaymentCredentials` to `MonadUtxoQuery`
* Add `_PlutusScriptWitnessV2` to `Convex.Lenses`

### Deleted

* Deleted the `trading-bot` and `muesli` packages.
* Deleted `spendPlutusV1Ref` as it does not make sense.

## [0.0.1] - 2023-04-26

* Initial release
