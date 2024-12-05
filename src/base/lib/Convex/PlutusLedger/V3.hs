{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-| Translating between cardano-api/cardano-ledger and plutus V3 representations
-}
module Convex.PlutusLedger.V3 (
  -- * Tx IDs
  unTransTxOutRef,
  transTxOutRef,
) where

import qualified Cardano.Api.Shelley as C
import qualified PlutusLedgerApi.V3  as PV3
import qualified PlutusTx.Prelude    as PlutusTx

unTransTxOutRef :: PV3.TxOutRef -> Either C.SerialiseAsRawBytesError C.TxIn
unTransTxOutRef PV3.TxOutRef{PV3.txOutRefId = PV3.TxId bs, PV3.txOutRefIdx} =
  let i = C.deserialiseFromRawBytes C.AsTxId $ PlutusTx.fromBuiltin bs
  in C.TxIn <$> i <*> pure (C.TxIx $ fromIntegral txOutRefIdx)

transTxOutRef :: C.TxIn -> PV3.TxOutRef
transTxOutRef (C.TxIn txId (C.TxIx ix)) =
  let i = PV3.TxId $ PlutusTx.toBuiltin $ C.serialiseToRawBytes txId
  in PV3.TxOutRef i (fromIntegral ix)
