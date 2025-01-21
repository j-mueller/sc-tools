{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- A plutus validator that only succeeds if the redeemer is identical to the script's input index
module Scripts.MatchingIndex (
  validator,
  mintingPolicy,
) where

import PlutusLedgerApi.V1.Scripts (Redeemer (..))
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V3.Contexts (
  ScriptContext (..),
  ScriptInfo (..),
  TxInInfo (..),
  TxInfo (..),
 )
import PlutusLedgerApi.V3.MintValue (mintValueMinted)
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.IsData.Class (UnsafeFromData (unsafeFromBuiltinData))
import PlutusTx.Prelude (BuiltinData, BuiltinUnit)
import PlutusTx.Prelude qualified as P

{-# INLINEABLE validator #-}
validator :: BuiltinData -> BuiltinUnit
validator (unsafeFromBuiltinData -> ScriptContext{scriptContextScriptInfo = SpendingScript txOutRef _, scriptContextTxInfo = TxInfo{txInfoInputs}, scriptContextRedeemer = (unsafeFromBuiltinData P.. getRedeemer -> idx :: P.Integer)}) =
  let isOwnIndex TxInInfo{txInInfoOutRef} = txInInfoOutRef P.== txOutRef
      ownIndex = P.findIndex isOwnIndex txInfoInputs
   in if ownIndex P.== (P.Just idx) then BI.unitval else P.traceError "Different indices"
validator _ = P.error ()

{-# INLINEABLE mintingPolicy #-}
mintingPolicy :: BuiltinData -> BuiltinUnit
mintingPolicy (unsafeFromBuiltinData -> ScriptContext{scriptContextScriptInfo = MintingScript ownCs, scriptContextTxInfo = TxInfo{txInfoMint}, scriptContextRedeemer = (unsafeFromBuiltinData P.. getRedeemer -> idx :: P.Integer)}) =
  let mintList = flattenValue (mintValueMinted txInfoMint)
      isOwnIndex (cs, _, _) = cs P.== ownCs
      ownIndex = P.findIndex isOwnIndex mintList
   in if ownIndex P.== (P.Just idx) then BI.unitval else P.traceError "Different indices"
mintingPolicy _ = P.error ()
