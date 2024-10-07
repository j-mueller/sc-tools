{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns       #-}
-- A plutus validator that only succeeds if the redeemer is identical to the script's input index
module Scripts.MatchingIndex(
    validator
  , mintingPolicy
) where

import           PlutusLedgerApi.V1.Scripts  (Redeemer (..))
import           PlutusLedgerApi.V1.Value    (flattenValue)
import           PlutusLedgerApi.V3.Contexts (ScriptContext (..),
                                              ScriptInfo (..), TxInInfo (..),
                                              TxInfo (..))
import qualified PlutusTx.Builtins.Internal  as BI
import           PlutusTx.IsData.Class       (UnsafeFromData (unsafeFromBuiltinData))
import           PlutusTx.Prelude            (BuiltinData, BuiltinUnit)
import qualified PlutusTx.Prelude            as P

{-# INLINABLE validator #-}
validator :: BuiltinData -> BuiltinUnit
validator (unsafeFromBuiltinData -> ScriptContext{scriptContextScriptInfo=SpendingScript txOutRef _, scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextRedeemer=(unsafeFromBuiltinData P.. getRedeemer -> idx :: P.Integer)}) =
  let isOwnIndex TxInInfo{txInInfoOutRef} = txInInfoOutRef P.== txOutRef
      ownIndex   = P.findIndex isOwnIndex txInfoInputs
  in if ownIndex P.== (P.Just idx) then BI.unitval else P.traceError "Different indices"
validator _ = P.error ()

{-# INLINABLE mintingPolicy #-}
mintingPolicy :: BuiltinData -> BuiltinUnit
mintingPolicy (unsafeFromBuiltinData -> ScriptContext{scriptContextScriptInfo=MintingScript ownCs, scriptContextTxInfo=TxInfo{txInfoMint}, scriptContextRedeemer=(unsafeFromBuiltinData P.. getRedeemer -> idx :: P.Integer)}) =
  let mintList = flattenValue txInfoMint
      isOwnIndex (cs,_,_) = cs P.== ownCs
      ownIndex = P.findIndex isOwnIndex mintList
  in if ownIndex P.== (P.Just idx) then BI.unitval else P.traceError "Different indices"
mintingPolicy _ = P.error ()
