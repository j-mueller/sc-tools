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

import           PlutusLedgerApi.V2.Contexts (ScriptContext (..),
                                            ScriptPurpose (..), TxInInfo (..),
                                            TxInfo (..))
import           PlutusLedgerApi.V1.Value    (flattenValue)
import           PlutusTx.IsData.Class     (UnsafeFromData (unsafeFromBuiltinData))
import           PlutusTx.Prelude          (BuiltinData)
import qualified PlutusTx.Prelude          as P

{-# INLINABLE validator #-}
validator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validator _datum (unsafeFromBuiltinData -> idx :: P.Integer) (unsafeFromBuiltinData -> ScriptContext{scriptContextPurpose=Spending txOutRef, scriptContextTxInfo=TxInfo{txInfoInputs}}) =
  let isOwnIndex TxInInfo{txInInfoOutRef} = txInInfoOutRef P.== txOutRef
      ownIndex   = P.findIndex isOwnIndex txInfoInputs
  in if ownIndex P.== (P.Just idx) then () else P.traceError "Different indices"
validator _ _ _ = P.error ()

{-# INLINABLE mintingPolicy #-}
mintingPolicy :: BuiltinData -> BuiltinData -> ()
mintingPolicy (unsafeFromBuiltinData -> idx :: P.Integer) (unsafeFromBuiltinData -> ScriptContext{scriptContextPurpose=Minting ownCs, scriptContextTxInfo=TxInfo{txInfoMint}}) =
  let mintList = flattenValue txInfoMint
      isOwnIndex (cs,_,_) = cs P.== ownCs
      ownIndex = P.findIndex isOwnIndex mintList
  in if ownIndex P.== (P.Just idx) then () else P.traceError "Different indices"
mintingPolicy _ _ = P.error ()
