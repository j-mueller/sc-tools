{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
{-| UnAda minting policy
-}
module UnAda.OnChain.MintingPolicy (
  unAdaMintingPolicy
) where

import           Plutus.V1.Ledger.Address    (Address (..))
import           Plutus.V1.Ledger.Credential (Credential (..))
import           Plutus.V1.Ledger.Scripts    (ValidatorHash (..))
import           Plutus.V1.Ledger.Value      (CurrencySymbol (..),
                                              TokenName (..), Value, adaSymbol,
                                              adaToken, valueOf)
import           Plutus.V2.Ledger.Contexts   (TxInInfo (..))
import           Plutus.V2.Ledger.Tx         (TxOut (..))
import           PlutusTx.Builtins           (unsafeDataAsB)
import           PlutusTx.IsData.Class       (unsafeFromBuiltinData)
import           PlutusTx.Prelude
import           UnAda.OnChain.Types         (BuiltinData (ScriptContext, TxInfoV2, inputs, outputs, purpose, rest8, txInfo),
                                              TxInfoRest8 (TxInfoPartTwo, mint, validRange),
                                              scriptPurposeMinting, validRange)
{-# INLINABLE unAdaMintingPolicy #-}
unAdaMintingPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
unAdaMintingPolicy (ValidatorHash . unsafeDataAsB -> validator) (TokenName . unsafeDataAsB -> tokenName) _redeemer ScriptContext{purpose, txInfo} = scriptPurposeMinting purpose $ \(unsafeFromBuiltinData -> currencySymbol) ->
  case txInfo of
    TxInfoV2{inputs, outputs, rest8=TxInfoPartTwo{validRange, mint}} ->
      if totalValMinted currencySymbol tokenName (unsafeFromBuiltinData mint) ==
            (adaLockedByScript validator (unsafeFromBuiltinData outputs))
            - (adaUnlockedFromScript validator (unsafeFromBuiltinData inputs))
      then ()
      else traceError "unAdaMintingPolicy: total value locked must equal total value minted" ()
    _ -> traceError "unAdaMintingPolicy: pattern match failure on TxInfoV2" ()
unAdaMintingPolicy _ _ _ _ = traceError "unAdaMintingPolicy: pattern match failure on unAdaMintingPolicy" ()

{-# INLINEABLE totalValMinted #-}
totalValMinted :: CurrencySymbol -> TokenName -> Value -> Integer
totalValMinted bs tn valueMinted =
  valueOf valueMinted bs tn

{-# INLINEABLE adaLockedByScript #-}
adaLockedByScript :: ValidatorHash -> [TxOut] -> Integer
adaLockedByScript vali outputs = foldl (\s o -> s + getVl o) 0 outputs where
  getVl TxOut{txOutAddress=Address (ScriptCredential s) _, txOutValue} | s == vali = valueOf txOutValue adaSymbol adaToken
  getVl _ = 0

{-# INLINEABLE adaUnlockedFromScript #-}
adaUnlockedFromScript :: ValidatorHash -> [TxInInfo] -> Integer
adaUnlockedFromScript vali inputs = adaLockedByScript vali (fmap txInInfoResolved inputs)
