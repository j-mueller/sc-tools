{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
{-| UnAda minting policy
-}
module UnAda.OnChain.MintingPolicy (
  unAdaMintingPolicy
) where

import qualified Plutus.V1.Ledger.Interval as Interval
import           Plutus.V1.Ledger.Value    (CurrencySymbol (..), TokenName (..),
                                            Value, valueOf)
import           Plutus.V2.Ledger.Contexts (TxInInfo (..))
import           Plutus.V2.Ledger.Tx       (TxOut (..))
import           PlutusTx.Builtins         (unsafeDataAsB)
import           PlutusTx.IsData.Class     (unsafeFromBuiltinData)
import           PlutusTx.Prelude
import           UnAda.OnChain.Types       (BuiltinData (ScriptContext, TxInfoV2, UnAdaStateBuiltin, inputs, outputs, purpose, rest8, txInfo),
                                            TxInfoRest8 (TxInfoPartTwo, mint, validRange),
                                            scriptPurposeMinting, validRange)
{-# INLINABLE unAdaMintingPolicy #-}
unAdaMintingPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
unAdaMintingPolicy _validator (TokenName . unsafeDataAsB -> tokenName) _redeemer ScriptContext{purpose, txInfo} = scriptPurposeMinting purpose $ \(unsafeFromBuiltinData -> currencySymbol) ->
  case txInfo of
    TxInfoV2{inputs, outputs, rest8=TxInfoPartTwo{validRange, mint}} ->
      if totalValMinted currencySymbol tokenName (unsafeFromBuiltinData mint) ==
            (totalValLocked currencySymbol tokenName (unsafeFromBuiltinData outputs))
            - (totalValUnlocked currencySymbol tokenName (unsafeFromBuiltinData inputs))
      then traceError "unAdaMintingPolicy: Success" ()
      else traceError "unAdaMintingPolicy: total value locked must equal total value minted" ()
    _ -> traceError "unAdaMintingPolicy: pattern match failure on TxInfoV2" ()
unAdaMintingPolicy _ _ _ _ = traceError "unAdaMintingPolicy: pattern match failure on unAdaMintingPolicy" ()

{-# INLINEABLE totalValMinted #-}
totalValMinted :: CurrencySymbol -> TokenName -> Value -> Integer
totalValMinted bs tn valueMinted =
  valueOf valueMinted bs tn

{-# INLINEABLE totalValLocked #-}
totalValLocked :: CurrencySymbol -> TokenName -> [TxOut] -> Integer
totalValLocked bs tn outputs = foldl (\s o -> s + getVl o) 0 outputs where
  getVl TxOut{txOutValue} = valueOf txOutValue bs tn

{-# INLINEABLE totalValUnlocked #-}
totalValUnlocked :: CurrencySymbol -> TokenName -> [TxInInfo] -> Integer
totalValUnlocked bs tn inputs = totalValLocked bs tn (fmap txInInfoResolved inputs)
