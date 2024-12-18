{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}

-- | UnAda validator
module UnAda.OnChain.Validator (
  unAdaValidator,
) where

import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Value (
  CurrencySymbol (..),
  TokenName (..),
  Value,
  valueOf,
 )
import Plutus.V2.Ledger.Contexts (TxInInfo (..))
import Plutus.V2.Ledger.Tx (TxOut (..))
import PlutusTx.Builtins (unsafeDataAsB)
import PlutusTx.IsData.Class (unsafeFromBuiltinData)
import PlutusTx.Prelude
import UnAda.OnChain.Types (
  BuiltinData (ScriptContext, TxInfoV2, UnAdaStateBuiltin, inputs, outputs, rest8, txInfo),
  TxInfoRest8 (TxInfoPartTwo, mint, validRange),
  validRange,
 )

{-# INLINEABLE unAdaValidator #-}
unAdaValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
unAdaValidator (TokenName . unsafeDataAsB -> tokenName) (UnAdaStateBuiltin spendAfter mps) _redeemer ScriptContext{txInfo} =
  case txInfo of
    TxInfoV2{inputs, outputs, rest8 = TxInfoPartTwo{validRange, mint}} ->
      let !itvl = unsafeFromBuiltinData validRange
          !minted = totalValMinted mps tokenName (unsafeFromBuiltinData mint)
          !locked = totalValLocked mps tokenName (unsafeFromBuiltinData outputs)
          !prod = totalValUnlocked mps tokenName (unsafeFromBuiltinData inputs)
       in if spendAfter `Interval.before` itvl
            then
              if minted == locked - prod
                then
                  if minted == 0
                    then traceError "must mint non-zero amount of tokens" ()
                    else ()
                else traceError "total value locked must equal total value minted" ()
            else traceError "spendAfter must be before validity interval" ()

{-# INLINEABLE totalValMinted #-}
totalValMinted :: BuiltinByteString -> TokenName -> Value -> Integer
totalValMinted bs tn valueMinted = valueOf valueMinted (CurrencySymbol bs) tn

{-# INLINEABLE totalValLocked #-}
totalValLocked :: BuiltinByteString -> TokenName -> [TxOut] -> Integer
totalValLocked bs tn outputs = foldl (\s o -> s + getVl o) 0 outputs
 where
  getVl TxOut{txOutValue} = valueOf txOutValue (CurrencySymbol bs) tn

{-# INLINEABLE totalValUnlocked #-}
totalValUnlocked :: BuiltinByteString -> TokenName -> [TxInInfo] -> Integer
totalValUnlocked bs tn inputs = totalValLocked bs tn (fmap txInInfoResolved inputs)
