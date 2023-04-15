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
import           Plutus.V1.Ledger.Scripts    (Datum (..), MintingPolicyHash,
                                              ValidatorHash (..))
import           Plutus.V1.Ledger.Value      (CurrencySymbol (..),
                                              TokenName (..), Value, adaSymbol,
                                              adaToken, currencyMPSHash,
                                              valueOf)
import           Plutus.V2.Ledger.Contexts   (TxInInfo (..))
import           Plutus.V2.Ledger.Tx         (OutputDatum (..), TxOut (..))
import           PlutusTx.Builtins           (unsafeDataAsB)
import           PlutusTx.IsData.Class       (fromBuiltinData,
                                              unsafeFromBuiltinData)
import           PlutusTx.Prelude
import           UnAda.OnChain.Types         (BuiltinData (ScriptContext, TxInfoV2, inputs, outputs, purpose, rest8, txInfo),
                                              TxInfoRest8 (TxInfoPartTwo, mint, validRange),
                                              UnAdaState (..),
                                              scriptPurposeMinting, validRange)
{-# INLINABLE unAdaMintingPolicy #-}
unAdaMintingPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
unAdaMintingPolicy (ValidatorHash . unsafeDataAsB -> validator) (TokenName . unsafeDataAsB -> tokenName) _redeemer ScriptContext{purpose, txInfo} = scriptPurposeMinting purpose $ \(unsafeFromBuiltinData -> currencySymbol) ->
  case txInfo of
    TxInfoV2{inputs, outputs, rest8=TxInfoPartTwo{validRange, mint}} ->
      let hsh = currencyMPSHash currencySymbol
      in
        if totalValMinted currencySymbol tokenName (unsafeFromBuiltinData mint) ==
            (adaLockedByScript hsh validator (unsafeFromBuiltinData outputs))
            - (adaUnlockedFromScript hsh validator (unsafeFromBuiltinData inputs))
        then ()
        else traceError "unAdaMintingPolicy: total value locked must equal total value minted" ()
    _ -> traceError "unAdaMintingPolicy: pattern match failure on TxInfoV2" ()
unAdaMintingPolicy _ _ _ _ = traceError "unAdaMintingPolicy: pattern match failure on unAdaMintingPolicy" ()

{-# INLINEABLE totalValMinted #-}
totalValMinted :: CurrencySymbol -> TokenName -> Value -> Integer
totalValMinted bs tn valueMinted =
  valueOf valueMinted bs tn

{-# INLINEABLE adaLockedByScript #-}
adaLockedByScript :: MintingPolicyHash -> ValidatorHash -> [TxOut] -> Integer
adaLockedByScript ownHash vali outputs = foldl (\s o -> s + getVl o) 0 outputs where
  getVl TxOut{txOutAddress=Address (ScriptCredential s) _, txOutValue, txOutDatum}
    | s == vali =
      case txOutDatum of
        OutputDatum (Datum (fromBuiltinData -> Just UnAdaState{mps}))
          | mps == ownHash -> valueOf txOutValue adaSymbol adaToken
        _ -> 0
  getVl _ = 0

{-# INLINEABLE adaUnlockedFromScript #-}
adaUnlockedFromScript :: MintingPolicyHash -> ValidatorHash -> [TxInInfo] -> Integer
adaUnlockedFromScript ownHash vali inputs = adaLockedByScript ownHash vali (fmap txInInfoResolved inputs)
