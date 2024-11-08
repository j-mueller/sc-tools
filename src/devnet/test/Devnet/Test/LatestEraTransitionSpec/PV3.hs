{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}

module Devnet.Test.LatestEraTransitionSpec.PV3
  ( readBitTestMintingPolicyScriptPV3
  , writeBitTestMintingPolicyScriptPV3
  ) where

import qualified Cardano.Api                as C
import           Convex.PlutusTx            (compiledCodeToScript)
import qualified PlutusCore.Version         as Version
import           PlutusTx                   (CompiledCode)
import qualified PlutusTx
import qualified PlutusTx.Builtins.Internal as BI
import qualified PlutusTx.Prelude           as PlutusTx

{-# INLINABLE readBitTestMintingPolicy #-}
readBitTestMintingPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit
readBitTestMintingPolicy d _ =
  if PlutusTx.readBit (BI.unsafeDataAsB d) 2 then BI.unitval else PlutusTx.error ()

readBitTestMintingPolicyScriptPV3 :: PlutusTx.BuiltinData -> C.PlutusScript C.PlutusScriptV3
readBitTestMintingPolicyScriptPV3 = compiledCodeToScript . readBitTestMintingPolicyCompiled
 where
  readBitTestMintingPolicyCompiled :: PlutusTx.BuiltinData -> CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
  readBitTestMintingPolicyCompiled str =
    $$(PlutusTx.compile [|| \str' c -> readBitTestMintingPolicy str' c ||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode Version.plcVersion110 str

{-# INLINABLE writeBitTestMintingPolicy #-}
writeBitTestMintingPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit
writeBitTestMintingPolicy d _ =
  let !_ = PlutusTx.writeBits (BI.unsafeDataAsB d) [0] False in BI.unitval

writeBitTestMintingPolicyScriptPV3 :: PlutusTx.BuiltinData -> C.PlutusScript C.PlutusScriptV3
writeBitTestMintingPolicyScriptPV3 = compiledCodeToScript . writeBitTestMintingPolicyCompiled
 where
  writeBitTestMintingPolicyCompiled :: PlutusTx.BuiltinData -> CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
  writeBitTestMintingPolicyCompiled str =
    $$(PlutusTx.compile [|| \str' c -> writeBitTestMintingPolicy str' c ||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode Version.plcVersion110 str
