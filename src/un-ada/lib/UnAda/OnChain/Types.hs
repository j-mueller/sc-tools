{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns       #-}
module UnAda.OnChain.Types(
  UnAdaState(..),
  BuiltinData(
    UnAdaStateBuiltin,
    -- * Script context
    ScriptContext,
    txInfo,
    purpose,
    -- * Matching on tx info
    TxInfoV2,
    inputs,
    referenceInputs,
    outputs,
    fee,
    rest8,
    -- * Validity intervals
    FinitePOSIXTimeRange,
    fiFrom,
    fiTo,
    FiniteExtended,
    ItvlBound,

    -- * UnAda context
    UnAdaContext,
    uacValueMinted,
    uacValidRange
    ),
  TxInfoRest8(
    TxInfoPartTwo,
    mint,
    dcert,
    wdrl,
    validRange,
    rest4
  ),
  -- * Script Purpose
  scriptPurposeMinting,
  UnAdaRedeemer
) where

import           GHC.Generics          (Generic)
import           Plutus.V1.Ledger.Api  (MintingPolicyHash)
import           Plutus.V1.Ledger.Time (POSIXTime)
import qualified PlutusTx
import           PlutusTx.Builtins     (unsafeDataAsB, unsafeDataAsConstr,
                                        unsafeDataAsI)
import           PlutusTx.Prelude

{-| The time after which the output can be spent, and the
minting policy
-}
data UnAdaState =
  UnAdaState
    { spendAfter :: POSIXTime
    , mps        :: MintingPolicyHash
    }
    deriving stock Generic

PlutusTx.makeIsDataIndexed ''UnAdaState [('UnAdaState, 0)]

pattern UnAdaStateBuiltin :: Integer -> BuiltinByteString -> BuiltinData
pattern UnAdaStateBuiltin spendAfterB mpsB <- (unsafeDataAsConstr -> (_, [unsafeDataAsI -> spendAfterB, unsafeDataAsB -> mpsB]))

{-# INLINABLE txInfo #-}
pattern ScriptContext :: BuiltinData -> BuiltinData -> BuiltinData
pattern ScriptContext{txInfo, purpose} <-
  (unsafeDataAsConstr ->
    (_, [txInfo, purpose]
    ))

newtype TxInfoRest8 = TxInfoRest8 [BuiltinData]

{-# INLINABLE mint #-}
{-# INLINABLE validRange #-}
pattern TxInfoPartTwo :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> [BuiltinData] -> TxInfoRest8
pattern TxInfoPartTwo{mint, dcert, wdrl, validRange, rest4} <- TxInfoRest8 (mint:dcert:wdrl:validRange:rest4)

{-# INLINABLE rest8 #-}
-- inputs, referenceInputs, outputs, fee, mint, dcert, wdrl, validRange, signatories, redeemers, dat, txi
pattern TxInfoV2 :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> TxInfoRest8 -> BuiltinData
pattern TxInfoV2{inputs, referenceInputs, outputs, fee, rest8} <-
  (unsafeDataAsConstr ->
    (_, inputs:referenceInputs:outputs:fee:(TxInfoRest8 -> rest8)
    ))

pattern FiniteExtended :: Integer -> BuiltinData
pattern FiniteExtended a <- (unsafeDataAsConstr -> (_, [unsafeDataAsI -> a]))

{-# INLINABLE scriptPurposeMinting #-}
scriptPurposeMinting :: BuiltinData -> (BuiltinData -> r) -> r
scriptPurposeMinting (unsafeDataAsConstr -> (n, [arg])) mint =
  if n == 0
    then mint arg
    else traceError "scriptPurposeMinting: Wrong script purpose" ()

-- {-# INLINEABLE matchExtended #-}
-- matchExtended :: BuiltinData -> r -> (BuiltinData -> r) -> r -> r
-- matchExtended dt negInf finite posInf = case unsafeDataAsConstr dt of
--   (n, lst)
--     | n == 0 ->
--         case lst of
--           [] -> negInf
--     | n == 1 ->
--       case lst of
--         [x] -> finite x
--     | n == 2 ->
--         case lst of
--           [] -> posInf

pattern ItvlBound :: BuiltinData -> BuiltinData -> BuiltinData
pattern ItvlBound ext closure <- (unsafeDataAsConstr -> (_, [ext, closure]))

-- data UnAdaContext = UnAdaContext_{ uacValidRange_ :: BuiltinData, uacValueMinted_ :: BuiltinData }

{-# INLINABLE uacValueMinted #-}
{-# INLINABLE uacValidRange #-}
pattern UnAdaContext :: BuiltinData -> BuiltinData -> BuiltinData
pattern UnAdaContext{uacValueMinted, uacValidRange} <- ScriptContext{txInfo=TxInfoV2{rest8=TxInfoPartTwo{mint=uacValueMinted,validRange=uacValidRange}}}

-- {-# INLINABLE unsafeUnAdaContext #-}
-- unsafeUnAdaContext :: BuiltinData -> UnAdaContext
-- unsafeUnAdaContext (unsafeDataAsConstr -> (_, fields)) =
--   let !r1 = tail $ tail $ tail $ tail fields
--       !uacValueMinted = head r1
--       !r2 = tail $ tail $ tail r1
--       !uacValidRange = head r2
--   in UnAdaContext{uacValueMinted, uacValidRange}

{-| Match on 'POSIXTimeRange' where both ends are finite
-}
pattern FinitePOSIXTimeRange :: BuiltinData -> BuiltinData -> BuiltinData
pattern FinitePOSIXTimeRange{fiFrom, fiTo} <-
  (unsafeDataAsConstr ->
    (_, [fiFrom, fiTo]))

{-| Always use the same redeemer
-}
type UnAdaRedeemer = ()
