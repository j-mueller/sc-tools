{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TupleSections      #-}
{-| Changing hashes
-}
module Convex.UtxoMod(
  replaceHash,
  tryReplaceHash,
  replaceHashAnyLang,
  FullTx(..)
) where

import           Cardano.Api     (Hash, Script, ScriptHash,
                                  ScriptInAnyLang (..))
import qualified Cardano.Api     as C
import           Cardano.Binary  (DecoderError)
import           Control.Monad   (guard)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Functor    (($>))
import           Data.Map        (Map)
import           Data.Proxy      (Proxy (..))
import           GHC.Generics    (Generic)

{-| Replace all occurrences of a hash in the serialised script
with a new hash. Throws an 'error' if it fails
-}
tryReplaceHash ::
  ( C.SerialiseAsCBOR (Hash h1)
  , C.SerialiseAsCBOR (Hash h2)
  , C.IsScriptLanguage lang
  ) => Hash h1 -> Hash h2 -> Script lang -> Script lang
tryReplaceHash oldHash newHash  =
  either (error . show) id . replaceHash oldHash newHash

{-| Replace all occurrences of a hash in the serialised script
with a new hash
-}
replaceHash ::
  ( C.SerialiseAsCBOR (Hash h1)
  , C.SerialiseAsCBOR (Hash h2)
  , C.IsScriptLanguage lang
  ) => Hash h1 -> Hash h2 -> Script lang -> Either DecoderError (Script lang)
replaceHash oldHash newHash oldScript =
  let oldHashB   = C.serialiseToCBOR oldHash
      newHashB   = C.serialiseToCBOR newHash
      oldScriptB = C.serialiseToCBOR oldScript
      newScriptB = replaceBS oldHashB newHashB oldScriptB
  in C.deserialiseFromCBOR (C.proxyToAsType Proxy) newScriptB

{-| Replace all occurrences of the first bytestring with the second
bytestring in the third bytestring
-}
replaceBS :: ByteString -> ByteString -> ByteString -> ByteString
replaceBS old new target
  | BS.null target = mempty
  | otherwise      = h <> new <> replaceBS old new rest
    where
      (h, rest) = BS.breakSubstring old target

{-| Variant of 'replaceHash' that works inside the
existentially quantified 'ScriptInAnyLang'
Returns the hash of the new script if it is different
(ie. if the script has been modified)
-}
replaceHashAnyLang ::
  ( C.SerialiseAsCBOR (Hash h1)
  , C.SerialiseAsCBOR (Hash h2)
  ) => Hash h1 -> Hash h2 -> ScriptInAnyLang -> Either DecoderError (ScriptInAnyLang, Maybe ScriptHash)
replaceHashAnyLang oldHash newHash = \case
  C.ScriptInAnyLang C.SimpleScriptLanguage script        -> Right (C.ScriptInAnyLang C.SimpleScriptLanguage script, Nothing)
  C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) script -> do
    new <- replaceHash oldHash newHash script
    let oldScriptHash = C.hashScript script
        newScriptHash = C.hashScript new
    pure (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) new, guard (oldScriptHash /= newScriptHash) $> newScriptHash)
  C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) script -> do
    new <- replaceHash oldHash newHash script
    let oldScriptHash = C.hashScript script
        newScriptHash = C.hashScript new
    pure (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) new, guard (oldScriptHash /= newScriptHash) $> newScriptHash)
  C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) script -> do
    new <- replaceHash oldHash newHash script
    let oldScriptHash = C.hashScript script
        newScriptHash = C.hashScript new
    pure (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) new, guard (oldScriptHash /= newScriptHash) $> newScriptHash)

-- TODO
-- 1. Download a full "spending transaction"
--    - the transaction
--    - all inputs and outputs

{-| A transaction with fully resolved inputs
-}
data FullTx =
  FullTx
    { ftxTransaction :: C.Tx C.ConwayEra
    , ftxInputs      :: Map C.TxIn (C.TxOut C.CtxUTxO C.ConwayEra)
    }
    deriving stock (Eq, Show, Generic)
    -- deriving anyclass (ToJSON, FromJSON)
