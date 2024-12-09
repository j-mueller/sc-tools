{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TupleSections      #-}
{-| Changing hashes
-}
module Convex.UtxoMod(
  replaceHash,
  tryReplaceHash
) where

import           Cardano.Api     (Hash, Script, ScriptHash,
                                  ScriptInAnyLang (..))
import qualified Cardano.Api     as C
import           Control.Monad   (guard)
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Functor    (($>), (<&>))
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Proxy      (Proxy (..))
import           GHC.Generics    (Generic)

{-| Replace all occurrences of a hash in the serialised script
with a new hash. Throws an 'error' if it fails
-}
tryReplaceHash ::
  ( C.SerialiseAsRawBytes (Hash h1)
  , C.SerialiseAsRawBytes (Hash h2)
  , C.SerialiseAsRawBytes (Script lang)
  , C.HasTypeProxy h1
  , C.HasTypeProxy h2
  , C.HasTypeProxy lang
  ) => Hash h1 -> Hash h2 -> Script lang -> Script lang
tryReplaceHash oldHash newHash  =
  either (error . show) id . replaceHash oldHash newHash

{-| Replace all occurrences of a hash in the serialised script
with a new hash
-}
replaceHash ::
  ( C.SerialiseAsRawBytes (Hash h1)
  , C.SerialiseAsRawBytes (Hash h2)
  , C.SerialiseAsRawBytes (Script lang)
  , C.HasTypeProxy h1
  , C.HasTypeProxy h2
  , C.HasTypeProxy lang
  ) => Hash h1 -> Hash h2 -> Script lang -> Either C.SerialiseAsRawBytesError (Script lang)
replaceHash oldHash newHash oldScript =
  let oldHashB   = C.serialiseToRawBytes oldHash
      newHashB   = C.serialiseToRawBytes newHash
      oldScriptB = C.serialiseToRawBytes oldScript
      newScriptB = replaceBS oldHashB newHashB oldScriptB
  in C.deserialiseFromRawBytes (C.proxyToAsType Proxy) newScriptB

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
  ( C.SerialiseAsRawBytes (Hash h1)
  , C.SerialiseAsRawBytes (Hash h2)
  , C.HasTypeProxy h1
  , C.HasTypeProxy h2
  ) => Hash h1 -> Hash h2 -> ScriptInAnyLang -> Either C.SerialiseAsRawBytesError (ScriptInAnyLang, Maybe ScriptHash)
replaceHashAnyLang oldHash newHash = \case
  C.ScriptInAnyLang C.SimpleScriptLanguage script        -> Right (C.ScriptInAnyLang C.SimpleScriptLanguage script, Nothing)
  C.ScriptInAnyLang (C.PlutusScriptLanguage lang) script -> do
    new <- replaceHash oldHash newHash script
    let oldHash = C.hashScript script
        newHash = C.hashScript new
    pure (C.ScriptInAnyLang (C.PlutusScriptLanguage lang) new, guard (oldHash /= newHash) $> newHash)

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

{-| Download the full transaction
-}
-- TODO: Move to blockfrost package
resolveTx :: C.TxId -> m FullTx
resolveTx = undefined

-- 2. Adjust the reference scripts with replaceHash (this effectively changes the StablecoinParams)

-- 3. Adjust the script address of the output to match the new script hash
