{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-| Conversion functions and other conveniences
-}
module Convex.Utils(
  scriptFromCbor,
  unsafeScriptFromCbor,
  scriptAddress,
  -- Plutus V1
  scriptFromCborV1,
  unsafeScriptFromCborV1,
  scriptAddressV1
) where

import           Cardano.Api    (NetworkId, PlutusScript, PlutusScriptV1,
                                 PlutusScriptV2)
import qualified Cardano.Api    as C
import           Data.Aeson     (Result (..), fromJSON, object, (.=))
import           Data.Bifunctor (Bifunctor (..))
import           Data.Function  ((&))
import           Data.Proxy     (Proxy (..))

scriptFromCborV1 :: String -> Either String (PlutusScript PlutusScriptV1)
scriptFromCborV1 cbor = do
  let vl = object ["type" .= s "PlutusScriptV1", "description" .= s "", "cborHex" .= cbor]
  textEnvelope <- fromJSON vl & (\case { Error err -> Left (show err); Success e -> Right e })
  C.deserialiseFromTextEnvelope (C.proxyToAsType $ Proxy @(PlutusScript PlutusScriptV1)) textEnvelope & first show

unsafeScriptFromCborV1 :: String -> PlutusScript PlutusScriptV1
unsafeScriptFromCborV1 = either error id . scriptFromCborV1

{-| Script address without staking key
-}
scriptAddressV1 :: NetworkId -> PlutusScript PlutusScriptV1 -> C.AddressInEra C.BabbageEra
scriptAddressV1 network script =
  let hash = C.hashScript (C.PlutusScript C.PlutusScriptV1 script)
  in C.makeShelleyAddressInEra network (C.PaymentCredentialByScript hash) C.NoStakeAddress

scriptFromCbor :: String -> Either String (PlutusScript PlutusScriptV2)
scriptFromCbor cbor = do
  let vl = object ["type" .= s "PlutusScriptV2", "description" .= s "", "cborHex" .= cbor]
  textEnvelope <- fromJSON vl & (\case { Error err -> Left (show err); Success e -> Right e })
  C.deserialiseFromTextEnvelope (C.proxyToAsType $ Proxy @(PlutusScript PlutusScriptV2)) textEnvelope & first show

unsafeScriptFromCbor :: String -> PlutusScript PlutusScriptV2
unsafeScriptFromCbor = either error id . scriptFromCbor

{-| Script address without staking key
-}
scriptAddress :: NetworkId -> PlutusScript PlutusScriptV2 -> C.AddressInEra C.BabbageEra
scriptAddress network script =
  let hash = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
  in C.makeShelleyAddressInEra network (C.PaymentCredentialByScript hash) C.NoStakeAddress

s :: String -> String
s = id
