{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (try)
import Maestro.Client.V1 -- @Maestro.Client.V1@ defines all the client utilities to query Maestro API endpoints.
import Maestro.Types.V1 -- @Maestro.Types.V1@ defines all the types used.

main :: IO ()
main = do
  env <- mkMaestroEnv @'V1 "ASsgZfnJ8V67O8NwEfUYxFth4O94Gjsa" Preprod defaultBackoff -- This is how we create an environment against which we'll query endpoints.
  chainTip :: ChainTip <- getTimestampedData <$> getChainTip env -- Maestro endpoint to get for chain-tip has data & timestamp against which data was calculated. All endpoints which are timestamped, has functions `getTimestampedData` to get for underlying data & `getTimestamp` to get the timestamp.
  addressesUTxOs :: Either MaestroError [UtxoWithSlot] <-
    try $ -- To catch for any errors, given in type `MaestroError`.
      allPages $ -- Since this endpoint is paged, we have a helper utility `allPages` to accumulate data from all the pages.
        flip
          ( utxosAtMultiAddresses
              env
              (Just True) -- We would like to have datums resolved. This is for @resolve_datums@ query parameter.
              (Just False) -- We would not like to include CBOR encodings of the transaction outputs in the response.
          )
          ["addr_test1vry38htgy3ycp43ve3tp9ft0q24kh00zurk3dck59jvvumqs9zkzc"] -- Mention your list of addresses to query for.
  print addressesUTxOs
