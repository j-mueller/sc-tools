{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Api qualified as C
import Control.Exception (try)

-- @Maestro.Client.V1@ defines all the client utilities to query Maestro API endpoints.
-- @Maestro.Types.V1@ defines all the types used.
import Convex.Maestro.MonadBlockchain (getUtxoByTxIn)
import Data.Set qualified as Set
import Maestro.Client.V1
import Maestro.Types.V1

main :: IO ()
main = do
  env <- mkMaestroEnv @'V1 "ASsgZfnJ8V67O8NwEfUYxFth4O94Gjsa" Preprod defaultBackoff -- This is how we create an environment against which we'll query endpoints.
  _chainTip :: ChainTip <- getTimestampedData <$> getChainTip env -- Maestro endpoint to get for chain-tip has data & timestamp against which data was calculated. All endpoints which are timestamped, has functions `getTimestampedData` to get for underlying data & `getTimestamp` to get the timestamp.
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

  result <- getUtxoByTxIn env (Set.fromList [C.TxIn "80ef5e073e9d703182cf7368a9b65caedee0b2477798430246234d297fba4a6c" (C.TxIx 0)])
  print result
