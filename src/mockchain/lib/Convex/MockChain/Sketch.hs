{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wmissing-export-lists #-}
{-# LANGUAGE TypeApplications #-}

module Convex.MockChain.Sketch (sketch) where

import Data.Foldable (traverse_)
import Data.Bifunctor (first)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Core (TxBody, mkBasicTx, bodyTxL, 
  inputsTxBodyL, outputsTxBodyL)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError, collectPlutusScriptsWithContext)
import Cardano.Ledger.Plutus.Evaluate (PlutusWithContext)
import Convex.MockChain.Defaults (ledgerProtocolParameters, 
  nodeParams, systemStart, epochSize, slotLength)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import qualified Cardano.Api.Shelley as C
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody (BabbageTxBody))
import Cardano.Ledger.Shelley.API (Coin, TxIn)
import Convex.MockChain (genesisUTxO)
import Control.Lens (view)
import Cardano.Ledger.UTxO (UTxO (UTxO))
import qualified Data.Map as Map
import Control.Monad (unless)
import qualified Data.Set as Set

-- Note from Koz: C.TxBodyContent C.BuildTx C.BabbageEra is what Convex.BuildTx
-- calls a 'TxBody'. This is rather misleading, as Cardano _definitely_
-- disagrees on that point. Thus, we have to take the following steps:
--
-- 1. Cook a C.TxBodyContent C.BuildTx C.BabbageEra into a TxBody (BabbageEra
--    StandardCrypto). This can fail with a TxBodyError.
-- 2. Cook the resulting of 1 into a C.Tx C.BabbageEra. This is non-erroring
--    (probably).
-- 3. Transform the bits so far into a UTxO (BabbageEra StandardCrypto). This is
--    done by using the provided mapping for coins, then checking that we aren't
--    missing any inputs.
-- 4. Smash all the above together with collectPlutusScriptsWithContext. This
--    can fail with a CollectError (BabbageEra StandardCrypto).
--
-- Since we have (at least!) two kinds of failures, we ned a dedicated type to
-- handle them.

data SketchError = 
  FailedTxBody C.TxBodyError |
  FailedCollect [CollectError (BabbageEra StandardCrypto)] |
  UnmatchedInput (TxIn StandardCrypto)

sketch :: 
  C.TxBodyContent C.BuildTx C.BabbageEra -> 
  [(C.AddressInEra C.BabbageEra, Coin)] -> 
  Either SketchError [PlutusWithContext StandardCrypto]
sketch txBodyContent mappings = do
  txBody <- first FailedTxBody . C.createAndValidateTransactionBody C.ShelleyBasedEraBabbage $ txBodyContent
  let (C.ShelleyTx _ tx) = _ txBody
  let epochInfo = fixedEpochInfo epochSize slotLength
  -- We have to keep using these explicit arguments, because type classes
  -- without fundeps mean that GHC can't figure out what we mean.
  let utxo@(UTxO innards) = genesisUTxO @(BabbageEra StandardCrypto) mappings
  -- verify that we have every input
  let allInputs = view (bodyTxL @(BabbageEra StandardCrypto) . inputsTxBodyL) tx
  let utxoInputs = Map.keysSet innards
  case Set.lookupMin . Set.difference allInputs $ utxoInputs of 
    Just unmatched -> Left . UnmatchedInput $ unmatched
    Nothing -> first FailedCollect . 
                collectPlutusScriptsWithContext epochInfo systemStart ledgerProtocolParameters tx $ utxo
