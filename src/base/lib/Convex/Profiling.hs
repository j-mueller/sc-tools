{-# LANGUAGE GADTs #-}
{-| Profiling script execution
-}
module Convex.Profiling(
  ScriptExecution(..),
  txScripts
) where

import           Cardano.Api                     (BabbageEra, Tx (..))
import           Cardano.Api.Shelley             (ExecutionUnits, TxBody (..))
import qualified Cardano.Api.Shelley             as C
import           Cardano.Ledger.Alonzo.TxWitness (unRedeemers)
import qualified Data.Map                        as Map

data ScriptExecution =
  ScriptExecution
    { seExUnits :: ExecutionUnits
    }

{-| Extract script execution information from a
cardano tx.
-}
txScripts :: Tx BabbageEra -> [ScriptExecution]
txScripts (Tx txBody _) =
  let ShelleyTxBody _ txBody' _scripts scriptData _auxiliaryData _ = txBody
      txReds = case scriptData of
              C.TxBodyScriptData C.ScriptDataInBabbageEra _ r -> unRedeemers r
              _                                               -> mempty

  in fmap (ScriptExecution . C.fromAlonzoExUnits . snd . snd) (Map.toList txReds)
