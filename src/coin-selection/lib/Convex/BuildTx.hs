{-# LANGUAGE DataKinds #-}
{-| Building transactions
-}
module Convex.BuildTx(
  -- * Building transactions
  spendPublicKeyOutput,
  payToAddress
  ) where

import qualified Cardano.Api.Shelley as C
import           Control.Lens        (over)
import qualified Convex.Lenses       as L

{-| Spend an output locked by a public key
-}
spendPublicKeyOutput :: C.TxIn -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
spendPublicKeyOutput txIn =
  let wit = C.BuildTxWith (C.KeyWitness (C.KeyWitnessForSpending))
  in over L.txIns ((txIn, wit) :)

payToAddress :: C.AddressInEra C.BabbageEra -> C.Value -> C.TxBodyContent C.BuildTx C.BabbageEra -> C.TxBodyContent C.BuildTx C.BabbageEra
payToAddress addr vl =
  let txo = C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra vl) C.TxOutDatumNone C.ReferenceScriptNone
  in over L.txOuts ((:) txo)
