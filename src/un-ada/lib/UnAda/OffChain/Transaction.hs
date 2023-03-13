{-# LANGUAGE ViewPatterns #-}
{-| Building transactions that mint and burn
unAda
-}
module UnAda.OffChain.Transaction(
  mintUnAda,
  burnUnAda,
  findUnAdaOutputs
) where

import           Cardano.Api            (BabbageEra, CtxTx, NetworkId, Quantity,
                                         Tx, TxIn, TxOut, lovelaceToQuantity,
                                         lovelaceToValue, quantityToLovelace,
                                         selectLovelace)
import qualified Cardano.Api.Shelley    as C
import           Control.Lens           (_1, _2, _3, preview, to, view, (^.))
import           Convex.BuildTx         (TxBuild, mintPlutusV2, payToPlutusV2,
                                         spendPlutusV2)
import qualified Convex.Lenses          as L
import           Convex.Scripts         (fromScriptData)
import           Convex.Utils           (txnUtxos)
import           Data.Maybe             (mapMaybe)
import           UnAda.OffChain.Scripts (assetName, mintingPolicyScript,
                                         unAdaPaymentCredential,
                                         validatorScript)

mintUnAda :: NetworkId -> Quantity -> TxBuild
mintUnAda n q =
  addOutputFor n q
  . mintPlutusV2 mintingPolicyScript () assetName q

addOutputFor :: NetworkId -> Quantity -> TxBuild
addOutputFor n q =
  let vl = lovelaceToValue (quantityToLovelace q)
  in payToPlutusV2 n validatorScript () vl

burnUnAda :: NetworkId -> TxIn -> TxOut CtxTx BabbageEra -> Quantity -> TxBuild
burnUnAda n txi txOut q =
  let unlockedVl = view (L._TxOut . _2 . L._TxOutValue) txOut
      unlockedQuantity = lovelaceToQuantity (selectLovelace unlockedVl)
      remainingQuantity = max 0 (unlockedQuantity - q)
  in
    spendPlutusV2 txi validatorScript () () -- FIXME: Use proper datum
    . mintPlutusV2 mintingPolicyScript () assetName (negate q)
    . if remainingQuantity > 0
        then addOutputFor n remainingQuantity
        else id

extract :: TxOut CtxTx BabbageEra -> Maybe ()
extract txOut
  | preview (L._TxOut . _1 . L._AddressInEra . L._Address . _2 . to C.fromShelleyPaymentCredential) txOut == Just unAdaPaymentCredential =
    case txOut ^. L._TxOut . _3 of
      C.TxOutDatumInTx _ (fromScriptData -> Just ()) -> Just ()
      _                                              -> Nothing
  | otherwise = Nothing

findUnAdaOutputs :: Tx BabbageEra -> [(TxIn, (TxOut CtxTx BabbageEra, ()))]
findUnAdaOutputs =
  let f (txi, txo) = fmap (\dt -> (txi, (txo, dt))) (extract txo)
  in mapMaybe f . txnUtxos
