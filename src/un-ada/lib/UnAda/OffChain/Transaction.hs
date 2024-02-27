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
import           Control.Lens           (_1, _2, _3, preview, set, to, view,
                                         (^.))
import           Convex.BuildTx         (MonadBuildTx (..), addBtx,
                                         mintPlutusV2, payToPlutusV2,
                                         spendPlutusV2)
import qualified Convex.Lenses          as L
import           Convex.Scripts         (fromScriptData)
import           Convex.Utils           (txnUtxos)
import           Data.Maybe             (mapMaybe)
import           Plutus.V1.Ledger.Time  (POSIXTime (..))
import           UnAda.OffChain.Scripts (assetName, mintingPolicyHash,
                                         mintingPolicyScript,
                                         unAdaPaymentCredential,
                                         validatorScript)
import           UnAda.OnChain.Types    (UnAdaState (..))

mintUnAda :: MonadBuildTx m => NetworkId -> POSIXTime -> Quantity -> m ()
mintUnAda n currentTime q =
  addOutputFor n currentTime q
  >> mintPlutusV2 mintingPolicyScript () assetName q

addOutputFor ::
  MonadBuildTx m =>
  NetworkId
  -> POSIXTime
  -- ^ Current time
  -> Quantity
  -> m ()
addOutputFor n spendAfter q =
  let vl = lovelaceToValue (quantityToLovelace q)
      dt = UnAdaState{spendAfter, mps = mintingPolicyHash}
  in payToPlutusV2 n validatorScript dt C.NoStakeAddress vl

burnUnAda :: MonadBuildTx m => NetworkId -> POSIXTime -> TxIn -> TxOut CtxTx BabbageEra -> UnAdaState -> Quantity -> m ()
burnUnAda n currentTime txi txOut oldState q =
  let unlockedVl = view (L._TxOut . _2 . L._TxOutValue) txOut
      unlockedQuantity = lovelaceToQuantity (selectLovelace unlockedVl)
      remainingQuantity = max 0 (unlockedQuantity - q)
      dummyRange = (C.TxValidityLowerBound C.ValidityLowerBoundInBabbageEra 0, C.TxValidityUpperBound C.ValidityUpperBoundInBabbageEra 200)
  in
    spendPlutusV2 txi validatorScript oldState ()
    >> mintPlutusV2 mintingPolicyScript () assetName (negate q)
    >> addBtx (set L.txValidityRange dummyRange)
    >> if remainingQuantity > 0
        then addOutputFor n currentTime remainingQuantity
        else pure ()

extract :: TxOut CtxTx BabbageEra -> Maybe UnAdaState
extract txOut
  | preview (L._TxOut . _1 . L._AddressInEra . L._Address . _2 . to C.fromShelleyPaymentCredential) txOut == Just unAdaPaymentCredential =
    case txOut ^. L._TxOut . _3 of
      C.TxOutDatumInTx _ (fromScriptData -> Just st) -> Just st
      _                                              -> Nothing
  | otherwise = Nothing

findUnAdaOutputs :: Tx BabbageEra -> [(TxIn, (TxOut CtxTx BabbageEra, UnAdaState))]
findUnAdaOutputs =
  let f (txi, txo) = fmap (\dt -> (txi, (txo, dt))) (extract txo)
  in mapMaybe f . txnUtxos
