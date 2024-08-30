{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}
{-| 'TxBodyContent' with resolved inputs
-}
module Convex.BuildTx.BodyContentResolved(
  AnyOutput,
  TxBodyContent,
  resolvedInputs,
  bodyContent,
  BodyContentResolved(..),
  emptyBodyContentResolved,
  addTxInSpending,
  addTxInCollateral,
  addTxInReference,
  allInputsResolved,
  toBodyContextResolved,
  requiredTxIns
) where

import           Cardano.Api.Shelley      (Hash, HashableScriptData, NetworkId,
                                           PaymentKey, PlutusScript,
                                           PlutusScriptV1, PlutusScriptV2,
                                           ScriptHash, WitCtxTxIn)
import qualified Cardano.Api.Shelley      as C
import           Control.Lens             (makeLensesFor, over, view, (&))
import qualified Convex.CardanoApi.Lenses as L
import           Convex.Class             (MonadBlockchain (..))
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           GHC.Generics             (Generic)

type AnyOutput = C.InAnyCardanoEra (C.TxOut C.CtxUTxO)

type TxBodyContent = C.TxBodyContent C.BuildTx C.BabbageEra

-- Invariant: All of the inputs in 'bcrBodyContent' have an entry in the 'bcrInput' map
data BodyContentResolved =
  BodyContentResolved
    { bcrResolvedInputs :: Map C.TxIn AnyOutput -- ^ Resolved inputs
    , bcrBodyContent    :: TxBodyContent
    }
    deriving stock Generic

makeLensesFor
  [ ("bcrResolvedInputs", "resolvedInputs")
  , ("bcrBodyContent", "bodyContent")
  ] ''BodyContentResolved

emptyBodyContentResolved :: BodyContentResolved
emptyBodyContentResolved =
  BodyContentResolved
    { bcrBodyContent = L.emptyTx
    , bcrResolvedInputs = Map.empty
    }

-- | Spend a resolved 'TxIn'
addTxInSpending :: C.TxIn -> AnyOutput -> C.Witness WitCtxTxIn C.BabbageEra -> BodyContentResolved -> BodyContentResolved
addTxInSpending txIn txOut redeemer BodyContentResolved{bcrResolvedInputs = oldResolved, bcrBodyContent = oldContent} =
  BodyContentResolved
    { bcrResolvedInputs = Map.insert txIn txOut oldResolved
    , bcrBodyContent    = oldContent & over L.txIns ((:) (txIn, C.BuildTxWith redeemer))
    }

-- | Add a resolved collateral input
addTxInCollateral :: C.TxIn -> AnyOutput -> BodyContentResolved -> BodyContentResolved
addTxInCollateral txIn txOut BodyContentResolved{bcrResolvedInputs = oldResolved, bcrBodyContent = oldContent} =
  BodyContentResolved
    { bcrResolvedInputs = Map.insert txIn txOut oldResolved
    , bcrBodyContent    = oldContent & over (L.txInsCollateral . L._TxInsCollateralIso) ((:) txIn)
    }

-- | Add a resolved reference input
addTxInReference :: C.TxIn -> AnyOutput -> BodyContentResolved -> BodyContentResolved
addTxInReference txIn txOut BodyContentResolved{bcrResolvedInputs = oldResolved, bcrBodyContent = oldContent} =
  BodyContentResolved
    { bcrResolvedInputs = Map.insert txIn txOut oldResolved
    , bcrBodyContent    = oldContent & over (L.txInsReference . L._TxInsReferenceIso) ((:) txIn)
    }

-- TODO: Use this for testing
allInputsResolved :: BodyContentResolved -> Bool
allInputsResolved = undefined

-- | Resolve all inputs used by the 'TxBodyContent'
toBodyContextResolved :: (MonadBlockchain m) => TxBodyContent -> m BodyContentResolved
toBodyContextResolved txBody = do
  utxos <- Map.toList . C.unUTxO <$> utxoByTxIn (requiredTxIns txBody)
  pure BodyContentResolved
        { bcrResolvedInputs = Map.fromList $ fmap (fmap (C.inAnyCardanoEra C.BabbageEra)) utxos
        , bcrBodyContent    = txBody
        }

requiredTxIns :: C.TxBodyContent v era -> Set C.TxIn
requiredTxIns body =
  Set.fromList (fst <$> view L.txIns body)
  <> Set.fromList (view (L.txInsReference . L.txInsReferenceTxIns) body)
  <> Set.fromList (view (L.txInsCollateral . L.txInsCollateralTxIns) body)
