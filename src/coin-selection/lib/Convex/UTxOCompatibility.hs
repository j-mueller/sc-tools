{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-| Filter the UTxO set for outputs that are compatible
    with specific versions of Plutus
-}
module Convex.UTxOCompatibility(
  UTxOCompatibility(..),
  compatibleWith,
  deleteInlineDatums,
  scriptWitnessCompat,
  anyScriptWitnessCompat,
  txCompatibility
) where

import           Cardano.Api         (BabbageEra, UTxO (..))
import qualified Cardano.Api.Shelley as C
import qualified Control.Lens        as L
import qualified Convex.Lenses       as L
import           Data.Aeson          (FromJSON, ToJSON)
import qualified Data.Map            as Map
import           Data.Maybe          (isJust)
import           GHC.Generics        (Generic)

-- | Compatibility setting for coin selection.
--   Used to filter out incompatible UTxOs for
--   coin selection.
data UTxOCompatibility =
  -- NOTE: The order of constructors is important
  --       as it determines the 'Ord' instance, which
  --       we use in 'txCompatibility'
  PlutusV1Compatibility -- ^ Plutus V1 scripts can't be run in a transaction that also spends (script OR public key) outputs with inline datums.
  | AnyCompatibility -- ^ All outputs
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Delete incompatible outputs from the UTxO set
compatibleWith :: UTxOCompatibility -> UTxO BabbageEra -> UTxO BabbageEra
compatibleWith = \case
  PlutusV1Compatibility -> deleteInlineDatums
  AnyCompatibility -> id

{-| Delete UTxOs that have inline datums, as this is not supported by Plutus V1
-}
deleteInlineDatums :: UTxO BabbageEra -> UTxO BabbageEra
deleteInlineDatums (UTxO o) =
  let hasInlineDatum = isJust . L.preview (L._TxOut . L._3 . L._TxOutDatumInline)
  in UTxO (Map.filter (not . hasInlineDatum) o)

anyScriptWitnessCompat :: forall era. C.AnyScriptWitness era -> UTxOCompatibility
anyScriptWitnessCompat = \case
  C.AnyScriptWitness wit -> scriptWitnessCompat wit

-- | The highest possible compatibility level at which the script
--   can be run
scriptWitnessCompat :: forall witctx era. C.ScriptWitness witctx era -> UTxOCompatibility
scriptWitnessCompat (C.PlutusScriptWitness lang _ _ _ _ _) = case lang of
  C.PlutusScriptV1InAlonzo  -> PlutusV1Compatibility
  C.PlutusScriptV1InBabbage -> PlutusV1Compatibility
  C.PlutusScriptV1InConway  -> PlutusV1Compatibility
  C.PlutusScriptV2InBabbage -> AnyCompatibility
  C.PlutusScriptV2InConway  -> AnyCompatibility
  C.PlutusScriptV3InConway  -> AnyCompatibility
  C.SimpleScriptInShelley   -> AnyCompatibility
  C.SimpleScriptInAllegra   -> AnyCompatibility
  C.SimpleScriptInMary      -> AnyCompatibility
  C.SimpleScriptInAlonzo    -> AnyCompatibility
  C.SimpleScriptInBabbage   -> AnyCompatibility
  C.SimpleScriptInConway    -> AnyCompatibility
scriptWitnessCompat (C.SimpleScriptWitness _ _) = AnyCompatibility

-- | Compatibility level of the transaction
txCompatibility :: C.TxBodyContent C.BuildTx C.BabbageEra -> UTxOCompatibility
txCompatibility = foldr min AnyCompatibility . fmap (anyScriptWitnessCompat . snd) . C.collectTxBodyScriptWitnesses C.ShelleyBasedEraBabbage
