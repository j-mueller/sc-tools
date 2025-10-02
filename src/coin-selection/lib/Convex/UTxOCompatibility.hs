{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | Filter the UTxO set for outputs that are compatible
    with specific versions of Plutus
-}
module Convex.UTxOCompatibility (
  UTxOCompatibility (..),
  compatibleWith,
  deleteInlineDatums,
  scriptWitnessCompat,
  anyScriptWitnessCompat,
  txCompatibility,
) where

import Cardano.Api (UTxO (..))
import Cardano.Api qualified as C
import Control.Lens qualified as L
import Convex.CardanoApi.Lenses qualified as L
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import GHC.Generics (Generic)

{- | Compatibility setting for coin selection.
  Used to filter out incompatible UTxOs for
  coin selection.
-}
data UTxOCompatibility
  = -- NOTE: The order of constructors is important
    --       as it determines the 'Ord' instance, which
    --       we use in 'txCompatibility'

    -- | Plutus V1 scripts can't be run in a transaction that also spends (script OR public key) outputs with inline datums.
    PlutusV1Compatibility
  | -- | All outputs
    AnyCompatibility
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Delete incompatible outputs from the UTxO set
compatibleWith :: (C.IsBabbageBasedEra era) => UTxOCompatibility -> UTxO era -> UTxO era
compatibleWith = \case
  PlutusV1Compatibility -> deleteInlineDatums
  AnyCompatibility -> id

-- | Delete UTxOs that have inline datums, as this is not supported by Plutus V1
deleteInlineDatums :: (C.IsBabbageBasedEra era) => UTxO era -> UTxO era
deleteInlineDatums (UTxO o) =
  let hasInlineDatum = isJust . L.preview (L._TxOut . L._3 . L._TxOutDatumInline)
   in UTxO (Map.filter (not . hasInlineDatum) o)

anyScriptWitnessCompat :: forall era. C.AnyScriptWitness era -> UTxOCompatibility
anyScriptWitnessCompat = \case
  C.AnyScriptWitness wit -> scriptWitnessCompat wit

{- | The highest possible compatibility level at which the script
  can be run
-}
scriptWitnessCompat :: forall witctx era. C.ScriptWitness witctx era -> UTxOCompatibility
scriptWitnessCompat (C.PlutusScriptWitness lang _ _ _ _ _) = case lang of
  C.PlutusScriptV1InAlonzo -> PlutusV1Compatibility
  C.PlutusScriptV1InBabbage -> PlutusV1Compatibility
  C.PlutusScriptV1InConway -> PlutusV1Compatibility
  C.PlutusScriptV2InBabbage -> AnyCompatibility
  C.PlutusScriptV2InConway -> AnyCompatibility
  C.PlutusScriptV3InConway -> AnyCompatibility
  C.SimpleScriptInShelley -> AnyCompatibility
  C.SimpleScriptInAllegra -> AnyCompatibility
  C.SimpleScriptInMary -> AnyCompatibility
  C.SimpleScriptInAlonzo -> AnyCompatibility
  C.SimpleScriptInBabbage -> AnyCompatibility
  C.SimpleScriptInConway -> AnyCompatibility
scriptWitnessCompat (C.SimpleScriptWitness _ _) = AnyCompatibility

-- | Compatibility level of the transaction
txCompatibility :: (C.IsShelleyBasedEra era) => C.TxBodyContent C.BuildTx era -> UTxOCompatibility
txCompatibility = foldr (min . anyScriptWitnessCompat . snd) AnyCompatibility . C.collectTxBodyScriptWitnesses C.shelleyBasedEra
