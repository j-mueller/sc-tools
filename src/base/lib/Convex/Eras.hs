{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-| Utilities for dealing with eras
-}
module Convex.Eras(
  InAnyBabbageEraOnwards(..),
  inAnyBabbageEraOnwards,
  inAnyBabbageEraOnwardsBabbage,
  inAnyBabbageEraOnwardsConway,
  toInAnyCardanoEra,
  -- * Downgrades to babbage era
  babbageProtocolParams,
  babbageTxOut,
  babbageUTxO,
  downgradeTxOut
) where

import           Cardano.Api              (BabbageEra, BabbageEraOnwards (..),
                                           ConwayEra)
import           Cardano.Api.Shelley      (CtxUTxO, InAnyCardanoEra (..),
                                           LedgerProtocolParameters (..),
                                           ReferenceScript (..), TxOut (..),
                                           TxOutDatum (..), UTxO (..))
import qualified Cardano.Api.Shelley      as C
import qualified Cardano.Ledger.Core      as PParams
import qualified Control.Lens             as L
import qualified Convex.CardanoApi.Lenses as L
import           Data.Typeable            (Typeable)

-- | A value with unknown Babbage-onwards era type.
--   Similar to Cardano.Api.Eras.Core but for modern eras.
data InAnyBabbageEraOnwards thing where
  InAnyBabbageEraOnwards
    :: Typeable era
    => BabbageEraOnwards era
    -> thing era
    -> InAnyBabbageEraOnwards thing

-- | Relax 'InAnyBabbageEraOnwards' to 'InAnyCardanoEra'
toInAnyCardanoEra :: InAnyBabbageEraOnwards thing -> InAnyCardanoEra thing
toInAnyCardanoEra = \case
  InAnyBabbageEraOnwards BabbageEraOnwardsBabbage thing -> C.inAnyCardanoEra C.cardanoEra thing
  InAnyBabbageEraOnwards BabbageEraOnwardsConway thing -> C.inAnyCardanoEra C.cardanoEra thing

-- | Constructor for 'InAnyBabbageEraOnwards'
inAnyBabbageEraOnwards :: Typeable era => BabbageEraOnwards era -> thing era -> InAnyBabbageEraOnwards thing
inAnyBabbageEraOnwards = InAnyBabbageEraOnwards

-- | 'inAnyBabbageEraOnwards' specialized to babbage
inAnyBabbageEraOnwardsBabbage :: thing BabbageEra -> InAnyBabbageEraOnwards thing
inAnyBabbageEraOnwardsBabbage = InAnyBabbageEraOnwards BabbageEraOnwardsBabbage

-- | 'inAnyBabbageEraOnwards' specialized to conway
inAnyBabbageEraOnwardsConway :: thing ConwayEra -> InAnyBabbageEraOnwards thing
inAnyBabbageEraOnwardsConway = InAnyBabbageEraOnwards BabbageEraOnwardsConway

-- | Babbage-era protocol parameters
babbageProtocolParams :: InAnyBabbageEraOnwards LedgerProtocolParameters -> LedgerProtocolParameters BabbageEra
babbageProtocolParams = \case
  InAnyBabbageEraOnwards BabbageEraOnwardsBabbage p -> p
  InAnyBabbageEraOnwards BabbageEraOnwardsConway (LedgerProtocolParameters p)  ->
    LedgerProtocolParameters $ PParams.downgradePParams () p

-- | Babbage-era UTxO
babbageUTxO :: InAnyBabbageEraOnwards C.UTxO -> C.UTxO BabbageEra
babbageUTxO = \case
  InAnyBabbageEraOnwards BabbageEraOnwardsBabbage p -> p
  InAnyBabbageEraOnwards BabbageEraOnwardsConway (UTxO m) ->
    UTxO $ fmap downgradeTxOut m

-- | Babbage-era tx outputs. We can always downgrade conway-era outputs because
--   they didn't change compared to babbage.
babbageTxOut :: InAnyBabbageEraOnwards (TxOut CtxUTxO) -> TxOut CtxUTxO BabbageEra
babbageTxOut = \case
  InAnyBabbageEraOnwards BabbageEraOnwardsBabbage o -> o
  InAnyBabbageEraOnwards BabbageEraOnwardsConway txOut ->
    downgradeTxOut txOut

downgradeTxOut :: TxOut CtxUTxO ConwayEra -> TxOut CtxUTxO BabbageEra
downgradeTxOut txOut =
  let (addr, vl, dat, ref) = L.view L._TxOut txOut
  in TxOut
      (C.fromShelleyAddr C.shelleyBasedEra $ C.toShelleyAddr addr)
      (C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toMaryValue $ C.txOutValueToValue vl)
      (downgradeTxOutDatum dat)
      (downgradeTxOutRefScript ref)

downgradeTxOutDatum :: TxOutDatum CtxUTxO ConwayEra -> TxOutDatum CtxUTxO BabbageEra
downgradeTxOutDatum = \case
  TxOutDatumNone -> TxOutDatumNone
  TxOutDatumHash _era hash -> TxOutDatumHash C.alonzoBasedEra hash
  TxOutDatumInline _era dat -> TxOutDatumInline C.babbageBasedEra dat

downgradeTxOutRefScript :: ReferenceScript ConwayEra -> ReferenceScript BabbageEra
downgradeTxOutRefScript = \case
  ReferenceScriptNone -> ReferenceScriptNone
  ReferenceScript _era script -> ReferenceScript C.babbageBasedEra script
