module Convex.Eon (
  IsShelleyToBabbageEra(shelleyToBabbageEra)
) where

import qualified Cardano.Api as C

-- TODO This was deleted from cardano-api because they said it was unused. See
-- https://github.com/IntersectMBO/cardano-api/pull/256. However, because of the
-- Certificate type starting at ConwayEra, that typeclass is a nice to have.
class C.IsShelleyBasedEra era => IsShelleyToBabbageEra era where
  shelleyToBabbageEra :: C.ShelleyToBabbageEra era

instance IsShelleyToBabbageEra C.ShelleyEra where
  shelleyToBabbageEra = C.ShelleyToBabbageEraShelley

instance IsShelleyToBabbageEra C.AllegraEra where
  shelleyToBabbageEra = C.ShelleyToBabbageEraAllegra

instance IsShelleyToBabbageEra C.MaryEra where
  shelleyToBabbageEra = C.ShelleyToBabbageEraMary

instance IsShelleyToBabbageEra C.AlonzoEra where
  shelleyToBabbageEra = C.ShelleyToBabbageEraAlonzo

instance IsShelleyToBabbageEra C.BabbageEra where
  shelleyToBabbageEra = C.ShelleyToBabbageEraBabbage
