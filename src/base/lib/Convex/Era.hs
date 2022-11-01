module Convex.Era(ERA) where

import           Cardano.Ledger.Babbage (BabbageEra)
import           Cardano.Ledger.Crypto  (StandardCrypto)

type ERA = BabbageEra StandardCrypto
