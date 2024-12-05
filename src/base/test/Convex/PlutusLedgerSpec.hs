{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}

module Convex.PlutusLedgerSpec where

import qualified Cardano.Api.Ledger         as Shelley
import qualified Cardano.Api.Shelley        as C
import           Convex.PlutusLedger.V1     (transAddressShelley,
                                             unTransAddressShelley)
import qualified Test.Gen.Cardano.Api.Typed as CGen
import qualified Test.QuickCheck            as QC
import qualified Test.QuickCheck.Hedgehog   as QC

newtype ArbitraryNetworkMagic = ArbitraryNetworkMagic C.NetworkMagic
  deriving stock (Show)

instance QC.Arbitrary ArbitraryNetworkMagic where
  arbitrary = fmap ArbitraryNetworkMagic $ QC.hedgehog CGen.genNetworkMagic

newtype ArbitraryAddressShelley = ArbitraryAddressShelley (C.Address C.ShelleyAddr)
  deriving stock (Show)

instance QC.Arbitrary ArbitraryAddressShelley where
  arbitrary = fmap ArbitraryAddressShelley $ QC.hedgehog CGen.genAddressShelley

prop_rountripAddressShelleyPlutusTranslation :: ArbitraryNetworkMagic -> ArbitraryAddressShelley -> Bool
prop_rountripAddressShelleyPlutusTranslation
  (ArbitraryNetworkMagic nm)
  (ArbitraryAddressShelley addr@(C.ShelleyAddress n _ _))
  = do
  let nid = case n of Shelley.Mainnet -> C.Mainnet; Shelley.Testnet -> C.Testnet nm
  case unTransAddressShelley nid (transAddressShelley addr) of
    Left _err   -> False
    Right addr' -> addr' == addr
