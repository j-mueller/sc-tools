{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Convex.PlutusLedgerSpec where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as Shelley
import Convex.PlutusLedger.V1 (
  transAddressShelley,
  unTransAddressShelley,
 )
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Hedgehog qualified as QC

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
  (ArbitraryAddressShelley addr@(C.ShelleyAddress n _ _)) =
    do
      let nid = case n of Shelley.Mainnet -> C.Mainnet; Shelley.Testnet -> C.Testnet nm
      case unTransAddressShelley nid (transAddressShelley addr) of
        Left _err -> False
        Right addr' -> addr' == addr
