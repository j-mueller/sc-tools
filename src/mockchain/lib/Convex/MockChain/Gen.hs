{-| QuickCheck generators for @sc-tools@ types
-}
module Convex.MockChain.Gen(
  seed,
  signingKeyExtended,
  signingKey,
  stakeKey,
  operator
  ) where

import qualified Cardano.Api            as C
import           Cardano.Crypto.Seed    (Seed)
import qualified Cardano.Crypto.Seed    as Seed
import           Convex.Wallet.Operator (Operator (..), PaymentExtendedKey (..),
                                         Signing)
import qualified Data.ByteString        as BS
import           Data.Proxy             (Proxy (..))
import qualified Test.QuickCheck        as QC

-- | A seed value for cryptographic keys
seed :: QC.Gen Seed
seed = Seed.mkSeedFromBytes . BS.pack <$> QC.vector 128

-- | A signing key
signingKeyExtended :: QC.Gen (C.SigningKey C.PaymentExtendedKey)
signingKeyExtended = fmap (C.deterministicSigningKey (C.proxyToAsType Proxy)) seed

-- | An extended (HD) signing key
signingKey :: QC.Gen (C.SigningKey C.PaymentKey)
signingKey = fmap (C.deterministicSigningKey (C.proxyToAsType Proxy)) seed

-- | A stake key
stakeKey :: QC.Gen (C.SigningKey C.StakeKey)
stakeKey = fmap (C.deterministicSigningKey (C.proxyToAsType Proxy)) seed

-- | An 'Convex.Wallet.Operator' with payment and optional staking credentials
operator :: QC.Gen (Operator Signing)
operator =
  Operator
    -- <$> fmap (either PESigningEx PESigning) (QC.oneof [fmap Left signingKeyExtended, fmap Right signingKey])
    <$> fmap PESigning signingKey
    -- <*> QC.oneof [pure Nothing, Just . C.getVerificationKey <$> stakeKey]
    <*> pure Nothing
