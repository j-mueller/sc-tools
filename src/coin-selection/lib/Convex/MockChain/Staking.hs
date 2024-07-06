{-# LANGUAGE NumericUnderscores #-}
module Convex.MockChain.Staking (registerPool) where

import qualified Cardano.Api.Shelley            as C
import           Control.Monad                  (void)
import           Control.Monad.Except           (MonadError)
import           Control.Monad.IO.Class         (MonadIO (..))
import qualified Convex.BuildTx                 as BuildTx
import           Convex.Class                   (MonadMockchain)
import           Convex.CoinSelection           (BalanceTxError,
                                                 ChangeOutputPosition (TrailingChange),
                                                 ERA)
import           Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import qualified Convex.MockChain.Defaults      as Defaults
import           Convex.Wallet                  (Wallet)
import           Data.Ratio                     ((%))

{-| Run the 'Mockchain' action with registered pool
-}
registerPool :: forall m. (MonadIO m, MonadMockchain m, MonadError (BalanceTxError Convex.CoinSelection.ERA) m, MonadFail m) => Wallet -> m C.PoolId
registerPool wallet = do
  stakeKey <- C.generateSigningKey C.AsStakeKey
  vrfKey <- C.generateSigningKey C.AsVrfKey
  stakePoolKey <- C.generateSigningKey C.AsStakePoolKey

  let
    vrfHash =
      C.verificationKeyHash . C.getVerificationKey $ vrfKey

    stakeHash =
      C.verificationKeyHash . C.getVerificationKey $ stakeKey

    stakeCred = C.StakeCredentialByKey stakeHash

    stakeCert =
      C.makeStakeAddressRegistrationCertificate
      . C.StakeAddrRegistrationPreConway C.ShelleyToBabbageEraBabbage
      $ stakeCred
    stakeAddress = C.makeStakeAddress Defaults.networkId stakeCred

    stakePoolVerKey = C.getVerificationKey stakePoolKey
    poolId = C.verificationKeyHash stakePoolVerKey

    delegationCert =
      C.makeStakeAddressDelegationCertificate
      $ C.StakeDelegationRequirementsPreConway C.ShelleyToBabbageEraBabbage stakeCred poolId

    stakePoolParams =
     C.StakePoolParameters
       poolId
       vrfHash
       340_000_000 -- cost
       (3 % 100) -- margin
       stakeAddress
       0 -- pledge
       [stakeHash] -- owners
       [] -- relays
       Nothing

    poolCert =
      C.makeStakePoolRegistrationCertificate
      . C.StakePoolRegistrationRequirementsPreConway C.ShelleyToBabbageEraBabbage
      . C.toShelleyPoolParams
      $ stakePoolParams

    stakeCertTx = BuildTx.execBuildTx $ do
      BuildTx.addCertificate stakeCert

    poolCertTx = BuildTx.execBuildTx $ do
      BuildTx.addCertificate poolCert

    delegCertTx = BuildTx.execBuildTx $ do
      BuildTx.addCertificate delegationCert

  void $ tryBalanceAndSubmit mempty wallet stakeCertTx TrailingChange []
  void $ tryBalanceAndSubmit mempty wallet poolCertTx TrailingChange [C.WitnessStakeKey stakeKey, C.WitnessStakePoolKey stakePoolKey]
  void $ tryBalanceAndSubmit mempty wallet delegCertTx TrailingChange [C.WitnessStakeKey stakeKey]

  pure poolId
