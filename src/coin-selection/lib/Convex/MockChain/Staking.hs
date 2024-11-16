{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
module Convex.MockChain.Staking (registerPool) where

import qualified Cardano.Api.Ledger             as Ledger
import qualified Cardano.Api.Shelley            as C
import qualified Cardano.Ledger.Core            as Ledger
import           Control.Lens                   ((^.))
import           Control.Monad                  (void)
import           Control.Monad.Except           (MonadError)
import           Control.Monad.IO.Class         (MonadIO (..))
import qualified Convex.BuildTx                 as BuildTx
import           Convex.Class                   (MonadBlockchain (queryProtocolParameters),
                                                 MonadMockchain)
import           Convex.CoinSelection           (BalanceTxError,
                                                 ChangeOutputPosition (TrailingChange))
import           Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import qualified Convex.MockChain.Defaults      as Defaults
import           Convex.Wallet                  (Wallet)
import           Data.Ratio                     ((%))

{-| Run the 'Mockchain' action with registered pool
-}
registerPool :: forall era m. (MonadIO m, MonadMockchain era m, MonadError (BalanceTxError era) m, MonadFail m, C.IsConwayBasedEra era) => Wallet -> m C.PoolId
registerPool wallet = case C.conwayBasedEra @era of
  C.ConwayEraOnwardsConway -> do
    stakeKey <- C.generateSigningKey C.AsStakeKey
    vrfKey <- C.generateSigningKey C.AsVrfKey
    stakePoolKey <- C.generateSigningKey C.AsStakePoolKey

    let
      vrfHash =
        C.verificationKeyHash . C.getVerificationKey $ vrfKey

      stakeHash =
        C.verificationKeyHash . C.getVerificationKey $ stakeKey

      stakeCred = C.StakeCredentialByKey stakeHash

    pp <- fmap C.unLedgerProtocolParameters queryProtocolParameters
    let
      stakeCert =
        C.makeStakeAddressRegistrationCertificate
        . C.StakeAddrRegistrationConway C.ConwayEraOnwardsConway (pp ^. Ledger.ppKeyDepositL)
        $ stakeCred
      stakeAddress = C.makeStakeAddress Defaults.networkId stakeCred

      stakePoolVerKey = C.getVerificationKey stakePoolKey
      poolId = C.verificationKeyHash stakePoolVerKey

      delegationCert =
        C.makeStakeAddressDelegationCertificate
        $ C.StakeDelegationRequirementsConwayOnwards C.ConwayEraOnwardsConway stakeCred (Ledger.DelegStake $ C.unStakePoolKeyHash poolId)

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
        . C.StakePoolRegistrationRequirementsConwayOnwards C.ConwayEraOnwardsConway
        . C.toShelleyPoolParams
        $ stakePoolParams

      stakeCertTx = BuildTx.execBuildTx $ do
        BuildTx.addCertificate stakeCert

      poolCertTx = BuildTx.execBuildTx $ do
        BuildTx.addCertificate poolCert

      delegCertTx = BuildTx.execBuildTx $ do
        BuildTx.addCertificate delegationCert

    void $ tryBalanceAndSubmit mempty wallet stakeCertTx TrailingChange [C.WitnessStakeKey stakeKey]
    void $ tryBalanceAndSubmit mempty wallet poolCertTx TrailingChange [C.WitnessStakeKey stakeKey, C.WitnessStakePoolKey stakePoolKey]
    void $ tryBalanceAndSubmit mempty wallet delegCertTx TrailingChange [C.WitnessStakeKey stakeKey]

    pure poolId
