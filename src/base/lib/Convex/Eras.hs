{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-| Utilities for dealing with eras
-}
module Convex.Eras(
  InAnyBabbageEraOnwards(..),
  inAnyBabbageEraOnwards,
  inAnyBabbageEraOnwardsBabbage,
  inAnyBabbageEraOnwardsConway,
  toInAnyCardanoEra,
  fromInAnyCardanoEra,
  -- * Downgrades to babbage era
  babbageProtocolParams,
  babbageTxOut,
  babbageUTxO,
  upgradeUTxO,
  downgradeTxOut,
  upgradeTxBodyContent
) where

import           Cardano.Api                  (BabbageEra,
                                               BabbageEraOnwards (..),
                                               ConwayEra)
import           Cardano.Api.Ledger           (Coin, ConwayTxCert (..),
                                               ShelleyTxCert (..))
import           Cardano.Api.Shelley          (BuildTx, BuildTxWith (..),
                                               CardanoEra (..), CtxUTxO,
                                               InAnyCardanoEra (..),
                                               LedgerProtocolParameters (..),
                                               ReferenceScript (..),
                                               StakeAddress, TxBodyContent (..),
                                               TxOut (..), TxOutDatum (..),
                                               UTxO (..), WitCtxStake,
                                               Witness (..))
import qualified Cardano.Api.Shelley          as C
import qualified Cardano.Ledger.Conway.TxCert as L
import qualified Cardano.Ledger.Core          as PParams
import qualified Control.Lens                 as L
import qualified Convex.CardanoApi.Lenses     as L
import           Data.Bifunctor               (Bifunctor (..))
import           Data.Typeable                (Typeable)

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

-- | Strengthen 'InAnyCardanoEra' to 'InAnyBabbageEraOnwards'
fromInAnyCardanoEra :: InAnyCardanoEra thing -> Maybe (InAnyBabbageEraOnwards thing)
fromInAnyCardanoEra = \case
  InAnyCardanoEra BabbageEra thing -> Just (InAnyBabbageEraOnwards BabbageEraOnwardsBabbage thing)
  InAnyCardanoEra ConwayEra thing -> Just (InAnyBabbageEraOnwards BabbageEraOnwardsConway thing)
  _ -> Nothing

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

upgradeUTxO :: C.UTxO BabbageEra -> C.UTxO ConwayEra
upgradeUTxO (C.UTxO m) = C.UTxO (fmap upgradeTxOut m)

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

upgradeTxWitnes :: forall ctx. C.Witness ctx BabbageEra -> C.Witness ctx ConwayEra
upgradeTxWitnes = \case
  KeyWitness C.KeyWitnessForSpending -> KeyWitness C.KeyWitnessForSpending
  KeyWitness C.KeyWitnessForStakeAddr -> KeyWitness C.KeyWitnessForStakeAddr
  ScriptWitness purpose wit     -> ScriptWitness purpose (updateScriptWitness wit)

updateScriptWitness :: C.ScriptWitness ctx BabbageEra -> C.ScriptWitness ctx ConwayEra
updateScriptWitness wit =
  case wit of
    C.SimpleScriptWitness _ script -> C.SimpleScriptWitness C.SimpleScriptInConway script
    C.PlutusScriptWitness _ plutusScriptVersion plutusSCript scriptDatum redeemer exUnits ->
      C.PlutusScriptWitness (plutusScriptLanguageInConwayEra plutusScriptVersion) plutusScriptVersion plutusSCript scriptDatum redeemer exUnits

{-| Bump a cardano-era tx body content to a conway-era tx body content
-}
upgradeTxBodyContent :: C.LedgerProtocolParameters ConwayEra -> TxBodyContent BuildTx BabbageEra -> TxBodyContent BuildTx ConwayEra
upgradeTxBodyContent protParams txb =
  TxBodyContent
    { txIns = second (fmap upgradeTxWitnes) <$> txIns txb
    , txInsCollateral = upgradeTxInsCollateral (txInsCollateral txb)
    , txInsReference = upgradeTxInsReference (txInsReference txb)
    , txOuts = upgradeTxOut <$> txOuts txb
    , txTotalCollateral = upgradeTxTotalCollateral (txTotalCollateral txb)
    , txReturnCollateral = upgradeTxReturnCollateral (txReturnCollateral txb)
    , txFee = upgradeTxFee (txFee txb)
    , txValidityLowerBound = upgradeTxValidityLowerBound (txValidityLowerBound txb)
    , txValidityUpperBound = upgradeTxValidityUpperBound (txValidityUpperBound txb)
    , txMetadata = upgradeTxMetadata (txMetadata txb)
    , txAuxScripts = upgradeTxAuxScripts (txAuxScripts txb)
    , txExtraKeyWits = upgradeTxExtraKeyWits (txExtraKeyWits txb)
    , txProtocolParams = C.BuildTxWith (Just protParams)
    , txWithdrawals = updateTxWithdrawals (txWithdrawals txb)
    , txCertificates = updateTxCertificates (txCertificates txb)
    , txUpdateProposal = updateTxUpdateProposal (txUpdateProposal txb)
    , txMintValue = updateTxMintValue (txMintValue txb)
    , txScriptValidity = updateTxScriptValidity (txScriptValidity txb)
    , txProposalProcedures = Nothing
    , txVotingProcedures = Nothing
    , txCurrentTreasuryValue = Nothing
    , txTreasuryDonation = Nothing
    }

plutusScriptLanguageInConwayEra :: C.PlutusScriptVersion lang -> C.ScriptLanguageInEra lang ConwayEra
plutusScriptLanguageInConwayEra = \case
  C.PlutusScriptV1 -> C.PlutusScriptV1InConway
  C.PlutusScriptV2 -> C.PlutusScriptV2InConway
  C.PlutusScriptV3 -> C.PlutusScriptV3InConway

upgradeTxInsCollateral :: C.TxInsCollateral BabbageEra -> C.TxInsCollateral ConwayEra
upgradeTxInsCollateral = \case
  C.TxInsCollateralNone -> C.TxInsCollateralNone
  C.TxInsCollateral _ txIns_ -> C.TxInsCollateral C.alonzoBasedEra txIns_

upgradeTxInsReference :: C.TxInsReference BuildTx BabbageEra -> C.TxInsReference BuildTx ConwayEra
upgradeTxInsReference = \case
  C.TxInsReferenceNone -> C.TxInsReferenceNone
  C.TxInsReference _ txIns_ -> C.TxInsReference C.babbageBasedEra txIns_

upgradeTxOut :: TxOut ctx BabbageEra -> TxOut ctx ConwayEra
upgradeTxOut txOut =
  let (addr, vl, dat, ref) = L.view L._TxOut txOut
  in TxOut
      (C.fromShelleyAddr C.shelleyBasedEra $ C.toShelleyAddr addr)
      (C.TxOutValueShelleyBased C.shelleyBasedEra $ C.toMaryValue $ C.txOutValueToValue vl)
      (upgradeTxOutDatum dat)
      (upgradeTxOutRefScript ref)

upgradeTxOutDatum :: TxOutDatum ctx BabbageEra -> TxOutDatum ctx ConwayEra
upgradeTxOutDatum = \case
  TxOutDatumNone -> TxOutDatumNone
  TxOutDatumHash _era hash -> TxOutDatumHash C.alonzoBasedEra hash
  TxOutDatumInline _era dat -> TxOutDatumInline C.babbageBasedEra dat

upgradeTxOutRefScript :: ReferenceScript BabbageEra -> ReferenceScript ConwayEra
upgradeTxOutRefScript = \case
  ReferenceScriptNone -> ReferenceScriptNone
  ReferenceScript _era script -> ReferenceScript C.babbageBasedEra script

upgradeTxTotalCollateral :: C.TxTotalCollateral BabbageEra -> C.TxTotalCollateral ConwayEra
upgradeTxTotalCollateral = \case
  C.TxTotalCollateralNone -> C.TxTotalCollateralNone
  C.TxTotalCollateral _ coin -> C.TxTotalCollateral BabbageEraOnwardsConway coin

upgradeTxReturnCollateral :: C.TxReturnCollateral C.CtxTx BabbageEra -> C.TxReturnCollateral C.CtxTx ConwayEra
upgradeTxReturnCollateral = \case
  C.TxReturnCollateralNone -> C.TxReturnCollateralNone
  C.TxReturnCollateral _ txOut -> C.TxReturnCollateral BabbageEraOnwardsConway (upgradeTxOut txOut)

upgradeTxFee :: C.TxFee BabbageEra -> C.TxFee ConwayEra
upgradeTxFee = \case
  C.TxFeeExplicit _ coin -> C.TxFeeExplicit C.shelleyBasedEra coin

upgradeTxValidityLowerBound :: C.TxValidityLowerBound BabbageEra -> C.TxValidityLowerBound ConwayEra
upgradeTxValidityLowerBound = \case
  C.TxValidityNoLowerBound -> C.TxValidityNoLowerBound
  C.TxValidityLowerBound _era slotNo -> C.TxValidityLowerBound C.allegraBasedEra slotNo

upgradeTxValidityUpperBound :: C.TxValidityUpperBound BabbageEra -> C.TxValidityUpperBound ConwayEra
upgradeTxValidityUpperBound = \case
  C.TxValidityUpperBound _era slotNo -> C.TxValidityUpperBound C.shelleyBasedEra slotNo

upgradeTxMetadata :: C.TxMetadataInEra BabbageEra -> C.TxMetadataInEra ConwayEra
upgradeTxMetadata = \case
  C.TxMetadataNone -> C.TxMetadataNone
  C.TxMetadataInEra _ md -> C.TxMetadataInEra C.shelleyBasedEra md

upgradeTxAuxScripts :: C.TxAuxScripts BabbageEra -> C.TxAuxScripts ConwayEra
upgradeTxAuxScripts = \case
  C.TxAuxScriptsNone -> C.TxAuxScriptsNone
  C.TxAuxScripts _era scripts -> C.TxAuxScripts C.allegraBasedEra (upgradeScriptInEra <$> scripts)

upgradeScriptInEra :: C.ScriptInEra BabbageEra -> C.ScriptInEra ConwayEra
upgradeScriptInEra = \case
  C.ScriptInEra lang script -> C.ScriptInEra (updateScriptLanguageInEra lang) script

updateScriptLanguageInEra :: C.ScriptLanguageInEra lang BabbageEra -> C.ScriptLanguageInEra lang ConwayEra
updateScriptLanguageInEra = \case
  C.SimpleScriptInBabbage -> C.SimpleScriptInConway
  C.PlutusScriptV1InBabbage -> C.PlutusScriptV1InConway
  C.PlutusScriptV2InBabbage -> C.PlutusScriptV2InConway

upgradeTxExtraKeyWits :: C.TxExtraKeyWitnesses BabbageEra -> C.TxExtraKeyWitnesses ConwayEra
upgradeTxExtraKeyWits = \case
  C.TxExtraKeyWitnessesNone -> C.TxExtraKeyWitnessesNone
  C.TxExtraKeyWitnesses _ keys -> C.TxExtraKeyWitnesses C.alonzoBasedEra keys

updateTxWithdrawals :: C.TxWithdrawals BuildTx BabbageEra -> C.TxWithdrawals BuildTx ConwayEra
updateTxWithdrawals = \case
  C.TxWithdrawalsNone -> C.TxWithdrawalsNone
  C.TxWithdrawals _era withdrawals -> C.TxWithdrawals C.shelleyBasedEra (updateWithdrawal <$> withdrawals)

updateWithdrawal :: (StakeAddress, Coin, BuildTxWith BuildTx (Witness WitCtxStake BabbageEra)) -> (StakeAddress, Coin, BuildTxWith BuildTx (Witness WitCtxStake ConwayEra))
updateWithdrawal (addr, coin, btx) =
  let btx' = fmap upgradeTxWitnes btx
  in (addr, coin, btx')

updateTxCertificates :: C.TxCertificates BuildTx BabbageEra -> C.TxCertificates BuildTx ConwayEra
updateTxCertificates = \case
  C.TxCertificatesNone -> C.TxCertificatesNone
  C.TxCertificates _era certs buildWith -> C.TxCertificates C.shelleyBasedEra (upgradeCertificate <$> certs) (fmap upgradeTxWitnes <$> buildWith)

upgradeCertificate :: C.Certificate BabbageEra -> C.Certificate ConwayEra
upgradeCertificate = \case
  C.ShelleyRelatedCertificate _era cert -> C.ConwayCertificate C.conwayBasedEra (upgradeTxCert cert)
  C.ConwayCertificate _era _cert -> error "Impossible: Conway certificate in babbage era"

upgradeTxCert :: ShelleyTxCert (C.ShelleyLedgerEra BabbageEra) -> ConwayTxCert (C.ShelleyLedgerEra ConwayEra)
upgradeTxCert = \case
  ShelleyTxCertDelegCert cert -> ConwayTxCertDeleg (L.fromShelleyDelegCert cert)
  ShelleyTxCertPool cert      -> ConwayTxCertPool cert
  ShelleyTxCertGenesisDeleg{} -> error "Convx.Eras.upgradeTxCert Not supported: ShelleyTxCertGenesisDeleg"
  ShelleyTxCertMir{}          -> error "Convx.Eras.upgradeTxCert Not supported: ShelleyTxCertMir"

updateTxUpdateProposal :: C.TxUpdateProposal BabbageEra -> C.TxUpdateProposal ConwayEra
updateTxUpdateProposal = \case
  C.TxUpdateProposalNone -> C.TxUpdateProposalNone
  C.TxUpdateProposal{} -> error "Convx.Eras.updateTxUpdateProposal Not supported: TxUpdateProposal"

updateTxMintValue :: C.TxMintValue BuildTx BabbageEra -> C.TxMintValue BuildTx ConwayEra
updateTxMintValue = \case
  C.TxMintNone -> C.TxMintNone
  C.TxMintValue _era value wits -> C.TxMintValue C.maryBasedEra value (fmap (fmap updateScriptWitness) wits)

updateTxScriptValidity :: C.TxScriptValidity BabbageEra -> C.TxScriptValidity ConwayEra
updateTxScriptValidity = \case
  C.TxScriptValidityNone -> C.TxScriptValidityNone
  C.TxScriptValidity _ v -> C.TxScriptValidity C.alonzoBasedEra v
