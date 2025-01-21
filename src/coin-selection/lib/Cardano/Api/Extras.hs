{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

{- | This module exists solely to hold a copy of 'substituteExecutionUnits'
from @cardano-api@.
-}
module Cardano.Api.Extras (
  substituteExecutionUnits,
) where

import Cardano.Api.Ledger qualified as CLedger
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley (
  BuildTx,
  TxBodyContent,
 )
import Cardano.Api.Shelley qualified as C
import Control.Lens ((&))
import Data.Function (on)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (
  maybeToList,
 )
import GHC.IsList (IsList (fromList, toList))

substituteExecutionUnits
  :: forall era
   . Map C.ScriptWitnessIndex C.ExecutionUnits
  -> TxBodyContent BuildTx era
  -> Either (C.TxBodyErrorAutoBalance era) (TxBodyContent BuildTx era)
substituteExecutionUnits
  exUnitsMap
  txbodycontent@( C.TxBodyContent
                    txIns
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    _
                    txWithdrawals
                    txCertificates
                    _
                    txMintValue
                    _
                    txProposalProcedures
                    txVotingProcedures
                    _
                    _
                  ) = do
    mappedTxIns <- mapScriptWitnessesTxIns txIns
    mappedWithdrawals <- mapScriptWitnessesWithdrawals txWithdrawals
    mappedMintedVals <- mapScriptWitnessesMinting txMintValue
    mappedTxCertificates <- mapScriptWitnessesCertificates txCertificates
    mappedVotes <- mapScriptWitnessesVotes txVotingProcedures
    mappedProposals <- mapScriptWitnessesProposals txProposalProcedures

    Right $
      txbodycontent
        & C.setTxIns mappedTxIns
        & C.setTxMintValue mappedMintedVals
        & C.setTxCertificates mappedTxCertificates
        & C.setTxWithdrawals mappedWithdrawals
        & C.setTxVotingProcedures mappedVotes
        & C.setTxProposalProcedures mappedProposals
   where
    substituteExecUnits
      :: C.ScriptWitnessIndex
      -> C.ScriptWitness witctx era
      -> Either (C.TxBodyErrorAutoBalance era) (C.ScriptWitness witctx era)
    substituteExecUnits _ wit@C.SimpleScriptWitness{} = Right wit
    substituteExecUnits idx (C.PlutusScriptWitness langInEra version script datum redeemer _) =
      case Map.lookup idx exUnitsMap of
        Nothing ->
          Left $ C.TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap idx exUnitsMap
        Just exunits ->
          Right $
            C.PlutusScriptWitness
              langInEra
              version
              script
              datum
              redeemer
              exunits

    adjustScriptWitness
      :: (C.ScriptWitness witctx era -> Either (C.TxBodyErrorAutoBalance era) (C.ScriptWitness witctx era))
      -> C.Witness witctx era
      -> Either (C.TxBodyErrorAutoBalance era) (C.Witness witctx era)
    adjustScriptWitness _ (C.KeyWitness ctx) = Right $ C.KeyWitness ctx
    adjustScriptWitness g (C.ScriptWitness ctx witness') = C.ScriptWitness ctx <$> g witness'

    mapScriptWitnessesTxIns
      :: [(C.TxIn, C.BuildTxWith BuildTx (C.Witness C.WitCtxTxIn era))]
      -> Either (C.TxBodyErrorAutoBalance era) [(C.TxIn, C.BuildTxWith BuildTx (C.Witness C.WitCtxTxIn era))]
    mapScriptWitnessesTxIns txins =
      let mappedScriptWitnesses
            :: [ ( C.TxIn
                 , Either (C.TxBodyErrorAutoBalance era) (C.BuildTxWith BuildTx (C.Witness C.WitCtxTxIn era))
                 )
               ]
          mappedScriptWitnesses =
            [ (txin, C.BuildTxWith <$> wit')
            | (ix, txin, wit) <- indexTxIns txins
            , let wit' = adjustScriptWitness (substituteExecUnits ix) wit
            ]
       in traverse
            (\(txIn, eWitness) -> (txIn,) <$> eWitness)
            mappedScriptWitnesses

    mapScriptWitnessesWithdrawals
      :: C.TxWithdrawals BuildTx era
      -> Either (C.TxBodyErrorAutoBalance era) (C.TxWithdrawals BuildTx era)
    mapScriptWitnessesWithdrawals C.TxWithdrawalsNone = Right C.TxWithdrawalsNone
    mapScriptWitnessesWithdrawals txWithdrawals'@(C.TxWithdrawals supported _) =
      let mappedWithdrawals
            :: [ ( C.StakeAddress
                 , L.Coin
                 , Either (C.TxBodyErrorAutoBalance era) (C.BuildTxWith BuildTx (C.Witness C.WitCtxStake era))
                 )
               ]
          mappedWithdrawals =
            [ (addr, withdrawal, C.BuildTxWith <$> mappedWitness)
            | (ix, addr, withdrawal, wit) <- indexTxWithdrawals txWithdrawals'
            , let mappedWitness = adjustScriptWitness (substituteExecUnits ix) wit
            ]
       in C.TxWithdrawals supported
            <$> traverse
              (\(sAddr, ll, eWitness) -> (sAddr,ll,) <$> eWitness)
              mappedWithdrawals

    mapScriptWitnessesCertificates
      :: C.TxCertificates BuildTx era
      -> Either (C.TxBodyErrorAutoBalance era) (C.TxCertificates BuildTx era)
    mapScriptWitnessesCertificates C.TxCertificatesNone = Right C.TxCertificatesNone
    mapScriptWitnessesCertificates txCertificates'@(C.TxCertificates supported certs _) =
      let mappedScriptWitnesses
            :: [(C.StakeCredential, Either (C.TxBodyErrorAutoBalance era) (C.Witness C.WitCtxStake era))]
          mappedScriptWitnesses =
            [ (stakeCred, witness')
            | (ix, _, stakeCred, witness) <- indexTxCertificates txCertificates'
            , let witness' = adjustScriptWitness (substituteExecUnits ix) witness
            ]
       in C.TxCertificates supported certs . C.BuildTxWith
            <$> traverse
              (\(sCred, eScriptWitness) -> (sCred,) <$> eScriptWitness)
              mappedScriptWitnesses

    mapScriptWitnessesVotes
      :: Maybe (C.Featured C.ConwayEraOnwards era (C.TxVotingProcedures build era))
      -> Either
          (C.TxBodyErrorAutoBalance era)
          (Maybe (C.Featured C.ConwayEraOnwards era (C.TxVotingProcedures build era)))
    mapScriptWitnessesVotes Nothing = return Nothing
    mapScriptWitnessesVotes (Just (C.Featured _ C.TxVotingProceduresNone)) = return Nothing
    mapScriptWitnessesVotes (Just (C.Featured _ (C.TxVotingProcedures _ C.ViewTx))) = return Nothing
    mapScriptWitnessesVotes (Just (C.Featured era txVotingProcedures'@(C.TxVotingProcedures vProcedures (C.BuildTxWith _)))) = do
      let eSubstitutedExecutionUnits =
            [ (vote, updatedWitness)
            | (ix, vote, witness) <- indexTxVotingProcedures txVotingProcedures'
            , let updatedWitness = substituteExecUnits ix witness
            ]

      substitutedExecutionUnits <- traverseScriptWitnesses eSubstitutedExecutionUnits

      return $
        Just
          (C.Featured era (C.TxVotingProcedures vProcedures (C.BuildTxWith $ fromList substitutedExecutionUnits)))

    mapScriptWitnessesProposals
      :: Maybe (C.Featured C.ConwayEraOnwards era (C.TxProposalProcedures build era))
      -> Either
          (C.TxBodyErrorAutoBalance era)
          (Maybe (C.Featured C.ConwayEraOnwards era (C.TxProposalProcedures build era)))
    mapScriptWitnessesProposals Nothing = return Nothing
    mapScriptWitnessesProposals (Just (C.Featured _ C.TxProposalProceduresNone)) = return Nothing
    mapScriptWitnessesProposals (Just (C.Featured _ (C.TxProposalProcedures _ C.ViewTx))) = return Nothing
    mapScriptWitnessesProposals (Just (C.Featured era txpp@(C.TxProposalProcedures osetProposalProcedures (C.BuildTxWith _)))) = do
      let eSubstitutedExecutionUnits =
            [ (proposal, updatedWitness)
            | (ix, proposal, scriptWitness) <- indexTxProposalProcedures txpp
            , let updatedWitness = substituteExecUnits ix scriptWitness
            ]

      substitutedExecutionUnits <- traverseScriptWitnesses eSubstitutedExecutionUnits

      return $
        Just
          ( C.Featured
              era
              (C.TxProposalProcedures osetProposalProcedures (C.BuildTxWith $ fromList substitutedExecutionUnits))
          )

    mapScriptWitnessesMinting
      :: C.TxMintValue BuildTx era
      -> Either (C.TxBodyErrorAutoBalance era) (C.TxMintValue BuildTx era)
    mapScriptWitnessesMinting C.TxMintNone = Right C.TxMintNone
    mapScriptWitnessesMinting txMintValue'@(C.TxMintValue w _) = do
      let mappedScriptWitnesses =
            [ (policyId, pure . (assetName',quantity,) <$> substitutedWitness)
            | (ix, policyId, assetName', quantity, C.BuildTxWith witness) <- C.indexTxMintValue txMintValue'
            , let substitutedWitness = C.BuildTxWith <$> substituteExecUnits ix witness
            ]
      final <- Map.fromListWith (<>) <$> traverseScriptWitnesses mappedScriptWitnesses
      pure $ C.TxMintValue w final

{- | Index transaction inputs ordered by TxIn
Please note that the result can contain also 'KeyWitness'es.
See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
-}
indexTxIns
  :: C.TxIns BuildTx era
  -> [(C.ScriptWitnessIndex, C.TxIn, C.Witness C.WitCtxTxIn era)]
indexTxIns txins =
  [ (C.ScriptWitnessIndexTxIn ix, txIn, witness)
  | (ix, (txIn, C.BuildTxWith witness)) <- zip [0 ..] $ orderTxIns txins
  ]

{- | Index the withdrawals with witnesses in the order of stake addresses.
See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
-}
indexTxWithdrawals
  :: C.TxWithdrawals BuildTx era
  -> [(C.ScriptWitnessIndex, C.StakeAddress, L.Coin, C.Witness C.WitCtxStake era)]
indexTxWithdrawals C.TxWithdrawalsNone = []
indexTxWithdrawals (C.TxWithdrawals _ withdrawals) =
  [ (C.ScriptWitnessIndexWithdrawal ix, addr, coin, witness)
  | (ix, (addr, coin, C.BuildTxWith witness)) <- zip [0 ..] (orderStakeAddrs withdrawals)
  ]

{- | Index certificates with witnesses by the order they appear in the list (in the transaction). If there are multiple witnesses for the same stake credential, they will be present multiple times with the same index.
are multiple witnesses for the credential, there will be multiple entries for
See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
-}
indexTxCertificates
  :: C.TxCertificates BuildTx era
  -> [(C.ScriptWitnessIndex, C.Certificate era, C.StakeCredential, C.Witness C.WitCtxStake era)]
indexTxCertificates C.TxCertificatesNone = []
indexTxCertificates (C.TxCertificates _ certs (C.BuildTxWith witnesses)) =
  [ (C.ScriptWitnessIndexCertificate ix, cert, stakeCred, wit)
  | (ix, cert) <- zip [0 ..] certs
  , stakeCred <- maybeToList (C.selectStakeCredentialWitness cert)
  , wit <- findAll stakeCred witnesses
  ]
 where
  findAll needle = map snd . filter ((==) needle . fst)

-- | Index voting procedures by the order of the votes ('Ord').
indexTxVotingProcedures
  :: C.TxVotingProcedures BuildTx era
  -> [ ( C.ScriptWitnessIndex
       , L.Voter (CLedger.EraCrypto (C.ShelleyLedgerEra era))
       , C.ScriptWitness C.WitCtxStake era
       )
     ]
indexTxVotingProcedures C.TxVotingProceduresNone = []
indexTxVotingProcedures (C.TxVotingProcedures vProcedures (C.BuildTxWith sWitMap)) =
  [ (C.ScriptWitnessIndexVoting $ fromIntegral index, vote, scriptWitness)
  | let allVoteMap = L.unVotingProcedures vProcedures
  , (vote, scriptWitness) <- toList sWitMap
  , index <- maybeToList $ Map.lookupIndex vote allVoteMap
  ]

-- | Index proposal procedures by their order ('Ord').
indexTxProposalProcedures
  :: C.TxProposalProcedures BuildTx era
  -> [(C.ScriptWitnessIndex, L.ProposalProcedure (C.ShelleyLedgerEra era), C.ScriptWitness C.WitCtxStake era)]
indexTxProposalProcedures C.TxProposalProceduresNone = []
indexTxProposalProcedures txpp@(C.TxProposalProcedures _ (C.BuildTxWith witnesses)) = do
  let allProposalsList = toList $ C.convProposalProcedures txpp
  [ (C.ScriptWitnessIndexProposing $ fromIntegral ix, proposal, scriptWitness)
    | (proposal, scriptWitness) <- toList witnesses
    , ix <- maybeToList $ List.elemIndex proposal allProposalsList
    ]

traverseScriptWitnesses
  :: [(a, Either (C.TxBodyErrorAutoBalance era) b)]
  -> Either (C.TxBodyErrorAutoBalance era) [(a, b)]
traverseScriptWitnesses =
  traverse (\(item, eRes) -> eRes >>= (\res -> Right (item, res)))

{- | This relies on the TxId Ord instance being consistent with the
Ledger.TxId Ord instance via the toShelleyTxId conversion.
This is checked by prop_ord_distributive_TxId
-}
orderTxIns :: [(C.TxIn, v)] -> [(C.TxIn, v)]
orderTxIns = List.sortBy (compare `on` fst)

{- | This relies on the StakeAddress Ord instance being consistent with the
Shelley.RewardAcnt Ord instance via the toShelleyStakeAddr conversion.
This is checked by prop_ord_distributive_StakeAddress
-}
orderStakeAddrs :: [(C.StakeAddress, x, v)] -> [(C.StakeAddress, x, v)]
orderStakeAddrs = List.sortBy (compare `on` (\(k, _, _) -> k))
