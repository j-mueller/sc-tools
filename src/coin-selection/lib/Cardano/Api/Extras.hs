{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

{- | This module exists solely to hold a copy of 'substituteExecutionUnits'
from @cardano-api@.
-}
module Cardano.Api.Extras (
  substituteExecutionUnits,
) where

import Cardano.Api (
  BuildTx,
  BuildTxWith (..),
  Certificate,
  ConwayEraOnwards,
  ExecutionUnits,
  Featured (..),
  PolicyAssets,
  PolicyId,
  ScriptWitness (..),
  ScriptWitnessIndex (..),
  ShelleyLedgerEra,
  StakeAddress,
  StakeCredential,
  TxBodyContent,
  TxBodyErrorAutoBalance (..),
  TxCertificates (..),
  TxIn,
  TxMintValue (..),
  TxProposalProcedures (..),
  TxVotingProcedures (..),
  TxWithdrawals (..),
  WitCtxMint,
  WitCtxStake,
  WitCtxTxIn,
  Witness (..),
  conwayEraOnwardsConstraints,
  mkTxProposalProcedures,
  setTxCertificates,
  setTxIns,
  setTxMintValue,
  setTxProposalProcedures,
  setTxVotingProcedures,
  setTxWithdrawals,
 )
import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Control.Lens ((&))
import Data.Bitraversable (bitraverse)
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
   . Map ScriptWitnessIndex ExecutionUnits
  -> TxBodyContent BuildTx era
  -> Either (TxBodyErrorAutoBalance era) (TxBodyContent BuildTx era)
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
        & setTxIns mappedTxIns
        & setTxMintValue mappedMintedVals
        & setTxCertificates mappedTxCertificates
        & setTxWithdrawals mappedWithdrawals
        & setTxVotingProcedures mappedVotes
        & setTxProposalProcedures mappedProposals
   where
    substituteExecUnits
      :: ScriptWitnessIndex
      -> ScriptWitness witctx era
      -> Either (TxBodyErrorAutoBalance era) (ScriptWitness witctx era)
    substituteExecUnits _ wit@SimpleScriptWitness{} = Right wit
    substituteExecUnits idx (PlutusScriptWitness langInEra version script datum redeemer _) =
      case Map.lookup idx exUnitsMap of
        Nothing ->
          Left $ TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap idx exUnitsMap
        Just exunits ->
          Right $
            PlutusScriptWitness
              langInEra
              version
              script
              datum
              redeemer
              exunits

    adjustScriptWitness
      :: (ScriptWitness witctx era -> Either (TxBodyErrorAutoBalance era) (ScriptWitness witctx era))
      -> Witness witctx era
      -> Either (TxBodyErrorAutoBalance era) (Witness witctx era)
    adjustScriptWitness _ (KeyWitness ctx) = Right $ KeyWitness ctx
    adjustScriptWitness g (ScriptWitness ctx witness') = ScriptWitness ctx <$> g witness'

    mapScriptWitnessesTxIns
      :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
      -> Either (TxBodyErrorAutoBalance era) [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
    mapScriptWitnessesTxIns txins =
      let mappedScriptWitnesses
            :: [ ( TxIn
                 , Either (TxBodyErrorAutoBalance era) (BuildTxWith BuildTx (Witness WitCtxTxIn era))
                 )
               ]
          mappedScriptWitnesses =
            [ (txin, BuildTxWith <$> wit')
            | (ix, txin, wit) <- indexTxIns txins
            , let wit' = adjustScriptWitness (substituteExecUnits ix) wit
            ]
       in traverse
            (\(txIn, eWitness) -> (txIn,) <$> eWitness)
            mappedScriptWitnesses

    mapScriptWitnessesWithdrawals
      :: TxWithdrawals BuildTx era
      -> Either (TxBodyErrorAutoBalance era) (TxWithdrawals BuildTx era)
    mapScriptWitnessesWithdrawals TxWithdrawalsNone = Right TxWithdrawalsNone
    mapScriptWitnessesWithdrawals txWithdrawals'@(TxWithdrawals supported _) =
      let mappedWithdrawals
            :: [ ( StakeAddress
                 , L.Coin
                 , Either (TxBodyErrorAutoBalance era) (BuildTxWith BuildTx (Witness WitCtxStake era))
                 )
               ]
          mappedWithdrawals =
            [ (addr, withdrawal, BuildTxWith <$> mappedWitness)
            | (ix, addr, withdrawal, wit) <- indexTxWithdrawals txWithdrawals'
            , let mappedWitness = adjustScriptWitness (substituteExecUnits ix) wit
            ]
       in TxWithdrawals supported
            <$> traverse
              (\(sAddr, ll, eWitness) -> (sAddr,ll,) <$> eWitness)
              mappedWithdrawals

    mapScriptWitnessesCertificates
      :: TxCertificates BuildTx era
      -> Either (TxBodyErrorAutoBalance era) (TxCertificates BuildTx era)
    mapScriptWitnessesCertificates TxCertificatesNone = Right TxCertificatesNone
    mapScriptWitnessesCertificates txCertificates'@(TxCertificates supported _) = do
      let indexAndAdjustScriptCertificates C.TxCertificatesNone = []
          indexAndAdjustScriptCertificates (C.TxCertificates _ certs) =
            [ (cert, eWitness)
            | (ix, (cert, C.BuildTxWith mWitness)) <- zip [0 ..] (toList certs)
            , let eWitness = case mWitness of
                    Nothing -> pure $ C.BuildTxWith Nothing
                    Just (stakeCred, witness) ->
                      let adjustedWitness =
                            adjustScriptWitness
                              (substituteExecUnits $ C.ScriptWitnessIndexCertificate ix)
                              witness
                       in C.BuildTxWith . Just . (stakeCred,) <$> adjustedWitness
            ]

      let mappedScriptWitnesses
            :: [ ( Certificate era
                 , Either
                    (TxBodyErrorAutoBalance era)
                    ( BuildTxWith
                        BuildTx
                        ( Maybe
                            ( StakeCredential
                            , Witness WitCtxStake era
                            )
                        )
                    )
                 )
               ]
          -- mappedScriptWitnesses =
          --   [ (cert, BuildTxWith . Just . (stakeCred,) <$> eWitness')
          --   | (ix, cert, stakeCred, witness) <- indexTxCertificates txCertificates'
          --   , let eWitness' = adjustScriptWitness (substituteExecUnits ix) witness
          --   ]
          mappedScriptWitnesses = indexAndAdjustScriptCertificates txCertificates'
      TxCertificates supported . fromList <$> traverseScriptWitnesses mappedScriptWitnesses

    mapScriptWitnessesVotes
      :: Maybe (Featured ConwayEraOnwards era (TxVotingProcedures build era))
      -> Either
          (TxBodyErrorAutoBalance era)
          (Maybe (Featured ConwayEraOnwards era (TxVotingProcedures build era)))
    mapScriptWitnessesVotes Nothing = return Nothing
    mapScriptWitnessesVotes (Just (Featured _ TxVotingProceduresNone)) = return Nothing
    mapScriptWitnessesVotes (Just (Featured _ (TxVotingProcedures _ ViewTx))) = return Nothing
    mapScriptWitnessesVotes (Just (Featured era txVotingProcedures'@(TxVotingProcedures vProcedures (BuildTxWith _)))) = do
      let eSubstitutedExecutionUnits =
            [ (vote, updatedWitness)
            | (ix, vote, witness) <- indexTxVotingProcedures txVotingProcedures'
            , let updatedWitness = substituteExecUnits ix witness
            ]

      substitutedExecutionUnits <- traverseScriptWitnesses eSubstitutedExecutionUnits

      return $
        Just
          (Featured era (TxVotingProcedures vProcedures (BuildTxWith $ fromList substitutedExecutionUnits)))

    mapScriptWitnessesProposals
      :: Maybe (Featured ConwayEraOnwards era (TxProposalProcedures BuildTx era))
      -> Either
          (TxBodyErrorAutoBalance era)
          (Maybe (Featured ConwayEraOnwards era (TxProposalProcedures BuildTx era)))
    mapScriptWitnessesProposals Nothing = return Nothing
    mapScriptWitnessesProposals (Just (Featured era proposals)) = do
      substitutedExecutionUnits <-
        traverse
          (bitraverse pure $ traverse $ uncurry substituteExecUnits)
          $ indexWitnessedTxProposalProcedures proposals
      pure $
        Just $
          Featured era $
            conwayEraOnwardsConstraints era $
              mkTxProposalProcedures substitutedExecutionUnits

    mapScriptWitnessesMinting
      :: TxMintValue BuildTx era
      -> Either (TxBodyErrorAutoBalance era) (TxMintValue BuildTx era)
    mapScriptWitnessesMinting TxMintNone = Right TxMintNone
    mapScriptWitnessesMinting txMintValue'@(TxMintValue w _) = do
      let mappedScriptWitnesses =
            [ (policyId, (assets,) <$> substitutedWitness)
            | (ix, policyId, assets, BuildTxWith witness) <- indexTxMintValue txMintValue'
            , let substitutedWitness = BuildTxWith <$> substituteExecUnits ix witness
            ]
          -- merge map values, wit1 == wit2 will always hold
          mergeValues (assets1, wit1) (assets2, _wit2) = (assets1 <> assets2, wit1)
      final <- Map.fromListWith mergeValues <$> traverseScriptWitnesses mappedScriptWitnesses
      pure $ TxMintValue w final

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

-- Commented out as use resulted in removal of no witness certificates. See
-- note on `mapScriptWitnessesCertificates`
-- indexTxCertificates
--   :: C.TxCertificates BuildTx era
--   -> [(C.ScriptWitnessIndex, C.Certificate era, C.StakeCredential, C.Witness C.WitCtxStake era)]
-- indexTxCertificates C.TxCertificatesNone = []
-- indexTxCertificates (C.TxCertificates _ certsWits) =
--   [ (C.ScriptWitnessIndexCertificate ix, cert, stakeCred, witness)
--   | (ix, (cert, C.BuildTxWith (Just (stakeCred, witness)))) <- zip [0 ..] $ toList certsWits
--   ]

-- | Index voting procedures by the order of the votes ('Ord').
indexTxVotingProcedures
  :: TxVotingProcedures BuildTx era
  -> [ ( ScriptWitnessIndex
       , L.Voter
       , ScriptWitness WitCtxStake era
       )
     ]
indexTxVotingProcedures TxVotingProceduresNone = []
indexTxVotingProcedures (TxVotingProcedures vProcedures (BuildTxWith sWitMap)) =
  [ (ScriptWitnessIndexVoting $ fromIntegral index, vote, scriptWitness)
  | let allVoteMap = L.unVotingProcedures vProcedures
  , (vote, scriptWitness) <- toList sWitMap
  , index <- maybeToList $ Map.lookupIndex vote allVoteMap
  ]

-- | Index proposal procedures by their order ('Ord').
indexWitnessedTxProposalProcedures
  :: TxProposalProcedures BuildTx era
  -> [ ( L.ProposalProcedure (ShelleyLedgerEra era)
       , Maybe (ScriptWitnessIndex, ScriptWitness WitCtxStake era)
       )
     ]
indexWitnessedTxProposalProcedures TxProposalProceduresNone = []
indexWitnessedTxProposalProcedures (TxProposalProcedures proposals) = do
  let allProposalsList = zip [0 ..] $ toList proposals
  [ (proposal, fmap (ScriptWitnessIndexProposing ix,) mScriptWitness)
    | (ix, (proposal, BuildTxWith mScriptWitness)) <- allProposalsList
    ]

{- | Index the assets with witnesses in the order of policy ids.
See section 4.1 of https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
-}
indexTxMintValue
  :: TxMintValue build era
  -> [ ( ScriptWitnessIndex
       , PolicyId
       , PolicyAssets
       , BuildTxWith build (ScriptWitness WitCtxMint era)
       )
     ]
indexTxMintValue TxMintNone = []
indexTxMintValue (TxMintValue _ policiesWithAssets) =
  [ (ScriptWitnessIndexMint ix, policyId, assets, witness)
  | (ix, (policyId, (assets, witness))) <- zip [0 ..] $ toList policiesWithAssets
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
