{-| Building transactions that mint and burn
unAda
-}
module UnAda.OffChain.Transaction(
  mintUnAda
) where

import           Cardano.Api            (NetworkId, Quantity, lovelaceToValue,
                                         quantityToLovelace)
import           Convex.BuildTx         (TxBuild, mintPlutusV2, payToPlutusV2)
import           UnAda.OffChain.Scripts (assetName, mintingPolicyScript,
                                         validatorScript)

mintUnAda :: NetworkId -> Quantity -> TxBuild
mintUnAda n q =
  let vl = lovelaceToValue (quantityToLovelace q)
  in payToPlutusV2 n validatorScript () vl
  . mintPlutusV2 mintingPolicyScript () assetName q
