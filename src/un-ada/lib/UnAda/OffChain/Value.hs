-- | Working with values that contain UnAda
module UnAda.OffChain.Value (
  selectUnLovelace,
  unLovelaceValue,
) where

import Cardano.Api (
  Quantity,
  Value,
  selectAsset,
  valueFromList,
 )
import UnAda.OffChain.Scripts (unAdaAssetId)

selectUnLovelace :: Value -> Quantity
selectUnLovelace vl = selectAsset vl unAdaAssetId

unLovelaceValue :: Quantity -> Value
unLovelaceValue q = valueFromList [(unAdaAssetId, q)]
