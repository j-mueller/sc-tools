{-# LANGUAGE LambdaCase #-}
{-| Conversion between blockfrost and @cardano-api@ types
-}
module Convex.Blockfrost.Types(
  toLovelace,
  toQuantity,
  toPolicyId,
  toTxHash,
  toAssetId
) where

import           Blockfrost.Types.Shared.Ada      (Lovelaces)
import           Blockfrost.Types.Shared.Amount   (Amount (..))
import           Blockfrost.Types.Shared.PolicyId (PolicyId (..))
import           Blockfrost.Types.Shared.Quantity (Quantity (..))
import           Blockfrost.Types.Shared.TxHash   (TxHash (..))
import           Cardano.Api                      (Lovelace)
import qualified Cardano.Api                      as C
import qualified Cardano.Api.Ledger               as C.Ledger
import           Data.Coerce                      (Coercible, coerce)
import           Data.String                      (IsString (..))
import qualified Data.Text                        as Text
import qualified Money

toLovelace :: Lovelaces -> Lovelace
toLovelace = C.Ledger.Coin . toInteger

toQuantity :: Quantity -> C.Quantity
toQuantity = coerce

toPolicyId :: PolicyId -> C.PolicyId
toPolicyId = textToIsString

toTxHash :: TxHash -> C.TxId
toTxHash = textToIsString

textToIsString :: (Coercible a Text.Text, IsString b) => a -> b
textToIsString = fromString . Text.unpack . coerce

toAssetId :: Amount -> (C.AssetId, C.Quantity)
toAssetId = \case
  AdaAmount lvl -> (C.AdaAssetId, C.lovelaceToQuantity $ toLovelace lvl)
  AssetAmount disc ->
    -- concatenation of asset policy ID and hex-encoded asset_name
    let (policyText, assetName) = Text.splitAt 56 (Money.someDiscreteCurrency disc)
        amount = Money.someDiscreteAmount disc
        -- TODO: We should probably also consider Money.someDiscreteScale
        --       but it looks like blockfrost just uses unitScale for native assets
    in (C.AssetId (textToIsString policyText) (textToIsString assetName), C.Quantity amount)
