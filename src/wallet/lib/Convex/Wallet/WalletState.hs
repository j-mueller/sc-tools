{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-| Serialisable state of a wallet
-}
module Convex.Wallet.WalletState(
  WalletState(..),
  walletState,
  utxoSet,
  chainPoint,
  initialWalletState,
  writeToFile,
  readFromFile
) where

import           Cardano.Api              (BlockHeader (..), ChainPoint (..))
import           Control.Exception        (SomeException, catch)
import           Convex.Constants         (lessRecent)
import           Convex.Utxos             (UtxoSet)
import           Data.Aeson               (FromJSON (..), ToJSON (..),
                                           Value (..), decode, object,
                                           withObject, (.:), (.=))
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BSL
import           GHC.Generics             (Generic)

data WalletState =
  WalletState
    { wsChainPoint :: JSONChainPoint
    , wsUtxos      :: UtxoSet
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

{-| Construct a 'WalletState' from a UTxO set and a block header
-}
walletState :: UtxoSet -> BlockHeader -> WalletState
walletState wsUtxos (BlockHeader slot hsh _)=
  let wsChainPoint = JSONChainPoint $ ChainPoint slot hsh
  in WalletState{wsUtxos, wsChainPoint}

chainPoint :: WalletState -> ChainPoint
chainPoint WalletState{wsChainPoint = JSONChainPoint c} = c

utxoSet :: WalletState -> UtxoSet
utxoSet WalletState{wsUtxos} = wsUtxos

newtype JSONChainPoint = JSONChainPoint ChainPoint
  deriving newtype (Eq, Show)

instance ToJSON JSONChainPoint where
  toJSON (JSONChainPoint jp) = case jp of
    ChainPointAtGenesis -> toJSON ("ChainPointAtGenesis" :: String)
    ChainPoint s h      -> object ["slot" .= s, "block_header" .= h]

instance FromJSON JSONChainPoint where
  parseJSON (String "ChainPointAtGenesis") = pure (JSONChainPoint ChainPointAtGenesis)
  parseJSON x = withObject "JSONChainPoint" (\obj ->
    fmap JSONChainPoint (ChainPoint <$> obj .: "slot" <*> obj .: "block_header")) x

initialWalletState :: WalletState
initialWalletState = WalletState (JSONChainPoint lessRecent) mempty

{-| Write the wallet state to a JSON file
-}
writeToFile :: FilePath -> WalletState -> IO ()
writeToFile file = BSL.writeFile file . encodePretty

{-| Read the wallet state from a JSON file
-}
readFromFile :: FilePath -> IO (Maybe WalletState)
readFromFile fp =
  catch (decode <$> BSL.readFile fp) $ \(_ :: SomeException) -> pure Nothing
