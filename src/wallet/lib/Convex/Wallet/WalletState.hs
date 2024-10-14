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

import           Cardano.Api                (BlockHeader (..), ChainPoint (..))
import qualified Cardano.Api                as C
import           Control.Exception          (SomeException, catch)
import           Convex.Constants           (lessRecent)
import           Convex.NodeClient.ChainTip (JSONChainPoint (..))
import           Convex.Utxos               (UtxoSet)
import           Data.Aeson                 (FromJSON (..), ToJSON (..), decode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy       as BSL
import           GHC.Generics               (Generic)

data WalletState =
  WalletState
    { wsChainPoint :: JSONChainPoint
    , wsUtxos      :: UtxoSet C.CtxTx ()
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

{-| Construct a 'WalletState' from a UTxO set and a block header
-}
walletState :: UtxoSet C.CtxTx () -> BlockHeader -> WalletState
walletState wsUtxos (BlockHeader slot hsh _)=
  let wsChainPoint = JSONChainPoint $ ChainPoint slot hsh
  in WalletState{wsUtxos, wsChainPoint}

chainPoint :: WalletState -> ChainPoint
chainPoint WalletState{wsChainPoint = JSONChainPoint c} = c

utxoSet :: WalletState -> UtxoSet C.CtxTx ()
utxoSet WalletState{wsUtxos} = wsUtxos

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
