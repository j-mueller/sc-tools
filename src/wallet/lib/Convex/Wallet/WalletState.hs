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

data WalletState era =
  WalletState
    { wsChainPoint :: JSONChainPoint
    , wsUtxos      :: UtxoSet C.CtxTx era ()
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

{-| Construct a 'WalletState' from a UTxO set and a block header
-}
walletState :: UtxoSet C.CtxTx era () -> BlockHeader -> WalletState era
walletState wsUtxos (BlockHeader slot hsh _)=
  let wsChainPoint = JSONChainPoint $ ChainPoint slot hsh
  in WalletState{wsUtxos, wsChainPoint}

chainPoint :: WalletState era -> ChainPoint
chainPoint WalletState{wsChainPoint = JSONChainPoint c} = c

utxoSet :: WalletState era -> UtxoSet C.CtxTx era ()
utxoSet WalletState{wsUtxos} = wsUtxos

initialWalletState :: WalletState era
initialWalletState = WalletState (JSONChainPoint lessRecent) mempty

{-| Write the wallet state to a JSON file
-}
writeToFile :: C.IsCardanoEra era => FilePath -> WalletState era -> IO ()
writeToFile file = BSL.writeFile file . encodePretty

{-| Read the wallet state from a JSON file
-}
readFromFile :: C.IsShelleyBasedEra era => FilePath -> IO (Maybe (WalletState era))
readFromFile fp =
  catch (decode <$> BSL.readFile fp) $ \(_ :: SomeException) -> pure Nothing
