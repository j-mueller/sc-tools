{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-| API for wallet UTxOs
-}
module Convex.Wallet.API(
  API,
  getHealth,
  getUTxOs,
  startServer
) where

import           Cardano.Api                     (ConwayEra, CtxTx,
                                                  IsShelleyBasedEra)
import           Control.Concurrent.STM          (TVar, readTVarIO)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Convex.Utxos                    (UtxoSet)
import           Convex.Wallet.WalletState       (WalletState, utxoSet)
import           Data.Proxy                      (Proxy (..))
import qualified Network.Wai.Handler.Warp        as Warp
import           Servant.API                     (Description, Get, JSON,
                                                  NoContent (..), type (:>),
                                                  (:<|>) (..))
import           Servant.Client                  (ClientEnv, client, runClientM)
import           Servant.Client.Core.ClientError (ClientError)
import           Servant.Server                  (Server, serve)

type API era =
  "healthcheck" :> Description "Is the server alive?" :> Get '[JSON] NoContent
  :<|> "utxos" :> Get '[JSON] (UtxoSet CtxTx era ())

{-| Call the "healthcheck" endpoint
-}
getHealth :: ClientEnv -> IO (Either ClientError NoContent)
getHealth clientEnv = do
  let healthcheck :<|> _ = client (Proxy @(API ConwayEra))
  runClientM healthcheck clientEnv

{-| Call the "utxos" endpoint
-}
getUTxOs :: forall era. IsShelleyBasedEra era => ClientEnv -> IO (Either ClientError (UtxoSet CtxTx era ()))
getUTxOs clientEnv = do
  let _ :<|> utxos = client (Proxy @(API era))
  runClientM utxos clientEnv

server :: TVar (WalletState era) -> Server (API era)
server walletState = health :<|> utxo
  where
    health = pure NoContent
    utxo = liftIO (utxoSet <$> readTVarIO walletState)

startServer :: forall era. IsShelleyBasedEra era => TVar (WalletState era) -> Int -> IO ()
startServer walletState port =
  let app = serve (Proxy @(API era)) (server walletState)
  in Warp.run port app
