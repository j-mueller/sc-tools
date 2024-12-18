{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | API for wallet UTxOs
module Convex.Wallet.API (
  API,
  getHealth,
  getUTxOs,
  startServer,
) where

import Cardano.Api (CtxTx)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad.IO.Class (MonadIO (..))
import Convex.Utxos (UtxoSet)
import Convex.Wallet.WalletState (WalletState, utxoSet)
import Data.Proxy (Proxy (..))
import Network.Wai.Handler.Warp qualified as Warp
import Servant.API (
  Description,
  Get,
  JSON,
  NoContent (..),
  (:<|>) (..),
  type (:>),
 )
import Servant.Client (ClientEnv, client, runClientM)
import Servant.Client.Core.ClientError (ClientError)
import Servant.Server (Server, serve)

type API =
  "healthcheck" :> Description "Is the server alive?" :> Get '[JSON] NoContent
    :<|> "utxos" :> Get '[JSON] (UtxoSet CtxTx ())

-- | Call the "healthcheck" endpoint
getHealth :: ClientEnv -> IO (Either ClientError NoContent)
getHealth clientEnv = do
  let healthcheck :<|> _ = client (Proxy @API)
  runClientM healthcheck clientEnv

-- | Call the "utxos" endpoint
getUTxOs :: ClientEnv -> IO (Either ClientError (UtxoSet CtxTx ()))
getUTxOs clientEnv = do
  let _ :<|> utxos = client (Proxy @API)
  runClientM utxos clientEnv

server :: TVar WalletState -> Server API
server walletState = health :<|> utxo
 where
  health = pure NoContent
  utxo = liftIO (utxoSet <$> readTVarIO walletState)

startServer :: TVar WalletState -> Int -> IO ()
startServer walletState port =
  let app = serve (Proxy @API) (server walletState)
   in Warp.run port app
