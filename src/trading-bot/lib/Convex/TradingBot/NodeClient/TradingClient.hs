{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-| Run the trading bot
-}
module Convex.TradingBot.NodeClient.TradingClient(
  tradingClient
  ) where

import           Cardano.Api                                    (AssetName,
                                                                 Block (..),
                                                                 BlockHeader (..),
                                                                 BlockInMode (..),
                                                                 BlockNo,
                                                                 CardanoMode,
                                                                 Env, Lovelace,
                                                                 NetworkId,
                                                                 PolicyId,
                                                                 Quantity,
                                                                 SlotNo, Value)
import           Control.Lens                                   (Lens', _2,
                                                                 anon, at,
                                                                 makeLenses,
                                                                 set, use, (%=),
                                                                 (&), (.=),
                                                                 (^.))
import           Control.Monad                                  (void, when)
import           Control.Monad.State.Strict                     (MonadState,
                                                                 execStateT)
import           Control.Monad.Trans.Maybe                      (runMaybeT)
import qualified Convex.Constants                               as Constants
import           Convex.Event                                   (ResolvedInputs,
                                                                 extract)
import           Convex.MonadLog                                (MonadLog,
                                                                 MonadLogKatipT (..),
                                                                 logInfoS,
                                                                 logUnless)
import           Convex.NodeClient.Fold                         (CatchingUp (..),
                                                                 catchingUp,
                                                                 catchingUpWithNode,
                                                                 foldClient)
import           Convex.NodeClient.Resuming                     (resumingClient)
import           Convex.NodeClient.Types                        (PipelinedLedgerStateClient)
import           Convex.TradingBot.LPPoolEvent                  (OrderbookEvent (..))
import qualified Convex.TradingBot.LPPoolEvent                  as LPPoolEvent
import           Convex.TradingBot.NodeClient.BacktestingClient (getOrderbookPrices)
import           Convex.TradingBot.Portfolio                    (Portfolio,
                                                                 defaultPortfolioConfig,
                                                                 emptyPortfolio)
import qualified Convex.TradingBot.Portfolio                    as Portfolio
import           Convex.TradingBot.Prices                       (LPPrices)
import qualified Convex.TradingBot.Prices                       as Prices
import           Convex.TradingBot.Rules                        (Rule,
                                                                 Signal (..))
import           Convex.Utxos                                   (UtxoSet)
import qualified Convex.Utxos                                   as Utxos
import           Convex.Wallet                                  (Wallet)
import qualified Convex.Wallet                                  as Wallet
import           Data.Foldable                                  (traverse_)
import           Data.Map                                       (Map)
import qualified Data.Map.Strict                                as Map
import           Data.Maybe                                     (fromMaybe)
import qualified Katip                                          as K

{-| State of the trading client
-}
data ClientState =
  ClientState
    { _walletState    :: !UtxoSet
    , _orderbookState :: !(ResolvedInputs OrderbookEvent)
    , _lastPrices     :: !(Map (PolicyId, AssetName) (Lovelace, Quantity))
    , _lpPrices       :: !(Map (PolicyId, AssetName) LPPrices)
    , _portfolio      :: !(Portfolio, Value)
    }

makeLenses ''ClientState

pricesFor :: (PolicyId, AssetName) -> Lens' ClientState LPPrices
pricesFor k = lpPrices . at k . anon Prices.empty Prices.null


initialState :: ClientState
initialState = ClientState mempty mempty mempty mempty (emptyPortfolio, mempty)

tradingClient :: Rule -> K.LogEnv -> K.Namespace -> Wallet -> NetworkId -> Env -> PipelinedLedgerStateClient
tradingClient rule logEnv ns wallet networkId env =
  let start = Constants.recent
  in resumingClient [start] $ \_ ->
      foldClient
        (catchingUpWithNode start Nothing, initialState)
        env
        (applyBlock rule logEnv ns wallet networkId)

applyBlock :: Rule -> K.LogEnv -> K.Namespace -> Wallet -> NetworkId -> CatchingUp -> (CatchingUp, ClientState) -> BlockInMode CardanoMode -> IO (Maybe (CatchingUp, ClientState))
applyBlock rule logEnv ns wallet networkId c (oldC, state) block = K.runKatipContextT logEnv () ns $ runMonadLogKatipT $ runMaybeT $ do
  let walletChange = Utxos.extract (Wallet.shelleyPaymentCredential wallet) (_walletState state) block
      (orderBookEvents, newOrderBookState) = extract (\e -> maybe Nothing (either (const Nothing) Just) . LPPoolEvent.extract e) (state ^. orderbookState) block
      newWalletState = Utxos.apply (state ^. walletState) walletChange
      BlockInMode (Block blockHeader _) _ = block
      BlockHeader currentSlot _ currentBlockNo = blockHeader
      newState = state
                  & set walletState newWalletState
                  & set orderbookState newOrderBookState
                  & set (portfolio . _2) (Utxos.totalBalance newWalletState)

  state' <- flip execStateT newState $ do
    updatePrices rule c currentBlockNo currentSlot (Map.unionsWith (<>) (getOrderbookPrices networkId . fmap Right <$> orderBookEvents))
    when (not (catchingUp c)) $ do
      when (catchingUp oldC) (logInfoS "Caught up with node")
      (pf, vl) <- use portfolio
      logInfoS $ "AUM: " <> Portfolio.formatAda (Portfolio.aum vl pf)
  pure (c, state')

updatePrices :: (MonadLog m, MonadState ClientState m) => Rule -> CatchingUp -> BlockNo -> SlotNo -> Map (PolicyId, AssetName) (Lovelace, Quantity) -> m ()
updatePrices rule c blck slt newPrices = do
  oldPrices <- use lastPrices
  flip traverse_ (Map.toList newPrices) $ \(k, newPrice) -> do
    let oldPrice = Map.findWithDefault (0, 0) k oldPrices
        pAt = Prices.pricesAt blck slt oldPrice newPrice
        p = fromMaybe mempty $ Prices.price $ Prices._stats pAt
    when (Prices.pmVolume p > 0) $ do
      logUnless (catchingUp c) $ " " <> show (snd k) <> ": " <> Prices.showPriceMeasure p
      pricesFor k %= Prices.prepend pAt
      newPrices' <- use (pricesFor k)
      portf <- use portfolio
      portf' <- Portfolio.execSimulatedPortfolioT defaultPortfolioConfig portf $ Portfolio.runLoggingPortfolioT $ do
        let pricePoint = (fst k, snd k, snd newPrice, fst newPrice)
        void (Portfolio.update pricePoint)
        case rule newPrices' of
          Buy  -> void (Portfolio.buy 1.0 pricePoint)
          Sell -> void (Portfolio.sell 1.0 pricePoint)
          _    -> pure ()
      portfolio .= portf'
  lastPrices .= oldPrices <> newPrices
