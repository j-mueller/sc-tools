{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
module Convex.TradingBot.NodeClient(
  backtestingClient,
  buyOrderClient,
  sellOrderClient,
  pricesClient,
  tradingClient
  ) where

import           Convex.TradingBot.NodeClient.BacktestingClient (backtestingClient)
import           Convex.TradingBot.NodeClient.OrderClient       (buyOrderClient,
                                                                 sellOrderClient)
import           Convex.TradingBot.NodeClient.PricesClient      (pricesClient)
import           Convex.TradingBot.NodeClient.TradingClient     (tradingClient)
