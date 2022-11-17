{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
module Convex.TradingBot.NodeClient(
  backtestingClient,
  orderClient
  ) where

import           Convex.TradingBot.NodeClient.BacktestingClient (backtestingClient)
import           Convex.TradingBot.NodeClient.OrderClient       (orderClient)
