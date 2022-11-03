{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Convex.Muesli.Contract(
  Order(..),
  OrderAction(..),
  OrderDatum(..)
) where

import           GHC.Generics            (Generic)
import           Plutus.V1.Ledger.Crypto (PubKeyHash)
import           Plutus.V1.Ledger.Value  (CurrencySymbol, TokenName)
import qualified PlutusTx
import           PlutusTx.Prelude        hiding (Applicative (..), unless)


data Order = Order
  { oCreator     :: !PubKeyHash
  , oBuyCurrency :: !CurrencySymbol
  , oBuyToken    :: !TokenName
  , oBuyAmount   :: !Integer
  }
  deriving (Generic)

instance Eq Order where
  {-# INLINABLE (==) #-}
  a == b =
    (oCreator a == oCreator b)
      && (oBuyCurrency a == oBuyCurrency b)
      && (oBuyToken a == oBuyToken b)
      && (oBuyAmount a == oBuyAmount b)

PlutusTx.unstableMakeIsData ''Order
PlutusTx.makeLift ''Order

data OrderAction = CancelOrder | FullMatch

PlutusTx.unstableMakeIsData ''OrderAction
PlutusTx.makeLift ''OrderAction

data OrderDatum = OrderDatum
  { odOrder :: !Order
  }

PlutusTx.unstableMakeIsData ''OrderDatum
PlutusTx.makeLift ''OrderDatum
