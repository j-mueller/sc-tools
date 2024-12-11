{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Some predefined mock wallets for testing
module Convex.Wallet.MockWallet (
  -- * Mock wallets for testing
  mockWallets,
  initialUTxOs,
  w1,
  w2,
  w3,
  w4,
  w5,
  w6,
  w7,
  w8,
  w9,
  w10,
) where

import Cardano.Ledger.Shelley.API (Coin (..))
import Convex.Wallet (Wallet, parse)
import Data.Text (Text)
import Data.Text qualified as Text

mkWallet :: Text -> Wallet
mkWallet txt = case parse txt of
  Left err -> error $ "failed to parse '" <> Text.unpack txt <> "': " <> show err
  Right w -> w

-- | Some predefined wallets for testing
mockWallets :: [Wallet]
mockWallets = [w1, w2, w3, w4, w5, w6, w7, w8, w9, w10]

w1 :: Wallet
w1 = mkWallet "addr_sk1ts8uksf097vzjl8yl7x4xawhjfmlcs2tmw3wrqunkvxu2cjgfj7q4x5mwg"

w2 :: Wallet
w2 = mkWallet "addr_sk1jhyfdp39sps7y9g8chlrgme9z8w6t5nqytsv54sz6fgs94spv2aqcegj7s"

w3 :: Wallet
w3 = mkWallet "addr_sk1j3p5c67k8zgqfh76kevuxzm6ampdd3mnfg9dgnwjan2ryqns2sdsrvyz2s"

w4 :: Wallet
w4 = mkWallet "addr_sk1z2gwlmnlspuh8pmkkf3kx06qttt5pyzc3um4q9tyty7dnl2ldf6swznq5q"

w5 :: Wallet
w5 = mkWallet "addr_sk1hpnuvd3rh6mgczjwd6f5yrya0e8p3q0sns2sc3c8qzezhtsqlhasqzzl2l"

w6 :: Wallet
w6 = mkWallet "addr_sk10ua3537rdvk63hgzlfarf32t6ntw69hwec3mudqmn2zs3m8r7cgsj2zgzl"

w7 :: Wallet
w7 = mkWallet "addr_sk1j6rwy2kajtfh4adj0allvalle6memx9an3sn740n9f45d6rqpz8qj5kqat"

w8 :: Wallet
w8 = mkWallet "addr_sk1d0z0prgh90tfcq6zfyepz8wmedcvyr5had3emxdlnzt2x9w4e0dq5yudvu"

w9 :: Wallet
w9 = mkWallet "addr_sk1xc4ad4jph6fp0kx5edkj8wdd4kw5qxgyu0yf7y0gkqgexa53spqsvlgtyh"

w10 :: Wallet
w10 = mkWallet "addr_sk129sf8z7c4qvzm3lgw0m0scu6nm456lxjdjtwpywcleqwp23mfv3swq8t6h"

-- | 100.000 Ada for each wallet
initialUTxOs :: [(Wallet, Coin)]
initialUTxOs =
  let c = Coin 100_000_000_000
   in fmap (\w -> (w, c)) mockWallets
