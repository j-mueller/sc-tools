{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main(main) where

import           Cardano.Api.Shelley                   (ScriptData (..))
import qualified Cardano.Api.Shelley                   as C
import           Control.Lens                          ((&))
import           Control.Monad                         (void)
import           Convex.BuildTx                        (mintPlutusV2,
                                                        payToPlutusV2Inline,
                                                        setMinAdaDepositAll,
                                                        spendPublicKeyOutput)
import           Convex.Lenses                         (emptyTx)
import           Convex.MockChain                      (Mockchain, walletUtxo)
import           Convex.MockChain.CoinSelection        (balanceAndSubmit,
                                                        paymentTo)
import qualified Convex.MockChain.Defaults             as Defaults
import           Convex.MockChain.Utils                (mockchainSucceeds)
import           Convex.Muesli.LP.BuildTx              (mkBuyOrderDatum)
import qualified Convex.Muesli.LP.BuildTx              as BuildTx
import qualified Convex.Muesli.LP.Constants            as Muesli
import           Convex.Muesli.LP.OnChain.OnChainUtils (integerToBS)
import           Convex.Muesli.LP.Types                (BuyOrder (..),
                                                        SellOrder (..),
                                                        unitPrice)
import           Convex.Utxos                          (selectUtxo)
import           Convex.Wallet                         (Wallet)
import qualified Convex.Wallet                         as Wallet
import qualified Convex.Wallet.MockWallet              as Wallet
import           Data.Maybe                            (fromMaybe)
import           Data.Proxy                            (Proxy (..))
import qualified Plutus.V1.Ledger.Api                  as PV1
import qualified Plutus.V2.Ledger.Api                  as V2
import qualified PlutusTx.Prelude                      as PlutusTx
import           Test.Tasty                            (TestTree, defaultMain,
                                                        testGroup)
import           Test.Tasty.HUnit                      (Assertion, assertEqual,
                                                        testCase)
import           Text.Hex                              (decodeHex)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "unit tests"
  [ testGroup "LP"
    [ testCase "Mint the liquidity pool NFT" (mockchainSucceeds $ mintLPNFT Wallet.w1)
    ]
  , testGroup "Orderbook v3"
    [ testCase "Place a limit buy order" (mockchainSucceeds $ placeOrder Wallet.w1)
    , testCase "Produce an output with reference script" (mockchainSucceeds $ putReferenceScript Wallet.w1)
    , testCase "Cancel a limit buy order" (mockchainSucceeds $ placeOrder Wallet.w1 >>= (\(txIn, buyOrder, returnAddress) -> cancelBuyOrder Wallet.w1 txIn returnAddress buyOrder))
    ]
  , testGroup "Serialisation"
    [ testCase "Buy order datum" buyOrderDatumTest
    , testCase "Sell order datum" sellOrderDatumTest
    , testCase "Orderbook v3 script hash" orderbookScriptHashTest
    ]
  ]

putReferenceScript :: Wallet -> Mockchain C.TxIn
putReferenceScript wallet = do
  let tx = emptyTx
            & payToPlutusV2Inline (Wallet.addressInEra Defaults.networkId wallet) Muesli.orderBookV3 (C.lovelaceToValue 1_000_000)
            & setMinAdaDepositAll Defaults.protocolParameters
  txId <- balanceAndSubmit wallet tx
  pure (C.TxIn txId (C.TxIx 0))

{-| Mint an LP NFT and return the asset name
-}
mintLPNFT :: Wallet -> Mockchain C.AssetName
mintLPNFT wllt = do
  void $ Wallet.w2 `paymentTo` wllt
  (txOutRef, _) <- walletUtxo wllt >>= maybe (fail "No utxos for wallet") pure . selectUtxo
  let ref@(PV1.TxOutRef refHash refIdx) = fromCardanoTxIn txOutRef
      tokenName = C.AssetName $ PlutusTx.fromBuiltin $ PlutusTx.sha2_256 $ V2.getTxId refHash <> integerToBS refIdx
      tx = emptyTx
            & mintPlutusV2 Muesli.nftMintingPolicy ref tokenName 1
            & spendPublicKeyOutput txOutRef
  _ <- balanceAndSubmit wllt tx
  pure tokenName

placeOrder :: Wallet -> Mockchain (C.TxIn, BuyOrder, C.AddressInEra C.BabbageEra)
placeOrder wllt = do
  let order =
        BuyOrder
          { buyCurrency = ("8a1cfae21368b8bebbbed9800fec304e95cce39a2a57dc35e2e3ebaa", "MILK")
          , buyQuantity = 8
          , buyPrice    = unitPrice 1 (20_000_000)
          }
      addr = Wallet.addressInEra Defaults.networkId wllt
      tx = emptyTx & BuildTx.buyOrder addr (Just Defaults.networkId) order
  txId <- balanceAndSubmit wllt tx
  pure (C.TxIn txId (C.TxIx 0), order, addr)

cancelBuyOrder :: Wallet -> C.TxIn -> C.AddressInEra C.BabbageEra -> BuyOrder -> Mockchain C.TxId
cancelBuyOrder wllt txIn returnAddress order = do
  let tx = emptyTx & BuildTx.cancelBuyOrder returnAddress txIn order
  balanceAndSubmit wllt tx

-- TODO: Move somewhere else!
fromCardanoTxIn :: C.TxIn -> PV1.TxOutRef
fromCardanoTxIn (C.TxIn txId (C.TxIx txIx)) = PV1.TxOutRef (fromCardanoTxId txId) (toInteger txIx)

-- TODO: Move somewhere else!
fromCardanoTxId :: C.TxId -> PV1.TxId
fromCardanoTxId txId = PV1.TxId $ PlutusTx.toBuiltin $ C.serialiseToRawBytes txId

buyOrderDatumTest :: Assertion
buyOrderDatumTest = do
  assertEqual "Datum from tx and reconstructed datum" limitBuyOrderDatum order8Milk
  assertEqual "Datum from tx and constructed datum" limitBuyOrderDatum milkBuyOrder

sellOrderDatumTest :: Assertion
sellOrderDatumTest = do
  assertEqual "Datum from tx and constructed datum" sell5Milk milkSellOrder

{-| A tx out datum for a limit buy order of 8 milk,
taken from https://cardanoscan.io/transaction/8e7bd1d7eb2e1924e622826eeaafad61db55a0aa2a4ebfa332b29f5f2b75888b
The datum was revealed in the spending transaction
https://cardanoscan.io/transaction/58259eec76e100dbaf48776b8815282323d6111cbc3d720fbf709f5ee525021b
-}
limitBuyOrderDatum :: C.ScriptData
limitBuyOrderDatum =
  either
    (error . show)
    id
    (C.deserialiseFromCBOR
      (C.proxyToAsType Proxy)
      (fromMaybe (error "error") $ decodeHex "d8799fd8799fd8799fd8799f581cab2c5ac05201927833174815d93c35cb1f8c3e2d80efd04ce6648073ffd8799fd8799fd8799f581c83c16e0ebacaf76702b6b4840e19f2c8ff35b3077387d8b14f2cd49effffffff581c8a1cfae21368b8bebbbed9800fec304e95cce39a2a57dc35e2e3ebaa444d494c4b404008d87a801a00286f90ffff")
      )

namiAddress :: C.AddressInEra C.BabbageEra
namiAddress = maybe (error "") id $ C.deserialiseAddress (C.proxyToAsType Proxy) "addr1qx4jckkq2gqey7pnzayptkfuxh93lrp79kqwl5zvuejgquurc9hqawk27ans9d45ss8pnukglu6mxpmnslvtznev6j0qd0dc2n"

milkBuyOrder :: C.ScriptData
milkBuyOrder =
  let order =
        BuyOrder
          { buyCurrency = ("8a1cfae21368b8bebbbed9800fec304e95cce39a2a57dc35e2e3ebaa", "MILK")
          , buyQuantity = 8
          , buyPrice = unitPrice 1 (2_500_000)
          }
  in mkBuyOrderDatum namiAddress order

milkSellOrder :: C.ScriptData
milkSellOrder =
  let order =
        SellOrder
          { sellCurrency = ("8a1cfae21368b8bebbbed9800fec304e95cce39a2a57dc35e2e3ebaa", "MILK")
          , sellQuantity = 5
          , sellPrice = unitPrice 5 (11_547_670)
          }
  in BuildTx.mkSellOrderDatum namiAddress order

{-| The same as 'limitOrderDatum', but more readable
-}
order8Milk :: C.ScriptData
order8Milk =
  let dec = fromMaybe (error "mkDatum: Failed") . decodeHex
      b1 = dec "ab2c5ac05201927833174815d93c35cb1f8c3e2d80efd04ce6648073" -- Receiver address
      b2 = dec "83c16e0ebacaf76702b6b4840e19f2c8ff35b3077387d8b14f2cd49e" -- Receiver stake key
      b3 = dec "8a1cfae21368b8bebbbed9800fec304e95cce39a2a57dc35e2e3ebaa" -- Policy ID
      b4 = "MILK" -- Asset name
  in ScriptDataConstructor 0
      [ ScriptDataConstructor 0
        [ ScriptDataConstructor 0
          [ ScriptDataConstructor 0
            [ScriptDataBytes b1]
          , ScriptDataConstructor 0
            [ScriptDataConstructor 0 [ScriptDataConstructor 0 [ScriptDataBytes b2]]]
          ]
        , ScriptDataBytes b3
        , ScriptDataBytes b4
        , ScriptDataBytes "" -- Ada Policy ID
        , ScriptDataBytes "" -- Ada Asset ID
        , ScriptDataNumber 8
        , ScriptDataConstructor 1 [] -- ?? Allow partial matches?
        , ScriptDataNumber 2_650_000
        ]
      ]

-- hash: 81feaa61b8eceed856c9af01bd96b65c8d4698cc012caa9d03c3ce3ea620e8a7
sell5Milk :: C.ScriptData
sell5Milk =
  ScriptDataConstructor 0
    [ ScriptDataConstructor 0
      [ ScriptDataConstructor 0
        [ ScriptDataConstructor 0
          [ ScriptDataBytes "\171,Z\192R\SOH\146x3\ETBH\NAK\217<5\203\US\140>-\128\239\208L\230d\128s"]
        , ScriptDataConstructor 0
          [ ScriptDataConstructor 0
            [ ScriptDataConstructor 0
              [ ScriptDataBytes "\131\193n\SO\186\202\247g\STX\182\180\132\SO\EM\242\200\255\&5\179\as\135\216\177O,\212\158"]
            ]
          ]
        ]
      , ScriptDataBytes ""
      , ScriptDataBytes ""
      , ScriptDataBytes "\138\FS\250\226\DC3h\184\190\187\190\217\128\SI\236\&0N\149\204\227\154*W\220\&5\226\227\235\170"
      , ScriptDataBytes "MILK"
      , ScriptDataNumber 10597670
      , ScriptDataConstructor 1 []
      , ScriptDataNumber 2650000
      ]
    ]

orderbookScriptHashTest :: Assertion
orderbookScriptHashTest = do
  assertEqual "order book script hashes should match" "00fb107bfbd51b3a5638867d3688e986ba38ff34fb738f5bd42b20d5" (C.hashScript (C.PlutusScript C.PlutusScriptV2 Muesli.orderBookV3))

-- Txn 7e4142b7a040eae45d14513000adf91ab42da33a1bd5ccffcfe851b3d93e1e5e output 1
