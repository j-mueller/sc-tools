{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main(main) where

import qualified Cardano.Api.Shelley                   as C
import           Control.Lens                          ((&))
import           Control.Monad                         (void)
import           Convex.BuildTx                        (mintPlutusV2,
                                                        spendPublicKeyOutput)
import           Convex.Lenses                         (emptyTx)
import           Convex.MockChain                      (Mockchain, walletUtxo)
import           Convex.MockChain.CoinSelection        (balanceAndSubmit,
                                                        paymentTo)
import qualified Convex.MockChain.Defaults             as Defaults
import           Convex.MockChain.Utils                (mockchainSucceeds)
import qualified Convex.Muesli.LP.Constants            as Muesli
import           Convex.Muesli.LP.OnChain.OnChainUtils (integerToBS)
import           Convex.Wallet                         (Wallet)
import qualified Convex.Wallet.MockWallet              as Wallet
import           Convex.Wallet.Utxos                   (selectUtxo)
import qualified Plutus.V1.Ledger.Api                  as PV1
import qualified Plutus.V2.Ledger.Api                  as V2
import qualified PlutusTx.Prelude                      as PlutusTx
import           Test.Tasty                            (TestTree, defaultMain,
                                                        testGroup)
import           Test.Tasty.HUnit                      (testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "unit tests"
  [ testGroup "LP"
    [ testCase "Mint the liquidity pool NFT" (mockchainSucceeds $ mintLPNFT Wallet.w1)
    ]
  ]


{-| Mint an LP NFT and return the asset name
-}
mintLPNFT :: Wallet -> Mockchain C.AssetName
mintLPNFT wllt = do
  void $ Wallet.w2 `paymentTo` wllt
  (txOutRef, _) <- walletUtxo Defaults.networkId wllt >>= maybe (fail "No utxos for wallet") pure . selectUtxo
  let ref@(PV1.TxOutRef refHash refIdx) = fromCardanoTxIn txOutRef
      tokenName = C.AssetName $ PlutusTx.fromBuiltin $ PlutusTx.sha2_256 $ V2.getTxId refHash <> integerToBS refIdx
      tx = emptyTx
            & mintPlutusV2 Muesli.nftMintingPolicy ref tokenName 1
            & spendPublicKeyOutput txOutRef
  _ <- balanceAndSubmit wllt tx
  pure tokenName

-- TODO: Move somewhere else!
fromCardanoTxIn :: C.TxIn -> PV1.TxOutRef
fromCardanoTxIn (C.TxIn txId (C.TxIx txIx)) = PV1.TxOutRef (fromCardanoTxId txId) (toInteger txIx)

-- TODO: Move somewhere else!
fromCardanoTxId :: C.TxId -> PV1.TxId
fromCardanoTxId txId = PV1.TxId $ PlutusTx.toBuiltin $ C.serialiseToRawBytes txId
