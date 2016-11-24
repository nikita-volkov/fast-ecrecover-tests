module Main where

import Rebase.Prelude
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import qualified Blockchain.Data.Address as C
import qualified Blockchain.Data.Code as B
import qualified Blockchain.Data.RLP as H
import qualified Blockchain.Data.Transaction as D
import qualified Blockchain.Data.TransactionDef as I
import qualified Blockchain.ExtendedECDSA as F
import qualified Blockchain.FastECRecover as E
import qualified Blockchain.SHA as J
import qualified Network.Haskoin.Crypto as A
import qualified Network.Haskoin.Internals as G


main =
  defaultMain $
  testGroup ""
  [
    testCase "getPubKeyFromSignature_fast == getPubKeyFromSignature" getPubKeyFromSignatureImplementationsTest
    ,
    testCase "getPubKeyFromSignature_fast == derivePubKey" fastGetPubKeyFromSignatureEqualsDerivePubKeyTest
    ,
    testCase "getPubKeyFromSignature == derivePubKey" slowGetPubKeyFromSignatureEqualsDerivePubKeyTest
  ]

getPubKeyFromSignatureImplementationsTest =
  forM_ [0..100] $ \n -> do
    generateTransaction n >>= onTransaction
  where
    onTransaction transaction =
      assertEqual "Unequal" slowPubKey fastPubKey
      where
        extendedSignature =
          extendedSignatureFromTransaction transaction
        hash =
          hashFromTransaction transaction
        fastPubKey =
          E.getPubKeyFromSignature_fast extendedSignature hash
        slowPubKey =
          F.getPubKeyFromSignature extendedSignature hash

fastGetPubKeyFromSignatureEqualsDerivePubKeyTest =
  forM_ [0..100] $ \n -> do
    generateTransaction n >>= onTransaction
  where
    onTransaction transaction =
      assertEqual "Unequal" (Just derivedPubKey) fastPubKey
      where
        extendedSignature =
          extendedSignatureFromTransaction transaction
        hash =
          hashFromTransaction transaction
        fastPubKey =
          E.getPubKeyFromSignature_fast extendedSignature hash
        derivedPubKey =
          A.derivePubKey prvKey

slowGetPubKeyFromSignatureEqualsDerivePubKeyTest =
  forM_ [0..100] $ \n -> do
    generateTransaction n >>= onTransaction
  where
    onTransaction transaction =
      assertEqual "Unequal" (Just derivedPubKey) slowPubKey
      where
        extendedSignature =
          extendedSignatureFromTransaction transaction
        hash =
          hashFromTransaction transaction
        slowPubKey =
          F.getPubKeyFromSignature extendedSignature hash
        derivedPubKey =
          A.derivePubKey prvKey

prvKey :: A.PrvKey
prvKey =
  fromJust (A.makePrvKey 0xeede3a2ed7d98cfee7ee7f49fede3f5aa6ab0bc9dc9f2bd7198900e3c7105c9c)

address :: C.Address
address =
  C.pubKey2Address (A.derivePubKey prvKey)

generateTransaction :: Integer -> IO D.Transaction
generateTransaction n =
  A.withSource A.devURandom $ D.createContractCreationTX n 1 3141592 0 (B.Code "") prvKey

extendedSignatureFromTransaction :: D.Transaction -> F.ExtendedSignature
extendedSignatureFromTransaction t =
  F.ExtendedSignature (G.Signature (fromInteger (D.transactionR t)) (fromInteger (D.transactionS t))) (0x1c == D.transactionV t)

hashFromTransaction :: D.Transaction -> G.Word256
hashFromTransaction t =
  (\(J.SHA x) -> x) $ J.hash $ H.rlpSerialize $ I.partialRLPEncode t
