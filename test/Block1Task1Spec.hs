module Block1Task1Spec
  ( block1Task1TestTree
  ) where

import AVLTree
import Hashing
import Lib
import Prover
import Verifier
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import qualified Data.ByteString.UTF8 as BU

block1Task1TestTree :: IO TestTree
block1Task1TestTree = testSpec "block1Task1" block1Task1Spec

block1Task1Spec :: Spec
block1Task1Spec = do
  describe "Block1.Task1.Security" $ do
    it "test1" $
      getDigest (second3 (prove fourTree (Delete key3))) `shouldBe`
      second3 (verify (getDigest fourTree) (Delete key3) (first3 (prove fourTree (Delete key3))))

  where
    key1 :: BU.ByteString
    key1 = BU.fromString "1"
    key2 :: BU.ByteString
    key2 = BU.fromString "2"
    key3 :: BU.ByteString
    key3 = BU.fromString "3"
    key4 :: BU.ByteString
    key4 = BU.fromString "4"
    key5 :: BU.ByteString
    key5 = BU.fromString "5"

    valueA :: BU.ByteString
    valueA = BU.fromString "aaa"
    valueB :: BU.ByteString
    valueB = BU.fromString "bbb"
    valueC :: BU.ByteString
    valueC = BU.fromString "ccc"
    valueD :: BU.ByteString
    valueD = BU.fromString "ddd"
    valueE :: BU.ByteString
    valueE = BU.fromString "eee"

    oneTree :: AVLTree BU.ByteString BU.ByteString
    oneTree = AVLTree.fromList [(key1, valueA)]
    twoTree :: AVLTree BU.ByteString BU.ByteString
    twoTree = AVLTree.fromList [(key1, valueA), (key2, valueB)]
    threeTree :: AVLTree BU.ByteString BU.ByteString
    threeTree = AVLTree.fromList [(key1, valueA), (key2, valueB), (key3, valueC)]
    fourTree :: AVLTree BU.ByteString BU.ByteString
    fourTree = AVLTree.fromList [(key1, valueA), (key2, valueB), (key3, valueC), (key4, valueD)]
    fiveTree :: AVLTree BU.ByteString BU.ByteString
    fiveTree = AVLTree.fromList [(key1, valueA), (key2, valueB), (key3, valueC), (key4, valueD), (key5, valueE)]
    
    x :: ( [ProofNode BU.ByteString BU.ByteString]
         , AVLTree BU.ByteString BU.ByteString
         , Maybe BU.ByteString
         )
    x = prove fourTree (Delete key4)