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
      getDigest (second3 (prove smallTree (Delete bytes3))) `shouldBe`
      second3 (verify (getDigest smallTree) (Delete bytes3) (first3 (prove smallTree (Delete bytes3))))
  
  where
    label5 :: Label
    label5 = stringToLabel "5"
    label0 :: Label
    label0 = stringToLabel "0"
    
    bytes1 :: BU.ByteString
    bytes1 = BU.fromString "1"
    bytes2 :: BU.ByteString
    bytes2 = BU.fromString "2"
    bytes3 :: BU.ByteString
    bytes3 = BU.fromString "3"
    bytes4 :: BU.ByteString
    bytes4 = BU.fromString "4"
    bytes5 :: BU.ByteString
    bytes5 = BU.fromString "5"
    bytesSas :: BU.ByteString
    bytesSas = BU.fromString "sas"
    bytesLol :: BU.ByteString
    bytesLol = BU.fromString "lol"
    bytesKek :: BU.ByteString
    bytesKek = BU.fromString "kek"
    bytesFoo :: BU.ByteString
    bytesFoo = BU.fromString "foo"
    
    smallTree :: AVLTree BU.ByteString BU.ByteString
    smallTree = AVLTree.fromList [(bytes1, bytesSas), (bytes2, bytesKek), (bytes3, bytesLol), (bytes4, bytesFoo)]