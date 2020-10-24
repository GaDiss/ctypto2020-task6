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

import qualified Data.ByteString.UTF8 as BS
import qualified Data.Set as DS

type S = BS.ByteString
type Tree = AVLTree S S
type OperationResult = (Maybe S, DS.Set S, Tree)
type Proof = [ProofNode S S]
type ProveResult = (Proof, Tree, Maybe S)
type VerifyResult = (Result, RootDigest, Maybe S)

block1Task1TestTree :: IO TestTree
block1Task1TestTree = testSpec "block1Task1" block1Task1Spec

block1Task1Spec :: Spec
block1Task1Spec = do  
  describe "Block1.Task1.Insert" $ do
    it "test1" $
      let
        res1 :: OperationResult
        res1 = doLookup key1 emptyTree
        res2 :: OperationResult
        res2 = doInsert key1 valueA emptyTree
        newTree :: Tree
        newTree = third3 res2
        res3 :: OperationResult
        res3 = doLookup key1 newTree
      in not (checkFst valueA res1) &&
             (checkFst valueA res2) &&
             (checkFst valueA res3) `shouldBe` True
             
    it "test2" $
      let
        res1 :: OperationResult
        res1 = doLookup key5 threeTree
        res2 :: OperationResult
        res2 = doInsert key5 valueE threeTree
        newTree :: Tree
        newTree = third3 res2
        res3 :: OperationResult
        res3 = doLookup key5 newTree
      in not (checkFst valueE res1) && 
             (checkFst valueE res2) &&
             (checkFst valueE res3) `shouldBe` True
  
  describe "Block1.Task1.Security" $ do
    it "test1" $
      let
        oldTree :: Tree
        oldTree = fourTree
        proveRes :: ProveResult
        proveRes = prove oldTree (Delete key3);
        newTree :: Tree
        newTree = second3 proveRes
        proof :: Proof
        proof = first3 proveRes
        verifyRes :: VerifyResult
        verifyRes = verify (getDigest oldTree) (Delete key3) proof
        getVerifiedDigest = second3 verifyRes
      in getDigest newTree `shouldBe` getVerifiedDigest

  where
    check :: S -> Maybe S -> Bool
    check val1 (Just val2) = val1 == val2
    check _ Nothing = False
    
    checkFst :: S -> OperationResult -> Bool
    checkFst val1 = (check val1) . first3
    
    key1 :: S
    key1 = BS.fromString "1"
    key2 :: S
    key2 = BS.fromString "2"
    key3 :: S
    key3 = BS.fromString "3"
    key4 :: S
    key4 = BS.fromString "4"
    key5 :: S
    key5 = BS.fromString "5"

    valueA :: S
    valueA = BS.fromString "aaa"
    valueB :: S
    valueB = BS.fromString "bbb"
    valueC :: S
    valueC = BS.fromString "ccc"
    valueD :: S
    valueD = BS.fromString "ddd"
    valueE :: S
    valueE = BS.fromString "eee"

    emptyTree :: Tree
    emptyTree = AVLTree.fromList []
    oneTree :: Tree
    oneTree = AVLTree.fromList [(key1, valueA)]
    twoTree :: Tree
    twoTree = AVLTree.fromList [(key1, valueA), (key2, valueB)]
    threeTree :: Tree
    threeTree = AVLTree.fromList [(key1, valueA), (key2, valueB), (key3, valueC)]
    fourTree :: Tree
    fourTree = AVLTree.fromList [(key1, valueA), (key2, valueB), (key3, valueC), (key4, valueD)]
    fiveTree :: Tree
    fiveTree = AVLTree.fromList [(key1, valueA), (key2, valueB), (key3, valueC), (key4, valueD), (key5, valueE)]