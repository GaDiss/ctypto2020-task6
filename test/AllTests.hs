module AllTests
  ( allTestTree
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

allTestTree :: IO TestTree
allTestTree = testSpec "AllTests" allSpec

allSpec :: Spec
allSpec = do
  describe "InsertTests" $ do
    it "test1: insert (key1, valueA) in emptyTree" $
      let
        ans = commonTest doInsert emptyTree key1 valueA
      in ans `shouldBe` (False, True, True)
    it "test2: insert (key2, valueB) in oneTree" $
      let
        ans = commonTest doInsert oneTree key2 valueB
      in ans `shouldBe` (False, True, True)

  describe "SecurityTests" $ do
    it "test1: delete (key3) from fourTree" $
      let
        ans = securityTest fourTree (Delete key3) (Delete key3)
      in ans `shouldBe` (True, True, True)

    it "test2: insert (key2, valueB) in oneTree" $
      let
        ans = securityTest oneTree (Insert key2 valueB) (Insert key2 valueB)
      in ans `shouldBe` (True, True, True)

    it "test3: insert (key1, valueA) in fiveTree" $
      let
        ans = securityTest fiveTree (Insert key1 valueA) (Insert key1 valueA)
      in ans `shouldBe` (True, True, True)
         
    it "test4: insert (key5, valueE) in threeTree" $
      let
        ans = securityTest threeTree (Insert key5 valueE) (Delete key5)
      in ans `shouldBe` (False, False, True)

  where

    securityTest oldTree fOp sOp = (res1, res2, res3)
      where
        proveRes :: ProveResult
        proveRes = prove oldTree fOp;
        newTree :: Tree
        newTree = second3 proveRes
        proof :: Proof
        proof = first3 proveRes
        verifyRes :: VerifyResult
        verifyRes = verify (getDigest oldTree) sOp proof
        getVerifiedDigest = second3 verifyRes
        getResult = third3
        res1 = (getDigest newTree == getVerifiedDigest)
        res2 = (getResult proveRes == getResult verifyRes)
        res3 = (first3 verifyRes == Accept)
    
    commonTest action oldTree key value = (res1, res2, res3)
      where
        opRes1 :: OperationResult
        opRes1 = doLookup key oldTree
        opRes2 :: OperationResult
        opRes2 = action key value oldTree
        newTree :: Tree
        newTree = third3 opRes2
        opRes3 :: OperationResult
        opRes3 = doLookup key newTree
        res1 = (checkFst value opRes1)
        res2 = (checkFst value opRes2)
        res3 = (checkFst value opRes3)

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