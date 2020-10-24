module AllTests
  ( allTestTree
  ) where

import AVLTree
import Lib
import Prover
import Verifier
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import qualified Data.ByteString.UTF8 as BS
import qualified Data.Set as DS

import Control.Monad.Random hiding (Random)

type S = BS.ByteString
type Tree = AVLTree S S
type OperationResult = (Maybe S, DS.Set S, Tree)
type Proof = [ProofNode S S]
type ProveResult = (Proof, Tree, Maybe S)
type VerifyResult = (Result, RootDigest, Maybe S)

randomList :: Int -> IO [(BS.ByteString, BS.ByteString)]
randomList 0 = return []
randomList n = do
  r1  <- randomIO :: IO Int
  r2  <- randomIO :: IO Int
  rs <- randomList (n - 1)
  return ((BS.fromString $ show r1, BS.fromString $ show r2) : rs)

randomTree :: Int -> IO (AVLTree BS.ByteString BS.ByteString)
randomTree n = do
  rl <- randomList n
  return (AVLTree.fromList rl)

randomTest :: Int -> IO (Bool)
randomTest n = do
  t <- randomTree n
  let key3 = BS.fromString "3"
  let rT = (third3 $ doInsert key3 key3 t)
  let oldDigest = getDigest rT
  let op = Delete key3
  let proveResult = prove rT op
  return ((getDigest (second3 proveResult)) == (second3 (verify oldDigest op (first3 proveResult))))

allTestTree :: IO TestTree
allTestTree = testSpec "AllTests" allSpec

allSpec :: Spec
allSpec = do
  describe "InsertTests" $ do
    it "test1: insert (key1, valueA) in emptyTree" $
      let
        (_, ans) = commonTest (doInsert key1 valueA) emptyTree key1 valueA
      in ans `shouldBe` (False, True, True)

    it "test2: insert (key2, valueB) in oneTree" $
      let
        (_, ans) = commonTest (doInsert key2 valueB) oneTree key2 valueB
      in ans `shouldBe` (False, True, True)

    it "test3: insert (key2, valueB) in twoTree" $
      let
        (_, ans) = commonTest (doInsert key2 valueB) twoTree key2 valueB
      in ans `shouldBe` (True, False, True)

  describe "DeleteTests" $ do
    it "test1: delete (key1) from emptyTree" $
      let
        (_, ans) = commonTest (doDelete key1) emptyTree key1 valueA
      in ans `shouldBe` (False, False, False)

    it "test1: delete (key2) from fiveTree" $
      let
        (_, ans) = commonTest (doDelete key2) fiveTree key2 valueB
      in ans `shouldBe` (True, True, False)

  describe "MultiInsertTests" $ do
    it "add1, add2, del1, check1, check2" $
      let
        (newTree1, ans1) = singleCommonTest (doInsert key1 valueA) emptyTree valueA
        (newTree2, ans2) = singleCommonTest (doInsert key2 valueB) newTree1 valueB
        (newTree3, ans3) = singleCommonTest (doDelete key1) newTree2 valueA
        (newTree4, ans4) = singleCommonTest (doLookup key1) newTree3 valueA
        (_, ans5) = singleCommonTest (doLookup key2) newTree4 valueB
      in (ans1 == True) &&
         (ans2 == True) &&
         (ans3 == True) &&
         (ans4 == False) &&
         (ans5 == True) `shouldBe` True

    it "add1, add2, del3, check1, check2" $
      let
        (newTree1, ans1) = singleCommonTest (doInsert key1 valueA) emptyTree valueA
        (newTree2, ans2) = singleCommonTest (doInsert key2 valueB) newTree1 valueB
        (newTree3, ans3) = singleCommonTest (doDelete key3) newTree2 valueC
        (newTree4, ans4) = singleCommonTest (doLookup key1) newTree3 valueA
        (_, ans5) = singleCommonTest (doLookup key2) newTree4 valueB
      in (ans1 == True) &&
         (ans2 == True) &&
         (ans3 == False) &&
         (ans4 == True) &&
         (ans5 == True) `shouldBe` True

    it "add1-value1, add1-value2, check1" $
      let
        (newTree1, ans1) = singleCommonTest (doInsert key1 valueA) emptyTree valueA
        (newTree2, ans2) = singleCommonTest (doInsert key1 valueB) newTree1 valueB
        (_, ans3) = singleCommonTest (doLookup key1) newTree2 valueA
      in (ans1 == True) &&
         (ans2 == False) &&
         (ans3 == True) `shouldBe` True

    it "add1, add2, add3, add4, verify all prove" $
      let
        (newTree1, ans1) = singleCommonTest (doInsert key1 valueA) emptyTree valueA
        (newTree2, ans2) = singleCommonTest (doInsert key2 valueB) newTree1 valueB
        (newTree3, ans3) = singleCommonTest (doInsert key3 valueC) newTree2 valueC
        (newTree4, ans4) = singleCommonTest (doInsert key4 valueD) newTree3 valueD
        (newTree5, ans5) = securityTest newTree4 (Insert key1 valueA) (Insert key1 valueA)
        (newTree6, ans6) = securityTest newTree5 (Insert key2 valueB) (Insert key2 valueB)
        (newTree7, ans7) = securityTest newTree6 (Insert key3 valueC) (Insert key3 valueC)
        (_, ans8) = securityTest newTree7 (Insert key4 valueD) (Insert key4 valueD)
      in (ans1 == True) &&
         (ans2 == True) &&
         (ans3 == True) &&
         (ans4 == True) &&
         (ans5 == (True, True, True)) &&
         (ans6 == (True, True, True)) &&
         (ans7 == (True, True, True)) &&
         (ans8 == (True, True, True)) `shouldBe` True

    it "add1, add3, add5, del3, check1, check2, check5" $
      let
        (newTree1, ans1) = singleCommonTest (doInsert key1 valueA) emptyTree valueA
        (newTree2, ans2) = singleCommonTest (doInsert key3 valueC) newTree1 valueC
        (newTree3, ans3) = singleCommonTest (doInsert key5 valueE) newTree2 valueE
        (newTree4, ans4) = singleCommonTest (doDelete key3) newTree3 valueC
        (newTree5, ans5) = singleCommonTest (doLookup key1) newTree4 valueA
        (newTree6, ans6) = singleCommonTest (doLookup key3) newTree5 valueC
        (_, ans7) = singleCommonTest (doLookup key5) newTree6 valueE
      in (ans1 == True) &&
         (ans2 == True) &&
         (ans3 == True) &&
         (ans4 == True) &&
         (ans5 == True) &&
         (ans6 == False) &&
         (ans7 == True) `shouldBe` True

  describe "SecurityTests" $ do
    it "test1: delete3-delete3 (key3) from fourTree" $
      let
        (_, ans) = securityTest fourTree (Delete key3) (Delete key3)
      in ans `shouldBe` (True, True, True)

    it "test2: insert2-insert2 (key2, valueB) in oneTree" $
      let
        (_, ans) = securityTest oneTree (Insert key2 valueB) (Insert key2 valueB)
      in ans `shouldBe` (True, True, True)

    it "test3: insert1-insert1 (key1, valueA) in fiveTree" $
      let
        (_, ans) = securityTest fiveTree (Insert key1 valueA) (Insert key1 valueA)
      in ans `shouldBe` (True, True, True)

    it "test4: insert5-delete5 (key5, valueE) in threeTree" $
      let
        (_, ans) = securityTest threeTree (Insert key5 valueE) (Delete key5)
      in ans `shouldBe` (False, False, True)

    it "test5: delete5-insert5 (key5, valueE) in threeTree" $
      let
        (_, ans) = securityTest threeTree (Delete key5) (Insert key5 valueE)
      in ans `shouldBe` (False, False, True)

    it "test6: delete5-delete5 (key5) in emptyTree" $
      let
        (_, ans) = securityTest emptyTree (Delete key5) (Delete key5)
      in ans `shouldBe` (True, True, True)

    it "test7: insert2B-insert2C (key2, valueB) (key2, valueC) in oneTree" $
      let
        (_, ans) = securityTest oneTree (Insert key2 valueB) (Insert key2 valueC)
      in ans `shouldBe` (False, False, True)

    it "test8: delete2-delete3 (key2) (key3) in fourTree" $
      let
        (_, ans) = securityTest fourTree (Delete key2) (Delete key3)
      in ans `shouldBe` (False, False, True)

  describe "StressTests" $ do
    it "test1" $ do
      ans <- (randomTest 10)
      ans `shouldBe` True
    it "test2" $ do
      ans <- (randomTest 100)
      ans `shouldBe` True
    it "test3" $ do
      ans <- (randomTest 1000)
      ans `shouldBe` True
    it "test4" $ do
      ans <- (randomTest 10000)
      ans `shouldBe` True
    it "test5" $ do
      ans <- (randomTest 100000)
      ans `shouldBe` True

  where

    securityTest oldTree fOp sOp = (newTree, (res1, res2, res3))
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

    commonTest action oldTree key value = (newTree, (res1, res2, res3))
      where
        opRes1 :: OperationResult
        opRes1 = doLookup key oldTree
        opRes2 :: OperationResult
        opRes2 = action oldTree
        newTree :: Tree
        newTree = third3 opRes2
        opRes3 :: OperationResult
        opRes3 = doLookup key newTree
        res1 = (checkFst value opRes1)
        res2 = (checkFst value opRes2)
        res3 = (checkFst value opRes3)

    singleCommonTest action oldTree value = (newTree, res1)
      where
        opRes1 :: OperationResult
        opRes1 = action oldTree
        newTree :: Tree
        newTree = third3 opRes1
        res1 = (checkFst value opRes1)

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