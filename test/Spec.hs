module Main
  ( main
  ) where

import Test.Tasty
import Block1Task1Spec (block1Task1TestTree)

main :: IO ()
main = do
  test1 <- block1Task1TestTree
  defaultMain $ testGroup "Tests" [test1]