module Main
  ( main
  ) where

import Test.Tasty
import AllTests (allTestTree)

main :: IO ()
main = do
  test <- allTestTree
  defaultMain $ testGroup "Tests" [test]