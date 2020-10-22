module Lib
  (
    Label (..)
  , Height (..)
  , RootDigest (..)
  , Result (..)
  , Direction (..)
  , ProofNode (..)
  , Operation (..)
  , stringToLabel
  ) where

import Crypto.Hash
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU

type Label = Digest Blake2b_512

type Height = Int

data RootDigest = RootDigest Label Int

data Result = Accept | Reject
  deriving (Show, Eq)

data Direction = LeftNode | RightNode
  deriving (Show, Eq)

data ProofNode key value =
       PathNode
     | NodeLabel Direction Label Height
     | StartingLeaf key value
     deriving Show

data Operation key value =
       Insert key value
     | Delete key
     | Lookup key
     | Replace key value
     deriving Show

stringToLabel :: String -> Label
stringToLabel = hashWith Blake2b_512 . BU.fromString
