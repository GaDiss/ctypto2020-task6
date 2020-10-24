module Lib
  (
    Label
  , Height
  , RootDigest (..)
  , Result (..)
  , Direction (..)
  , ProofNode (..)
  , Operation (..)
  , first3
  , second3
  , third3
  ) where

import Crypto.Hash
type Label = Digest Blake2b_512

type Height = Int

data RootDigest = RootDigest Label Int
  deriving (Show, Eq)

data Result = Accept | Reject
  deriving (Show, Eq)

data Direction = LeftNode | RightNode | RootNode
  deriving (Show, Eq)

data ProofNode key value =
       PathNode key
     | NodeLabel Direction (Maybe key) Label Height
     | StartingLeaf key value
     deriving Show

data Operation key value =
       Insert {getKey :: key, getValue :: value}
     | Delete {getKey :: key}
     | Lookup {getKey :: key}
     | Replace {getKey :: key, getValue :: value}
     deriving Show

first3 :: (a, b, c) -> a
first3 (x, _, _) = x

second3 :: (a, b, c) -> b
second3 (_, y, _) = y

third3 :: (a, b, c) -> c
third3 (_, _, z) = z
