module Prover
  (
    prove
  ) where

import AVLTree
import Hashing
import Lib

import Data.ByteArray (ByteArrayAccess)
import Data.Set (Set, insert, union, empty, member)

-- | applies operation and generates proof
prove :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
      => (AVLTree key value)     -- ^ Old Tree
      -> (Operation key value)   -- ^ Operation
      -> ( [ProofNode key value] -- ^ Proof of the operation
         , (AVLTree key value)   -- ^ New Tree
         , Maybe value           -- ^ Operation return value
         )
prove t op = (buildProof RootNode (getKey op) t set, newT, retV)
  where
    -- applies operation to a tree
    (retV, set, newT) = modify op t

-- | generates a proof
buildProof :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
           => Direction             -- ^ direction of Node
           -> key                   -- ^ Changed Leaf's key
           -> (AVLTree key value)   -- ^ oldTree
           -> (Set key)             -- ^ visited nodes
           -> [ProofNode key value] -- ^ proof
buildProof direction k (Node _ nk _ l r) set
  | member nk set = (PathNode nk : (buildProof LeftNode k l set) ++ (buildProof RightNode k r set))
buildProof direction k (Leaf lk lv) set
  | k == lk = [StartingLeaf lk lv]
buildProof direction k node set = [getProofNode direction node]

-- ProofNode constructor
getProofNode :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
             => Direction -> AVLTree key value -> ProofNode key value
getProofNode direction MinLeaf                = NodeLabel direction Nothing emptyLabel 0
getProofNode direction (Leaf k v)             = NodeLabel direction (Just k) (labelLeaf k v) 0
getProofNode direction (Node h k label _ _)   = NodeLabel direction (Just k) label h
