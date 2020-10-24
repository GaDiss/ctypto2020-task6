module Verifier
  (
    verify
  ) where

import AVLTree
import Lib

import Data.ByteArray (ByteArrayAccess)

-- | verifies proof of a given operation
verify :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
       => RootDigest            -- ^ old digest
       -> Operation key value   -- ^ applied operation
       -> [ProofNode key value] -- ^ proof
       -> ( Result              -- ^ verifier's verdict
          , RootDigest          -- ^ new digest
          , Maybe value         -- ^ return value
          )
verify (RootDigest rootLbl h) op proof = (result, newD, retV)
  where
    -- partially recreates a  after checking that proof is not too long
    lenCheck = compare (length proof) (2 * (2 + h))
    recT = case lenCheck of
      GT -> MinLeaf
      _  -> fst $ recreateTree proof
    -- compares recreated digest with starting digest
    result = if getDigest recT == RootDigest rootLbl h && (lenCheck /= GT)
      then Accept
      else Reject
    -- applies operation
    (retV, _, newT) = AVLTree.modify op recT
    -- calculates a new digest
    newD = getDigest newT

-- | partially recreates a tree from proof
recreateTree :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
             => [ProofNode key value]
             -> ( AVLTree key value
                , [ProofNode key value]
                )
recreateTree (StartingLeaf k v : rest)             = (Leaf k v, rest)
recreateTree (NodeLabel _ (Just k) lbl h : rest) =
  case h of
    0 -> (LabelLeaf k lbl, rest)
    _ -> (Node h k lbl MinLeaf MinLeaf, rest)
recreateTree (PathNode k : rest)                   = (newNode k l r, rr)
  where
    (l, lr) = recreateTree rest
    (r, rr) = recreateTree lr
recreateTree (_ : rest)                            = (MinLeaf, rest)
recreateTree []                                    = (MinLeaf, [])
