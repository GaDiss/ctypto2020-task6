module Verifier where

import Lib
import AVLTree
import Data.ByteArray (ByteArrayAccess)

-- TODO
verify :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
       => RootDigest              -- ^ old digest
       -> (Operation key value)   -- ^ applied operation
       -> [(ProofNode key value)] -- ^ proof
       -> ( Result                -- ^ verifier's verdict
          , RootDigest            -- ^ new digest
          , Maybe value           -- ^ return value
          )

verify (RootDigest rootLbl h) op proof = (result, newD, retV)
  where
    recT = case (compare (length proof) (2 * (2 + h))) of
      GT -> MinLeaf
      _  -> fst $ recreateTree proof
    result = if (label recT) == rootLbl
      then Accept
      else Reject
    (retV, _, newT) = AVLTree.modify op recT
    newD = getDigest newT

recreateTree :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
             => [ProofNode key value]
             -> (AVLTree key value, [ProofNode key value])
recreateTree (StartingLeaf k v : rest)             = ((Leaf k v), rest)
recreateTree (NodeLabel dir (Just k) lbl h : rest) =
  case h of
    0 -> (LabelLeaf k lbl, rest)
    _ -> (Node h k lbl MinLeaf MinLeaf, rest)
recreateTree (PathNode k : rest)                   = (newNode k l r, rr)
  where
    (l, lr) = recreateTree rest
    (r, rr) = recreateTree lr
recreateTree (_ : rest)                            = (MinLeaf, rest)
