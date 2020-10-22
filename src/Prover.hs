module Prover
  (
    prover
  ) where

import Lib
import AVLTree
import qualified Data.ByteString as B
import Crypto.Hash
import Data.ByteArray (ByteArrayAccess)

-- TODO
prover :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
       => (AVLTree key value)
       -> (Operation key value)
       -> ([(ProofNode key value)], (AVLTree key value), Maybe value)
prover t _ = ([], t, Nothing)

emptyLabel = stringToLabel ""

labelLeaf :: (ByteArrayAccess key, ByteArrayAccess value)
          => key -> value -> Label
labelLeaf k v =
    hashFinalize $ flip hashUpdate v
                 $ flip hashUpdate k
                 $ flip hashUpdate (B.singleton 0)
                 $ hashInit

getProofNode :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
             => Direction -> AVLTree key value -> ProofNode key value
getProofNode direction MinLeaf                = NodeLabel direction emptyLabel 0
getProofNode direction (Leaf k v)             = NodeLabel direction (labelLeaf k v) 0
getProofNode direction (Node h _ _ label _ _) = NodeLabel direction label h
