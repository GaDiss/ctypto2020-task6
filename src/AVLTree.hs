module AVLTree
  (
    AVLTree (..)
  , doLookup
  , doReplace
  , doInsert
  , doRemove
  ) where

import Lib
import Data.ByteArray (ByteArrayAccess, unpack)
import qualified Data.ByteString as B
import Crypto.Hash

emptyLabel = stringToLabel ""

data AVLTree key value =
      MinLeaf
    | Leaf key value
    | Node Heighth key Label (AVLTree key value) (AVLTree key value)
  deriving Show

labelLeaf :: (ByteArrayAccess key, ByteArrayAccess value)
          => key -> value -> Label
labelLeaf k v =
    hashFinalize $ flip hashUpdate v
                 $ flip hashUpdate k
                 $ flip hashUpdate (B.singleton 0)
                 $ hashInit

getProofNode :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
             => Direction -> AVLTree key value -> ProofNode key value
getProofNode direction MinLeaf               = Neighbour direction emptyLabel 0
getProofNode direction (Leaf k v)            = Neighbour direction (labelLeaf k v) 0
getProofNode direction (Node h _ label _ _)  = Neighbour direction label h

-- | Returns minimal value in the tree
getMinValue :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
            => (AVLTree key value) -> (Maybe value, [ProofNode key value])
getMinValue MinLeaf = (Nothing, [])
getMinValue (Leaf k v) = (Just v, [StartingLeaf k v])
getMinValue (Node _ _ _ l r) = (mv, (getProofNode RightNode r : mp))
  where
    (mv, mp) = getMinValue l

doLookup :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => key
         -> (AVLTree key value)
         -> (Maybe value, [ProofNode key value])
doLookup _ MinLeaf = (Nothing, [])
doLookup k (Leaf lk lv) | k == lk   = (Just lv, [StartingLeaf lk lv])
                        | otherwise = (Nothing, [])
doLookup k (Node _ nk _ lc rc) | k < nk  = (lv, (getProofNode RightNode rc : lp))
                               | k > nk  = (rv, (getProofNode LeftNode lc : rp))
                               | k == nk = (mv, (getProofNode LeftNode lc : mp))
  where
    (lv, lp) = doLookup k lc
    (rv, rp) = doLookup k rc
    (mv, mp) = getMinValue rc

-- TODO
doReplace :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
          => key
          -> value
          -> (AVLTree key value)
          -> (AVLTree key value, [ProofNode key value])
doReplace _ _ t = (t, [])

-- TODO
doInsert :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => key
         -> value
         -> (AVLTree key value)
         -> (AVLTree key value, [ProofNode key value])
doInsert _ _ t = (t, [])

-- TODO
doRemove :: (Ord key, ByteArrayAccess key)
         => key
         -> (AVLTree key value)
         -> (AVLTree key value, [ProofNode key value])
doRemove _ t = (t, [])
