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

data AVLTree key value =
      MinLeaf
    | Leaf key value
    | Node
        Height              -- ^ height of a tree
        Bool                -- ^ visited
        key                 -- ^ key
        Label               -- ^ label
        (AVLTree key value) -- ^ left subtree
        (AVLTree key value) -- ^ right subtree
  deriving Show

notVisited :: (AVLTree key value) -> (AVLTree key value)
notVisited (Node h _ k lbl l r) = (Node h False k lbl l r)
notVisited leaf = leaf

-- | Returns minimal value in the tree
getMinValue :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
            => (AVLTree key value) -> (Maybe value, (AVLTree key value))
getMinValue MinLeaf = (Nothing, MinLeaf)
getMinValue (Leaf k v) = (Just v, (Leaf k v))
getMinValue (Node h _ k lbl l r) = (mv, (Node h True k lbl (notVisited l) mp))
  where
    (mv, mp) = getMinValue l

doLookup :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => key
         -> (AVLTree key value)
         -> (Maybe value, AVLTree key value)
doLookup _ MinLeaf = (Nothing, MinLeaf)
doLookup k (Leaf lk lv) | k == lk   = (Just lv, (Leaf lk lv))
                        | otherwise = (Nothing, (Leaf lk lv))
doLookup k (Node h _ nk lbl lc rc) | k < nk  = (lv, (Node h True nk lbl lp (notVisited rc)))
                                   | k > nk  = (rv, (Node h True nk lbl (notVisited lc) rp))
                                   | k == nk = (mv, (Node h True nk lbl (notVisited lc) mp))
  where
    (lv, lp) = doLookup k lc
    (rv, rp) = doLookup k rc
    (mv, mp) = getMinValue rc

-- TODO
doReplace :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
          => key
          -> value
          -> (AVLTree key value)
          -> (AVLTree key value)
doReplace _ _ t = t

-- TODO
doInsert :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => key
         -> value
         -> (AVLTree key value)
         -> (AVLTree key value)
doInsert _ _ t = t

-- TODO
doRemove :: (Ord key, ByteArrayAccess key)
         => key
         -> (AVLTree key value)
         -> (AVLTree key value)
doRemove _ t = t
