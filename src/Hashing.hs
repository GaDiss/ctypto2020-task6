module Hashing
  ( emptyLabel
  , labelLeaf
  ,  labelCombine
  , stringToLabel
  ) where

import Lib

import Crypto.Hash
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU

-- | gets label of a NA Leaf
emptyLabel :: Label
emptyLabel = stringToLabel ""

-- | gets label of a leaf
labelLeaf :: (ByteArrayAccess key, ByteArrayAccess value)
          => key -> value -> Label
labelLeaf k v =
  hashFinalize $ flip hashUpdate v
               $ flip hashUpdate k
               $ hashUpdate hashInit (B.singleton 0)

-- | combines two labels
labelCombine :: Height -> Label -> Label -> Label
labelCombine h l r =
  hashFinalize $ flip hashUpdates     [l, r]
               $ hashUpdates hashInit [B.singleton 1, B.singleton (fromIntegral h)]

-- | converts string to a label
stringToLabel :: String -> Label
stringToLabel str = hashFinalize $ hashUpdate hashInit (BU.fromString str)