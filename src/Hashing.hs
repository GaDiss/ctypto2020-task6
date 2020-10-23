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
emptyLabel = stringToLabel ""

-- | gets label of a leaf
labelLeaf :: (ByteArrayAccess key, ByteArrayAccess value)
          => key -> value -> Label
labelLeaf k v =
  hashFinalize $ flip hashUpdate v
               $ flip hashUpdate k
               $ flip hashUpdate (B.singleton 0)
               $ hashInit

-- | combines two labels
labelCombine :: Height -> Label -> Label -> Label
labelCombine h l r =
  hashFinalize $ flip hashUpdates     [l, r]
               $ hashUpdates hashInit [B.singleton 1, B.singleton (fromIntegral h)]

-- | converts string to a label
stringToLabel :: String -> Label
stringToLabel str = hashFinalize $ flip hashUpdate (BU.fromString str)
                                 $ hashInit