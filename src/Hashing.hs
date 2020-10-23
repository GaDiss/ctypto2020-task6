module Hashing
  (
    emptyLabel,
    labelLeaf,
    labelCombine
  ) where

import Lib
import Crypto.Hash
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteString as B

emptyLabel = stringToLabel ""

labelLeaf :: (ByteArrayAccess key, ByteArrayAccess value)
          => key -> value -> Label
labelLeaf k v =
  hashFinalize $ flip hashUpdate v
               $ flip hashUpdate k
               $ flip hashUpdate (B.singleton 0)
               $ hashInit

labelCombine :: Height -> Label -> Label -> Label
labelCombine h l r =
  hashFinalize $ flip hashUpdates     [l, r]
               $ hashUpdates hashInit [B.singleton 1, B.singleton (fromIntegral h)]