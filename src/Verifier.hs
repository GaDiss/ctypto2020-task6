module Verifier
  (
    verifier
  ) where

import Lib
import AVLTree
import Data.ByteArray (ByteArrayAccess)

-- TODO
verifier :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
         => RootDigest
         -> (Operation key value)
         -> [(ProofNode key value)]
          -> (Result, RootDigest, Maybe value)

verifier d _ _ = (Reject, d, Nothing)
