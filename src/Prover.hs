module Prover
  (
    prover
  ) where

import Lib
import AVLTree
import Data.ByteArray (ByteArrayAccess)

-- TODO
prover :: (Ord key, ByteArrayAccess key, ByteArrayAccess value)
       => (AVLTree key value)
       -> (Operation key value)
       -> ([(ProofNode key value)], (AVLTree key value), Maybe value)
prover t _ = ([], t, Nothing)
