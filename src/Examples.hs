module Examples where

import Lib
import AVLTree
import Crypto.Hash
import qualified Data.ByteString.UTF8 as BU

label5 = stringToLabel "5"
label0 = stringToLabel "0"

bytes1 = BU.fromString "123123"
bytes2 = BU.fromString "665432"
