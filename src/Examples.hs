module Examples where

import AVLTree
import Hashing
import Lib

import Crypto.Hash
import qualified Data.ByteString.UTF8 as BU

label5 = stringToLabel "5"
label0 = stringToLabel "0"

bytes1 = BU.fromString "1"
bytes2 = BU.fromString "2"
bytes3 = BU.fromString "3"
bytes4 = BU.fromString "4"
bytes5 = BU.fromString "5"
bytesSas = BU.fromString "sas"
bytesLol = BU.fromString "lol"
bytesKek = BU.fromString "kek"
bytesFoo = BU.fromString "foo"

smallTree = (AVLTree.fromList [(bytes1, bytesSas), (bytes2, bytesKek), (bytes3, bytesLol), (bytes4, bytesFoo)])