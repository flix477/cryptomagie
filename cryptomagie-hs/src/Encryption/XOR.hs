module Encryption.XOR (encrypt) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BT (pack, unpack, zipWith)
import Data.Tuple (uncurry)
import Data.Word (Word8)

encrypt :: ByteString -> ByteString -> ByteString
encrypt k
  = BT.pack
  . zipWith xor (cycle $ BT.unpack k)
  . BT.unpack
