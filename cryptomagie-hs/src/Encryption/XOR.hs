module Encryption.XOR (encrypt, encrypt') where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BT (pack, unpack, map)
import Data.Word (Word8)

encrypt :: ByteString -> ByteString -> ByteString
encrypt k
  = BT.pack
  . zipWith xor (cycle $ BT.unpack k)
  . BT.unpack

encrypt' :: Word8 -> ByteString -> ByteString
encrypt' k = BT.map $ xor k
