module Encoding.XOR (encode) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BT (pack, zip)
import Data.Tuple (uncurry)
import Data.Word (Word8)

encode :: ByteString -> ByteString -> ByteString
encode a
  = BT.pack
  . map (uncurry xor)
  . BT.zip a
