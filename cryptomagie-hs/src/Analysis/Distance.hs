module Analysis.Distance (hammingDistance, normalizedHammingDistance) where

import Data.Bits (xor, popCount)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BT (zip, length)
import Data.Word (Word8)

hammingDistance' :: Word8 -> Word8 -> Int
hammingDistance' a = popCount . xor a

hammingDistance :: ByteString -> ByteString -> Int
hammingDistance a
  = sum
  . map (uncurry hammingDistance')
  . BT.zip a

normalizedHammingDistance :: ByteString -> ByteString -> Float
normalizedHammingDistance a b = w / n
  where w = fromIntegral $ hammingDistance a b
        n = fromIntegral $ BT.length a
