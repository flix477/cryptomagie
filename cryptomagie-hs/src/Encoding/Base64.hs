module Encoding.Base64
  ( encode
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.ByteString (ByteString, unpack, pack)
import Data.Char (chr)
import Data.Array.IArray (Array, listArray, (!))
import Data.Word (Word8)
import Data.List.Split (chunksOf)

charTable :: Array Word8 Char
charTable = listArray (0, 63)
  [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
  , 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T'
  , 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd'
  , 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'
  , 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x'
  , 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7'
  , '8', '9', '+', '/'
  ]

-- yikes
sextets :: [Word8] -> [Word8]
sextets (a:b:c:_) =
    [ (a .&. 0xFC) `shiftR` 2
    , ((a .&. 0x3) `shiftL` 4) .|. ((b .&. 0xF0) `shiftR` 4)
    , ((b .&. 0xF) `shiftL` 2) .|. ((c .&. 0xC0) `shiftR` 6)
    , c .&. 0x3F
    ]
sextets (a:b:_) =
    [ (a .&. 0xFC) `shiftR` 2
    , ((a .&. 0x3) `shiftL` 4) .|. ((b .&. 0xF0) `shiftR` 4)
    , (b .&. 0xF) `shiftL` 2
    ]
sextets (a:_) =
  [ (a .&. 0xFC) `shiftR` 2
  , (a .&. 0x3) `shiftL` 4
  ]
sextets _ = []

-- yikes
encodeChunk :: [Word8] -> String
encodeChunk [] = ""
encodeChunk xs
  | (_:_:_:_) <- xs = enc
  | (_:_:_) <- xs = enc <> "="
  | (_:_) <- xs = enc <> "=="
  where enc = map ((!) charTable) $ sextets xs

encode :: ByteString -> String
encode
  = concatMap encodeChunk
  . chunksOf 3
  . unpack
