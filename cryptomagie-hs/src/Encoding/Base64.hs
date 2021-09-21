module Encoding.Base64
  ( encode
  , decode
  ) where

import Data.Array.IArray (Array, listArray, (!), assocs)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString, unpack, pack)
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (fromList, lookup)
import Data.Word (Word8)

import Util (flipTuple)

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

reverseTable :: Map Char Word8
reverseTable
  = M.fromList
  . map flipTuple
  $ assocs charTable

decodeChar :: Char -> Maybe Word8
decodeChar c = M.lookup c reverseTable

-- yikes
decodeChunk :: [Char] -> Maybe [Word8]
decodeChunk (a:b:'=':'=':_) = do
  a1 <- decodeChar a
  b1 <- decodeChar b
  pure [ (a1 `shiftL` 2) .|. ((b1 .&. 0x30) `shiftR` 4) ]
decodeChunk (a:b:c:'=':_) = do
  a1 <- decodeChar a
  b1 <- decodeChar b
  c1 <- decodeChar c
  pure
    [ (a1 `shiftL` 2) .|. ((b1 .&. 0x30) `shiftR` 4)
    , ((b1 .&. 0xF) `shiftL` 4) .|. ((c1 .&. 0x3C) `shiftR` 2)
    ]
decodeChunk (a:b:c:d:_) = do
  a1 <- decodeChar a
  b1 <- decodeChar b
  c1 <- decodeChar c
  d1 <- decodeChar d
  pure
    [ (a1 `shiftL` 2) .|. ((b1 .&. 0x30) `shiftR` 4)
    , ((b1 .&. 0xF) `shiftL` 4) .|. ((c1 .&. 0x3C) `shiftR` 2)
    , ((c1 .&. 0x3) `shiftL` 6) .|. d1
    ]
decodeChunk _ = Nothing

decode :: String -> Maybe ByteString
decode
  = fmap pack
  . fmap concat
  . mapM decodeChunk
  . chunksOf 4
