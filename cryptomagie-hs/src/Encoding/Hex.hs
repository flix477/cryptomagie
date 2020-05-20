module Encoding.Hex (encode, decode) where

import Data.Array.IArray (Array, listArray, (!), assocs)
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BT (pack, unpack, empty)
import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, toList)
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import Data.Tuple (fst)
import Data.Word (Word8)

charTable :: Array Word8 Char
charTable = listArray (0, 15)
  [ '0', '1', '2', '3', '4', '5', '6', '7'
  , '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
  ]

nibbles :: Word8 -> (Word8, Word8)
nibbles x =
  ((x .&. 0xF0) `shiftR` 4
  , x .&. 0xF
  )

encodeNibble :: Word8 -> Char
encodeNibble = (!) charTable

encode :: ByteString -> String
encode
  = map encodeNibble
  . concatMap (\(a, b) -> [a, b])
  . map (nibbles)
  . BT.unpack

decodeNibble :: Char -> Word8
decodeNibble c
  = fst
  $ fromJust
  $ find (\(i, x) -> x == c)
  $ assocs charTable

decodeByte :: NonEmpty Char -> Word8
decodeByte (a:|b:_) =
  let nibble1 = decodeNibble a
      nibble2 = decodeNibble b
  in (nibble1 `shiftL` 4) .|. nibble2
decodeByte (a:|_) = decodeNibble a

decode :: String -> ByteString
decode
  = BT.pack
  . map decodeChunk
  . chunksOf 2
  where decodeChunk = decodeByte . fromJust . nonEmpty
