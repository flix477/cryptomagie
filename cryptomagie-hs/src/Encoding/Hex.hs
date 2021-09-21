module Encoding.Hex (encode, decode) where

import Data.Array.IArray (Array, listArray, (!), assocs)
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BT (pack, unpack)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (fromList, lookup)
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import Data.Word (Word8)

import Util (flipTuple)

charTable :: Array Word8 Char
charTable = listArray (0, 15)
  [ '0', '1', '2', '3', '4', '5', '6', '7'
  , '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
  ]

reverseTable :: Map Char Word8
reverseTable
  = M.fromList
  . map flipTuple
  $ assocs charTable

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
decodeNibble c = fromJust $ M.lookup c reverseTable

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
