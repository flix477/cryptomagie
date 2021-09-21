module Util
  ( flipTuple
  , (<?)
  , isWhitespace
  , isPrintable
  , isLowercase
  , isUppercase
  , fst3
  , trd
  , maybeList
  ) where

import Data.Word (Word8)

flipTuple :: (a, b) -> (b, a)
flipTuple (a, b) = (b, a)

-- 🦀 readability is gone 🦀
(<?) :: Ord a => a -> (a, a) -> Bool
(<?) a (l, u) = l <= a && a <= u

isWhitespace :: Word8 -> Bool
isWhitespace c = c == 0x9 || c == 0xA || c == 0xD

isPrintable :: Word8 -> Bool
isPrintable c = c <? (0x20, 0x7E)

isUppercase :: Word8 -> Bool
isUppercase c = c <? (0x41, 0x5A)

isLowercase :: Word8 -> Bool
isLowercase c = c <? (0x61, 0x7A)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

trd :: (a,b,c) -> c
trd (_,_,x) = x

maybeList :: [a] -> Maybe [a]
maybeList [] = Nothing
maybeList xs = Just xs

