module Analysis.XOR
  ( findByteKey
  , findByteKey'
  , findRepeatingKey
  , findRepeatingKeyWithLength
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BT (pack, splitAt, unpack, transpose, length, take)
import Data.List (minimumBy, sortBy)
import Data.List.Split (chunksOf)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Word (Word8)

import Analysis.Frequency.Monogram (distanceFromEnglish)
import Analysis.Distance (normalizedHammingDistance)
import qualified Encryption.XOR as XOR (encrypt')
import Util (maybeList, fst3, trd)

bytePossibilities :: ByteString -> [(Word8, ByteString)]
bytePossibilities
  = zipWith enc [0..0xFF]
  . repeat
  where enc = (\i b -> (i, XOR.encrypt' i b))

-- TODO: switch out that tuple for a custom data type
findByteKey :: ByteString -> Maybe (Word8, ByteString, Float)
findByteKey
  = fmap (minimumBy (compare `on` trd))
  . maybeList
  . mapMaybe (\(a, b) -> (,,) a b <$> distanceFromEnglish b)
  . bytePossibilities

findByteKey' :: [ByteString] -> Maybe (Word8, ByteString, Float)
findByteKey'
  = fmap (minimumBy (compare `on` trd))
  . maybeList
  . mapMaybe findByteKey

keySampleDistance :: Int -> ByteString -> Float
keySampleDistance n
  = uncurry normalizedHammingDistance
  . BT.splitAt n
  . BT.take (n * 2)

lengthRange :: ByteString -> [Int]
lengthRange xs = [2..(min n 40)]
  where n = round $ (fromIntegral $ BT.length xs) / 2

findKeyLengths :: ByteString -> [(Int, Float)]
findKeyLengths xs
  = sortBy (compare `on` snd)
  . filter (not . isNaN . snd)
  . map (\l -> (,) l $ keySampleDistance l xs)
  $ lengthRange xs

findRepeatingKeyWithLength :: Int -> ByteString -> Maybe ByteString
findRepeatingKeyWithLength n
  = fmap BT.pack
  . fmap (map fst3)
  . mapM findByteKey
  . BT.transpose
  . map BT.pack -- yikes
  . chunksOf n
  . BT.unpack

findRepeatingKey :: ByteString -> [ByteString]
findRepeatingKey xs
  = mapMaybe (uncurry findRepeatingKeyWithLength)
  . zip ns
  $ repeat xs
  where ns = map fst $ findKeyLengths xs
