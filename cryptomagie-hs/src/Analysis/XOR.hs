module Analysis.XOR
  ( findByteKey
  , findByteKey'
  , findRepeatingKey
  , findRepeatingKeyWithLength
  , findKeyLengths
  , lel
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BT (pack, splitAt, unpack, transpose)
import Data.List (minimumBy, sortBy)
import Data.List.Split (chunksOf)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Word (Word8)

import Analysis.Frequency (distanceFromEnglish)
import Analysis.Distance (normalizedHammingDistance)
import qualified Encryption.XOR as XOR (encrypt')

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

trd :: (a,b,c) -> c
trd (_,_,x) = x

maybeList :: [a] -> Maybe [a]
maybeList [] = Nothing
maybeList xs = Just xs

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

lengthRange :: [Int]
lengthRange = [2..40]

avg :: [Float] -> Float
avg xs = (sum xs) / n
  where n = fromIntegral $ length xs

lel :: Int -> ByteString -> Float
lel n
  = avg
  . map (uncurry normalizedHammingDistance . BT.splitAt n . BT.pack)
  . take 2
  . chunksOf n
  . BT.unpack

findKeyLengths :: ByteString -> [Int]
findKeyLengths
  = map fst
  . sortBy (compare `on` snd)
  . map (\(n, xs) -> (n, lel n xs))
  . zip lengthRange
  . repeat

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
  $ zip ns
  $ repeat xs
  where ns = take 3 $ findKeyLengths xs
