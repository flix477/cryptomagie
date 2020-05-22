module Analysis (distanceFromEnglish, byteFrequencyMap, findBestSingleByteXOR, findBestSingleByteXOR', singleByteXORPossibilities, getEnglishFrequency) where

import Data.Array.IArray (Array, listArray, (!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BT (unpack, length)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (insertWith, empty, toList)
import Data.Maybe (mapMaybe)
import Data.Word (Word8)

import qualified Encryption.XOR as XOR (encrypt')

-- 🦀 readability is gone 🦀
(<?) :: Ord a => a -> (a, a) -> Bool
(<?) a (l, u) = l <= a && a <= u

trd :: (a,b,c) -> c
trd (_,_,x) = x

maybeList :: [a] -> Maybe [a]
maybeList [] = Nothing
maybeList xs = Just xs

isSpace :: Word8 -> Bool
isSpace c = c == 0x9 || c == 0xA

distance :: Num a => a -> a -> a
distance a = abs . (-) a

-- ideally we'd have a full printableAsciiFrequencyMap
englishFrequencyMap :: Array Word8 Float
englishFrequencyMap = listArray (0, 26)
  [ 0.08167, 0.01492, 0.02782, 0.04253, 0.12702
  , 0.02228, 0.02015, 0.06094, 0.06966, 0.00153
  , 0.00772, 0.04025, 0.02406, 0.06749, 0.07507
  , 0.01929, 0.00095, 0.05987, 0.06327, 0.09056
  , 0.02758, 0.00978, 0.02360, 0.00150, 0.01974
  , 0.00074
  ]

byteFrequencyMap :: ByteString -> Map Word8 Float
byteFrequencyMap xs
  = foldr (\v acc -> M.insertWith update v inc acc) M.empty
  $ BT.unpack xs
  where inc = 1.0 / (fromIntegral $ BT.length xs)
        update = (\previous _ -> previous + inc)

singleByteXORPossibilities :: ByteString -> [(Word8, ByteString)]
singleByteXORPossibilities
  = zipWith enc [0..0xFF]
  . repeat
  where enc = (\i b -> (i, XOR.encrypt' i b))

getEnglishFrequency :: Word8 -> Maybe Float
getEnglishFrequency c
  | c <? (0x41, 0x5A) = f $ c - 0x41
  | c <? (0x61, 0x7A) = f $ c - 0x61
  | isSpace c || c <? (0x20, 0x7E) = Just 0
  | otherwise = Nothing
  where f = Just . (!) englishFrequencyMap

distanceFromEnglish :: ByteString -> Maybe Float
distanceFromEnglish
  = fmap sum
  . mapM freqDistance
  . M.toList
  . byteFrequencyMap
  where freqDistance (c, freq) = distance freq <$> getEnglishFrequency c

-- TODO: switch out that tuple for a custom data type
findBestSingleByteXOR :: ByteString -> Maybe (Word8, ByteString, Float)
findBestSingleByteXOR
  = fmap (minimumBy (compare `on` trd))
  . maybeList
  . mapMaybe (\(a, b) -> (,,) a b <$> distanceFromEnglish b)
  . singleByteXORPossibilities

findBestSingleByteXOR' :: [ByteString] -> Maybe (Word8, ByteString, Float)
findBestSingleByteXOR'
  = fmap (minimumBy (compare `on` trd))
  . maybeList
  . mapMaybe findBestSingleByteXOR
