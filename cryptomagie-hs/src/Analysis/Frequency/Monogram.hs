module Analysis.Frequency.Monogram (distanceFromEnglish) where

import Data.Array.IArray (Array, listArray, (!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BT (unpack, length)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (insertWith, empty, fromList, toList, findWithDefault)
import Data.Word (Word8)

import Util
  ( isWhitespace
  , isPrintable
  , isLowercase
  , isUppercase
  )

distance :: Num a => a -> a -> a
distance a = abs . (-) a

-- ideally we'd have a full printableAsciiFrequencyMap
englishFrequencyMap :: Array Word8 Float
englishFrequencyMap = listArray (0, 26)
  [ 0.0855, 0.0160, 0.0316, 0.0387, 0.1210
  , 0.0218, 0.0209, 0.0496, 0.0733, 0.0022
  , 0.0081, 0.0421, 0.0253, 0.0717, 0.0747
  , 0.0207, 0.0010, 0.0633, 0.0673, 0.0894
  , 0.0268, 0.0106, 0.0183, 0.0019, 0.0172
  , 0.0011
  ]

byteFrequencyMap :: ByteString -> Map Word8 Float
byteFrequencyMap xs
  = foldr (\v acc -> M.insertWith update v inc acc) M.empty
  $ BT.unpack xs
  where inc = 1.0 / (fromIntegral $ BT.length xs)
        update = (\previous _ -> previous + inc)

getEnglishFrequency :: Word8 -> Maybe Float
getEnglishFrequency c
  | isUppercase c = f $ c - 0x41
  | isLowercase c = f $ c - 0x61
  | isWhitespace c || isPrintable c = Just 0
  | otherwise = Nothing
  where f = Just . (!) englishFrequencyMap

distanceFromEnglish :: ByteString -> Maybe Float
distanceFromEnglish
  = fmap sum
  . mapM freqDistance
  . M.toList
  . byteFrequencyMap
  where freqDistance (c, freq) = distance freq <$> getEnglishFrequency c
