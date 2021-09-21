module Analysis.Frequency.Bigram (bigramScore) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BT (unpack, length)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
  ( insertWith
  , empty
  , fromList
  , toList
  , findWithDefault
  )

bigramTable :: Map (Char, Char) Float
bigramTable = M.fromList
  $ map (\((a:b:_), s) -> ((a, b), s))
  [ ("TH", 0.0271), ("HE", 0.0233), ("IN", 0.0203)
  , ("ER", 0.0178), ("AN", 0.0161), ("RE", 0.0141)
  , ("ES", 0.0132), ("ON", 0.0132), ("ST", 0.0125)
  , ("NT", 0.0117), ("EN", 0.0113), ("AT", 0.0112)
  , ("ED", 0.0108), ("ND", 0.0107), ("TO", 0.0107)
  , ("OR", 0.0106), ("EA", 0.0100), ("TI", 0.0099)
  , ("AR", 0.0098), ("TE", 0.0098), ("NG", 0.0089)
  , ("AL", 0.0088), ("IT", 0.0088), ("AS", 0.0087)
  , ("IS", 0.0086), ("HA", 0.0083), ("ET", 0.0076)
  , ("SE", 0.0073), ("OU", 0.0072), ("OF", 0.0071)
  ]

bigrams :: String -> [(Char, Char)]
bigrams (a:xs@(b:_)) = (a, b) : bigrams xs
bigrams _ = []

bigramScore :: (Char, Char) -> Float
bigramScore x = M.findWithDefault 0 x bigramTable

bigramFrequencyMap :: String -> Map (Char, Char) Float
bigramFrequencyMap xs
  = foldr (\v acc -> M.insertWith update v inc acc) M.empty
  $ bigrams xs
  where inc = 1.0 / (fromIntegral $ length xs)
        update = (\previous _ -> previous + inc)
