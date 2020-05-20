module Analysis (byteFrequencyMap, englishFrequencyMap) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BT (unpack, sort, length)
import Data.Heap (Heap)
import qualified Data.Heap as H (fromList)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (insertWith, empty, toList, fromList)
import Data.Word (Word8)

-- TODO: use heap
englishFrequencyMap :: Map Char Float
englishFrequencyMap = M.fromList
  [ ('e', 0.12702)
  , ('t', 0.09056)
  , ('a', 0.08167)
  , ('o', 0.07507)
  , ('i', 0.06966)
  , ('n', 0.06749)
  , ('s', 0.06327)
  , ('h', 0.06094)
  , ('r', 0.05987)
  , ('d', 0.04253)
  , ('l', 0.04025)
  , ('c', 0.02782)
  , ('u', 0.02758)
  , ('m', 0.02406)
  , ('w', 0.02360)
  , ('f', 0.02228)
  , ('g', 0.02015)
  , ('y', 0.01974)
  , ('p', 0.01929)
  , ('b', 0.01492)
  , ('v', 0.00978)
  , ('k', 0.00772)
  , ('j', 0.00153)
  , ('x', 0.00150)
  , ('q', 0.00095)
  , ('z', 0.00074)
  ]

byteFrequencyMap :: ByteString -> Map Word8 Float
byteFrequencyMap xs
  = foldr (\v acc -> M.insertWith update v 0.0 acc) M.empty
  $ BT.unpack xs
  where inc = 1.0 / (fromIntegral $ BT.length xs)
        update = (\previous _ -> previous + inc)
