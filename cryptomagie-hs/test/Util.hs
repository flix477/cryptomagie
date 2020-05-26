module Util (utf8) where

import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)

utf8 :: String -> ByteString
utf8 = encodeUtf8 . pack
