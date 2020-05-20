import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Foldable (for_)
import Test.Hspec

import qualified Encoding.Base64 as Base64 (encode)
import qualified Encoding.Hex as Hex (decode)

main :: IO ()
main = hspec $ do
  describe "Set 1" $ do
    describe "Challenge 1" $ do
      it "should return the proper base64" $ do
        let input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        let expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
        let output = Base64.encode $ Hex.decode input
        output `shouldBe` expected
