import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Foldable (for_)
import Test.Hspec

import qualified Analysis (findBestSingleByteXOR)
import qualified Encoding.Base64 as Base64 (encode)
import qualified Encoding.Hex as Hex (decode, encode)
import qualified Encryption.XOR as XOR (encrypt)

main :: IO ()
main = hspec $ do
  describe "Set 1" $ do
    describe "Challenge 1" $ do
      it "should return the proper base64" $ do
        let input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        let expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
        let output = Base64.encode $ Hex.decode input
        output `shouldBe` expected
    describe "Challenge 2" $ do
      it "should return the proper XOR'd string" $ do
        let input = Hex.decode "1c0111001f010100061a024b53535009181c"
        let key = Hex.decode "686974207468652062756c6c277320657965"
        let expected = "746865206b696420646f6e277420706c6179"
        let output = Hex.encode $ XOR.encrypt input key
        output `shouldBe` expected
    describe "Challenge 3" $ do
      it "should return the right XOR key and message" $ do
        let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
        let expectedKey = 88
        let expectedMsg =  "Cooking MC's like a pound of bacon"
        let output = Analysis.findBestSingleByteXOR $ Hex.decode input
        output `shouldBe` (expectedKey, encodeUtf8 $ pack expectedMsg)
