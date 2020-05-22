import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BT (putStrLn)
import Data.Foldable (for_)
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec

import qualified Analysis (findBestSingleByteXOR, findBestSingleByteXOR')
import qualified Encoding.Base64 as Base64 (encode)
import qualified Encoding.Hex as Hex (decode, encode)
import qualified Encryption.XOR as XOR (encrypt)

utf8 :: String -> ByteString
utf8 = encodeUtf8 . pack

main :: IO ()
main = hspec $ do
  describe "Set 1" $ do
    describe "Challenge 1 - Convert hex to base64" $ do
      it "should return the proper base64" $ do
        let input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        let expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
        let output = Base64.encode $ Hex.decode input
        output `shouldBe` expected

    describe "Challenge 2 - Fixed XOR" $ do
      it "should return the proper XOR'd string" $ do
        let input = Hex.decode "1c0111001f010100061a024b53535009181c"
        let key = Hex.decode "686974207468652062756c6c277320657965"
        let expected = "746865206b696420646f6e277420706c6179"
        let output = Hex.encode $ XOR.encrypt key input
        output `shouldBe` expected

    describe "Challenge 3 - Single-byte XOR cipher" $ do
      it "should return the right XOR key and message" $ do
        let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
        let expectedKey = 88
        let expectedMsg =  "Cooking MC's like a pound of bacon"
        let Just (key, msg, _) = Analysis.findBestSingleByteXOR $ Hex.decode input
        key `shouldBe` expectedKey
        msg `shouldBe` (utf8 expectedMsg)

    describe "Challenge 4 - Detect single-character XOR" $ do
      it "should find the correct single byte XOR'd line" $ do
        f <- readFile "./test/challenge4.txt"
        let Just (key, msg, _) = Analysis.findBestSingleByteXOR' $ map Hex.decode $ lines f
        let expectedKey = 53
        let expectedMsg = "Now that the party is jumping\n"
        key `shouldBe` expectedKey
        msg `shouldBe` (utf8 expectedMsg)

    describe "Challenge 5 - Implement repeating-key XOR" $ do
      it "should encrypt the input correctly" $ do
        let input = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
        let key = "ICE"
        let expected = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
        let output = Hex.encode $ XOR.encrypt (utf8 key) (utf8 input)
        output `shouldBe` expected
