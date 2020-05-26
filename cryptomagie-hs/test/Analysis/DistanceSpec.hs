module Analysis.DistanceSpec (spec) where

import Test.Hspec

import Analysis.Distance (hammingDistance)
import Util (utf8)

spec :: Spec
spec = do
  describe "Hamming distance" $ do
    it "should return correct distance" $ do
      let a = utf8 "this is a test"
      let b = utf8 "wokka wokka!!!"
      let expected = 37
      let output = hammingDistance a b
      output `shouldBe` expected
