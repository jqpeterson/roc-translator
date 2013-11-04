module Protocol.ROC.FloatSpec (main,spec) where
import Test.Hspec

import Protocol.ROC.Float





{-|

Sample coding for 100  IEEE764 Std
0x42C80000 = 01000010 11001000 00000000 00000000


Sample coding for 100 RocFloat Std
0x0000C842= 00000000 00000000 11001000 01000010

|-}

main :: IO ()
main = hspec spec


-- 1.00000000000000000000000
spec :: Spec
spec = do 
  describe "calculateExponentBits" $ do
    it "should change 0x85 into -17" $ do
      (calculateExponentBits 0x85) == (-17) `shouldBe` True
  describe "test xBytesMask" $ do
    it "should grab all exponent Bits from a RockFloat and return them" $ do
      testXBytesMask `shouldBe` True


