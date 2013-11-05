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
  describe "calculateMantessaBit" $ do 
         it "should tach on a 1 or 0 at the point a mantessa can't exist, the 24th bit" $ do 
                                      (calculateMantessaBit 0x007FFF00  `shouldBe` 0x00FFFF00)
                                      (calculateMantessaBit 0x00FFFF00  `shouldBe` 0x007FFF00)
  describe "testxBytesMask" $ do
    it "should return the exponent bit mask" $ do
      testXBytesMask `shouldBe` True
  describe "reorderExpBits" $ do 
         it "should grab all the exponent bits and return them shifted right" $ do 
                                reorderExpBits 0x0000807F `shouldBe` 0x000000FF
                                reorderExpBits 0x0000C842   `shouldBe` 133

  describe "reorderMBits" $ do 
         it "should grab the mantessa bits and swap to order" $ do 
                              reorderMbits 0xFEFF7F80 `shouldBe` 0xFFFFFFFE
                              reorderMbits 0x0000C842 `shouldBe` 0x00480000
  describe "rocToFloat" $ do 
         it "should create a float based on the ROC encoded IEEE764 std" $ do 
                            (rocFloatToFloat 0x0000C842 == 100.0) `shouldBe` True
  
