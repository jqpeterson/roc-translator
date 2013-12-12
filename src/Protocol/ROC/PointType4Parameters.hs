{-# LANGUAGE TupleSections, OverloadedStrings,TypeFamilies,MultiParamTypeClasses,FlexibleInstances  #-}

module Protocol.ROC.PointType4Parameters where
       
import qualified Data.ByteString as BS       
import qualified Data.ByteString.Lazy as LB
import Data.Int
import Data.Word
import Control.Applicative
import Data.ByteString.Builder


-- newtype Test1 = Test1 { test1 :: Word8 }
-- newtype Test2 = Test2 { test2 :: Bool }
-- newtype Test3 = Test3 { test3 :: Float }

word8parameters :: [Int]
word8parameters = [1,2,3,4]

boolparameters :: [Int]
boolparameters = [5,6,7,8]

floatparameters :: [Int]
floatparameters = [9,10,11,12]


mkPossibleInput :: Int ->

data PossibleInput = PI1 Word8 | PI2 Bool | PI3 Float                                             
                                             
--type CFG = Int


setParamType :: (RocSerialize a) => Int -> a -> Either String BS.ByteString
setParamType si pi  
  | elem si word8parameters  = case pi of
                                 PI1 k -> Right (word8BS k)
                                 _             -> Left "wrong input type expecting Word8"
  | elem si boolparameters   = case pi of
                                 PI2 k -> Right (boolBS k)
                                 _             -> Left "wrong input type expecting Boolean"
  | elem si floatparameters  = case pi of
                                 PI3 k -> Right (floatBS k)
                                 _             -> Left "wrong input type expecting Float"  

word8BS :: Word8 -> BS.ByteString
word8BS x = BS.singleton x

-- make16ByteString :: Word16 -> BS.ByteString
-- make16ByteString x = LB.toStrict.toLazyByteString.word16LE x

boolBS :: Bool -> BS.ByteString
boolBS x 
    | x == True = BS.singleton 0x01
    | otherwise = BS.singleton 0x00  
                  
floatBS :: Float -> BS.ByteString                  
floatBS x = (LB.toStrict.toLazyByteString.floatLE $ x)