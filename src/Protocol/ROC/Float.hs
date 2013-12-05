{-# LANGUAGE CPP, OverloadedStrings, MagicHash, RankNTypes, BangPatterns  #-}

module Protocol.ROC.Float where
import Data.Word
import Data.Binary
-- import Data.Bool
import Data.Int
import Data.Bits
import Data.Binary.Get
-- import Numeric
-- import Control.Applicative
import Debug.Trace

{-| 

STD IEEE FORMAT:
 s  e  e  e  e  e  e  e |e   m  m  m  m  m  m  m   m  m  m  m  m  m  m  m   m  m  m  m  m  m  m  m
31 30 29 28 27 26 25 24 |23 22 21 20 19 18 17 16 |15 14 13 12 11 10 09 08 |07 06 05 04 03 02 01 00 
  MSB                       MSB -1                    LSB + 1                     LSB


 
Where: MSB = most significant byte 
       LSB = least significant byte 

However, in the ROC protocol, the bytes of each floating-point number 
are returned in the following order: 
 m  m  m  m  m  m  m  m | m  m  m  m  m  m  m  m | e  m  m  m  m  m  m  m | s  e  e  e  e  e  e  e
07 06 05 04 03 02 01 00 |15 14 13 12 11 10 09 08 |23 22 21 20 19 18 17 16 |31 30 29 28 27 26 25 24 -> Natural Encoding 
31 30 29 28 27 26 25 24 |23 22 21 20 19 18 17 16 |15 14 13 12 11 10 09 08 |07 06 05 04 03 02 01 00 -> Real Encoding
     LSB                        LSB + 1                MSB -1                    MSB




x0 => Real Encoding =>   mmmmmmmm | mmmmmmmm | emmmmmmm | seeeeeee  ByteString
e1 =  0X00,0x87 &  x0     =>   00000000 | 00000000 | e0000000 | 0eeeeeee  
m1 =  0xFF,0x08 &  x0     =>   mmmmmmmm | mmmmmmmm | 0mmmmmmm | s0000000



|-}

{- |Oooh kay so the mantessa is interpreted as 1.mmmmmmmmmmm...  the 1 is implied and has to be ored in the previous step
     But the actual number is 1mmmmmmmmm... so there is an implicit 1mmmmmmmmmm... * 2^(-23) which pulls it left.
     However we address that by adjusting e, so e is unsigned (non 1's compliment) but needs to be read as signed so we subtract
     127, then we shift it right 23 buts  so for the number 100 which coes in STD IEEE764 to :
     0x42C80000 = 01000010 11001000 00000000 00000000 , we have the exp = 10000101b - (0x7F+0x17) = -0x11
     our mantessa is applied by grabbing the raw bits and taching on the sign and the 1's bit...
    
     mant = 0100 1000 0000 0000 0000 0000 .|. 0x800000 = 15625  
            ^implied
 |-}


exponentPart :: Int16
exponentPart = 0x0085


mantessaPart :: Word32
mantessaPart = 0x00C80000



-- | in m * 2^(e) m are the M bits
-- Because there are signs on the Coefficient bits
reorderMbits :: Word32 -> Int32
reorderMbits f = let m         = msBytesMask .&. f  -- mmmmmmmm | mmmmmmmm | 0mmmmmmm | s0000000
                     mHighBits = byte1Mask .&. m    -- 00000000 | 00000000 | 0mmmmmmm | 00000000
                     mLowBits  = byte3Mask .&. m    -- mmmmmmmm | 00000000 | 00000000 | 00000000
                     mMidBits  = byte2Mask .&. m    -- 00000000 | mmmmmmmm | 00000000 | 00000000
                     mOrdered  = (lShiftByte mHighBits 2) .|. (rShiftByte mLowBits 2) .|. mMidBits -- 0mmmmmmm | mmmmmmmm | mmmmmmmm | 00000000
                     mPlaced   = rShiftByte mOrdered 1 ---- 00000000 | 0mmmmmmm | mmmmmmmm | mmmmmmmm 
                     sBit      = testBit m 7
                 in case sBit of  --Check if signed mantessa
                      True -> fromIntegral ((complement (mPlaced .|. 0x00800000)) + 0x00000001)
                      False -> fromIntegral (mPlaced .|. 0x00800000)
                     

-- | in m * 2^(e) e are the Exp bits                        
reorderExpBits :: Word32 -> Int32
reorderExpBits f = let e           = xBytesMask .&.f
                       expHighBits = byte0Mask .&. e -- => 00000000 | 00000000 | 00000000 | 0eeeeeee  
                       expLowBits  = byte1Mask .&. e -- => 00000000 | 00000000 | e0000000 | 00000000
                       swapBytes   = (rShiftByte expLowBits 1) .|. (lShiftByte expHighBits 1) -- => 00000000 | 00000000 | 0eeeeeee | e0000000 
                       placeBytes  = fromIntegral (rShift swapBytes 7)  -- => 00000000 | 000000000 | 00000000 | eeeeeeee
                   in  placeBytes 


calculateExponentBits :: (Integral a )=> a-> Int32
calculateExponentBits x = (fromIntegral x) - (127+23)

-- | The mantessa is traditionally represented as a decimal number
-- in formulae but that makes no sense in fixed point rep. 
-- the 1.xxxx the mantessa is 23 bits of xxx  
-- the 1 is tached on to the front as the 24th bit but it isn't the sign
-- that is why the sign has to be coded separately

calculateMantessaBit :: (Integral a) => a -> Int32 
calculateMantessaBit x = (fromIntegral x) -- `xor`0x00800000

byte3Mask :: Word32 
byte3Mask   = 0xFF000000

byte2Mask :: Word32 
byte2Mask   = 0x00FF0000

byte1Mask :: Word32
byte1Mask   = 0x0000FF00

byte0Mask :: Word32
byte0Mask   = 0x000000FF


-- |Eponent Bytes
xBytesMask :: Word32
xBytesMask  = 0x0000807F

testXBytesMask :: Bool
testXBytesMask = (xBytesMask .&. 0xFFFFFFFF ) == 0x0000807F
-- |Mantessa Bytes

msBytesMask :: Word32 
msBytesMask = 0xFFFF7F80 




rocFloatToFloat :: Word32 -> Float
rocFloatToFloat f = let mantessa = fromIntegral (calculateMantessaBit.reorderMbits $ f )  
                        e = fromIntegral (calculateExponentBits.reorderExpBits $ f)
                    in encodeFloat mantessa e 

  
rShiftByte :: Data.Bits.Bits a => a -> Int -> a
rShiftByte x n = shiftR x (8*n)

lShiftByte :: Data.Bits.Bits a => a -> Int -> a
lShiftByte x n = shiftL x (8*n)

rShift :: Data.Bits.Bits a => a -> Int -> a
rShift x n = shiftR x n 
lShift :: Data.Bits.Bits a => a -> Int -> a
lShift x n = shiftL x n

getIeeeFloat32 :: Get Float
getIeeeFloat32 = do 
  raw <- getWord32be
  return $ rocFloatToFloat raw

  

