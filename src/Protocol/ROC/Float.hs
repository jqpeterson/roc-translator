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
                      True -> fromIntegral (0xF800 .|. mPlaced) 
                      False -> fromIntegral (mPlaced)
                     

-- | in m * 2^(e) e are the Exp bits                        
reorderExpBits :: Word32 -> Int32
reorderExpBits f = let e           = xBytesMask .&.f
                       expHighBits = byte1Mask .&. e -- => 00000000 | 00000000 | 00000000 | 0eeeeeee  
                       expLowBits  = byte2Mask .&. e -- => 00000000 | 00000000 | e0000000 | 00000000
                       swapBytes   = (rShiftByte expLowBits 1) .|. (lShiftByte expHighBits 1) -- => 00000000 | 00000000 | 0eeeeeee | e0000000 
                       placeBytes  = (rShift swapBytes 7)  -- => 00000000 | 000000000 | 00000000 | eeeeeeee
                   in case testBit placeBytes 7 of -- Check if signed Exponent
                        True -> fromIntegral $  0xFFF0 .|. placeBytes 
                        False -> fromIntegral placeBytes

byte3Mask :: Word32
byte3Mask   = 0xF000 

byte2Mask :: Word32
byte2Mask   = 0x0F00 

byte1Mask :: Word32
byte1Mask   = 0x00F0 

byte0Mask :: Word32
byte0Mask   = 0x000F 

xBytesMask :: Word32
xBytesMask  = 0x0087

msBytesMask :: Word32 
msBytesMask = 0xFF08 


rocFloatToFloat :: Word32 -> Float
rocFloatToFloat f = let mantessa = fromIntegral (reorderMbits f )
                        e = fromIntegral (reorderExpBits f)
                    in  encodeFloat e mantessa
  
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
  raw <- getWord32host
  return $ rocFloatToFloat raw
  
