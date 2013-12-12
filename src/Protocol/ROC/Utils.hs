{-# LANGUAGE TupleSections, OverloadedStrings,TypeFamilies,MultiParamTypeClasses,FlexibleInstances  #-}

module Protocol.ROC.Utils where

import Control.Applicative
import Data.Binary
import Data.Int
import Data.Binary.Get
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LB
import Foreign.CRC
import Data.Time.Clock.POSIX

getTLP :: Get [Word8]
getTLP = do
  t <- getWord8
  l <- getWord8
  p <- getWord8
  let tlplist = ([t] ++ [l] ++ [p])    
  return $ tlplist

anyButNull :: Get Bool 
anyButNull = do 
  c <- getWord8
  return $ test c 
  where 
    test :: Word8 -> Bool 
    test x = (fromIntegral x) == 1

getInt16 :: Get Int16
getInt16 = do
  x <- getWord16le
  return $ fromIntegral x

getInt8 :: Get Int8
getInt8 = do  
  x <- getWord8
  return $ fromIntegral x

getTime :: Get [Word8]
getTime = do  
  second <- getWord8
  minute <- getWord8
  hour <- getWord8
  day <- getWord8
  month <- getWord8
  year <- getWord8
  let timeList = ([second] ++ [minute] ++ [hour] ++ [day] ++ [month] ++ [year])
  return $ timeList

getPosixTime :: Get POSIXTime
getPosixTime = do  
  x <- getWord32le
  return $ fromIntegral x
  
crcTestBS :: Word8 -> Bool
crcTestBS w 
  |w == 0 = True
  |otherwise = False

crcCheck :: BS.ByteString -> Bool
crcCheck x = let returnCRC = lzyBSto16BScrc $ pack8to16 $ BS.unpack $ x
            in BS.all crcTestBS returnCRC

bsEmpty :: BS.ByteString
bsEmpty = ""

-- | takes a lazy bytstring and returns the 16 bit crc as a LE bytestring

lzyBSto16BScrc :: LB.ByteString -> BS.ByteString
lzyBSto16BScrc lzyBS = LB.toStrict.toLazyByteString.word16LE.crcWord16List.makeWord16Listbe $ lzyBS

pack8to16 :: [Word8] -> LB.ByteString
pack8to16 wList = let cwList = fromIntegral <$> wList
                  in LB.concat $ map  (toLazyByteString.word16BE ) cwList

