{-# LANGUAGE RankNTypes, OverloadedStrings, BangPatterns #-}
module Protocol.ROC where

import Control.Applicative
import Control.Monad
import System.IO
import System.Hardware.Serialport
import Data.Bits
import Numeric
import Data.Word
import qualified Data.ByteString as BS
import Foreign.CRC
import qualified Data.ByteString.Lazy as LB
import Control.Monad.IO.Class
import Data.ByteString.Builder
import Control.Lens
import Control.Lens.Lens
import Control.Lens.Getter
import Control.Monad.Trans.State.Strict
import Protocol.ROC.PointTypes

--crcTestBS :: Word8 -> Bool
--crcTestBS w 
--  |w == 0 = True
--  |otherwise = False


opCode0 port usr = do
  sendPort port (usr ++ [1,0,0,2,0,255])
  receivebs <- recievePort port (usr ++ [1,0,0,2,0,255])
  let wordList = BS.unpack receivebs
  print $ showInt <$> wordList <*> [""]

  let numberDI = fromEnum (BS.head (BS.drop 6 receivebs))
  let numberTDI = BS.head (BS.drop 7 receivebs) 
  let numberAI = BS.head (BS.drop 8 receivebs)
  let numberMeterRuns = BS.head (BS.drop 9 receivebs) 
  let numberPulseInputs = BS.head (BS.drop 10 receivebs)
  let numberPIDs =  BS.head (BS.drop 11 receivebs)
  let numberTanks =  BS.head (BS.drop 12 receivebs)
  let numberAO = BS.head (BS.drop 13 receivebs) 
  let numberTDO = BS.head (BS.drop 14 receivebs) 
  let numberDO = BS.head (BS.drop 15 receivebs) 
  let alarmPointerHigh = BS.head (BS.drop 16 receivebs)
  let alarmPointerLow = BS.head (BS.drop 17 receivebs)
  let eventPointer = BS.append (BS.singleton (BS.head $ BS.drop 18 receivebs)) (BS.singleton (BS.head $ BS.drop 19 receivebs))
  let diagnostic = BS.append (BS.append (BS.singleton (BS.head $ BS.drop 22 receivebs)) (BS.singleton (BS.head $ BS.drop 23 receivebs))) (BS.append (BS.singleton (BS.head $ BS.drop 24 receivebs)) (BS.singleton (BS.head $ BS.drop 25 receivebs)))


  print $ numberDI
  print $ numberTDI
  print $ numberAI
  print $ numberMeterRuns
  print $ numberPulseInputs
  print $ numberPIDs
  print $ numberTanks
  print $ numberAO
  print $ numberTDO
  print $ numberDO
  print $ alarmPointerHigh
  print $ alarmPointerLow
  print $ eventPointer
  print $ diagnostic 

opCode7 port usr = do
  sendPort port (usr ++ [1,0,7,0])
  receivebs <- recievePort port (usr ++ [1,0,7,0]) 
  print $ showInt <$> BS.unpack receivebs <*> [""]  
 
opCode17 port usr = do
  sendPort port (usr ++ [1,0,17,5,76,79,73,232,3])
  receivebs <- recievePort port (usr ++ [1,0,17,5,76,79,73,232,3])
  print $ showInt <$> BS.unpack receivebs <*> [""]
--  print $ BS.unpack.LB.toStrict.toLazyByteString.string7 $ username

opCode167 port usr = do
  sendPort port (usr ++ [1,0,167,4,1,18,23,0])
  receivebs <- recievePort port
    
  return $ fetchPointType receivebs
--  print $ showInt <$> BS.unpack receivebs <*> [""]

sendPort port str = do
  s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
  send s $ BS.append (BS.pack str)(lzyBSto16BScrc (pack8to16 $ str))  

recievePort port = do
  s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
  receivebs <- recvAllBytes s 255 
--  let returnCRC = lzyBSto16BScrc $ pack8to16 $ BS.unpack $ receivebs  
--  let pulledCRC = BS.reverse.BS.take 2 $ BS.reverse receivebs  
--  print $ BS.all crcTestBS returnCRC 
  closeSerial s
  return receivebs

bsEmpty :: BS.ByteString
bsEmpty = ""

-- | takes a lazy bytstring and returns the 16 bit crc as a LE bytestring

lzyBSto16BScrc :: LB.ByteString -> BS.ByteString
lzyBSto16BScrc lstring = LB.toStrict.toLazyByteString.word16LE.crcWord16List.makeWord16Listbe $ lstring

pack8to16 :: [Word8] -> LB.ByteString
pack8to16 wList = let cwList = fromIntegral <$> wList
                  in LB.concat $ map  (toLazyByteString.word16BE ) cwList



-- | continues fetching elements from some source until stpTst is true 
  

data PortBytes = PortBytes { pbByteString :: !BS.ByteString,
                             pbSerialPort :: !SerialPort,
                             pbByteRead   :: !Int
                           } 

-- _pbBytes :: Lens' (PortBytes) BS.ByteString 
_pbBytes :: (Functor f) => (BS.ByteString -> f BS.ByteString) -> PortBytes -> f (PortBytes)
_pbBytes fcn (PortBytes bs s i)  = fmap (\bs' -> PortBytes bs' s i) (fcn bs)


recvAllBytes :: SerialPort -> Int -> IO BS.ByteString
recvAllBytes s i = do 
  let emptyBytes = PortBytes "" s i
  pbByteString <$> loop emptyBytes 
    where
      loop !b = do 
        bNew <- recv s i         
        if (BS.null bNew) 
          then _pbBytes return b
          else _pbBytes (\bOld -> return $ BS.append bOld bNew) b >>= loop
