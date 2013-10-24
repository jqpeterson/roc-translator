module Protocol.ROC where

import Control.Applicative
--import System.Hardware.Serialport.Posix
import Control.Monad
import System.IO
import System.Hardware.Serialport
import Data.Bits
import Numeric
import Data.Word
import qualified Data.ByteString as BS
import Foreign.CRC
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Builder

testSndRcvPort port str = do
--  let port = "/dev/ttyUSB0"  -- Linux
--  let str = [3,1,240,240,255,3,20,7,5,167,164] :: [Word8]
  s <- openSerial port defaultSerialSettings { commSpeed = CS9600, timeout = 1 }
  let stringCRC = LB.toStrict.toLazyByteString.word16LE.crcWord16List.makeWord16Listbe $ pack8to16 str
  send s $ BS.append (BS.pack str) stringCRC
--  print (showHex <$> BS.unpack (BS.append (BS.pack str) stringCRC) <*> [""])
  bs <- recv s 20
--  send s $ BS.pack str
  bs1 <- recv s 20
  bs2 <- recv s 10
  bs3 <- recv s 10
  bs4 <- recv s 10
--  bs5 <- recv s 10
--  bs6 <- recv s 10
--  bs7 <- recv s 10
--  bs8 <- recv s 10
--  bs9 <- recv s 10
--  bs10 <- recv s 10
--  bs11 <- recv s 10
--  bs12 <- recv s 10
--  bs13 <- recv s 10
--  bs14 <- recv s 10
--  bs15 <- recv s 10
--  bs16 <- recv s 10
--  bs17 <- recv s 10
--  bs18 <- recv s 10
--  bs19 <- recv s 10
--  bs20 <- recv s 10
--  bs21 <- recv s 10
--  bs22 <- recv s 10
--  bs23 <- recv s 10
--  bs24 <- recv s 10
--  bs25 <- recv s 10
--  bs26 <- recv s 10
--  bs27 <- recv s 10
--  bs28 <- recv s 10
--  bs29 <- recv s 10
--  bs30 <- recv s 10

  print (showHex <$> BS.unpack bs  <*> [""])
  print (showHex <$> BS.unpack bs1  <*> [""])
  print (showHex <$> BS.unpack bs2  <*> [""])
  print (showHex <$> BS.unpack bs3  <*> [""])
  print (showHex <$> BS.unpack bs4  <*> [""])
--  print (showHex <$> BS.unpack bs5  <*> [""])
--  print (showHex <$> BS.unpack bs6  <*> [""])
--  print (showHex <$> BS.unpack bs7  <*> [""])
--  print (showHex <$> BS.unpack bs8  <*> [""])
--  print (showHex <$> BS.unpack bs9  <*> [""])
--  print (showHex <$> BS.unpack bs10  <*> [""])
--  print (showHex <$> BS.unpack bs11  <*> [""])
--  print (showHex <$> BS.unpack bs12  <*> [""])
--  print (showHex <$> BS.unpack bs13  <*> [""])
--  print (showHex <$> BS.unpack bs14  <*> [""])
--  print (showHex <$> BS.unpack bs15  <*> [""])
--  print (showHex <$> BS.unpack bs16  <*> [""])
--  print (showHex <$> BS.unpack bs17  <*> [""])
--  print (showHex <$> BS.unpack bs18  <*> [""])
--  print (showHex <$> BS.unpack bs19  <*> [""])
--  print (showHex <$> BS.unpack bs20  <*> [""])
--  print (showHex <$> BS.unpack bs21  <*> [""])
--  print (showHex <$> BS.unpack bs22  <*> [""])
--  print (showHex <$> BS.unpack bs23  <*> [""])
--  print (showHex <$> BS.unpack bs24  <*> [""])
--  print (showHex <$> BS.unpack bs25  <*> [""])
--  print (showHex <$> BS.unpack bs26  <*> [""])
--  print (showHex <$> BS.unpack bs27  <*> [""])
--  print (showHex <$> BS.unpack bs28  <*> [""])
--  print (showHex <$> BS.unpack bs29  <*> [""])
--  print (showHex <$> BS.unpack bs30  <*> [""])
  closeSerial s



pack8to16 :: [Word8] -> LB.ByteString
pack8to16 wList = let cwList = fromIntegral <$> wList
                  in LB.concat $ map  (toLazyByteString.word16BE ) cwList
