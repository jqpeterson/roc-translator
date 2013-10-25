module Protocol.ROC where

import Control.Applicative
import System.Hardware.Serialport.Posix
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

testSndRcvPort str = do
  let port = "/dev/ttyUSB0"  -- Linux
--  let str = [1,2,1,0,7,0] :: [Word8]
  s <- openSerial port defaultSerialSettings {commSpeed = CS9600, timeout = 5 }
  let stringCRC = LB.toStrict.toLazyByteString.word16LE.crcWord16List.makeWord16Listbe $ pack8to16 str
  send s $ BS.append (BS.pack str) stringCRC
--  print (showHex <$> BS.unpack (BS.append (BS.pack str) stringCRC) <*> [""])
  bs <- recv s 248
  print (showHex <$> BS.unpack bs  <*> [""])
  closeSerial s

bsEmpty :: BS.ByteString
bsEmpty = ""

pack8to16 :: [Word8] -> LB.ByteString
pack8to16 wList = let cwList = fromIntegral <$> wList
                  in LB.concat $ map  (toLazyByteString.word16BE ) cwList
