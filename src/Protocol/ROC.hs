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

testSndRcvPort = do
  let port = "/dev/ttyUSB0"  -- Linux
  let str = [1,2,1,0,7,0,123,221] :: [Word8]
  let str' = [0x01,0x02,0x01,0x00,0x07,0x00,0x7B,0xDD]
  s <- openSerial port defaultSerialSettings {commSpeed = CS19200, timeout = 50 }
--  let test = LB.toStrict.toLazyByteString.word16LE.crcWord16List.makeWord16Listbe $ pack16 str'
 -- let tString =  pack16 str'
 --     test    =  LB.toStrict.toLazyByteString.word16LE.crcWord16List.makeWord16Listbe $ tString
--  print (showHex <$> BS.unpack test <*> [""])
--  let output = BS.append (BS.pack str') test
--  print (showHex <$> BS.unpack output <*> [""])
--  send s $ BS.append (BS.pack str') test
  send s $ BS.pack str
  bs <- recv s 248
  print (showHex <$> BS.unpack bs <*> [""])
  closeSerial s

pack16 :: [Word16] -> LB.ByteString
pack16 wList = LB.concat $ map  (toLazyByteString.word16BE ) wList
