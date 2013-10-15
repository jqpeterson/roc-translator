module Protocol.ROC where

import Control.Monad
import System.IO
import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
import qualified System.Timeout as T
testSendPort = do
  let port = "/dev/ttyUSB0"  -- Linux
  s <- openSerial port defaultSerialSettings { commSpeed = CS19200 }
  send s $ B.pack "Ar"
  recv s 10 >>= print
  closeSerial s


testRcvPort = do
  let port = "/dev/ttyUSB0"  -- Linux
  s <- openSerial port defaultSerialSettings { commSpeed = CS19200, timeout = 50 }
  recv s 10000 >>= print
  closeSerial s
  testRcvPort

testRcvByte = do 
  let port = "/dev/ttyUSB0"   -- Linux
  hOpenSerial port defaultSerialSettings { commSpeed = CS19200, timeout = 50 }
  return ()
  
--https://github.com/jputcu/serialport.git


receiveLoop :: Handle -> IO Handle
receiveLoop h = do
  hTest <- hReady h
  case hTest of 
    True -> return h
    False -> receiveLoop h
