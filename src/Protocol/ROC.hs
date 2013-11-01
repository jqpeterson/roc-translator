{-# LANGUAGE RankNTypes, OverloadedStrings, BangPatterns #-}
module Protocol.ROC where

import Control.Applicative
import Control.Monad
import System.IO
import System.Hardware.Serialport
import Data.Word
import qualified Data.ByteString as BS
import Foreign.CRC
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Builder
import Control.Lens
import Control.Lens.Lens
import Control.Lens.Getter
import Protocol.ROC.PointTypes
import Data.Binary.Get


--imports for testing??
-----------------------------------------
--import Control.Monad.Trans.State.Strict
--import Control.Monad.IO.Class
--import Data.Bits
import Numeric
import Data.Char
-----------------------------------------

type RocAddress = [Word8]

data RocConfig = RocConfig { rocConfigPort :: FilePath
                             ,rocConfigRocAddress :: RocAddress
                             ,rocConfigHostAddress :: RocAddress
                             } deriving (Eq,Read,Show)
letssee :: Char -> Word8
letssee x = fromIntegral x

crcTestBS :: Word8 -> Bool
crcTestBS w 
  |w == 0 = True
  |otherwise = False

opCode0 cfg = do
  
  let port = rocConfigPort cfg
      hostAddress = rocConfigHostAddress cfg
      rocAddress = rocConfigRocAddress cfg
      
  sendPort port (rocAddress ++ hostAddress ++ [0,2,0,255])
  receivebs <- receivePort port
  print $ showInt <$> BS.unpack receivebs <*> [""]

opCode7 cfg = do
  
  let port = rocConfigPort cfg
      hostAddress = rocConfigHostAddress cfg
      rocAddress = rocConfigRocAddress cfg
  
  sendPort port (rocAddress ++ hostAddress ++ [7,0])
  receivebs <- receivePort port 
  print $ showInt <$> BS.unpack receivebs <*> [""]  
 

--opCode17 cfg login pass = do
--  
--  let port = rocConfigPort cfg
--      hostAddress = rocConfigHostAddress cfg
--      rocAddress = rocConfigRocAddress cfg
--      login = 
--  sendPort port (rocAddress ++ hostAddress ++ [17,5] ++ login ++ pass)
--  receivebs <- receivePort port
--  print $ showInt <$> BS.unpack receivebs <*> [""]



opCode167 cfg = do
  
  let port = rocConfigPort cfg
      hostAddress = rocConfigHostAddress cfg
      rocAddress = rocConfigRocAddress cfg
      
  sendPort port (rocAddress ++ hostAddress ++ [167,4,1,17,23,0])
  receivebs <- receivePort port
  print $ showInt <$> BS.unpack receivebs <*> [""]
  let dataBytes = BS.drop 10 receivebs
  let dataBytesTest = BS.drop 54 receivebs    
  (Done _ n _ ) <- return $ fetchPointTypeTest (LB.fromStrict dataBytesTest)    
--  debugDecoderPointType fetchedPointTypeTest
  print n
  fetchedPointType <- return $ fetchPointType (LB.fromStrict dataBytes)
  debugDecoderPointType fetchedPointType 
  

sendPort port str = do
  s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
  send s $ BS.append (BS.pack str)(lzyBSto16BScrc (pack8to16 $ str))  


receivePort port = do
  s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
  receivebs <- recvAllBytes s 255
  closeSerial s
  when ((not $ crcCheck receivebs) || (BS.index receivebs 4 == 255)) (print "Failed")
  return receivebs
  
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
