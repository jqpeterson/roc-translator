{-# LANGUAGE RankNTypes, OverloadedStrings, BangPatterns #-}
module Protocol.ROC where

import Control.Applicative
--import Control.Monad
--import System.IO
import System.Hardware.Serialport
import Data.Word
import qualified Data.ByteString as BS
import Foreign.CRC
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Builder
--import Control.Lens
--import Control.Lens.Lens
--import Control.Lens.Getter
import Protocol.ROC.PointTypes
--import Data.Binary.Get


--imports for testing??
-----------------------------------------
--import Control.Monad.Trans.State.Strict
--import Control.Monad.IO.Class
--import Data.Bits
import Numeric
-----------------------------------------

type RocAddress = [Word8]

data RocConfig = RocConfig { rocConfigPort :: FilePath
                             ,rocConfigRocAddress :: RocAddress
                             ,rocConfigHostAddress :: RocAddress
                             } deriving (Eq,Read,Show)
                                        
crcTestBS :: Word8 -> Bool
crcTestBS w 
  |w == 0 = True
  |otherwise = False

opCode0 :: RocConfig -> IO()
opCode0 cfg = do
  opCode17 cfg
  let port = rocConfigPort cfg
      hostAddress = rocConfigHostAddress cfg
      rocAddress = rocConfigRocAddress cfg
      
  _ <- sendPort port (rocAddress ++ hostAddress ++ [0,2,0,255])
  receivebs <- receivePort port
  print $ showInt <$> BS.unpack receivebs <*> [""]

opCode7 :: RocConfig -> IO()
opCode7 cfg = do
  opCode17 cfg
  let port = rocConfigPort cfg
      hostAddress = rocConfigHostAddress cfg
      rocAddress = rocConfigRocAddress cfg
  
  _ <- sendPort port (rocAddress ++ hostAddress ++ [7,0])
  receivebs <- receivePort port 
  print $ showInt <$> BS.unpack receivebs <*> [""]  
 
opCode17 :: RocConfig -> IO()
opCode17 cfg = do
  
  let port = rocConfigPort cfg
      hostAddress = rocConfigHostAddress cfg
      rocAddress = rocConfigRocAddress cfg
      
  _ <- sendPort port (rocAddress ++ hostAddress ++ [17,5,76,79,73,232,3])
  receivebs <- receivePort port
  print $ showInt <$> BS.unpack receivebs <*> [""]

--opCode165 :: RocConfig -> IO()
--opCode165 cfg ptid = do
--  opCode17 cfg
--  let port = rocConfigPort cfg
--      hostAddress = rocConfigHostAddress cfg
--      rocAddress = rocConfigRocAddress cfg
--  _ <- sendPort port (rocAddress ++ hostAddress ++ [165,4,0,0,0,0])
--  receivebs <- receivePort port
--  print $ showInt <$> BS.unpack receivebs <*> [""]    
--  let dataBytes = BS.drop 10 $ BS.init $ BS.init receivebs
--  print $ BS.length dataBytes      
--  fetchedPointType <- return $ fetchPointType ptid (LB.fromStrict dataBytes)
--  print fetchedPointType

--opCode167 :: forall a.  RocConfig -> Protocol.ROC.PointTypes.PointTypes a -> IO()
opCode167 cfg ptid = do
  opCode17 cfg
  
  let port = rocConfigPort cfg
      hostAddress = rocConfigHostAddress cfg
      rocAddress = rocConfigRocAddress cfg
                                 
-----------------------------------SINGLE PULL---------------------------------------  
  _ <- sendPort port (rocAddress ++ hostAddress ++ [167,4,decodePTID ptid,0,8,0])
  receivebs <- receivePort port      
  print $ showInt <$> BS.unpack receivebs <*> [""]
  let dataBytes = BS.drop 10 $ BS.init $ BS.init receivebs
  print $ BS.length dataBytes      
  fetchedPointType <- return $ fetchPointType ptid (LB.fromStrict dataBytes)
  print fetchedPointType  
------------------------------------------------------------------------------------  
  
--------------------------------MULTIPLE PULL---------------------------------------  
  
--  _ <- sendPort port (rocAddress ++ hostAddress ++ [167,4,decodePTID ptid,0,50,0])
--  receivebs <- receivePort port
--  _ <- sendPort port (rocAddress ++ hostAddress ++ [167,4,decodePTID ptid,0,50,50])
--  receivebs1 <- receivePort port
--  _ <- sendPort port (rocAddress ++ hostAddress ++ [167,4,decodePTID ptid,0,50,100])
--  receivebs2 <- receivePort port
--  _ <- sendPort port (rocAddress ++ hostAddress ++ [167,4,decodePTID ptid,0,50,150])
--  receivebs3 <- receivePort port
--  _ <- sendPort port (rocAddress ++ hostAddress ++ [167,4,decodePTID ptid,0,50,200])
--  receivebs4 <- receivePort port
--  _ <- sendPort port (rocAddress ++ hostAddress ++ [167,4,decodePTID ptid,0,0,0])
--  receivebs5 <- receivePort port
--  let dataBytes1 = BS.append (BS.init $ BS.init $ BS.drop 10 receivebs) (BS.init $ BS.init $ BS.drop 10 receivebs1)
--  let dataBytes2 = BS.append (BS.init $ BS.init $ BS.drop 10 receivebs2) (BS.init $ BS.init $ BS.drop 10 receivebs3)    
--  let dataBytes3 = BS.append (BS.init $ BS.init $ BS.drop 10 receivebs4) (BS.init $ BS.init $ BS.drop 10 receivebs5)        
--  let dataBytes  = BS.append (BS.append dataBytes1 dataBytes2) dataBytes3    
--  print $ BS.length dataBytes      
--  fetchedPointType <- return $ fetchPointType ptid (LB.fromStrict dataBytes)
--  print fetchedPointType
-------------------------------------------------------------------------------------
      

--  debugDecoderPointType fetchedPointType 

sendPort :: FilePath -> [Word8] -> IO Int
sendPort port str = do
  s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
  send s $ BS.append (BS.pack str)(lzyBSto16BScrc (pack8to16 $ str))  

receivePort :: FilePath -> IO BS.ByteString
receivePort port = do
  s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
  receivebs <- recvAllBytes s 255
  closeSerial s
--  when ((not $ crcCheck receivebs) || (BS.index receivebs 4 == 255)) (print "Failed")
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
