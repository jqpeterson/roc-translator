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

crcTestBS :: Word8 -> Bool
crcTestBS w 
  |w == 0 = True
  |otherwise = False

opCode7 port usr = do
  bs <- sndRcvPort port (usr ++ [1,0,7,0])
  print $ showInt <$> BS.unpack bs <*> [""]  
 
opCode17 port usr = do
  
  bs <- sndRcvPort port (usr ++ [1,0,17,5,76,79,73,232,3])
  print $ showInt <$> BS.unpack bs <*> [""]
--  print $ BS.unpack.LB.toStrict.toLazyByteString.string7 $ username
--  let code7list = [7,0] :: [Word8]
--  let bytestr = sndRcvPort port ((usr ++ dest) ++ code7list)
--  print $ BS.index bytestr 4  

sndRcvPort port str = do
  s <- openSerial port defaultSerialSettings { commSpeed = CS9600 }
  send s $ BS.append (BS.pack str)(lzyBSto16BScrc (pack8to16 $ str))  
  bs <- xsrecvAllBytes s 20 
 -- print $ showHex <$> (BS.unpack bs) <*> [""]
  let returnCRC = lzyBSto16BScrc $ pack8to16 $ BS.unpack $ bs  
  let pulledCRC = BS.reverse.BS.take 2 $ BS.reverse bs  
--  print $ BS.all crcTestBS returnCRC 
  closeSerial s
  return bs

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
