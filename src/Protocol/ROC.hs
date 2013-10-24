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


testSndRcvPort port str = do
--  let port = "/dev/ttyUSB0"  -- Linux
--  let str = [3,1,240,240,255,3,20,7,5,167,164] :: [Word8]
  
  s <- openSerial port defaultSerialSettings { commSpeed = CS9600 }
  send s $ BS.append (BS.pack str)(lzyBSto16BScrc (pack8to16 $ str))  
  bs <-  recvAllBytes s 20 
  print $ showHex <$> (BS.unpack bs) <*> [""]
  let returnCRC = lzyBSto16BScrc $ pack8to16 $ BS.unpack $ bs  
  let pulledCRC = BS.reverse.BS.take 2 $ BS.reverse bs  
  print $ showHex <$> (BS.unpack pulledCRC) <*> [""]
  print $ showHex <$> (BS.unpack returnCRC) <*> [""]
  print $ BS.all crcTestBS returnCRC 
  closeSerial s



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


  






            
-- emptyState :: ByteState 
-- emptyState = ("",0)
