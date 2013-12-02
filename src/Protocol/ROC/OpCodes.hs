{-# LANGUAGE RankNTypes, OverloadedStrings, BangPatterns #-}
module Protocol.ROC.OpCodes where

import Protocol.ROC.ROCConfig
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Builder
import System.Hardware.Serialport
--import Protocol.ROC.Utils
import Control.Applicative
      
opCode0 :: BlockNumber -> RocConfig -> BS.ByteString
opCode0 bn cfg = let hostAddress = rocConfigHostAddress cfg
                     rocAddress = rocConfigRocAddress cfg
                 in BS.pack (rocAddress ++ hostAddress ++ [0,2,bn,255])

opCode6 :: RocConfig -> BS.ByteString
opCode6 cfg = let hostAddress = rocConfigHostAddress cfg
                  rocAddress = rocConfigRocAddress cfg
              in BS.pack (rocAddress ++ hostAddress ++ [6,0])

opCode7 :: RocConfig -> BS.ByteString
opCode7 cfg = let hostAddress = rocConfigHostAddress cfg
                  rocAddress = rocConfigRocAddress cfg
              in BS.pack (rocAddress ++ hostAddress ++ [7,0])
                 
                 
-- opCode8 :: UTCTime -> RocConfig -> BS.ByteString
-- opCode8 timeData cfg = let hostAddress = rocConfigHostAddress cfg
--                            rocAddress = rocConfigRocAddress cfg
--                            timeDataBytes = BS.
--                        in BS.append (BS.pack (rocAddress ++ hostAddress ++ [8])) (timeData)
 
opCode17 :: RocConfig -> BS.ByteString
opCode17 cfg = let hostAddress = rocConfigHostAddress cfg
                   rocAddress = rocConfigRocAddress cfg
                   login = toLazyByteString.stringUtf8 $ rocLogin cfg
                   password = toLazyByteString.word16LE  $ rocPassword cfg
               in  BS.append (BS.pack (rocAddress ++ hostAddress ++ [17,5])) (LB.toStrict $ LB.append login password)


opCode167 :: Word8 -> PointNumber -> ParameterCount -> StartParameter -> RocConfig -> BS.ByteString
opCode167 ptid pn pc ps cfg = let hostAddress = rocConfigHostAddress cfg
                                  rocAddress = rocConfigRocAddress cfg
                              in BS.pack (rocAddress ++ hostAddress ++ [167,4,ptid,pn,pc,ps])

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


