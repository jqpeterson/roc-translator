{-# LANGUAGE RankNTypes, OverloadedStrings, BangPatterns #-}
module Protocol.ROC where

import System.Hardware.Serialport
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
--import Data.ByteString.Builder
import Protocol.ROC.Utils
import Protocol.ROC.PointTypes
import Protocol.ROC.FullyDefinedPointType 
import Protocol.ROC.ROCConfig
import Protocol.ROC.OpCodes
import Control.Applicative
import Numeric                                            

getPointType :: RocConfig -> DefaultPointType -> Word8 -> IO ()
getPointType cfg fdpt pn = do
  let fdataBytes = fdptRxProtocol fdpt
      ptid = fdptPointTypeID fdpt
      pc = fdptParameterCount fdpt
      sp = fdptStartParameter fdpt
  dataBytes <- fdataBytes cfg pn ptid pc sp    
  fetchedPointType <- return $ fetchPointType ptid (LB.fromStrict $ dataBytes)
  print fetchedPointType

runOpCodeRaw :: RocConfig -> (RocConfig -> BS.ByteString) -> IO BS.ByteString
runOpCodeRaw cfg opCode = do
  let port = rocConfigPort cfg
      commRate = rocCommSpeed cfg            
  s <- openSerial port defaultSerialSettings { commSpeed = commRate }
  _ <- send s $ BS.append (opCode cfg) (lzyBSto16BScrc.pack8to16 $ BS.unpack $ opCode cfg)
  receivebs <- recvAllBytes s 255
  closeSerial s
  print $ showInt <$> BS.unpack receivebs <*> [""]
  return receivebs