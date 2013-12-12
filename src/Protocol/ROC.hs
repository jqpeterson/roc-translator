{-# LANGUAGE RankNTypes, OverloadedStrings, BangPatterns #-}
module Protocol.ROC where

import System.Hardware.Serialport
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Protocol.ROC.Utils
import Protocol.ROC.PointTypes
import Protocol.ROC.FullyDefinedPointType 
import Protocol.ROC.ROCConfig
import Protocol.ROC.OpCodes
import Protocol.ROC.RocSerialize
import Control.Applicative
import Numeric                                            
import Data.Int
import Data.ByteString.Builder
import Data.Word

getPointType :: RocConfig -> DefaultPointType -> PointNumber -> IO ()
getPointType cfg fdpt pn = do
  let fdataBytes = fdptRxProtocol fdpt
      ptid = fdptPointTypeID fdpt
      pc = fdptParameterCount fdpt
      sp = fdptStartParameter fdpt
  dataBytes <- fdataBytes cfg pn ptid pc sp    
  fetchedPointType <- return $ fetchPointType ptid (LB.fromStrict $ dataBytes)
  print fetchedPointType

writePointType :: RocSerialize a => RocConfig -> DefaultPointType -> PointNumber -> ParameterNumber -> a -> IO ()
writePointType cfg fdpt pn prn pdata = do
  let port = rocConfigPort cfg
      commRate = rocCommSpeed cfg
      ptid = fdptPointTypeID fdpt
      pt = decodePTID ptid
      databytes = BS.append (opCode166 pt pn prn pdata cfg) (lzyBSto16BScrc.pack8to16 $ BS.unpack $ opCode166 pt pn prn pdata cfg)
  s <- openSerial port defaultSerialSettings { commSpeed = commRate } 
  print $ showInt <$> BS.unpack databytes <*> [""]
  _ <- send s $ databytes
  receivebs <- recvAllBytes s 255
  closeSerial s
  print $ showInt <$> BS.unpack receivebs <*> [""]
  

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