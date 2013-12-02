{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.FullyDefinedPointType where

import Protocol.ROC.ROCConfig
import Protocol.ROC.OpCodes
import System.Hardware.Serialport
import Protocol.ROC.Utils
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as LB
import Protocol.ROC.PointTypes
import Data.Word

              
type FB107PT1    = DefaultPointType                        
type FB107PT15   = DefaultPointType
type FB107PT93   = DefaultPointType
type FB107PT94   = DefaultPointType

defaultRxProtocol :: RocConfig -> Word8 -> PointTypes () -> Word8 -> Word8 -> IO BS.ByteString
defaultRxProtocol cfg pn ptid pc sp  = do
  let port = rocConfigPort cfg
      commRate = rocCommSpeed cfg
      pt = decodePTID ptid
  s <- openSerial port defaultSerialSettings { commSpeed = commRate }
  _ <- send s $ BS.append (opCode167 pt pn pc sp cfg) (lzyBSto16BScrc.pack8to16 $ BS.unpack $ opCode167 pt pn pc sp cfg)   
  receivebs <- recvAllBytes s 255
  closeSerial s
  let databytes = BS.drop 10 $ BS.init $ BS.init receivebs
  return databytes  

fbUnit107PT1 :: FB107PT1
fbUnit107PT1 = FDPT 107 pt1 23 0 defaultRxProtocol

fbUnit107PT15 :: FB107PT15
fbUnit107PT15 = FDPT 107 pt15 26 0 defaultRxProtocol  

fbUnit107PT93 :: FB107PT93
fbUnit107PT93 = FDPT 107 pt93 11 0 defaultRxProtocol

fbUnit107PT94 :: FB107PT94
fbUnit107PT94 = FDPT 107 pt94 12 0 defaultRxProtocol