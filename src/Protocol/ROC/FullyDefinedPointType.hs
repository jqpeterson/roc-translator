{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.FullyDefinedPointType where

import Protocol.ROC.ROCConfig
import Protocol.ROC.OpCodes
import System.Hardware.Serialport
import Protocol.ROC.Utils
import qualified Data.ByteString as BS
import Protocol.ROC.PointTypes
import Data.Word
import Numeric
import Control.Applicative

type FB107PT0      = DefaultPointType
type FB107PT1      = DefaultPointType
type FB107PT2      = DefaultPointType
type FB107PT3      = DefaultPointType
type FB107PT4      = DefaultPointType
type FB107PT5      = DefaultPointType
type FB107PT6      = DefaultPointType
type FB107PT7      = DefaultPointType
type FB107PT8      = DefaultPointType
type FB107PT10     = DefaultPointType
type FB107PT12     = DefaultPointType
type FB107PT13     = DefaultPointType
type FB107PT14     = DefaultPointType
type FB107PT15     = DefaultPointType
type FB107PT16     = DefaultPointType
type FB107PT17     = DefaultPointType
type FB107PT19     = DefaultPointType
type FB107PT20     = DefaultPointType
type FB107PT21     = DefaultPointType
type FB107PT40     = DefaultPointType
type FB107PT41     = DefaultPointType
type FB107PT42     = DefaultPointType
type FB107PT43     = DefaultPointType
type FB107PT44     = DefaultPointType
type FB107PT45     = DefaultPointType
type FB107PT46     = DefaultPointType
type FB107PT47     = DefaultPointType
type FB107PT48     = DefaultPointType
type FB107PT55     = DefaultPointType
type FB107PT57     = DefaultPointType
type FB107PT58     = DefaultPointType
type FB107PT59     = DefaultPointType
type FB107PT80     = DefaultPointType
--type FB107PT81     = DefaultPointType
type FB107PT85     = DefaultPointType
type FB107PT86     = DefaultPointType
type FB107PT88     = DefaultPointType
type FB107PT89     = DefaultPointType
type FB107PT93     = DefaultPointType
type FB107PT94     = DefaultPointType
type FB107PT98     = DefaultPointType
type FB107PT117     = DefaultPointType
type FB107PT118     = DefaultPointType
--type FB107PT120     = DefaultPointType
type FB107PT121     = DefaultPointType
type FB107PT122     = DefaultPointType
--type FB107PT176     = DefaultPointType
--type FB107PT177     = DefaultPointType

defaultRxProtocol' :: (Integral a, Integral b) => RocConfig -> Word8 -> PointTypes () -> a -> b -> IO BS.ByteString
defaultRxProtocol' c p ptid pc sp = defaultRxProtocol c p ptid (fromIntegral pc) (fromIntegral sp) 

defaultRxProtocol :: RocConfig -> Word8 -> PointTypes () -> Word8 -> Word8 -> IO BS.ByteString
defaultRxProtocol cfg pn ptid pc sp  = do
  let port = rocConfigPort cfg
      commRate = rocCommSpeed cfg
      pt = decodePTID ptid
  s <- openSerial port defaultSerialSettings { commSpeed = commRate }
  _ <- send s $ BS.append (opCode167 pt pn pc sp cfg) (lzyBSto16BScrc.pack8to16 $ BS.unpack $ opCode167 pt pn pc sp cfg)   
  receivebs <- recvAllBytes s 255
  closeSerial s
  print $ showInt <$> BS.unpack receivebs <*> [""]
  let databytes = BS.drop 10 $ BS.init $ BS.init receivebs      
  print $ BS.length databytes    
  print $ showHex <$> BS.unpack databytes <*> [""] 
  return databytes  

rxProtocol2Block :: RocConfig -> Word8 -> PointTypes () -> Word -> Word8 -> IO BS.ByteString
rxProtocol2Block cfg pn ptid pc sp = do
  let firstpc = div pc 2
      secondpc = (pc - (div pc 2))
      secondsp = firstpc
  databytes1 <- defaultRxProtocol' cfg pn ptid firstpc sp
  databytes2 <- defaultRxProtocol' cfg pn ptid secondpc secondsp
  return $ BS.append databytes1 databytes2

rxProtocol3Block :: RocConfig -> Word8 -> PointTypes () -> Word -> Word8 -> IO BS.ByteString
rxProtocol3Block  cfg pn ptid pc sp = do
   let divpc = div pc 6
       firstpc = (pc - divpc - divpc - divpc - divpc - divpc) 
       secondsp = firstpc
       thirdsp = (secondsp + divpc)
       fourthsp = (thirdsp + divpc)
       fifthsp = (fourthsp + divpc)
       sixthsp = (fifthsp + divpc)
   databytes1 <- defaultRxProtocol' cfg pn ptid firstpc sp      
   databytes2 <- defaultRxProtocol' cfg pn ptid divpc secondsp
   databytes3 <- defaultRxProtocol' cfg pn ptid divpc thirdsp
   databytes4 <- defaultRxProtocol' cfg pn ptid divpc fourthsp
   databytes5 <- defaultRxProtocol' cfg pn ptid divpc fifthsp
   databytes6 <- defaultRxProtocol' cfg pn ptid divpc sixthsp
   return $ BS.append databytes1 $ BS.append databytes2 $ BS.append databytes3 $ BS.append databytes4 $ BS.append databytes5 databytes6
   
fbUnit107PT0 :: FB107PT0
fbUnit107PT0 = FDPT 107 pt0 45 0 defaultRxProtocol'
fbUnit107PT1 :: FB107PT1 
fbUnit107PT1 = FDPT 107 pt1 23 0 defaultRxProtocol'
fbUnit107PT2 :: FB107PT2 
fbUnit107PT2 = FDPT 107 pt2 18 0 defaultRxProtocol'
fbUnit107PT3 :: FB107PT3 
fbUnit107PT3 = FDPT 107 pt3 36 0 defaultRxProtocol'
fbUnit107PT4 :: FB107PT4 
fbUnit107PT4 = FDPT 107 pt4 13 0 defaultRxProtocol'
fbUnit107PT5 :: FB107PT5 
fbUnit107PT5 = FDPT 107 pt5 22 0 defaultRxProtocol'
fbUnit107PT6 :: FB107PT6 
fbUnit107PT6 = FDPT 107 pt6 37 0 defaultRxProtocol'
fbUnit107PT7 :: FB107PT7 
fbUnit107PT7 = FDPT 107 pt7 54 0 defaultRxProtocol'
fbUnit107PT8 :: FB107PT8 
fbUnit107PT8 = FDPT 107 pt8 60 0 defaultRxProtocol'
fbUnit107PT10 :: FB107PT10 
fbUnit107PT10 = FDPT 107 pt10 21 0 defaultRxProtocol'
fbUnit107PT12 :: FB107PT12 
fbUnit107PT12 = FDPT 107 pt12 11 0 defaultRxProtocol'
fbUnit107PT13 :: FB107PT13 
fbUnit107PT13 = FDPT 107 pt13 32 0 defaultRxProtocol'
fbUnit107PT14 :: FB107PT14 
fbUnit107PT14 = FDPT 107 pt14 40 0 defaultRxProtocol'
fbUnit107PT15 :: FB107PT15 
fbUnit107PT15 = FDPT 107 pt15 26 0 defaultRxProtocol'  
fbUnit107PT16 :: FB107PT16 
fbUnit107PT16 = FDPT 107 pt16 28 0 defaultRxProtocol'
fbUnit107PT17 :: FB107PT17 
fbUnit107PT17 = FDPT 107 pt17 23 0 defaultRxProtocol'
fbUnit107PT19 :: FB107PT19 
fbUnit107PT19 = FDPT 107 pt19 8 0 defaultRxProtocol'
fbUnit107PT20 :: FB107PT20 
fbUnit107PT20 = FDPT 107 pt20 31 0 rxProtocol2Block
fbUnit107PT21 :: FB107PT21 
fbUnit107PT21 = FDPT 107 pt21 4 0 defaultRxProtocol'
fbUnit107PT40 :: FB107PT40 
fbUnit107PT40 = FDPT 107 pt40 54 0 defaultRxProtocol'
fbUnit107PT41 :: FB107PT41 
fbUnit107PT41 = FDPT 107 pt41 55 0 defaultRxProtocol'
fbUnit107PT42 :: FB107PT42 
fbUnit107PT42 = FDPT 107 pt42 29 0 defaultRxProtocol'
fbUnit107PT43 :: FB107PT43 
fbUnit107PT43 = FDPT 107 pt43 50 0 rxProtocol2Block
fbUnit107PT44 :: FB107PT44 
fbUnit107PT44 = FDPT 107 pt44 20 0 defaultRxProtocol'
fbUnit107PT45 :: FB107PT45 
fbUnit107PT45 = FDPT 107 pt45 15 0 defaultRxProtocol'
fbUnit107PT46 :: FB107PT46 
fbUnit107PT46 = FDPT 107 pt46 94 0 rxProtocol2Block
fbUnit107PT47 :: FB107PT47 
fbUnit107PT47 = FDPT 107 pt47 75 0 rxProtocol2Block
fbUnit107PT48 :: FB107PT48 
fbUnit107PT48 = FDPT 107 pt48 39 0 defaultRxProtocol'
fbUnit107PT55 :: FB107PT55 
fbUnit107PT55 = FDPT 107 pt55 108 0 defaultRxProtocol'
fbUnit107PT57 :: FB107PT57 
fbUnit107PT57 = FDPT 107 pt57 6 0 defaultRxProtocol'
fbUnit107PT58 :: FB107PT58 
fbUnit107PT58 = FDPT 107 pt58 4 0 defaultRxProtocol'
fbUnit107PT59 :: FB107PT59 
fbUnit107PT59 = FDPT 107 pt59 13 0 defaultRxProtocol'
fbUnit107PT80 :: FB107PT80 
fbUnit107PT80 = FDPT 107 pt80 122 0 rxProtocol3Block
--fbUnit107PT81 :: FB107PT81 
--fbUnit107PT81 = FDPT 107 pt81 13 0 defaultRxProtocol'
fbUnit107PT85 :: FB107PT85 
fbUnit107PT85 = FDPT 107 pt85 256 0 rxProtocol3Block
fbUnit107PT86 :: FB107PT86 
fbUnit107PT86 = FDPT 107 pt86 102 0 defaultRxProtocol'
fbUnit107PT88 :: FB107PT88 
fbUnit107PT88 = FDPT 107 pt88 3 0 defaultRxProtocol'
fbUnit107PT89 :: FB107PT89 
fbUnit107PT89 = FDPT 107 pt89 8 0 defaultRxProtocol'
fbUnit107PT93 :: FB107PT93 
fbUnit107PT93 = FDPT 107 pt93 11 0 defaultRxProtocol'
fbUnit107PT94 :: FB107PT94 
fbUnit107PT94 = FDPT 107 pt94 12 0 defaultRxProtocol'
fbUnit107PT98 :: FB107PT98 
fbUnit107PT98 = FDPT 107 pt98 44 0 defaultRxProtocol'
fbUnit107PT117 :: FB107PT117 
fbUnit107PT117 = FDPT 107 pt117 30 0 defaultRxProtocol'
fbUnit107PT118 :: FB107PT118 
fbUnit107PT118 = FDPT 107 pt118 91 0 defaultRxProtocol'
-- fbUnit107PT120 :: FB107PT120 
-- fbUnit107PT120 = FDPT 107 pt120 0 defaultRxProtocol'
fbUnit107PT121 :: FB107PT121 
fbUnit107PT121 = FDPT 107 pt121 151 0 defaultRxProtocol'
fbUnit107PT122 :: FB107PT122 
fbUnit107PT122 = FDPT 107 pt122 29 0 defaultRxProtocol'
-- fbUnit107PT176 :: FB107PT176 
-- fbUnit107PT176 = FDPT 107 pt176 0 defaultRxProtocol'
-- fbUnit107PT177 :: FB107PT177 
-- fbUnit107PT177 = FDPT 107 pt177 0 defaultRxProtocol' 
