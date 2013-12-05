{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType172 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Utils

data PointType172 = PointType172 {
 
 pointType172RemoteRTUTag                 :: !PointType172RemoteRTUTag                      
,pointType172ROCDeviceID                  :: !PointType172ROCDeviceID                      
,pointType172ComissionListIndex           :: !PointType172ComissionListIndex                      
,pointType172ComissionFlag                :: !PointType172ComissionFlag                      

} deriving (Eq, Show, Generic)                       

type PointType172RemoteRTUTag             = BS.ByteString            
type PointType172ROCDeviceID              = Word32            
type PointType172ComissionListIndex       = Word8            
type PointType172ComissionFlag            = Word8            
  
pointType172Parser :: Get PointType172
pointType172Parser = do 

  remoteRTUTag <- getByteString 20
  rOCDeviceID <- getWord32le
  comissionListIndex <- getWord8
  comissionFlag <- getWord8
  
  return $ PointType172 remoteRTUTag rOCDeviceID comissionListIndex comissionFlag
