{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType2 where

import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Builder as LBB
import Data.Word
import Data.Binary
import Data.Bool
import Data.Int
import Data.Binary.Get
import Numeric
import Control.Applicative
import Protocol.ROC.Float

data PointType2 = PointType2 {
 pointType2PointTag            :: !PointType2PointTag       
,pointType2TimeOn              :: !PointType2TimeOn         
,pointType2Spare               :: !PointType2Spare          
,pointType2Status              :: !PointType2Status         
,pointType2BitfieldHigh        :: !PointType2BitfieldHigh   
,pointType2BitfieldLow         :: !PointType2BitfieldLow    
,pointType2AccumulatedValue    :: !PointType2AccumulatedValue
,pointType2Units               :: !PointType2Units          
,pointType2CycleTime           :: !PointType2CycleTime      
,pointType2Count0              :: !PointType2Count0         
,pointType2Count100            :: !PointType2Count100       
,pointType2LowReading          :: !PointType2LowReading     
,pointType2HighReading         :: !PointType2HighReading    
,pointType2EuValue             :: !PointType2EuValue    
,pointType2AlarmMode           :: !PointType2AlarmMode      
,pointType2ScanningMode        :: !PointType2ScanningMode   
,pointType2ManualState         :: !PointType2ManualState    
,pointType2PhysicalState       :: !PointType2PhysicalState  
} deriving (Read,Eq, Show, Generic)                       

type PointType2PointTag              = BS.ByteString
type PointType2TimeOn                = Word16
type PointType2Spare                 = Word8
type PointType2Status                = Bool
type PointType2BitfieldHigh          = Word8
type PointType2BitfieldLow           = Word8
type PointType2AccumulatedValue      = Word32
type PointType2Units                 = BS.ByteString
type PointType2CycleTime             = Word16
type PointType2Count0                = Int16
type PointType2Count100              = Int16
type PointType2LowReading            = Float
type PointType2HighReading           = Float
type PointType2EuValue               = Float
type PointType2AlarmMode             = Word8
type PointType2ScanningMode          = Bool
type PointType2ManualState           = Word8
type PointType2PhysicalState         = Word8

anyButNull :: Get Bool 
anyButNull = do 
  c <- getWord8
  return $ test c 
  where 
    test :: Word8 -> Bool 
    test x = (fromIntegral x) == 1

int16 :: Get Int16
int16 = do
  x <- getWord16le
  return $ fromIntegral x

pointType2Parser :: Get PointType2
pointType2Parser = do 
  id <- getByteString 10 
  timeon <- getWord16le
  spare <- getWord8
  sts <- anyButNull
  cfg <- getWord8
  alarmCode <- getWord8
  accumulatedvalue <- getWord32le
  units <- getByteString 10
  cycleTime <- getWord16le 
  count0 <- int16
  count100 <- int16
  lowReading <- getIeeeFloat32
  highReading <- getIeeeFloat32
  euValue <- getIeeeFloat32
  alarmMode <- getWord8
  scanningMode <- anyButNull
  manualState <- getWord8
  physicalState <- getWord8
  
  return $ PointType2 id timeon spare sts cfg alarmCode accumulatedvalue units cycleTime count0 count100 lowReading highReading euValue alarmMode scanningMode manualState physicalState


--fetchPointType1 :: LB.ByteString -> Decoder PointType1 
--fetchPointType1 bs = runGetIncremental pointType1Parser `pushChunks` bs
