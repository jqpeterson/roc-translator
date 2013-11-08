{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType2 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Int
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.PointTypes.Utils

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


pointType2Parser :: Get PointType2
pointType2Parser = do 
  pointId <- getByteString 10 
  timeon <- getWord16le
  spare <- getWord8
  sts <- anyButNull
  cfg <- getWord8
  alarmCode <- getWord8
  accumulatedvalue <- getWord32le
  units <- getByteString 10
  cycleTime <- getWord16le 
  count0 <- getInt16
  count100 <- getInt16
  lowReading <- getIeeeFloat32
  highReading <- getIeeeFloat32
  euValue <- getIeeeFloat32
  alarmMode <- getWord8
  scanningMode <- anyButNull
  manualState <- getWord8
  physicalState <- getWord8
  
  return $ PointType2 pointId timeon spare sts cfg alarmCode accumulatedvalue units cycleTime count0 count100 lowReading highReading euValue alarmMode scanningMode manualState physicalState



