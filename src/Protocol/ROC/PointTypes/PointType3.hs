{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType3 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Int
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.PointTypes.Utils

data PointType3 = PointType3 {
 pointType3PointTag                      :: !PointType3PointTag                       
,pointType3TimeOn                        :: !PointType3TimeOn                       
,pointType3ScanPeriod                    :: !PointType3ScanPeriod                    
,pointType3Filter                        :: !PointType3Filter                        
,pointType3AdjustedDA0                   :: !PointType3AdjustedDA0                  
,pointType3AdjustedDA100                 :: !PointType3AdjustedDA100                 
,pointType3LowReading                    :: !PointType3LowReading                
,pointType3HighReading                   :: !PointType3HighReading                   
,pointType3LowAlarm                      :: !PointType3LowAlarm                      
,pointType3HighAlarm                     :: !PointType3HighAlarm                     
,pointType3LowLowAlarm                   :: !PointType3LowLowAlarm                   
,pointType3HighHighAlarm                 :: !PointType3HighHighAlarm                 
,pointType3RateAlarm                     :: !PointType3RateAlarm                     
,pointType3AlarmDeadband                 :: !PointType3AlarmDeadband             
,pointType3FilteredEU                    :: !PointType3FilteredEU                    
,pointType3Bitfieldcfg                   :: !PointType3Bitfieldcfg                  
,pointType3AlarmCode                     :: !PointType3AlarmCode                    
,pointType3RawDAInput                    :: !PointType3RawDAInput                
,pointType3ActualScanTime                :: !PointType3ActualScanTime            
,pointType3FaultEUValue                  :: !PointType3FaultEUValue              
,pointType3Calibrated0ADValue            :: !PointType3Calibrated0ADValue        
,pointType3CalibratedMidPoint1ADValue    :: !PointType3CalibratedMidPoint1ADValue
,pointType3CalibratedMidPoint2ADValue    :: !PointType3CalibratedMidPoint2ADValue
,pointType3CalibratedMidPoint3ADValue    :: !PointType3CalibratedMidPoint3ADValue
,pointType3CalibratedSpanADValue         :: !PointType3CalibratedSpanADValue     
,pointType3Calibrated0EUValue            :: !PointType3Calibrated0EUValue        
,pointType3CalibratedMidPoint1EUValue    :: !PointType3CalibratedMidPoint1EUValue
,pointType3CalibratedMidPoint2EUValue    :: !PointType3CalibratedMidPoint2EUValue
,pointType3CalibratedMidPoint3EUValue    :: !PointType3CalibratedMidPoint3EUValue
,pointType3CalibratedSpanEUValue         :: !PointType3CalibratedSpanEUValue     
,pointType3Offset                        :: !PointType3Offset                    
,pointType3CalibrationSetEUValue         :: !PointType3CalibrationSetEUValue     
,pointType3ManualEU                      :: !PointType3ManualEU                  
,pointType3Timer                         :: !PointType3Timer                     
,pointType3CalibrationMode               :: !PointType3CalibrationMode                         
,pointType3CablibrationType              :: !PointType3CablibrationType                       
} deriving (Read,Eq, Show, Generic)                       

type PointType3PointTag                         = BS.ByteString    
type PointType3TimeOn                           = Word16            
type PointType3ScanPeriod                       = Word16           
type PointType3Filter                           = Word16               
type PointType3AdjustedDA0                      = Int16             
type PointType3AdjustedDA100                    = Int16            
type PointType3LowReading                       = Float           
type PointType3HighReading                      = Float          
type PointType3LowAlarm                         = Float           
type PointType3HighAlarm                        = Float           
type PointType3LowLowAlarm                      = Float            
type PointType3HighHighAlarm                    = Float             
type PointType3RateAlarm                        = Float             
type PointType3AlarmDeadband                    = Float              
type PointType3FilteredEU                       = Float             
type PointType3Bitfieldcfg                      = Word8              
type PointType3AlarmCode                        = Word8              
type PointType3RawDAInput                       = Int16             
type PointType3ActualScanTime                   = Word16              
type PointType3FaultEUValue                     = Float            
type PointType3Calibrated0ADValue               = Int16            
type PointType3CalibratedMidPoint1ADValue       = Int16                
type PointType3CalibratedMidPoint2ADValue       = Int16                
type PointType3CalibratedMidPoint3ADValue       = Int16                
type PointType3CalibratedSpanADValue            = Int16                 
type PointType3Calibrated0EUValue               = Float                 
type PointType3CalibratedMidPoint1EUValue       = Float               
type PointType3CalibratedMidPoint2EUValue       = Float              
type PointType3CalibratedMidPoint3EUValue       = Float                 
type PointType3CalibratedSpanEUValue            = Float                  
type PointType3Offset                           = Float                
type PointType3CalibrationSetEUValue            = Float              
type PointType3ManualEU                         = Float            
type PointType3Timer                            = Word16         
type PointType3CalibrationMode                  = Word8        
type PointType3CablibrationType                 = Word8        


pointType3Parser :: Get PointType3
pointType3Parser = do 
  pointId <- getByteString 10 
  timeon <- getWord16le
  scanPeriod <- getWord16le
  fltr <- getWord16le
  adjustedDA0 <- getInt16
  adjustedDA100 <- getInt16
  lowReading <- getIeeeFloat32
  highReading <- getIeeeFloat32
  lowAlarm <- getIeeeFloat32
  highAlarm <- getIeeeFloat32
  lowLowAlarm <- getIeeeFloat32
  highHighAlarm <- getIeeeFloat32
  rateAlarm <- getIeeeFloat32
  alarmDeadband <- getIeeeFloat32
  filteredEU <- getIeeeFloat32
  cfg <- getWord8
  alarmCode <- getWord8
  rawDAInput <- getInt16
  actualScanTime <- getWord16le
  faultEUValue <- getIeeeFloat32
  calibrated0ADValue <- getInt16
  calibratedMidpoint1ADValue <- getInt16
  calibratedMidpoint2ADValue <- getInt16
  calibratedMidpoint3ADValue <- getInt16
  calibratedSpanADValue <- getInt16
  calibrated0EUValue <- getIeeeFloat32
  calibratedMidpoint1EUValue <- getIeeeFloat32
  calibratedMidpoint2EUValue <- getIeeeFloat32
  calibratedMidpoint3EUValue <- getIeeeFloat32
  calibratedSpanEUValue <- getIeeeFloat32
  offset <- getIeeeFloat32
  calibrationSetEUValue <- getIeeeFloat32
  manualEU <- getIeeeFloat32
  timer <- getWord16le
  calibrationMode <- getWord8
  calibrationType <- getWord8
  
  return $ PointType3 pointId timeon scanPeriod fltr adjustedDA0 adjustedDA100 lowReading highReading lowAlarm highAlarm lowLowAlarm highHighAlarm rateAlarm alarmDeadband filteredEU cfg alarmCode rawDAInput actualScanTime faultEUValue calibrated0ADValue calibratedMidpoint1ADValue calibratedMidpoint2ADValue calibratedMidpoint3ADValue calibratedSpanADValue calibrated0EUValue calibratedMidpoint1EUValue calibratedMidpoint2EUValue calibratedMidpoint3EUValue calibratedSpanEUValue offset calibrationSetEUValue manualEU timer calibrationMode calibrationType


