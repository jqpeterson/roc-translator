{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType40 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float

data PointType40 = PointType40 {
  
 pointType40SensorTag                 :: !PointType40SensorTag                            
,pointType40SensorAddress             :: !PointType40SensorAddress                                   
,pointType40SensorCFG                 :: !PointType40SensorCFG                                    
,pointType40PollMode                  :: !PointType40PollMode                                         
,pointType40InterfaceRev              :: !PointType40InterfaceRev                                        
,pointType40SensorStatus1             :: !PointType40SensorStatus1                                              
,pointType40SensorStatus2             :: !PointType40SensorStatus2                                     
,pointType40SensorVoltage             :: !PointType40SensorVoltage                                       
,pointType40DPReading                 :: !PointType40DPReading                                        
,pointType40StatisPressAPReading      :: !PointType40StatisPressAPReading                                          
,pointType40TemperaturePTReading      :: !PointType40TemperaturePTReading                                         
,pointType40DPReverseFlow             :: !PointType40DPReverseFlow                                  
,pointType40StatisPressEffect         :: !PointType40StatisPressEffect                                       
,pointType40DPMinCalibValue           :: !PointType40DPMinCalibValue                                       
,pointType40DPCalibMidPnt1            :: !PointType40DPCalibMidPnt1                                        
,pointType40DPCalibMidPnt2            :: !PointType40DPCalibMidPnt2                                     
,pointType40DPCalibMidPnt3            :: !PointType40DPCalibMidPnt3                                      
,pointType40DPMaxCalibValue           :: !PointType40DPMaxCalibValue                                        
,pointType40APMinCalibValue           :: !PointType40APMinCalibValue                                      
,pointType40APCalibMidPnt1            :: !PointType40APCalibMidPnt1                                      
,pointType40APCalibMidPnt2            :: !PointType40APCalibMidPnt2                                          
,pointType40APCalibMidPnt3            :: !PointType40APCalibMidPnt3                                    
,pointType40APMaxCalibValue           :: !PointType40APMaxCalibValue                                      
,pointType40PTMinCalibValue           :: !PointType40PTMinCalibValue                                   
,pointType40PTCalibMidPnt1            :: !PointType40PTCalibMidPnt1                                   
,pointType40PTCalibMidPnt2            :: !PointType40PTCalibMidPnt2                                   
,pointType40PTCalibMidPnt3            :: !PointType40PTCalibMidPnt3                                   
,pointType40PTMaxCalibValue           :: !PointType40PTMaxCalibValue                                   
,pointType40CalibCommand              :: !PointType40CalibCommand                                   
,pointType40CalibType                 :: !PointType40CalibType                                   
,pointType40CalibSetValue             :: !PointType40CalibSetValue                                   
,pointType40ManualDP                  :: !PointType40ManualDP                                   
,pointType40ManualAP                  :: !PointType40ManualAP                                   
,pointType40ManualPT                  :: !PointType40ManualPT                                   
,pointType40DPMode                    :: !PointType40DPMode                                   
,pointType40DPAlarmCode               :: !PointType40DPAlarmCode                                   
,pointType40DPLowAlarm                :: !PointType40DPLowAlarm                                   
,pointType40DPHighAlarm               :: !PointType40DPHighAlarm                                   
,pointType40DPDeadband                :: !PointType40DPDeadband                                   
,pointType40DPAlarmFaultValue         :: !PointType40DPAlarmFaultValue                                   
,pointType40APMode                    :: !PointType40APMode                                   
,pointType40APAlarmCode               :: !PointType40APAlarmCode                                   
,pointType40APLowAlarm                :: !PointType40APLowAlarm                                   
,pointType40APHighAlarm               :: !PointType40APHighAlarm                                   
,pointType40APDeadband                :: !PointType40APDeadband                                   
,pointType40APAlarmFaultValue         :: !PointType40APAlarmFaultValue                                   
,pointType40PTMode                    :: !PointType40PTMode                                   
,pointType40PTAlarmCode               :: !PointType40PTAlarmCode                                   
,pointType40PTLowAlarm                :: !PointType40PTLowAlarm                                   
,pointType40PTHighAlarm               :: !PointType40PTHighAlarm                                   
,pointType40PTDeadband                :: !PointType40PTDeadband                                   
,pointType40PTFaultValue              :: !PointType40PTFaultValue                                   
,pointType40PTBias                    :: !PointType40PTBias                                   
,pointType40APOffset                  :: !PointType40APOffset                                   

} deriving (Read,Eq, Show, Generic)                       

type PointType40SensorTag              = BS.ByteString                                       
type PointType40SensorAddress          = Word8                                       
type PointType40SensorCFG              = Word8                                       
type PointType40PollMode               = Word8                                       
type PointType40InterfaceRev           = Word8                                       
type PointType40SensorStatus1          = Word8                                       
type PointType40SensorStatus2          = Word8                                       
type PointType40SensorVoltage          = Float                                       
type PointType40DPReading              = Float                                       
type PointType40StatisPressAPReading   = Float                                       
type PointType40TemperaturePTReading   = Float                                       
type PointType40DPReverseFlow          = Float                                       
type PointType40StatisPressEffect      = Float                                       
type PointType40DPMinCalibValue        = Float                                       
type PointType40DPCalibMidPnt1         = Float                                       
type PointType40DPCalibMidPnt2         = Float                                       
type PointType40DPCalibMidPnt3         = Float                                       
type PointType40DPMaxCalibValue        = Float                                       
type PointType40APMinCalibValue        = Float                                       
type PointType40APCalibMidPnt1         = Float                                       
type PointType40APCalibMidPnt2         = Float                                       
type PointType40APCalibMidPnt3         = Float                                       
type PointType40APMaxCalibValue        = Float                                       
type PointType40PTMinCalibValue        = Float                                                              
type PointType40PTCalibMidPnt1         = Float                                         
type PointType40PTCalibMidPnt2         = Float                                         
type PointType40PTCalibMidPnt3         = Float                                         
type PointType40PTMaxCalibValue        = Float                                         
type PointType40CalibCommand           = Word8                                         
type PointType40CalibType              = Word8                                         
type PointType40CalibSetValue          = Float                                         
type PointType40ManualDP               = Float                                         
type PointType40ManualAP               = Float                                         
type PointType40ManualPT               = Float                                         
type PointType40DPMode                 = Word8                                         
type PointType40DPAlarmCode            = Word8                                         
type PointType40DPLowAlarm             = Float                                         
type PointType40DPHighAlarm            = Float                                         
type PointType40DPDeadband             = Float                                         
type PointType40DPAlarmFaultValue      = Float                                         
type PointType40APMode                 = Word8                                         
type PointType40APAlarmCode            = Word8                                         
type PointType40APLowAlarm             = Float                                         
type PointType40APHighAlarm            = Float                                         
type PointType40APDeadband             = Float                                         
type PointType40APAlarmFaultValue      = Float                                         
type PointType40PTMode                 = Word8                                         
type PointType40PTAlarmCode            = Word8                                         
type PointType40PTLowAlarm             = Float                                         
type PointType40PTHighAlarm            = Float                                         
type PointType40PTDeadband             = Float                                         
type PointType40PTFaultValue           = Float                                         
type PointType40PTBias                 = Float                                         
type PointType40APOffset               = Float                                     
  
pointType40Parser :: Get PointType40 
pointType40Parser = do   
  
  sensorTag <- getByteString 10               
  sensorAddress <- getWord8             
  sensorCFG <- getWord8                
  pollMode <- getWord8                 
  interfaceRev <- getWord8             
  sensorStatus1 <- getWord8            
  sensorStatus2 <- getWord8            
  sensorVoltage <- getIeeeFloat32            
  dPReading <- getIeeeFloat32                
  statisPressAPReading <- getIeeeFloat32     
  temperaturePTReading <- getIeeeFloat32     
  dPReverseFlow <- getIeeeFloat32            
  statisPressEffect <- getIeeeFloat32        
  dPMinCalibValue <- getIeeeFloat32          
  dPCalibMidPnt1 <- getIeeeFloat32           
  dPCalibMidPnt2 <- getIeeeFloat32           
  dPCalibMidPnt3 <- getIeeeFloat32           
  dPMaxCalibValue <- getIeeeFloat32          
  aPMinCalibValue <- getIeeeFloat32          
  aPCalibMidPnt1 <- getIeeeFloat32           
  aPCalibMidPnt2 <- getIeeeFloat32           
  aPCalibMidPnt3 <- getIeeeFloat32           
  aPMaxCalibValue <- getIeeeFloat32          
  pTMinCalibValue <- getIeeeFloat32          
  pTCalibMidPnt1 <- getIeeeFloat32           
  pTCalibMidPnt2 <- getIeeeFloat32           
  pTCalibMidPnt3 <- getIeeeFloat32           
  pTMaxCalibValue <- getIeeeFloat32          
  calibCommand <- getWord8             
  calibType <- getWord8                
  calibSetValue <- getIeeeFloat32            
  manualDP <- getIeeeFloat32                 
  manualAP <- getIeeeFloat32                 
  manualPT <- getIeeeFloat32                 
  dPMode <- getWord8                   
  dPAlarmCode <- getWord8              
  dPLowAlarm <- getIeeeFloat32               
  dPHighAlarm <- getIeeeFloat32              
  dPDeadband <- getIeeeFloat32               
  dPAlarmFaultValue <- getIeeeFloat32        
  aPMode <- getWord8                   
  aPAlarmCode <- getWord8              
  aPLowAlarm <- getIeeeFloat32               
  aPHighAlarm <- getIeeeFloat32              
  aPDeadband <- getIeeeFloat32               
  aPAlarmFaultValue <- getIeeeFloat32        
  pTMode <- getWord8                   
  pTAlarmCode <- getWord8              
  pTLowAlarm <- getIeeeFloat32               
  pTHighAlarm <- getIeeeFloat32              
  pTDeadband <- getIeeeFloat32               
  pTFaultValue <- getIeeeFloat32             
  pTBias <- getIeeeFloat32                   
  aPOffset <- getIeeeFloat32                 
  
  return $ PointType40 sensorTag sensorAddress sensorCFG pollMode interfaceRev sensorStatus1 sensorStatus2 sensorVoltage dPReading statisPressAPReading temperaturePTReading 
    dPReverseFlow statisPressEffect dPMinCalibValue dPCalibMidPnt1 dPCalibMidPnt2 dPCalibMidPnt3 dPMaxCalibValue aPMinCalibValue aPCalibMidPnt1 aPCalibMidPnt2 aPCalibMidPnt3 
    aPMaxCalibValue pTMinCalibValue pTCalibMidPnt1 pTCalibMidPnt2 pTCalibMidPnt3 pTMaxCalibValue calibCommand calibType calibSetValue manualDP manualAP manualPT dPMode 
    dPAlarmCode dPLowAlarm dPHighAlarm dPDeadband dPAlarmFaultValue aPMode aPAlarmCode aPLowAlarm aPHighAlarm aPDeadband aPAlarmFaultValue pTMode pTAlarmCode pTLowAlarm 
    pTHighAlarm pTDeadband pTFaultValue pTBias aPOffset
































