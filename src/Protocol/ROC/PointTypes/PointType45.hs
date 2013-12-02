{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType45 where

import GHC.Generics
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.Utils

data PointType45 = PointType45 {
  
 pointType45CalibOption                     :: !PointType45CalibOption                               
,pointType45AmbTempMercury                  :: !PointType45AmbTempMercury                                          
,pointType45TempMercuryWhenCalib            :: !PointType45TempMercuryWhenCalib                                             
,pointType45CalibWeightGravAccel            :: !PointType45CalibWeightGravAccel                                                
,pointType45WaterTempWhenCalib              :: !PointType45WaterTempWhenCalib                                       
,pointType45AirTempWhenCalib                :: !PointType45AirTempWhenCalib                                          
,pointType45UserCorrectionFactor            :: !PointType45UserCorrectionFactor                                               
,pointType45SamplerEnable                   :: !PointType45SamplerEnable                                      
,pointType45SamplerAccumTrigger             :: !PointType45SamplerAccumTrigger                                   
,pointType45SamplerDuration                 :: !PointType45SamplerDuration                                             
,pointType45SensorModIntegrityAlarmCode     :: !PointType45SensorModIntegrityAlarmCode                                             
,pointType45SMIntegrityDeadbandTime         :: !PointType45SMIntegrityDeadbandTime                                                   
,pointType45SMAlarmControl                  :: !PointType45SMAlarmControl                                        
,pointType45IntegrityLevelTb                :: !PointType45IntegrityLevelTb                                                                                 
,pointType45TLPForSampler                   :: !PointType45TLPForSampler                                     

} deriving (Read,Eq, Show, Generic)                       

type PointType45CalibOption                  = Word8                                                             
type PointType45AmbTempMercury               = Float                                            
type PointType45TempMercuryWhenCalib         = Float                                           
type PointType45CalibWeightGravAccel         = Float                                           
type PointType45WaterTempWhenCalib           = Float                                             
type PointType45AirTempWhenCalib             = Float                                             
type PointType45UserCorrectionFactor         = Float                                              
type PointType45SamplerEnable                = Word8                                              
type PointType45SamplerAccumTrigger          = Float                                             
type PointType45SamplerDuration              = Float                                             
type PointType45SensorModIntegrityAlarmCode  = Word8                                              
type PointType45SMIntegrityDeadbandTime      = Word16                                              
type PointType45SMAlarmControl               = Word8                                             
type PointType45IntegrityLevelTb             = Word8                                             
type PointType45TLPForSampler                = [Word8]                                           
  
pointType45Parser :: Get PointType45 
pointType45Parser = do 
         
  calibOption <- getWord8                                                          
  ambTempMercury <- getIeeeFloat32                                           
  tempMercuryWhenCalib <- getIeeeFloat32                                   
  calibWeightGravAccel <- getIeeeFloat32                                       
  waterTempWhenCalib <- getIeeeFloat32                                         
  airTempWhenCalib <- getIeeeFloat32                                         
  userCorrectionFactor <- getIeeeFloat32                                     
  samplerEnable <- getWord8                                              
  samplerAccumTrigger <- getIeeeFloat32                                        
  samplerDuration <- getIeeeFloat32                                          
  sensorModIntegrityAlarmCode <- getWord8                            
  sMIntegrityDeadbandTime <- getWord16le                                    
  sMAlarmControl <- getWord8                                             
  integrityLevelTb <- getWord8                                         
  tLPForSampler <- getTLP                                          
                               
  return $ PointType45 calibOption ambTempMercury tempMercuryWhenCalib calibWeightGravAccel waterTempWhenCalib airTempWhenCalib userCorrectionFactor samplerEnable 
    samplerAccumTrigger samplerDuration sensorModIntegrityAlarmCode sMIntegrityDeadbandTime sMAlarmControl integrityLevelTb tLPForSampler