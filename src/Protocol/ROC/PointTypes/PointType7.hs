{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType7 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.PointTypes.Utils

data PointType7 = PointType7 {
 pointType7PointTag                   :: !PointType7PointTag                   
,pointType7Latitude                   :: !PointType7Latitude                          
,pointType7Elevation                  :: !PointType7Elevation                          
,pointType7CalcMethod                 :: !PointType7CalcMethod                              
,pointType7AGAConfig                  :: !PointType7AGAConfig                              
,pointType7SpecificGravity            :: !PointType7SpecificGravity                              
,pointType7HeatingValue               :: !PointType7HeatingValue                         
,pointType7GravityAcceleration        :: !PointType7GravityAcceleration                   
,pointType7ScanPeriod                 :: !PointType7ScanPeriod                             
,pointType7PipeDiameter               :: !PointType7PipeDiameter                             
,pointType7OrificeDiameter            :: !PointType7OrificeDiameter                         
,pointType7OrificeTemp                :: !PointType7OrificeTemp                      
,pointType7OrificeMaterial            :: !PointType7OrificeMaterial                       
,pointType7MeterRunPntDesc            :: !PointType7MeterRunPntDesc                       
,pointType7AlarmCode                  :: !PointType7AlarmCode                              
,pointType7LowAlarmFlow               :: !PointType7LowAlarmFlow                        
,pointType7HighAlarmFlow              :: !PointType7HighAlarmFlow                        
,pointType7Viscosity                  :: !PointType7Viscosity                              
,pointType7SpecificHeatRatio          :: !PointType7SpecificHeatRatio                    
,pointType7ContactBasePress           :: !PointType7ContactBasePress                     
,pointType7ContactBaseTemp            :: !PointType7ContactBaseTemp                          
,pointType7LowDPCutoffMeterFactor     :: !PointType7LowDPCutoffMeterFactor             
,pointType7UserCorrectionFactor       :: !PointType7UserCorrectionFactor      
,pointType7Nitrogen                   :: !PointType7Nitrogen                   
,pointType7CarbonDioxide              :: !PointType7CarbonDioxide                     
,pointType7HydrogenSulfide            :: !PointType7HydrogenSulfide                    
,pointType7Water                      :: !PointType7Water                                   
,pointType7Helium                     :: !PointType7Helium                                 
,pointType7Methane                    :: !PointType7Methane                                      
,pointType7Ethane                     :: !PointType7Ethane                               
,pointType7Propane                    :: !PointType7Propane                               
,pointType7nButane                    :: !PointType7nButane                                
,pointType7iButane                    :: !PointType7iButane                                  
,pointType7nPentane                   :: !PointType7nPentane                                
,pointType7iPentane                   :: !PointType7iPentane                         
,pointType7nHexane                    :: !PointType7nHexane                               
,pointType7nHeptane                   :: !PointType7nHeptane                              
,pointType7nOctane                    :: !PointType7nOctane                                
,pointType7nNonane                    :: !PointType7nNonane                             
,pointType7nDecane                    :: !PointType7nDecane                              
,pointType7Oxygen                     :: !PointType7Oxygen                                 
,pointType7CarbonMonoxide             :: !PointType7CarbonMonoxide                       
,pointType7Hydrogen                   :: !PointType7Hydrogen                             
,pointType7CalcUnits                  :: !PointType7CalcUnits                                
,pointType7EnableStackDP              :: !PointType7EnableStackDP                      
,pointType7LowDPInput                 :: !PointType7LowDPInput                           
,pointType7DPInputOrificeFRInput      :: !PointType7DPInputOrificeFRInput               
,pointType7StaticPressInput           :: !PointType7StaticPressInput                     
,pointType7TempInput                  :: !PointType7TempInput                              
,pointType7LowDPStpnt                 :: !PointType7LowDPStpnt                           
,pointType7HighDPStpnt                :: !PointType7HighDPStpnt                          
,pointType7MeterValueDP               :: !PointType7MeterValueDP                             
,pointType7StaticFlowingPressValue    :: !PointType7StaticFlowingPressValue            
,pointType7FlowingTempValue           :: !PointType7FlowingTempValue                     
} deriving (Read,Eq, Show, Generic)                       

type PointType7PointTag                      = BS.ByteString
type PointType7Latitude                      = Float
type PointType7Elevation                     = Float
type PointType7CalcMethod                    = Word8
type PointType7AGAConfig                     = Word8
type PointType7SpecificGravity               = Float
type PointType7HeatingValue                  = Float
type PointType7GravityAcceleration           = Float
type PointType7ScanPeriod                    = Word16
type PointType7PipeDiameter                  = Float
type PointType7OrificeDiameter               = Float
type PointType7OrificeTemp                   = Float
type PointType7OrificeMaterial               = Word8
type PointType7MeterRunPntDesc               = BS.ByteString
type PointType7AlarmCode                     = Word8
type PointType7LowAlarmFlow                  = Float
type PointType7HighAlarmFlow                 = Float
type PointType7Viscosity                     = Float
type PointType7SpecificHeatRatio             = Float
type PointType7ContactBasePress              = Float
type PointType7ContactBaseTemp               = Float
type PointType7LowDPCutoffMeterFactor        = Float
type PointType7UserCorrectionFactor          = Float
type PointType7Nitrogen                      = Float
type PointType7CarbonDioxide                 = Float
type PointType7HydrogenSulfide               = Float
type PointType7Water                         = Float
type PointType7Helium                        = Float
type PointType7Methane                       = Float
type PointType7Ethane                        = Float
type PointType7Propane                       = Float
type PointType7nButane                       = Float
type PointType7iButane                       = Float
type PointType7nPentane                      = Float
type PointType7iPentane                      = Float
type PointType7nHexane                       = Float
type PointType7nHeptane                      = Float
type PointType7nOctane                       = Float
type PointType7nNonane                       = Float
type PointType7nDecane                       = Float
type PointType7Oxygen                        = Float
type PointType7CarbonMonoxide                = Float
type PointType7Hydrogen                      = Float
type PointType7CalcUnits                     = Word8
type PointType7EnableStackDP                 = Bool
type PointType7LowDPInput                    = [Word8]                       
type PointType7DPInputOrificeFRInput         = [Word8]
type PointType7StaticPressInput              = [Word8]
type PointType7TempInput                     = [Word8]
type PointType7LowDPStpnt                    = Float
type PointType7HighDPStpnt                   = Float
type PointType7MeterValueDP                  = Float
type PointType7StaticFlowingPressValue       = Float
type PointType7FlowingTempValue              = Float
  
pointType7Parser :: Get PointType7 
pointType7Parser = do 
  pointId <- getByteString 10 
  latitude <- getIeeeFloat32
  elevation <- getIeeeFloat32
  calcMethod <- getWord8
  agaCFG <- getWord8
  specificGravity <- getIeeeFloat32
  heatingValue <- getIeeeFloat32
  gravityAccel <- getIeeeFloat32
  scanPeriod <- getWord16le
  pipeDiam <- getIeeeFloat32
  orificeDiam <- getIeeeFloat32
  orificeMeasuredTemp <- getIeeeFloat32
  orificeMat <- getWord8
  mtrRunPntDesc <- getByteString 30
  alarmCode <- getWord8
  lowAlarm <- getIeeeFloat32
  highAlarm <- getIeeeFloat32
  viscosity <- getIeeeFloat32
  specificHeatRatio <- getIeeeFloat32
  contactBasePress <- getIeeeFloat32
  contactBaseTemp <- getIeeeFloat32
  lowDPCutoffMeterFactor <- getIeeeFloat32
  userCorrectionFactor <- getIeeeFloat32
  nitrogen <- getIeeeFloat32
  carbonDioxide <- getIeeeFloat32
  hydrogenSulfide <- getIeeeFloat32
  water <- getIeeeFloat32
  helium <- getIeeeFloat32
  methane <- getIeeeFloat32
  ethane <- getIeeeFloat32
  propane <- getIeeeFloat32
  nButane <- getIeeeFloat32
  iButane <- getIeeeFloat32
  nPentane <- getIeeeFloat32
  iPentane <- getIeeeFloat32
  nHexane <- getIeeeFloat32
  nHeptane <- getIeeeFloat32
  nOctane <- getIeeeFloat32
  nNonane <- getIeeeFloat32
  nDecane <- getIeeeFloat32
  oxygen <- getIeeeFloat32
  carbonMonoxide <- getIeeeFloat32
  hydrogen <- getIeeeFloat32
  calcUnits <- getWord8
  enableStackedDP <- anyButNull
  lowDPInput <- getTLP
  dpInputFRInput <- getTLP
  staticPressInput <- getTLP
  tempPressInput <-getTLP
  lowDPStpnt <- getIeeeFloat32
  highDPStpnt <- getIeeeFloat32
  meterValueDPUncorrectedFR <- getIeeeFloat32
  staticFlowingPressValue <- getIeeeFloat32  
  flowingTempValue <- getIeeeFloat32
  
  return $ PointType7 pointId latitude elevation calcMethod agaCFG specificGravity heatingValue gravityAccel scanPeriod pipeDiam orificeDiam orificeMeasuredTemp orificeMat
    mtrRunPntDesc alarmCode lowAlarm highAlarm viscosity specificHeatRatio contactBasePress contactBaseTemp lowDPCutoffMeterFactor userCorrectionFactor nitrogen carbonDioxide  
    hydrogenSulfide water helium methane ethane propane nButane iButane nPentane iPentane nHexane nHeptane nOctane nNonane nDecane oxygen carbonMonoxide hydrogen calcUnits 
    enableStackedDP lowDPInput dpInputFRInput staticPressInput tempPressInput lowDPStpnt highDPStpnt meterValueDPUncorrectedFR staticFlowingPressValue flowingTempValue  