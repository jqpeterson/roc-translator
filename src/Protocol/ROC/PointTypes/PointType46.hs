{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType46 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.Utils

data PointType46 = PointType46 {
  
 pointType46PointTag                                  :: !PointType46PointTag                                                              
,pointType46PointDesc                                 :: !PointType46PointDesc                                                                     
,pointType46CalcMethod                                :: !PointType46CalcMethod                                                                          
,pointType46CalcMethodII                              :: !PointType46CalcMethodII                                                                         
,pointType46Options                                   :: !PointType46Options                                                                                
,pointType46ContractHour                              :: !PointType46ContractHour                                                                                        
,pointType46OrIntegrlOrTbBaseMultiPeriod              :: !PointType46OrIntegrlOrTbBaseMultiPeriod                                                                          
,pointType46PipeDiam                                  :: !PointType46PipeDiam                                                                                     
,pointType46PipeRefTemp                               :: !PointType46PipeRefTemp                                                                                 
,pointType46PipeMaterial                              :: !PointType46PipeMaterial                                                                                               
,pointType46OrDiam                                    :: !PointType46OrDiam                                                                                       
,pointType46OrRefTemp                                 :: !PointType46OrRefTemp                                                                         
,pointType46OrMaterial                                :: !PointType46OrMaterial                                    
,pointType46BaseOrContractPress                       :: !PointType46BaseOrContractPress                                                           
,pointType46BaseOrContractTemp                        :: !PointType46BaseOrContractTemp                                                                                
,pointType46AtmosPress                                :: !PointType46AtmosPress                                                                                
,pointType46SpecificGravity                           :: !PointType46SpecificGravity                                                                            
,pointType46HeatingValue                              :: !PointType46HeatingValue                                                                            
,pointType46Viscosity                                 :: !PointType46Viscosity                                                                                         
,pointType46SpecificHeatRatio                         :: !PointType46SpecificHeatRatio                                                                                 
,pointType46Eevation                                  :: !PointType46Eevation                                                                             
,pointType46Latitude                                  :: !PointType46Latitude                                                                                
,pointType46LocalGravAccel                            :: !PointType46LocalGravAccel                                                                                   
,pointType46Nitrogen                                  :: !PointType46Nitrogen                                                                                 
,pointType46CarbonDioxide                             :: !PointType46CarbonDioxide                                                                           
,pointType46HydrogenSulfide                           :: !PointType46HydrogenSulfide                                                                         
,pointType46Water                                     :: !PointType46Water                                                                                   
,pointType46Helium                                    :: !PointType46Helium                                                                             
,pointType46Methane                                   :: !PointType46Methane                                                                           
,pointType46Ethane                                    :: !PointType46Ethane                                    
,pointType46Propane                                   :: !PointType46Propane                                                                       
,pointType46nButane                                   :: !PointType46nButane                                                                            
,pointType46iButane                                   :: !PointType46iButane                                                                                                 
,pointType46nPentane                                  :: !PointType46nPentane                                                                                             
,pointType46iPentane                                  :: !PointType46iPentane                                                                                                    
,pointType46nHexane                                   :: !PointType46nHexane                                                                                                
,pointType46nHeptane                                  :: !PointType46nHeptane                                                                                                    
,pointType46nOctane                                   :: !PointType46nOctane                                                                                               
,pointType46nNonane                                   :: !PointType46nNonane                                                                                                       
,pointType46nDecane                                   :: !PointType46nDecane                                                                                                    
,pointType46Oxygen                                    :: !PointType46Oxygen                                                                                                          
,pointType46CarbonMonoxde                             :: !PointType46CarbonMonoxde                                                                                           
,pointType46Hydrogen                                  :: !PointType46Hydrogen                                                                                                       
,pointType46OrLowhwCutoffOrTbKFactor                  :: !PointType46OrLowhwCutoffOrTbKFactor                                                                                                    
,pointType46DPHighhwStpntOrLinearMtrLowFlowCutoff     :: !PointType46DPHighhwStpntOrLinearMtrLowFlowCutoff                                                                       
,pointType46DPLowhwStpnt                              :: !PointType46DPLowhwStpnt                                                                     
,pointType46DPEnabledStackedhw                        :: !PointType46DPEnabledStackedhw                                                                                      
,pointType46DPLowhwTLP                                :: !PointType46DPLowhwTLP                                                          
,pointType46OrhwOrTbUncrtdFlowRateTLP                 :: !PointType46OrhwOrTbUncrtdFlowRateTLP                                                                       
,pointType46PfStaticPressTLP                          :: !PointType46PfStaticPressTLP                                                                     
,pointType46TfFlowingTempTLP                          :: !PointType46TfFlowingTempTLP                                                                               
,pointType46OrDPhwOrTbUncrtdFlowRate                  :: !PointType46OrDPhwOrTbUncrtdFlowRate                                                                         
,pointType46PfFlowingPress                            :: !PointType46PfFlowingPress                                                                         
,pointType46TfFlowingTemp                             :: !PointType46TfFlowingTemp                                                            
,pointType46AlarmCode                                 :: !PointType46AlarmCode                                                                 
,pointType46LowAlarmFlow                              :: !PointType46LowAlarmFlow                                                                      
,pointType46HighAlarmFlow                             :: !PointType46HighAlarmFlow                                                                   
,pointType46AveragingTechnique                        :: !PointType46AveragingTechnique                                                                  
,pointType46FullRecalcFlag                            :: !PointType46FullRecalcFlag                                                                            
,pointType46TLPInputMultiKFactorCalc                  :: !PointType46TLPInputMultiKFactorCalc                                                                      
,pointType46DeadbandMultiKFactorCalc                  :: !PointType46DeadbandMultiKFactorCalc                                                                   
,pointType46LowestKFactorCalc                         :: !PointType46LowestKFactorCalc                                                                  
,pointType46SecondKFactorCalc                         :: !PointType46SecondKFactorCalc                                                                            
,pointType46ThirdKFactorCalc                          :: !PointType46ThirdKFactorCalc                                                                      
,pointType46FourthKFactorCalc                         :: !PointType46FourthKFactorCalc                                                                   
,pointType46HighestKFactorCalc                        :: !PointType46HighestKFactorCalc                                                                  
,pointType46LowestKFactorEU                           :: !PointType46LowestKFactorEU                                                                            
,pointType46SecondKFactorEU                           :: !PointType46SecondKFactorEU                                                                      
,pointType46ThirdKFactorEU                            :: !PointType46ThirdKFactorEU                                                                   
,pointType46FourthKFactorEU                           :: !PointType46FourthKFactorEU                                                                  
,pointType46HighestKFactorEU                          :: !PointType46HighestKFactorEU                                                                            
,pointType46SixthKFactorCalc                          :: !PointType46SixthKFactorCalc                                                                      
,pointType46SeventhKFactorCalc                        :: !PointType46SeventhKFactorCalc                                                                   
,pointType46EighthKFactorCalc                         :: !PointType46EighthKFactorCalc                                                                  
,pointType46NinthKFactorCalc                          :: !PointType46NinthKFactorCalc                                                                            
,pointType46TenthKFactorCalc                          :: !PointType46TenthKFactorCalc                                                                      
,pointType46EleventhKFactorCalc                       :: !PointType46EleventhKFactorCalc                                                                                      
,pointType46HighestKFactorCalc2                       :: !PointType46HighestKFactorCalc2                                                                                      
,pointType46SixthKFactorEU                            :: !PointType46SixthKFactorEU                                                                                      
,pointType46SeventhKFactorEU                          :: !PointType46SeventhKFactorEU                                                                                      
,pointType46EighthKFactorEU                           :: !PointType46EighthKFactorEU                                                                                      
,pointType46NinthKFactorEU                            :: !PointType46NinthKFactorEU                                                                                      
,pointType46TenthKFactorEU                            :: !PointType46TenthKFactorEU                                                                                      
,pointType46EleventhKFactorEU                         :: !PointType46EleventhKFactorEU                                                                                      
,pointType46HighestKFactorEU2                         :: !PointType46HighestKFactorEU2                                                                                      
,pointType46ArgonMolePercent                          :: !PointType46ArgonMolePercent                                                                                      
,pointType46CFGStatus                                 :: !PointType46CFGStatus                                                                                      
,pointType46LinearMtrSecWOPulsePriorNoFlow            :: !PointType46LinearMtrSecWOPulsePriorNoFlow                                                                                      
,pointType46OrType                                    :: !PointType46OrType                                                                                      
,pointType46CoeffOfDischarge                          :: !PointType46CoeffOfDischarge                                                                                      
,pointType46AlarmDeadband                             :: !PointType46AlarmDeadband                                                                                      
,pointType46PressLost                                 :: !PointType46PressLost                                                                                      
,pointType46JouleThompsonCoeff                        :: !PointType46JouleThompsonCoeff                                                                                      
,pointType46APIOptions                                :: !PointType46APIOptions                                                                                      

} deriving (Read,Eq, Show, Generic)                       

type PointType46PointTag                               = BS.ByteString                                            
type PointType46PointDesc                              = BS.ByteString                                 
type PointType46CalcMethod                             = Word8                                 
type PointType46CalcMethodII                           = Word8                                   
type PointType46Options                                = Word8                                 
type PointType46ContractHour                           = Word8                                 
type PointType46OrIntegrlOrTbBaseMultiPeriod           = Float                                
type PointType46PipeDiam                               = Float                                 
type PointType46PipeRefTemp                            = Float                                 
type PointType46PipeMaterial                           = Word8                                 
type PointType46OrDiam                                 = Float                                 
type PointType46OrRefTemp                              = Float                                 
type PointType46OrMaterial                             = Word8                               
type PointType46BaseOrContractPress                    = Float                                 
type PointType46BaseOrContractTemp                     = Float                                 
type PointType46AtmosPress                             = Float                                 
type PointType46SpecificGravity                        = Float                                 
type PointType46HeatingValue                           = Float                                   
type PointType46Viscosity                              = Float                                 
type PointType46SpecificHeatRatio                      = Float                                   
type PointType46Eevation                               = Float                                   
type PointType46Latitude                               = Float                                   
type PointType46LocalGravAccel                         = Float                                   
type PointType46Nitrogen                               = Float                                                          
type PointType46CarbonDioxide                          = Float                                     
type PointType46HydrogenSulfide                        = Float                                     
type PointType46Water                                  = Float                                     
type PointType46Helium                                 = Float                                     
type PointType46Methane                                = Float                                     
type PointType46Ethane                                 = Float                               
type PointType46Propane                                = Float                                     
type PointType46nButane                                = Float                                     
type PointType46iButane                                = Float                                     
type PointType46nPentane                               = Float                                     
type PointType46iPentane                               = Float                                     
type PointType46nHexane                                = Float                                     
type PointType46nHeptane                               = Float                                     
type PointType46nOctane                                = Float                                     
type PointType46nNonane                                = Float                                     
type PointType46nDecane                                = Float                                     
type PointType46Oxygen                                 = Float                                    
type PointType46CarbonMonoxde                          = Float                                     
type PointType46Hydrogen                               = Float                                     
type PointType46OrLowhwCutoffOrTbKFactor               = Float                                     
type PointType46DPHighhwStpntOrLinearMtrLowFlowCutoff  = Float                                     
type PointType46DPLowhwStpnt                           = Float                                     
type PointType46DPEnabledStackedhw                     = Bool                                    
type PointType46DPLowhwTLP                             = [Word8]                                    
type PointType46OrhwOrTbUncrtdFlowRateTLP              = [Word8]                                    
type PointType46PfStaticPressTLP                       = [Word8]                                    
type PointType46TfFlowingTempTLP                       = [Word8]                                    
type PointType46OrDPhwOrTbUncrtdFlowRate               = Float                                     
type PointType46PfFlowingPress                         = Float                                     
type PointType46TfFlowingTemp                          = Float                                 
type PointType46AlarmCode                              = Word8                                     
type PointType46LowAlarmFlow                           = Float                                     
type PointType46HighAlarmFlow                          = Float                                     
type PointType46AveragingTechnique                     = Word8                                     
type PointType46FullRecalcFlag                         = Bool                                     
type PointType46TLPInputMultiKFactorCalc               = [Word8]                                     
type PointType46DeadbandMultiKFactorCalc               = Float                                     
type PointType46LowestKFactorCalc                      = Float                                     
type PointType46SecondKFactorCalc                      = Float                                     
type PointType46ThirdKFactorCalc                       = Float                                     
type PointType46FourthKFactorCalc                      = Float                                     
type PointType46HighestKFactorCalc                     = Float                                    
type PointType46LowestKFactorEU                        = Float                                     
type PointType46SecondKFactorEU                        = Float                                     
type PointType46ThirdKFactorEU                         = Float                                     
type PointType46FourthKFactorEU                        = Float                                     
type PointType46HighestKFactorEU                       = Float                                     
type PointType46SixthKFactorCalc                       = Float                                    
type PointType46SeventhKFactorCalc                     = Float                                     
type PointType46EighthKFactorCalc                      = Float                                     
type PointType46NinthKFactorCalc                       = Float                                     
type PointType46TenthKFactorCalc                       = Float                                     
type PointType46EleventhKFactorCalc                    = Float                                     
type PointType46HighestKFactorCalc2                    = Float                                     
type PointType46SixthKFactorEU                         = Float                                   
type PointType46SeventhKFactorEU                       = Float                                    
type PointType46EighthKFactorEU                        = Float                                     
type PointType46NinthKFactorEU                         = Float                        
type PointType46TenthKFactorEU                         = Float                        
type PointType46EleventhKFactorEU                      = Float                        
type PointType46HighestKFactorEU2                      = Float                        
type PointType46ArgonMolePercent                       = Float                        
type PointType46CFGStatus                              = Word8                        
type PointType46LinearMtrSecWOPulsePriorNoFlow         = Word32                        
type PointType46OrType                                 = Word8                        
type PointType46CoeffOfDischarge                       = Float                        
type PointType46AlarmDeadband                          = Float                        
type PointType46PressLost                              = Float                        
type PointType46JouleThompsonCoeff                     = Float                        
type PointType46APIOptions                             = Word8                        
  
pointType46Parser :: Get PointType46 
pointType46Parser = do   
                                         
  pointTag <- getByteString 10                                                                 
  pointDesc <- getByteString 30                                                                
  calcMethod <- getWord8                                                                     
  calcMethodII <- getWord8                                                                   
  options <- getWord8                                                                        
  contractHour <- getWord8                                                            
  orIntegrlOrTbBaseMultiPeriod <- getIeeeFloat32                                           
  pipeDiam <- getIeeeFloat32                                                                       
  pipeRefTemp <- getIeeeFloat32                                                                      
  pipeMaterial <- getWord8                                                            
  orDiam <- getIeeeFloat32                                                                 
  orRefTemp <- getIeeeFloat32                                                                      
  orMaterial <- getWord8                                                                       
  baseOrContractPress <- getIeeeFloat32                                                     
  baseOrContractTemp <- getIeeeFloat32                                                     
  atmosPress <- getIeeeFloat32                                                                     
  specificGravity <- getIeeeFloat32                                                                  
  heatingValue <- getIeeeFloat32                                                            
  viscosity <- getIeeeFloat32                                                              
  specificHeatRatio <- getIeeeFloat32                                                              
  eevation <- getIeeeFloat32                                                                         
  latitude <- getIeeeFloat32                                                                
  localGravAccel <- getIeeeFloat32                                                         
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
  carbonMonoxde <- getIeeeFloat32                                
  hydrogen <- getIeeeFloat32                                     
  orLowhwCutoffOrTbKFactor <- getIeeeFloat32                     
  dPHighhwStpntOrLinearMtrLowFlowCutoff <- getIeeeFloat32           
  dPLowhwStpnt <- getIeeeFloat32                                 
  dPEnabledStackedhw <- anyButNull                           
  dPLowhwTLP <- getTLP                                 
  orhwOrTbUncrtdFlowRateTLP <- getTLP                   
  pfStaticPressTLP <- getTLP                            
  tfFlowingTempTLP <- getTLP                            
  orDPhwOrTbUncrtdFlowRate <- getIeeeFloat32                     
  pfFlowingPress <- getIeeeFloat32                               
  tfFlowingTemp <- getIeeeFloat32                                
  alarmCode <- getWord8                                   
  lowAlarmFlow <- getIeeeFloat32                                 
  highAlarmFlow <- getIeeeFloat32                                
  averagingTechnique <- getWord8                           
  fullRecalcFlag <- anyButNull                               
  tLPInputMultiKFactorCalc <- getTLP                    
  deadbandMultiKFactorCalc <- getIeeeFloat32                     
  lowestKFactorCalc <- getIeeeFloat32                            
  secondKFactorCalc <- getIeeeFloat32                            
  thirdKFactorCalc <- getIeeeFloat32                             
  fourthKFactorCalc <- getIeeeFloat32                            
  highestKFactorCalc <- getIeeeFloat32                           
  lowestKFactorEU <- getIeeeFloat32                              
  secondKFactorEU <- getIeeeFloat32                              
  thirdKFactorEU <- getIeeeFloat32                               
  fourthKFactorEU <- getIeeeFloat32                              
  highestKFactorEU <- getIeeeFloat32                             
  sixthKFactorCalc <- getIeeeFloat32                             
  seventhKFactorCalc <- getIeeeFloat32                           
  eighthKFactorCalc <- getIeeeFloat32                            
  ninthKFactorCalc <- getIeeeFloat32                             
  tenthKFactorCalc <- getIeeeFloat32                             
  eleventhKFactorCalc <- getIeeeFloat32                          
  highestKFactorCalc2 <- getIeeeFloat32                          
  sixthKFactorEU <- getIeeeFloat32                               
  seventhKFactorEU <- getIeeeFloat32                             
  eighthKFactorEU <- getIeeeFloat32                              
  ninthKFactorEU <- getIeeeFloat32                               
  tenthKFactorEU <- getIeeeFloat32                               
  eleventhKFactorEU <- getIeeeFloat32                            
  highestKFactorEU2 <- getIeeeFloat32                            
  argonMolePercent <- getIeeeFloat32                                                               
  cFGStatus <- getWord8                                                                        
  linearMtrSecWOPulsePriorNoFlow <- getWord32le                                         
  orType <- getWord8                                                                
  coeffOfDischarge <- getIeeeFloat32                                                              
  alarmDeadband <- getIeeeFloat32                                                                   
  pressLost <- getIeeeFloat32                                                                
  jouleThompsonCoeff <- getIeeeFloat32                                                      
  aPIOptions <- getWord8                                                                      
                                       
  return $ PointType46 pointTag pointDesc calcMethod calcMethodII options contractHour orIntegrlOrTbBaseMultiPeriod pipeDiam pipeRefTemp pipeMaterial orDiam orRefTemp 
    orMaterial baseOrContractPress baseOrContractTemp atmosPress specificGravity heatingValue viscosity specificHeatRatio eevation latitude localGravAccel nitrogen carbonDioxide 
    hydrogenSulfide water helium methane ethane propane nButane iButane nPentane iPentane nHexane nHeptane nOctane nNonane nDecane oxygen carbonMonoxde hydrogen 
    orLowhwCutoffOrTbKFactor dPHighhwStpntOrLinearMtrLowFlowCutoff dPLowhwStpnt dPEnabledStackedhw dPLowhwTLP orhwOrTbUncrtdFlowRateTLP pfStaticPressTLP tfFlowingTempTLP 
    orDPhwOrTbUncrtdFlowRate pfFlowingPress tfFlowingTemp alarmCode lowAlarmFlow highAlarmFlow averagingTechnique fullRecalcFlag tLPInputMultiKFactorCalc deadbandMultiKFactorCalc 
    lowestKFactorCalc secondKFactorCalc thirdKFactorCalc fourthKFactorCalc highestKFactorCalc lowestKFactorEU secondKFactorEU thirdKFactorEU fourthKFactorEU highestKFactorEU 
    sixthKFactorCalc seventhKFactorCalc eighthKFactorCalc ninthKFactorCalc tenthKFactorCalc eleventhKFactorCalc highestKFactorCalc2 sixthKFactorEU seventhKFactorEU eighthKFactorEU 
    ninthKFactorEU tenthKFactorEU eleventhKFactorEU highestKFactorEU2 argonMolePercent cFGStatus linearMtrSecWOPulsePriorNoFlow orType coeffOfDischarge alarmDeadband pressLost 
    jouleThompsonCoeff aPIOptions  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
