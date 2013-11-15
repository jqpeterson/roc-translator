{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType42 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float

data PointType42 = PointType42 {
 pointType42PointTag                           :: !PointType42PointTag                                           
,pointType42FlowToday                          :: !PointType42FlowToday                                                   
,pointType42FlowYesterday                      :: !PointType42FlowYesterday                                                    
,pointType42FlowMonth                          :: !PointType42FlowMonth                                                           
,pointType42FlowPrvsMonth                      :: !PointType42FlowPrvsMonth                                            
,pointType42FlowAccum                          :: !PointType42FlowAccum                                                   
,pointType42MinutesToday                       :: !PointType42MinutesToday                                                    
,pointType42MinutesYesteray                    :: !PointType42MinutesYesteray                                           
,pointType42MinutesMonth                       :: !PointType42MinutesMonth                                            
,pointType42MinutesPrvsMonth                   :: !PointType42MinutesPrvsMonth                                                   
,pointType42MinutesAccum                       :: !PointType42MinutesAccum                                                    
,pointType42EnergyToday                        :: !PointType42EnergyToday                                                           
,pointType42EnergyYesterday                    :: !PointType42EnergyYesterday                                            
,pointType42EnergyMonth                        :: !PointType42EnergyMonth                                                                                   
,pointType42EnergyPrvsMonth                    :: !PointType42EnergyPrvsMonth                                         
,pointType42EnergyAccum                        :: !PointType42EnergyAccum                          
,pointType42UncrtdToday                        :: !PointType42UncrtdToday                                 
,pointType42UncrtdYesterday                    :: !PointType42UncrtdYesterday                                  
,pointType42UncrtdMonth                        :: !PointType42UncrtdMonth                                         
,pointType42UncrtdPrvsMonth                    :: !PointType42UncrtdPrvsMonth                          
,pointType42UncrtdAccum                        :: !PointType42UncrtdAccum                                 
,pointType42OrPlateBoreDiam                    :: !PointType42OrPlateBoreDiam                                  
,pointType42MtrTubeIntDiamatFlowingTemp        :: !PointType42MtrTubeIntDiamatFlowingTemp                                 
,pointType42BetaDiamRatio                      :: !PointType42BetaDiamRatio                          
,pointType42EvApproachVelocity                 :: !PointType42EvApproachVelocity                                 
,pointType42CdDischargeCoeff                   :: !PointType42CdDischargeCoeff                                  
,pointType42ReynoldsNum                        :: !PointType42ReynoldsNum                                         
,pointType42UpStrAbsoluteStaticPress           :: !PointType42UpStrAbsoluteStaticPress                          
,pointType42MolecularWeight                    :: !PointType42MolecularWeight                                 

} deriving (Read,Eq, Show, Generic)                       

type PointType42PointTag                        = BS.ByteString                                           
type PointType42FlowToday                       = Float                                          
type PointType42FlowYesterday                   = Float                                         
type PointType42FlowMonth                       = Float                                         
type PointType42FlowPrvsMonth                   = Float                                           
type PointType42FlowAccum                       = Float                                           
type PointType42MinutesToday                    = Float                                            
type PointType42MinutesYesteray                 = Float                                            
type PointType42MinutesMonth                    = Float                                           
type PointType42MinutesPrvsMonth                = Float                                           
type PointType42MinutesAccum                    = Float                                            
type PointType42EnergyToday                     = Float                                            
type PointType42EnergyYesterday                 = Float                                           
type PointType42EnergyMonth                     = Float                                           
type PointType42EnergyPrvsMonth                 = Float                                         
type PointType42EnergyAccum                     = Float                                           
type PointType42UncrtdToday                     = Float                                     
type PointType42UncrtdYesterday                 = Float                                         
type PointType42UncrtdMonth                     = Float                                         
type PointType42UncrtdPrvsMonth                 = Float                                           
type PointType42UncrtdAccum                     = Float                                     
type PointType42OrPlateBoreDiam                 = Float                                         
type PointType42MtrTubeIntDiamatFlowingTemp     = Float                                         
type PointType42BetaDiamRatio                   = Float                                           
type PointType42EvApproachVelocity              = Float                                     
type PointType42CdDischargeCoeff                = Float                                         
type PointType42ReynoldsNum                     = Float                                         
type PointType42UpStrAbsoluteStaticPress        = Float                                           
type PointType42MolecularWeight                 = Float                                     
  
pointType42Parser :: Get PointType42 
pointType42Parser = do 
         
  pointTag <- getByteString 10                                
  flowToday <- getIeeeFloat32                             
  flowYesterday <- getIeeeFloat32                       
  flowMonth <- getIeeeFloat32                               
  flowPrvsMonth <- getIeeeFloat32                           
  flowAccum <- getIeeeFloat32                             
  minutesToday <- getIeeeFloat32                          
  minutesYesteray <- getIeeeFloat32                         
  minutesMonth <- getIeeeFloat32                            
  minutesPrvsMonth <- getIeeeFloat32                      
  minutesAccum <- getIeeeFloat32                        
  energyToday <- getIeeeFloat32                             
  energyYesterday <- getIeeeFloat32                         
  energyMonth <- getIeeeFloat32                           
  energyPrvsMonth <- getIeeeFloat32                     
  energyAccum <- getIeeeFloat32                             
  uncrtdToday <- getIeeeFloat32                             
  uncrtdYesterday <- getIeeeFloat32                       
  uncrtdMonth <- getIeeeFloat32                         
  uncrtdPrvsMonth <- getIeeeFloat32                         
  uncrtdAccum <- getIeeeFloat32                             
  orPlateBoreDiam <- getIeeeFloat32                       
  mtrTubeIntDiamatFlowingTemp <- getIeeeFloat32         
  betaDiamRatio <- getIeeeFloat32                           
  evApproachVelocity <- getIeeeFloat32                      
  cdDischargeCoeff <- getIeeeFloat32                      
  reynoldsNum <- getIeeeFloat32                         
  upStrAbsoluteStaticPress <- getIeeeFloat32                
  molecularWeight <- getIeeeFloat32                         
       
  return $ PointType42 pointTag flowToday flowYesterday flowMonth flowPrvsMonth flowAccum minutesToday minutesYesteray minutesMonth minutesPrvsMonth minutesAccum energyToday 
    energyYesterday energyMonth energyPrvsMonth energyAccum uncrtdToday uncrtdYesterday uncrtdMonth uncrtdPrvsMonth uncrtdAccum orPlateBoreDiam mtrTubeIntDiamatFlowingTemp 
    betaDiamRatio evApproachVelocity cdDischargeCoeff reynoldsNum upStrAbsoluteStaticPress molecularWeight  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  