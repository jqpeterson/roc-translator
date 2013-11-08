{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType10 where

import GHC.Generics
import Data.Binary
import Protocol.ROC.Float

data PointType10 = PointType10 {
 pointType10OrMeterDPValueTbUncrtdFlow          :: !PointType10OrMeterDPValueTbUncrtdFlow             
,pointType10PfStFlowingPressValue               :: !PointType10PfStFlowingPressValue                  
,pointType10TfFlowingTempValue                  :: !PointType10TfFlowingTempValue                     
,pointType10InstantaneousFlow                   :: !PointType10InstantaneousFlow                      
,pointType10InstantaneousEnergy                 :: !PointType10InstantaneousEnergy                   
,pointType10FlowToday                           :: !PointType10FlowToday                              
,pointType10EnergyToday                         :: !PointType10EnergyToday                        
,pointType10FlowYesterday                       :: !PointType10FlowYesterday                          
,pointType10EnergyYesterday                     :: !PointType10EnergyYesterday                        
,pointType10OrPressExtTbUncrctdFR               :: !PointType10OrPressExtTbUncrctdFR                  
,pointType10OrIntegMultValueTbBaseMultValue     :: !PointType10OrIntegMultValueTbBaseMultValue        
,pointType10SampleTime                          :: !PointType10SampleTime                             
,pointType10OrExpFactorTbFpm                    :: !PointType10OrExpFactorTbFpm                       
,pointType10OrFrFnReynoldsNumber                :: !PointType10OrFrFnReynoldsNumber               
,pointType10OrFtfTbFtm                          :: !PointType10OrFtfTbFtm                             
,pointType10Fpv                                 :: !PointType10Fpv                                   
,pointType10Fgr                                 :: !PointType10Fgr                                    
,pointType10OrCdFbTbFtm                         :: !PointType10OrCdFbTbFtm                          
,pointType10Fpb                                 :: !PointType10Fpb                                 
,pointType10Ftb                                 :: !PointType10Ftb                                 
,pointType10OrFaEv                              :: !PointType10OrFaEv                              
--,pointType10FlowingMin                          :: !PointType10FlowingMin                          

} deriving (Read,Eq, Show, Generic)                       

type PointType10OrMeterDPValueTbUncrtdFlow       = Float                               
type PointType10PfStFlowingPressValue            = Float                               
type PointType10TfFlowingTempValue               = Float                               
type PointType10InstantaneousFlow                = Float                               
type PointType10InstantaneousEnergy              = Float                               
type PointType10FlowToday                        = Float                               
type PointType10EnergyToday                      = Float                               
type PointType10FlowYesterday                    = Float                               
type PointType10EnergyYesterday                  = Float                               
type PointType10OrPressExtTbUncrctdFR            = Float                               
type PointType10OrIntegMultValueTbBaseMultValue  = Float                               
type PointType10SampleTime                       = Float                               
type PointType10OrExpFactorTbFpm                 = Float                               
type PointType10OrFrFnReynoldsNumber             = Float                               
type PointType10OrFtfTbFtm                       = Float                               
type PointType10Fpv                              = Float                               
type PointType10Fgr                              = Float                               
type PointType10OrCdFbTbFtm                      = Float                               
type PointType10Fpb                              = Float                               
type PointType10Ftb                              = Float                               
type PointType10OrFaEv                           = Float                               
--type PointType10FlowingMin                       = Float                               

pointType10Parser :: Get PointType10
pointType10Parser = do 
  orMeterDPValueTbUncrtdFlow <- getIeeeFloat32 
  pfStFlowingPressValue <- getIeeeFloat32
  tfFlowingTempValue <- getIeeeFloat32
  instantaneousFlow <- getIeeeFloat32
  instantaneousEnergy <- getIeeeFloat32
  flowToday <- getIeeeFloat32
  energyToday <- getIeeeFloat32
  flowYesterday <- getIeeeFloat32
  energyYesterday <- getIeeeFloat32 
  orPressExtTbUncrctdFR <- getIeeeFloat32
  orIntegMultValueTbBaseMultValue <- getIeeeFloat32
  sampleTime <- getIeeeFloat32
  orExpFactorTbFpm <- getIeeeFloat32
  orFrFnReynoldsNumber <- getIeeeFloat32
  orFtfTbFtm <- getIeeeFloat32
  fpv <- getIeeeFloat32
  fgr <- getIeeeFloat32
  orCdFbTbFtm <- getIeeeFloat32
  fpb <- getIeeeFloat32  
  ftb <- getIeeeFloat32  
  orFaEv <- getIeeeFloat32
--  flowingMin <- getIeeeFloat32

  return $ PointType10 orMeterDPValueTbUncrtdFlow pfStFlowingPressValue tfFlowingTempValue instantaneousFlow instantaneousEnergy flowToday energyToday flowYesterday energyYesterday 
    orPressExtTbUncrctdFR orIntegMultValueTbBaseMultValue sampleTime orExpFactorTbFpm orFrFnReynoldsNumber orFtfTbFtm fpv fgr orCdFbTbFtm fpb ftb orFaEv --flowingMin  



