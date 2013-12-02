{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType48 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.Utils

data PointType48 = PointType48 {
 
 pointType48PointTag                            :: !PointType48PointTag                       
,pointType48ControlType                         :: !PointType48ControlType                               
,pointType48ActiveLoopStatus                    :: !PointType48ActiveLoopStatus                            
,pointType48LoopPeriod                          :: !PointType48LoopPeriod                         
,pointType48ActualLoopPeriod                    :: !PointType48ActualLoopPeriod                             
,pointType48PrimPVInputPnt                      :: !PointType48PrimPVInputPnt                              
,pointType48PrimStpnt                           :: !PointType48PrimStpnt                               
,pointType48PrimStpntChangeMax                  :: !PointType48PrimStpntChangeMax                                
,pointType48PrimPorpGain                        :: !PointType48PrimPorpGain                                   
,pointType48PrimResetIntGain                    :: !PointType48PrimResetIntGain                              
,pointType48PrimRateDerivGain                   :: !PointType48PrimRateDerivGain                                  
,pointType48PrimScaleFactor                     :: !PointType48PrimScaleFactor                      
,pointType48PrimIntDeadband                     :: !PointType48PrimIntDeadband                               
,pointType48PrimProcessVar                      :: !PointType48PrimProcessVar                               
,pointType48PrimChangeOutput                    :: !PointType48PrimChangeOutput                            
,pointType48OvrdPVInputPnt                      :: !PointType48OvrdPVInputPnt                            
,pointType48OvrdStpnt                           :: !PointType48OvrdStpnt                            
,pointType48OvrdStpntChangeMax                  :: !PointType48OvrdStpntChangeMax                              
,pointType48OvrdPropGain                        :: !PointType48OvrdPropGain                              
,pointType48OvrdResetIntGain                    :: !PointType48OvrdResetIntGain                              
,pointType48OvrdRateDerivGain                   :: !PointType48OvrdRateDerivGain                              
,pointType48OvrdScaleFactor                     :: !PointType48OvrdScaleFactor                              
,pointType48OvrdIntDeadband                     :: !PointType48OvrdIntDeadband                              
,pointType48OvrdProcessVar                      :: !PointType48OvrdProcessVar                              
,pointType48OvrdChangeOutput                    :: !PointType48OvrdChangeOutput                              
,pointType48PIDCurrentOutput                    :: !PointType48PIDCurrentOutput                              
,pointType48PIDOutputPnt                        :: !PointType48PIDOutputPnt                               
,pointType48PID2ndOutput                        :: !PointType48PID2ndOutput                               
,pointType48OutputLowLimitValue                 :: !PointType48OutputLowLimitValue                             
,pointType48OutputHighLimitValue                :: !PointType48OutputHighLimitValue                             
,pointType48ControlLoopSelecion                 :: !PointType48ControlLoopSelecion                                   
,pointType48OvrdLoopThreshSwitch                :: !PointType48OvrdLoopThreshSwitch                              
,pointType48PrimLoopPVStpntUnits                :: !PointType48PrimLoopPVStpntUnits                              
,pointType48OvrdPVLoopStpntUnits                :: !PointType48OvrdPVLoopStpntUnits                              
,pointType48PIDOutputUnits                      :: !PointType48PIDOutputUnits                               
,pointType48PrimLoopProcessVarLowEu             :: !PointType48PrimLoopProcessVarLowEu                               
,pointType48PrimLoopProcessVarHighEu            :: !PointType48PrimLoopProcessVarHighEu                             
,pointType48OvrdLoopProcessVarLowEu             :: !PointType48OvrdLoopProcessVarLowEu                             
,pointType48OvrdLoopProcessVarHighEu            :: !PointType48OvrdLoopProcessVarHighEu                                   

} deriving (Read,Eq, Show, Generic)                       

type PointType48PointTag                         = BS.ByteString                              
type PointType48ControlType                      = Word8                                                                      
type PointType48ActiveLoopStatus                 = Word8                                                                             
type PointType48LoopPeriod                       = Float                                                                              
type PointType48ActualLoopPeriod                 = Float                                                                              
type PointType48PrimPVInputPnt                   = [Word8]                                                                              
type PointType48PrimStpnt                        = Float                                                                              
type PointType48PrimStpntChangeMax               = Float                                                                       
type PointType48PrimPorpGain                     = Float                                                                       
type PointType48PrimResetIntGain                 = Float                                                                       
type PointType48PrimRateDerivGain                = Float                                                                       
type PointType48PrimScaleFactor                  = Float                                                                       
type PointType48PrimIntDeadband                  = Float                                                                       
type PointType48PrimProcessVar                   = Float                                                                       
type PointType48PrimChangeOutput                 = Float                                                                       
type PointType48OvrdPVInputPnt                   = [Word8]                                                                       
type PointType48OvrdStpnt                        = Float                                                                       
type PointType48OvrdStpntChangeMax               = Float                                                                      
type PointType48OvrdPropGain                     = Float                                                                      
type PointType48OvrdResetIntGain                 = Float                                                                      
type PointType48OvrdRateDerivGain                = Float                                                                       
type PointType48OvrdScaleFactor                  = Float                                                                       
type PointType48OvrdIntDeadband                  = Float                                                                       
type PointType48OvrdProcessVar                   = Float                                                                       
type PointType48OvrdChangeOutput                 = Float                                                                       
type PointType48PIDCurrentOutput                 = Float                                                                              
type PointType48PIDOutputPnt                     = [Word8]                                                                              
type PointType48PID2ndOutput                     = [Word8]                                                                              
type PointType48OutputLowLimitValue              = Float                                                                              
type PointType48OutputHighLimitValue             = Float                                                                              
type PointType48ControlLoopSelecion              = Word8                                                                              
type PointType48OvrdLoopThreshSwitch             = Float                                                                       
type PointType48PrimLoopPVStpntUnits             = BS.ByteString                                                                       
type PointType48OvrdPVLoopStpntUnits             = BS.ByteString                                                                              
type PointType48PIDOutputUnits                   = BS.ByteString                                                                              
type PointType48PrimLoopProcessVarLowEu          = Float                                                                              
type PointType48PrimLoopProcessVarHighEu         = Float                                                                              
type PointType48OvrdLoopProcessVarLowEu          = Float                                                                              
type PointType48OvrdLoopProcessVarHighEu         = Float                                                                              
  
pointType48Parser :: Get PointType48 
pointType48Parser = do 
  
  pointTag <- getByteString 10                                                       
  controlType <- getWord8                                                            
  activeLoopStatus <- getWord8                                                               
  loopPeriod <- getIeeeFloat32                                                                      
  actualLoopPeriod <- getIeeeFloat32                                                              
  primPVInputPnt <- getTLP                                                        
  primStpnt <- getIeeeFloat32                                                                     
  primStpntChangeMax <- getIeeeFloat32                                                       
  primPorpGain <- getIeeeFloat32                                                             
  primResetIntGain <- getIeeeFloat32                                                         
  primRateDerivGain <- getIeeeFloat32                                                        
  primScaleFactor <- getIeeeFloat32                                                       
  primIntDeadband <- getIeeeFloat32                                                       
  primProcessVar <- getIeeeFloat32                                                        
  primChangeOutput <- getIeeeFloat32                                                      
  ovrdPVInputPnt <- getTLP                                                
  ovrdStpnt <- getIeeeFloat32                                                             
  ovrdStpntChangeMax <- getIeeeFloat32                                                    
  ovrdPropGain <- getIeeeFloat32                                                          
  ovrdResetIntGain <- getIeeeFloat32                                                      
  ovrdRateDerivGain <- getIeeeFloat32                                                     
  ovrdScaleFactor <- getIeeeFloat32                                                          
  ovrdIntDeadband <- getIeeeFloat32                                                          
  ovrdProcessVar <- getIeeeFloat32                                                           
  ovrdChangeOutput <- getIeeeFloat32                                                         
  pIDCurrentOutput <- getIeeeFloat32                                                         
  pIDOutputPnt <- getTLP                                                     
  pID2ndOutput <- getTLP                                                        
  outputLowLimitValue <- getIeeeFloat32                                                         
  outputHighLimitValue <- getIeeeFloat32                                                        
  controlLoopSelecion <- getWord8                                          
  ovrdLoopThreshSwitch <- getIeeeFloat32                                                     
  primLoopPVStpntUnits <- getByteString 10                                               
  ovrdPVLoopStpntUnits <- getByteString 10                                               
  pIDOutputUnits <- getByteString 10                                                     
  primLoopProcessVarLowEu <- getIeeeFloat32                                                         
  primLoopProcessVarHighEu <- getIeeeFloat32                                                        
  ovrdLoopProcessVarLowEu <- getIeeeFloat32                                                         
  ovrdLoopProcessVarHighEu <- getIeeeFloat32                                                        
          
  
  return $ PointType48 pointTag controlType activeLoopStatus loopPeriod actualLoopPeriod primPVInputPnt primStpnt primStpntChangeMax primPorpGain primResetIntGain primRateDerivGain 
    primScaleFactor primIntDeadband primProcessVar primChangeOutput ovrdPVInputPnt ovrdStpnt ovrdStpntChangeMax ovrdPropGain ovrdResetIntGain ovrdRateDerivGain ovrdScaleFactor 
    ovrdIntDeadband ovrdProcessVar ovrdChangeOutput pIDCurrentOutput pIDOutputPnt pID2ndOutput outputLowLimitValue outputHighLimitValue controlLoopSelecion ovrdLoopThreshSwitch 
    primLoopPVStpntUnits ovrdPVLoopStpntUnits pIDOutputUnits primLoopProcessVarLowEu primLoopProcessVarHighEu ovrdLoopProcessVarLowEu ovrdLoopProcessVarHighEu  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  