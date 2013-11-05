{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType6 where

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

data PointType6 = PointType6 {
 pointType6PntTag                                  :: !PointType6PntTag                                      
,pointType6CtrlType                                :: !PointType6CtrlType                                       
,pointType6SwitchStatus                            :: !PointType6SwitchStatus                                    
,pointType6ActualLoopPeriod                        :: !PointType6ActualLoopPeriod                                     
,pointType6PrimInputPnt                            :: !PointType6PrimInputPnt                                        
,pointType6PrimOutputPIDOutput                     :: !PointType6PrimOutputPIDOutput                                       
,pointType6PrimSwitchStpnt                         :: !PointType6PrimSwitchStpnt                                   
,pointType6PrimSwitchProcVar                       :: !PointType6PrimSwitchProcVar                                  
,pointType6PrimSwitchMode                          :: !PointType6PrimSwitchMode                                      
,pointType6OvrdInputPnt                            :: !PointType6OvrdInputPnt                                           
,pointType6OvrdInputPntPID2ndOutput                :: !PointType6OvrdInputPntPID2ndOutput                             
,pointType6OvrdSwitchStpnt                         :: !PointType6OvrdSwitchStpnt                               
,pointType6OvrdSwitchProcVar                       :: !PointType6OvrdSwitchProcVar                                  
,pointType6OvrdSwitchMode                          :: !PointType6OvrdSwitchMode                                     
,pointType6PrimStpnt                               :: !PointType6PrimStpnt                                           
,pointType6PrimStpntMinChangeMax                   :: !PointType6PrimStpntMinChangeMax                            
,pointType6PrimLoopPeriod                          :: !PointType6PrimLoopPeriod                                    
,pointType6PrimPropGain                            :: !PointType6PrimPropGain                                        
,pointType6PrimResetIntegralGain                   :: !PointType6PrimResetIntegralGain                             
,pointType6PrimRateDerivativeGain                  :: !PointType6PrimRateDerivativeGain                            
,pointType6PrimScaleFactor                         :: !PointType6PrimScaleFactor                                       
,pointType6PrimIntegralDeadband                    :: !PointType6PrimIntegralDeadband                            
,pointType6PrimProcVar                             :: !PointType6PrimProcVar                                   
,pointType6PrimOutputCurrentPIDOutput              :: !PointType6PrimOutputCurrentPIDOutput                    
,pointType6PrimSwitchProcVarPrimChangeOutput       :: !PointType6PrimSwitchProcVarPrimChangeOutput             
,pointType6MinCtrlTime                             :: !PointType6MinCtrlTime                                   
,pointType6OvrdStpnt                               :: !PointType6OvrdStpnt                                     
,pointType6OvrdStpntMinChangeMax                   :: !PointType6OvrdStpntMinChangeMax                         
,pointType6OvrdLoopPeriod                          :: !PointType6OvrdLoopPeriod                                
,pointType6OvrdPropGain                            :: !PointType6OvrdPropGain                                  
,pointType6OvrdResetIntegralGain                   :: !PointType6OvrdResetIntegralGain                         
,pointType6OvrdRateDerivativeGain                  :: !PointType6OvrdRateDerivativeGain                        
,pointType6OvrdScaleFactor                         :: !PointType6OvrdScaleFactor                               
,pointType6OvrdIntegralDeadband                    :: !PointType6OvrdIntegralDeadband                          
,pointType6OvrdProcVar                             :: !PointType6OvrdProcVar                                   
,pointType6OvrdOutputCurrentPIDOutput              :: !PointType6OvrdOutputCurrentPIDOutput                    
,pointType6OvrdSwitchProcVarOvrdChangeOutput       :: !PointType6OvrdSwitchProcVarOvrdChangeOutput                             


} deriving (Read,Eq, Show, Generic)                         
  
type PointType6PntTag                              = BS.ByteString
type PointType6CtrlType                            = Word8
type PointType6SwitchStatus                        = Word8
type PointType6ActualLoopPeriod                    = Word16
type PointType6PrimInputPnt                        = BS.ByteString
type PointType6PrimOutputPIDOutput                 = BS.ByteString
type PointType6PrimSwitchStpnt                     = Float
type PointType6PrimSwitchProcVar                   = BS.ByteString
type PointType6PrimSwitchMode                      = BS.ByteString
type PointType6OvrdInputPnt                        = BS.ByteString
type PointType6OvrdInputPntPID2ndOutput            = BS.ByteString
type PointType6OvrdSwitchStpnt                     = Float
type PointType6OvrdSwitchProcVar                   = BS.ByteString
type PointType6OvrdSwitchMode                      = BS.ByteString
type PointType6PrimStpnt                           = Float
type PointType6PrimStpntMinChangeMax               = Float
type PointType6PrimLoopPeriod                      = Word16
type PointType6PrimPropGain                        = Float
type PointType6PrimResetIntegralGain               = Float
type PointType6PrimRateDerivativeGain              = Float
type PointType6PrimScaleFactor                     = Float
type PointType6PrimIntegralDeadband                = Float
type PointType6PrimProcVar                         = Float
type PointType6PrimOutputCurrentPIDOutput          = Float
type PointType6PrimSwitchProcVarPrimChangeOutput   = Float
type PointType6MinCtrlTime                         = Word16
type PointType6OvrdStpnt                           = Float
type PointType6OvrdStpntMinChangeMax               = Float
type PointType6OvrdLoopPeriod                      = Word16
type PointType6OvrdPropGain                        = Float
type PointType6OvrdResetIntegralGain               = Float
type PointType6OvrdRateDerivativeGain              = Float
type PointType6OvrdScaleFactor                     = Float
type PointType6OvrdIntegralDeadband                = Float
type PointType6OvrdProcVar                         = Float
type PointType6OvrdOutputCurrentPIDOutput          = Float
type PointType6OvrdSwitchProcVarOvrdChangeOutput   = Float


pointType6Parser :: Get PointType6 
pointType6Parser = do 
  id <- getByteString 10 
  ctrlType <- getWord8  
  switchStatus <- getWord8
  actualLoopPeriod <- getWord16le
  primInputPnt <- getByteString 3
  primOutputPIDOutput <- getByteString 3
  primSwitchStpnt <- getIeeeFloat32
  primSwitchProcVar <- getByteString 3
  primSwitchMode <- getByteString 1
  ovrdInputPnt <- getByteString 3
  ovrdInputPntPID2ndOutput <- getByteString 3
  ovrdSwitchStpnt <- getIeeeFloat32
  ovrdSwitchProcVar <- getByteString 3
  ovrdSwitchMode <- getByteString 1
  primStpnt <- getIeeeFloat32
  primStpntMinChangeMax <- getIeeeFloat32
  primLoopPeriod <- getWord16le
  primPropGain <- getIeeeFloat32
  primResetIntegralGain <- getIeeeFloat32
  primRateDerivativeGain <- getIeeeFloat32
  primScaleFactor <- getIeeeFloat32
  primIntegralDeadband <- getIeeeFloat32
  primProcVar <- getIeeeFloat32
  primOutputCurrentPIDOutput <- getIeeeFloat32
  primSwitchProcVarPrimChangeOutput <- getIeeeFloat32
  minCtrlTime <- getWord16le
  ovrdStpnt <- getIeeeFloat32
  ovrdStpntMinChangeMax <- getIeeeFloat32
  ovrdLoopPeriod <- getWord16le
  ovrdPropGain <- getIeeeFloat32
  ovrdResetIntegralGain <- getIeeeFloat32
  ovrdRateDerivativGain <- getIeeeFloat32
  ovrdScaleFactor <- getIeeeFloat32
  ovrdIntegralDeadband <- getIeeeFloat32
  ovrdProcVar <- getIeeeFloat32
  ovrdOutputCurrentPIDOutput <- getIeeeFloat32
  ovrdSwitchProcVarOvrdChangeOutput <- getIeeeFloat32
  
  
  return $ PointType6 id ctrlType switchStatus actualLoopPeriod primInputPnt primOutputPIDOutput primSwitchStpnt primSwitchProcVar primSwitchMode ovrdInputPnt ovrdInputPntPID2ndOutput 
                  ovrdSwitchStpnt ovrdSwitchProcVar ovrdSwitchMode primStpnt primStpntMinChangeMax primLoopPeriod primPropGain primResetIntegralGain primRateDerivativeGain primScaleFactor 
                  primIntegralDeadband primProcVar primOutputCurrentPIDOutput primSwitchProcVarPrimChangeOutput minCtrlTime ovrdStpnt ovrdStpntMinChangeMax ovrdLoopPeriod ovrdPropGain 
                  ovrdResetIntegralGain ovrdRateDerivativGain ovrdScaleFactor ovrdIntegralDeadband ovrdProcVar ovrdOutputCurrentPIDOutput ovrdSwitchProcVarOvrdChangeOutput 





