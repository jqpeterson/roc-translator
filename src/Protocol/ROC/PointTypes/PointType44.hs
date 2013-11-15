{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType44 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Int
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.PointTypes.Utils

data PointType44 = PointType44 {
 pointType44PointTag                :: !PointType44PointTag                                      
,pointType44PwrStatus               :: !PointType44PwrStatus                                               
,pointType44PwrMode                 :: !PointType44PwrMode                                                    
,pointType44ValidRX                 :: !PointType44ValidRX                                                       
,pointType44StartTime1              :: !PointType44StartTime1                                           
,pointType44StartTime2              :: !PointType44StartTime2                                              
,pointType44StartTime3              :: !PointType44StartTime3                                                   
,pointType44OnTime1                 :: !PointType44OnTime1                                             
,pointType44OnTime2                 :: !PointType44OnTime2                                          
,pointType44OnTime3                 :: !PointType44OnTime3                                                    
,pointType44OffTime1                :: !PointType44OffTime1                                                   
,pointType44OffTime2                :: !PointType44OffTime2                                                         
,pointType44OffTime3                :: !PointType44OffTime3                                              
,pointType44ActiveTimeZone          :: !PointType44ActiveTimeZone                                                                                 
,pointType44HoldTime                :: !PointType44HoldTime                                           
,pointType44PwrTime                 :: !PointType44PwrTime                        
,pointType44DONum                   :: !PointType44DONum                               
,pointType44LowBattery              :: !PointType44LowBattery                                    
,pointType44OnCounter               :: !PointType44OnCounter                                       
,pointType44OffCounter              :: !PointType44OffCounter                            

} deriving (Read,Eq, Show, Generic)                       

type PointType44PointTag             =  BS.ByteString                                                
type PointType44PwrStatus            =  Int16                                       
type PointType44PwrMode              =  Int16                                      
type PointType44ValidRX              =  Int16                                      
type PointType44StartTime1           =  Int16                                        
type PointType44StartTime2           =  Int16                                        
type PointType44StartTime3           =  Int16                                         
type PointType44OnTime1              =  Int16                                         
type PointType44OnTime2              =  Int16                                        
type PointType44OnTime3              =  Int16                                        
type PointType44OffTime1             =  Int16                                         
type PointType44OffTime2             =  Int16                                         
type PointType44OffTime3             =  Int16                                        
type PointType44ActiveTimeZone       =  Int16                                        
type PointType44HoldTime             =  Int16                                      
type PointType44PwrTime              =  Int16                                        
type PointType44DONum                =  Int16                                  
type PointType44LowBattery           =  Float                                      
type PointType44OnCounter            =  Word32                                      
type PointType44OffCounter           =  Word32                                        
  
pointType44Parser :: Get PointType44 
pointType44Parser = do 
         
  pointTag <- getByteString 10                                       
  pwrStatus <- getInt16                                  
  pwrMode <- getInt16                                  
  validRX <- getInt16                                      
  startTime1 <- getInt16                                   
  startTime2 <- getInt16                                 
  startTime3 <- getInt16                                 
  onTime1 <- getInt16                                      
  onTime2 <- getInt16                                      
  onTime3 <- getInt16                                    
  offTime1 <- getInt16                                 
  offTime2 <- getInt16                                     
  offTime3 <- getInt16                                     
  activeTimeZone <- getInt16                             
  holdTime <- getInt16                                 
  pwrTime <- getInt16                                      
  dONum <- getInt16                                        
  lowBattery <- getIeeeFloat32                                 
  onCounter <- getWord32le                                
  offCounter <- getWord32le                                   
       
  return $ PointType44 pointTag pwrStatus pwrMode validRX startTime1 startTime2 startTime3 onTime1 onTime2 onTime3 offTime1 offTime2 offTime3 activeTimeZone holdTime pwrTime 
    dONum lowBattery onCounter offCounter
