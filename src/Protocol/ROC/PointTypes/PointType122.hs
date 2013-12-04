{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType122 where

import GHC.Generics
import Data.Int
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Utils
import qualified Data.ByteString as BS


data PointType122 = PointType122 {
  
 pointType122PowerSwitch                         :: !PointType122PowerSwitch                          
,pointType122RSIEnable                           :: !PointType122RSIEnable                          
,pointType122ETCPEnable                          :: !PointType122ETCPEnable                          
,pointType122IXDEnable                           :: !PointType122IXDEnable                          
,pointType122RSIRunning                          :: !PointType122RSIRunning                          
,pointType122ETCPRunning                         :: !PointType122ETCPRunning                          
,pointType122IXDRunning                          :: !PointType122IXDRunning                          
,pointType122CleanStoredResources                :: !PointType122CleanStoredResources                          
,pointType122Resource1Name                       :: !PointType122Resource1Name                          
,pointType122Resource1Status                     :: !PointType122Resource1Status                          
,pointType122Resource1ProgrammedCycleTime        :: !PointType122Resource1ProgrammedCycleTime                          
,pointType122Resource1CurrentCycleTime           :: !PointType122Resource1CurrentCycleTime                          
,pointType122Resource2Name                       :: !PointType122Resource2Name                          
,pointType122Resource2Status                     :: !PointType122Resource2Status                          
,pointType122Resource2ProgrammedCycleTime        :: !PointType122Resource2ProgrammedCycleTime                         
,pointType122Resource2CurrentCycleTime           :: !PointType122Resource2CurrentCycleTime                          
,pointType122Resource3Name                       :: !PointType122Resource3Name                          
,pointType122Resource3Status                     :: !PointType122Resource3Status                          
,pointType122Resource3ProgrammedCycleTime        :: !PointType122Resource3ProgrammedCycleTime                          
,pointType122Resource3CurrentCycleTime           :: !PointType122Resource3CurrentCycleTime                          
,pointType122Resource4Name                       :: !PointType122Resource4Name                          
,pointType122Resource4Status                     :: !PointType122Resource4Status                          
,pointType122Resource4ProgrammedCycleTime        :: !PointType122Resource4ProgrammedCycleTime                          
,pointType122Resource4CurrentCycleTime           :: !PointType122Resource4CurrentCycleTime                          
,pointType122KernelStatus                        :: !PointType122KernelStatus                          
,pointType122ClearKernelCommand                  :: !PointType122ClearKernelCommand                          
,pointType122UserCProgramProgramIdentifier       :: !PointType122UserCProgramProgramIdentifier                          
,pointType122UserCProgramVersionString           :: !PointType122UserCProgramVersionString                          
,pointType122ProgramTimeDateStamp                :: !PointType122ProgramTimeDateStamp                          
                             
  
} deriving (Read,Eq, Show, Generic)                       
                                  
type PointType122PowerSwitch                     = Bool   
type PointType122RSIEnable                       = Bool 
type PointType122ETCPEnable                      = Bool  
type PointType122IXDEnable                       = Bool 
type PointType122RSIRunning                      = Bool  
type PointType122ETCPRunning                     = Bool   
type PointType122IXDRunning                      = Bool  
type PointType122CleanStoredResources            = Bool            
type PointType122Resource1Name                   = BS.ByteString     
type PointType122Resource1Status                 = Int8       
type PointType122Resource1ProgrammedCycleTime    = Word32                    
type PointType122Resource1CurrentCycleTime       = Word32                 
type PointType122Resource2Name                   = BS.ByteString     
type PointType122Resource2Status                 = Int8       
type PointType122Resource2ProgrammedCycleTime    = Word32                    
type PointType122Resource2CurrentCycleTime       = Word32                 
type PointType122Resource3Name                   = BS.ByteString     
type PointType122Resource3Status                 = Int8       
type PointType122Resource3ProgrammedCycleTime    = Word32                    
type PointType122Resource3CurrentCycleTime       = Word32                 
type PointType122Resource4Name                   = BS.ByteString     
type PointType122Resource4Status                 = Int8       
type PointType122Resource4ProgrammedCycleTime    = Word32                    
type PointType122Resource4CurrentCycleTime       = Word32                 
type PointType122KernelStatus                    = Word8    
type PointType122ClearKernelCommand              = Bool          
type PointType122UserCProgramProgramIdentifier   = BS.ByteString                     
type PointType122UserCProgramVersionString       = BS.ByteString                 
type PointType122ProgramTimeDateStamp            = Word32            

  
pointType122Parser :: Get PointType122
pointType122Parser = do 
 
  
  powerSwitch <- anyButNull                      
  rSIEnable <- anyButNull                        
  eTCPEnable <- anyButNull                       
  iXDEnable <- anyButNull                        
  rSIRunning <- anyButNull                       
  eTCPRunning <- anyButNull                      
  iXDRunning <- anyButNull                       
  cleanStoredResources <- anyButNull             
  resource1Name <- getByteString 20                    
  resource1Status <- getInt8                   
  resource1ProgrammedCycleTime <- getWord32le     
  resource1CurrentCycleTime <- getWord32le        
  resource2Name <- getByteString 20                    
  resource2Status <- getInt8                           
  resource2ProgrammedCycleTime <- getWord32le          
  resource2CurrentCycleTime <- getWord32le             
  resource3Name <- getByteString 20                    
  resource3Status <- getInt8                           
  resource3ProgrammedCycleTime <- getWord32le          
  resource3CurrentCycleTime <- getWord32le             
  resource4Name <- getByteString 20                    
  resource4Status <- getInt8                           
  resource4ProgrammedCycleTime <- getWord32le          
  resource4CurrentCycleTime <- getWord32le             
  kernelStatus <- getWord8                     
  clearKernelCommand <- anyButNull               
  userCProgramProgramIdentifier <- getByteString 20    
  userCProgramVersionString <- getByteString 12        
  programTimeDateStamp <- getWord32le                

  return $ PointType122 powerSwitch rSIEnable eTCPEnable iXDEnable rSIRunning eTCPRunning iXDRunning cleanStoredResources resource1Name resource1Status resource1ProgrammedCycleTime 
    resource1CurrentCycleTime resource2Name resource2Status resource2ProgrammedCycleTime resource2CurrentCycleTime resource3Name resource3Status resource3ProgrammedCycleTime 
    resource3CurrentCycleTime resource4Name resource4Status resource4ProgrammedCycleTime resource4CurrentCycleTime kernelStatus clearKernelCommand userCProgramProgramIdentifier 
    userCProgramVersionString programTimeDateStamp  
  
  