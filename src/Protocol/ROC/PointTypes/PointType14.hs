{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType14 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float

data PointType14 = PointType14 {
 pointType14PointTag                         :: !PointType14PointTag       
,pointType14CommBaudRate                     :: !PointType14CommBaudRate         
,pointType14CommStopBit                      :: !PointType14CommStopBit          
,pointType14NumDataBits                      :: !PointType14NumDataBits         
,pointType14CommParity                       :: !PointType14CommParity   
,pointType14CommStatus                       :: !PointType14CommStatus    
,pointType14CommMode                         :: !PointType14CommMode
,pointType14KeyOnDelay                       :: !PointType14KeyOnDelay          
,pointType14KeyOffDelay                      :: !PointType14KeyOffDelay      
,pointType14RBSRetryCount                    :: !PointType14RBSRetryCount         
,pointType14ExtraKeyOnDelayorRetryTime       :: !PointType14ExtraKeyOnDelayorRetryTime       
,pointType14AlarmPointer                     :: !PointType14AlarmPointer     
,pointType14RecCounterCopy                   :: !PointType14RecCounterCopy    
,pointType14RRetryCounter                    :: !PointType14RRetryCounter    
,pointType14ValidRecCount                    :: !PointType14ValidRecCount      
,pointType14ModemStat                        :: !PointType14ModemStat   
,pointType14ModemType                        :: !PointType14ModemType    
,pointType14ConnectTime                      :: !PointType14ConnectTime  
,pointType14CFGCommand                       :: !PointType14CFGCommand    
,pointType14ConnectCommand                   :: !PointType14ConnectCommand  
,pointType14DisconnectTime                   :: !PointType14DisconnectTime       
,pointType14InactivityTime                   :: !PointType14InactivityTime         
,pointType14RBXTimeBase1                     :: !PointType14RBXTimeBase1          
,pointType14RBXRetryCount1                   :: !PointType14RBXRetryCount1         
,pointType14RBXTimeBase2                     :: !PointType14RBXTimeBase2   
,pointType14RBXRetryCount2                   :: !PointType14RBXRetryCount2    
,pointType14RBXTimeBase3                     :: !PointType14RBXTimeBase3
,pointType14RBXRetryCount3                   :: !PointType14RBXRetryCount3          
,pointType14RBXAddress                       :: !PointType14RBXAddress      
,pointType14RBXGroup                         :: !PointType14RBXGroup         
,pointType14StoreForwardAddress1             :: !PointType14StoreForwardAddress1       
,pointType14StoreForwardGroup1               :: !PointType14StoreForwardGroup1     
,pointType14StoreForwardAddress2             :: !PointType14StoreForwardAddress2    
,pointType14StoreForwardGroup2               :: !PointType14StoreForwardGroup2    
,pointType14StoreForwardAddress3             :: !PointType14StoreForwardAddress3      
,pointType14StoreForwardGroup3               :: !PointType14StoreForwardGroup3   
,pointType14IdleCharDelay                    :: !PointType14IdleCharDelay    
,pointType14ExtraKeyOnDelayorPortOwner       :: !PointType14ExtraKeyOnDelayorPortOwner  
,pointType14RecBufferAddress                 :: !PointType14RecBufferAddress    
,pointType14TransBufferAddress               :: !PointType14TransBufferAddress  

} deriving (Read,Eq, Show, Generic)                       

type PointType14PointTag                     = BS.ByteString            
type PointType14CommBaudRate                 = Word16             
type PointType14CommStopBit                  = Word8            
type PointType14NumDataBits                  = Word8                
type PointType14CommParity                   = Word8           
type PointType14CommStatus                   = Word8           
type PointType14CommMode                     = Word8          
type PointType14KeyOnDelay                   = Word8               
type PointType14KeyOffDelay                  = Word8                
type PointType14RBSRetryCount                = Word8                                           
type PointType14ExtraKeyOnDelayorRetryTime   = Word16                               
type PointType14AlarmPointer                 = Word16                 
type PointType14RecCounterCopy               = Word16                   
type PointType14RRetryCounter                = Word16                  
type PointType14ValidRecCount                = Word16                  
type PointType14ModemStat                    = Word8                      
type PointType14ModemType                    = Word8              
type PointType14ConnectTime                  = Float                
type PointType14CFGCommand                   = BS.ByteString                                          
type PointType14ConnectCommand               = BS.ByteString                     
type PointType14DisconnectTime               = Float                   
type PointType14InactivityTime               = Float                   
type PointType14RBXTimeBase1                 = Float                 
type PointType14RBXRetryCount1               = Word8                   
type PointType14RBXTimeBase2                 = Float                 
type PointType14RBXRetryCount2               = Word8                   
type PointType14RBXTimeBase3                 = Float                 
type PointType14RBXRetryCount3               = Word8                   
type PointType14RBXAddress                   = Word8               
type PointType14RBXGroup                     = Word8             
type PointType14StoreForwardAddress1         = Word8                                                  
type PointType14StoreForwardGroup1           = Word8                       
type PointType14StoreForwardAddress2         = Word8                         
type PointType14StoreForwardGroup2           = Word8                       
type PointType14StoreForwardAddress3         = Word8                         
type PointType14StoreForwardGroup3           = Word8                       
type PointType14IdleCharDelay                = Word16                  
type PointType14ExtraKeyOnDelayorPortOwner   = Word8                               
type PointType14RecBufferAddress             = Word32                       
type PointType14TransBufferAddress           = Word32                         

pointType14Parser :: Get PointType14
pointType14Parser = do 
  
  pointTag <- getByteString 10 
  commBaudRate <- getWord16le
  commStopBit <- getWord8
  numDataBits <- getWord8 
  commParity <- getWord8 
  commStatus <- getWord8 
  commMode <- getWord8 
  keyOnDelay <- getWord8 
  keyOffDelay <- getWord8 
  rBSRetryCount <- getWord8 
  extraKeyOnDelayorRetryTime <- getWord16le 
  alarmPointer <- getWord16le 
  recCounterCopy <- getWord16le 
  rRetryCounter <- getWord16le 
  validRecCount <- getWord16le 
  modemStat <- getWord8 
  modemType <- getWord8
  connectTime <- getIeeeFloat32 
  cFGCommand <- getByteString 40
  connectCommand <- getByteString 40
  disconnectTime <- getIeeeFloat32
  inactivityTime <- getIeeeFloat32
  rBXTimeBase1 <- getIeeeFloat32
  rBXRetryCount1 <- getWord8
  rBXTimeBase2 <- getIeeeFloat32
  rBXRetryCount2 <- getWord8
  rBXTimeBase3 <- getIeeeFloat32
  rBXRetryCount3 <- getWord8
  rBXAddress <- getWord8
  rBXGroup <- getWord8
  storeForwardAddress1 <- getWord8
  storeForwardGroup1 <- getWord8
  storeForwardAddress2 <- getWord8
  storeForwardGroup2 <- getWord8
  storeForwardAddress3 <- getWord8
  storeForwardGroup3 <- getWord8
  idleCharDelay <- getWord16le
  extraKeyOnDelayorPortOwner <- getWord8
  recBufferAddress <- getWord32le
  transBufferAddress <- getWord32le
 
  return $ PointType14 pointTag commBaudRate commStopBit numDataBits commParity commStatus commMode keyOnDelay keyOffDelay rBSRetryCount extraKeyOnDelayorRetryTime alarmPointer recCounterCopy 
    rRetryCounter validRecCount modemStat modemType connectTime cFGCommand connectCommand disconnectTime inactivityTime rBXTimeBase1 rBXRetryCount1 rBXTimeBase2 rBXRetryCount2 
    rBXTimeBase3 rBXRetryCount3 rBXAddress rBXGroup storeForwardAddress1 storeForwardGroup1 storeForwardAddress2 storeForwardGroup2 storeForwardAddress3 storeForwardGroup3 
    idleCharDelay extraKeyOnDelayorPortOwner recBufferAddress transBufferAddress







