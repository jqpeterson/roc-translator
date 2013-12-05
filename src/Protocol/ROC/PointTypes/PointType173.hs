{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType173 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.Utils

data PointType173 = PointType173 {
 
 pointType173DeviceTag                          :: !PointType173DeviceTag                         
,pointType173CommissionedDeviceID               :: !PointType173CommissionedDeviceID              
,pointType173NetworkID                          :: !PointType173NetworkID                         
,pointType173ROCGroupAddress                    :: !PointType173ROCGroupAddress                   
,pointType173ROCUnitAddress                     :: !PointType173ROCUnitAddress                    
,pointType173ROCType                            :: !PointType173ROCType                           
,pointType173ROCBackplaneTypeSlotUsage          :: !PointType173ROCBackplaneTypeSlotUsage         
,pointType173IntegritySummary                   :: !PointType173IntegritySummary                  
,pointType173CommunicationState                 :: !PointType173CommunicationState                
,pointType173ROCBatteryVoltage                  :: !PointType173ROCBatteryVoltage                 
,pointType173RadioSignalStrength                :: !PointType173RadioSignalStrength               
,pointType173NoiseLevel                         :: !PointType173NoiseLevel                        
,pointType173PercentGoodPacketsFromMaster       :: !PointType173PercentGoodPacketsFromMaster      
,pointType173RevNetworkCfgSoftware              :: !PointType173RevNetworkCfgSoftware             
,pointType173DecommisionFlag                    :: !PointType173DecommisionFlag                   
,pointType173ReflectedPowerFromRadio            :: !PointType173ReflectedPowerFromRadio           
,pointType173EnablePassThruRemoteNode           :: !PointType173EnablePassThruRemoteNode          
,pointType173OutgoingPassThruMsgCounter         :: !PointType173OutgoingPassThruMsgCounter        

} deriving (Eq, Show, Generic)                       

type PointType173DeviceTag                      = BS.ByteString                      
type PointType173CommissionedDeviceID           = Word32                      
type PointType173NetworkID                      = Word8                      
type PointType173ROCGroupAddress                = Word8                      
type PointType173ROCUnitAddress                 = Word8                      
type PointType173ROCType                        = Word8                      
type PointType173ROCBackplaneTypeSlotUsage      = Word32                      
type PointType173IntegritySummary               = Word8                        
type PointType173CommunicationState             = Word8                      
type PointType173ROCBatteryVoltage              = Float                      
type PointType173RadioSignalStrength            = Word8                      
type PointType173NoiseLevel                     = Word8                        
type PointType173PercentGoodPacketsFromMaster   = Word8                      
type PointType173RevNetworkCfgSoftware          = Word16                      
type PointType173DecommisionFlag                = Bool                      
type PointType173ReflectedPowerFromRadio        = Float                        
type PointType173EnablePassThruRemoteNode       = Bool                      
type PointType173OutgoingPassThruMsgCounter     = Word32                      
  
pointType173Parser :: Get PointType173
pointType173Parser = do 

  deviceTag <- getByteString 20
  commissionedDeviceID <- getWord32le
  networkID <- getWord8
  rOCGroupAddress <- getWord8
  rOCUnitAddress <- getWord8
  rOCType <- getWord8
  rOCBackplaneTypeSlotUsage <- getWord32le
  integritySummary <- getWord8
  communicationState <- getWord8
  rOCBatteryVoltage <- getIeeeFloat32
  radioSignalStrength <- getWord8
  noiseLevel <- getWord8
  percentGoodPacketsFromMaster <- getWord8
  revNetworkCfgSoftware <- getWord16le
  decommisionFlag <- anyButNull
  reflectedPowerFromRadio <- getIeeeFloat32
  enablePassThruRemoteNode <- anyButNull
  outgoingPassThruMsgCounter <- getWord32le
  
  return $ PointType173 deviceTag commissionedDeviceID networkID rOCGroupAddress rOCUnitAddress rOCType rOCBackplaneTypeSlotUsage integritySummary communicationState 
    rOCBatteryVoltage radioSignalStrength noiseLevel percentGoodPacketsFromMaster revNetworkCfgSoftware decommisionFlag reflectedPowerFromRadio enablePassThruRemoteNode 
    outgoingPassThruMsgCounter