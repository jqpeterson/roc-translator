{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType85 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.PointTypes.Utils


data PointType85 = PointType85 {
  
 pointType85ChannelTagId                                     :: !PointType85ChannelTagId                                                                         
,pointType85ChannelIOMode                                    :: !PointType85ChannelIOMode                                                     
,pointType85HARTCommMode                                     :: !PointType85HARTCommMode                                                     
,pointType85NumDevicesConnected                              :: !PointType85NumDevicesConnected                                           
,pointType85HARTCommStatus                                   :: !PointType85HARTCommStatus                                                
,pointType85AnalogMode                                       :: !PointType85AnalogMode                                                       
,pointType85ROCPrtclPassThruEnable                           :: !PointType85ROCPrtclPassThruEnable                                              
,pointType85HART1ResumePollTimeHART2IntrnlResister           :: !PointType85HART1ResumePollTimeHART2IntrnlResister                                   
,pointType85EUValue                                          :: !PointType85EUValue                                                   
,pointType85FailsafeOnReset                                  :: !PointType85FailsafeOnReset                                                             
,pointType85FailsafeValue                                    :: !PointType85FailsafeValue                                                                     
,pointType85ManualValue                                      :: !PointType85ManualValue                                                              
,pointType85AutoValue                                        :: !PointType85AutoValue                                                         
,pointType85PhysicalValue                                    :: !PointType85PhysicalValue                                                                        
,pointType85PhysicalRawDAOutput                              :: !PointType85PhysicalRawDAOutput                                                                              
,pointType85CalibLiveValue                                   :: !PointType85CalibLiveValue                                                                                         
,pointType85ZeroEUCalibValue                                 :: !PointType85ZeroEUCalibValue                                                                          
,pointType85EUCalibValueSpan                                 :: !PointType85EUCalibValueSpan                                                       
,pointType85RawEUValueInputorOutput                          :: !PointType85RawEUValueInputorOutput                                                                  
,pointType85ZeroRawEUCalibValue                              :: !PointType85ZeroRawEUCalibValue                                                                                         
,pointType85RawEUCalibValueSpan                              :: !PointType85RawEUCalibValueSpan                                                                    
,pointType85Device1PollMode                                  :: !PointType85Device1PollMode                                                     
,pointType85Device1PollingAddress                            :: !PointType85Device1PollingAddress                                                      
,pointType85Device1Status                                    :: !PointType85Device1Status                                                                  
,pointType85Device1ActualScanPeriod                          :: !PointType85Device1ActualScanPeriod                                                              
,pointType85Device1Tag                                       :: !PointType85Device1Tag                                                   
,pointType85Device1ResponseCodeStatus                        :: !PointType85Device1ResponseCodeStatus                                             
,pointType85Device1ActiveAlarms                              :: !PointType85Device1ActiveAlarms                                         
,pointType85Device1Current                                   :: !PointType85Device1Current                                                      
,pointType85Device1PercentOfRange                            :: !PointType85Device1PercentOfRange                                                 
,pointType85Device1FaultValueEnable                          :: !PointType85Device1FaultValueEnable                                                   
,pointType85Device1PVUnits                                   :: !PointType85Device1PVUnits                                         
,pointType85Device1PV                                        :: !PointType85Device1PV                                            
,pointType85Device1PVFailsafeOnResetValue                    :: !PointType85Device1PVFailsafeOnResetValue                                                  
,pointType85Device1SVUnits                                   :: !PointType85Device1SVUnits                                                 
,pointType85Device1SV                                        :: !PointType85Device1SV                                               
,pointType85Device1SVFailsafeOnResetValue                    :: !PointType85Device1SVFailsafeOnResetValue                                                
,pointType85Device1TVUnits                                   :: !PointType85Device1TVUnits                                          
,pointType85Device1TV                                        :: !PointType85Device1TV                                                     
,pointType85Device1TVFailsafeOnResetValue                    :: !PointType85Device1TVFailsafeOnResetValue                                                  
,pointType85Device1FVUnits                                   :: !PointType85Device1FVUnits                                                        
,pointType85Device1FV                                        :: !PointType85Device1FV                                           
,pointType85Device1FVFailsafeOnResetValue                    :: !PointType85Device1FVFailsafeOnResetValue                                           
,pointType85Device1Slot0Assignment                           :: !PointType85Device1Slot0Assignment                                                     
,pointType85Device1Slot0Units                                :: !PointType85Device1Slot0Units                                                      
,pointType85Device1Slot0Variable                             :: !PointType85Device1Slot0Variable                                                     
,pointType85Device1Slot1Assignment                           :: !PointType85Device1Slot1Assignment                                               
,pointType85Device1Slot1Units                                :: !PointType85Device1Slot1Units                                                
,pointType85Device1Slot1Variable                             :: !PointType85Device1Slot1Variable                                                      
,pointType85Device1Slot2Assignment                           :: !PointType85Device1Slot2Assignment                                                   
,pointType85Device1Slot2Units                                :: !PointType85Device1Slot2Units                                                           
,pointType85Device1Slot2Variable                             :: !PointType85Device1Slot2Variable                                                   
,pointType85Device1Slot3Assignment                           :: !PointType85Device1Slot3Assignment                                              
,pointType85Device1Slot3Units                                :: !PointType85Device1Slot3Units                                                            
,pointType85Device1Slot3Variable                             :: !PointType85Device1Slot3Variable                                                       
,pointType85Device1Message                                   :: !PointType85Device1Message                                                   
,pointType85Device1Descriptor                                :: !PointType85Device1Descriptor                                                      
,pointType85Device1MfrIDDeviceID                             :: !PointType85Device1MfrIDDeviceID                                
,pointType85Device1SerialNum                                 :: !PointType85Device1SerialNum                                                    
,pointType85Device1IDNum                                     :: !PointType85Device1IDNum                                        
,pointType85Device1SensorUnits                               :: !PointType85Device1SensorUnits                                                
,pointType85Device1UpperSensorLimit                          :: !PointType85Device1UpperSensorLimit                                                     
,pointType85Device1LowerSensorLimit                          :: !PointType85Device1LowerSensorLimit                                                    
,pointType85Device1MinSensorSpan                             :: !PointType85Device1MinSensorSpan                                                      
,pointType85Device1OutputUnits                               :: !PointType85Device1OutputUnits                                                  
,pointType85Device1UpperOutputLimit                          :: !PointType85Device1UpperOutputLimit                                            
,pointType85Device1LowerOutputLimit                          :: !PointType85Device1LowerOutputLimit                                                  
,pointType85Device1DampingValue                              :: !PointType85Device1DampingValue                                                    
,pointType85Device2PollMode                                  :: !PointType85Device2PollMode                                                                       
,pointType85Device2PollingAddress                            :: !PointType85Device2PollingAddress                                                        
,pointType85Device2Status                                    :: !PointType85Device2Status                                                                      
,pointType85Device2ActualScanPeriod                          :: !PointType85Device2ActualScanPeriod                                                             
,pointType85Device2Tag                                       :: !PointType85Device2Tag                                                                       
,pointType85Device2ResponseCodeStatus                        :: !PointType85Device2ResponseCodeStatus                                                       
,pointType85Device2ActiveAlarms                              :: !PointType85Device2ActiveAlarms                                                       
,pointType85Device2Current                                   :: !PointType85Device2Current                                                           
,pointType85Device2PercentOfRange                            :: !PointType85Device2PercentOfRange                                                              
,pointType85Device2FaultValueEnable                          :: !PointType85Device2FaultValueEnable                                                         
,pointType85Device2PVUnits                                   :: !PointType85Device2PVUnits                                                                  
,pointType85Device2PV                                        :: !PointType85Device2PV                                                                     
,pointType85Device2PVFailsafeOnResetValue                    :: !PointType85Device2PVFailsafeOnResetValue                                            
,pointType85Device2SVUnits                                   :: !PointType85Device2SVUnits                                                                      
,pointType85Device2SV                                        :: !PointType85Device2SV                                                                    
,pointType85Device2SVFailsafeOnResetValue                    :: !PointType85Device2SVFailsafeOnResetValue                                                       
,pointType85Device2TVUnits                                   :: !PointType85Device2TVUnits                                                           
,pointType85Device2TV                                        :: !PointType85Device2TV                                                                 
,pointType85Device2TVFailsafeOnResetValue                    :: !PointType85Device2TVFailsafeOnResetValue                                                   
,pointType85Device2FVUnits                                   :: !PointType85Device2FVUnits                                                                    
,pointType85Device2FV                                        :: !PointType85Device2FV                                                                          
,pointType85Device2FVFailsafeOnResetValue                    :: !PointType85Device2FVFailsafeOnResetValue                                                  
,pointType85Device2Slot0Assignment                           :: !PointType85Device2Slot0Assignment                                                 
,pointType85Device2Slot0Units                                :: !PointType85Device2Slot0Units                                                                
,pointType85Device2Slot0Variable                             :: !PointType85Device2Slot0Variable                                                              
,pointType85Device2Slot1Assignment                           :: !PointType85Device2Slot1Assignment                                                              
,pointType85Device2Slot1Units                                :: !PointType85Device2Slot1Units                                                          
,pointType85Device2Slot1Variable                             :: !PointType85Device2Slot1Variable                                                   
,pointType85Device2Slot2Assignment                           :: !PointType85Device2Slot2Assignment                                                              
,pointType85Device2Slot2Units                                :: !PointType85Device2Slot2Units                                                                   
,pointType85Device2Slot2Variable                             :: !PointType85Device2Slot2Variable                                                              
,pointType85Device2Slot3Assignment                           :: !PointType85Device2Slot3Assignment                                                         
,pointType85Device2Slot3Units                                :: !PointType85Device2Slot3Units                                                          
,pointType85Device2Slot3Variable                             :: !PointType85Device2Slot3Variable                                                              
,pointType85Device2Message                                   :: !PointType85Device2Message                                                                  
,pointType85Device2Descriptor                                :: !PointType85Device2Descriptor                                                                     
,pointType85Device2MfrIDDeviceID                             :: !PointType85Device2MfrIDDeviceID                                                           
,pointType85Device2SerialNum                                 :: !PointType85Device2SerialNum                                                            
,pointType85Device2IDNum                                     :: !PointType85Device2IDNum                                                                      
,pointType85Device2SensorUnits                               :: !PointType85Device2SensorUnits                                                                  
,pointType85Device2UpperSensorLimit                          :: !PointType85Device2UpperSensorLimit                                                    
,pointType85Device2LowerSensorLimit                          :: !PointType85Device2LowerSensorLimit                                                
,pointType85Device2MinSensorSpan                             :: !PointType85Device2MinSensorSpan                                                                
,pointType85Device2OutputUnits                               :: !PointType85Device2OutputUnits                                                                  
,pointType85Device2UpperOutputLimit                          :: !PointType85Device2UpperOutputLimit                                                           
,pointType85Device2LowerOutputLimit                          :: !PointType85Device2LowerOutputLimit                                                        
,pointType85Device2DampingValue                              :: !PointType85Device2DampingValue                                                        
,pointType85Device3PollMode                                  :: !PointType85Device3PollMode                                                                    
,pointType85Device3PollingAddress                            :: !PointType85Device3PollingAddress                                                            
,pointType85Device3Status                                    :: !PointType85Device3Status                                                                          
,pointType85Device3ActualScanPeriod                          :: !PointType85Device3ActualScanPeriod                                                         
,pointType85Device3Tag                                       :: !PointType85Device3Tag                                                                   
,pointType85Device3ResponseCodeStatus                        :: !PointType85Device3ResponseCodeStatus                                                                                          
,pointType85Device3ActiveAlarms                              :: !PointType85Device3ActiveAlarms                                             
,pointType85Device3Current                                   :: !PointType85Device3Current                                                 
,pointType85Device3PercentOfRange                            :: !PointType85Device3PercentOfRange                                          
,pointType85Device3FaultValueEnable                          :: !PointType85Device3FaultValueEnable                              
,pointType85Device3PVUnits                                   :: !PointType85Device3PVUnits                                            
,pointType85Device3PV                                        :: !PointType85Device3PV                                                        
,pointType85Device3PVFailsafeOnResetValue                    :: !PointType85Device3PVFailsafeOnResetValue                           
,pointType85Device3SVUnits                                   :: !PointType85Device3SVUnits                               
,pointType85Device3SV                                        :: !PointType85Device3SV                                                    
,pointType85Device3SVFailsafeOnResetValue                    :: !PointType85Device3SVFailsafeOnResetValue                                          
,pointType85Device3TVUnits                                   :: !PointType85Device3TVUnits                                                                 
,pointType85Device3TV                                        :: !PointType85Device3TV                                                               
,pointType85Device3TVFailsafeOnResetValue                    :: !PointType85Device3TVFailsafeOnResetValue                                      
,pointType85Device3FVUnits                                   :: !PointType85Device3FVUnits                                                                    
,pointType85Device3FV                                        :: !PointType85Device3FV                                                                               
,pointType85Device3FVFailsafeOnResetValue                    :: !PointType85Device3FVFailsafeOnResetValue                                                                      
,pointType85Device3Slot0Assignment                           :: !PointType85Device3Slot0Assignment                                                              
,pointType85Device3Slot0Units                                :: !PointType85Device3Slot0Units                                                
,pointType85Device3Slot0Variable                             :: !PointType85Device3Slot0Variable                                                        
,pointType85Device3Slot1Assignment                           :: !PointType85Device3Slot1Assignment                                                                             
,pointType85Device3Slot1Units                                :: !PointType85Device3Slot1Units                                                             
,pointType85Device3Slot1Variable                             :: !PointType85Device3Slot1Variable                                           
,pointType85Device3Slot2Assignment                           :: !PointType85Device3Slot2Assignment                                          
,pointType85Device3Slot2Units                                :: !PointType85Device3Slot2Units                                                           
,pointType85Device3Slot2Variable                             :: !PointType85Device3Slot2Variable                                                    
,pointType85Device3Slot3Assignment                           :: !PointType85Device3Slot3Assignment                                                           
,pointType85Device3Slot3Units                                :: !PointType85Device3Slot3Units                                                          
,pointType85Device3Slot3Variable                             :: !PointType85Device3Slot3Variable                                                   
,pointType85Device3Message                                   :: !PointType85Device3Message                                                                      
,pointType85Device3Descriptor                                :: !PointType85Device3Descriptor                                                              
,pointType85Device3MfrIDDeviceID                             :: !PointType85Device3MfrIDDeviceID                                                             
,pointType85Device3SerialNum                                 :: !PointType85Device3SerialNum                                                       
,pointType85Device3IDNum                                     :: !PointType85Device3IDNum                                                              
,pointType85Device3SensorUnits                               :: !PointType85Device3SensorUnits                                                              
,pointType85Device3UpperSensorLimit                          :: !PointType85Device3UpperSensorLimit                                                        
,pointType85Device3LowerSensorLimit                          :: !PointType85Device3LowerSensorLimit                                                      
,pointType85Device3MinSensorSpan                             :: !PointType85Device3MinSensorSpan                                                          
,pointType85Device3OutputUnits                               :: !PointType85Device3OutputUnits                                                      
,pointType85Device3UpperOutputLimit                          :: !PointType85Device3UpperOutputLimit                                                            
,pointType85Device3LowerOutputLimit                          :: !PointType85Device3LowerOutputLimit                                                         
,pointType85Device3DampingValue                              :: !PointType85Device3DampingValue                                                                   
,pointType85Device4PollMode                                  :: !PointType85Device4PollMode                                                         
,pointType85Device4PollingAddress                            :: !PointType85Device4PollingAddress                                                   
,pointType85Device4Status                                    :: !PointType85Device4Status                                                                     
,pointType85Device4ActualScanPeriod                          :: !PointType85Device4ActualScanPeriod                                                            
,pointType85Device4Tag                                       :: !PointType85Device4Tag                                                                        
,pointType85Device4ResponseCodeStatus                        :: !PointType85Device4ResponseCodeStatus                                                   
,pointType85Device4ActiveAlarms                              :: !PointType85Device4ActiveAlarms                                                     
,pointType85Device4Current                                   :: !PointType85Device4Current                                                                   
,pointType85Device4PercentOfRange                            :: !PointType85Device4PercentOfRange                                                           
,pointType85Device4FaultValueEnable                          :: !PointType85Device4FaultValueEnable                                                            
,pointType85Device4PVUnits                                   :: !PointType85Device4PVUnits                                                                
,pointType85Device4PV                                        :: !PointType85Device4PV                                                                  
,pointType85Device4PVFailsafeOnResetValue                    :: !PointType85Device4PVFailsafeOnResetValue                                                       
,pointType85Device4SVUnits                                   :: !PointType85Device4SVUnits                                                                    
,pointType85Device4SV                                        :: !PointType85Device4SV                                                                       
,pointType85Device4SVFailsafeOnResetValue                    :: !PointType85Device4SVFailsafeOnResetValue                                                      
,pointType85Device4TVUnits                                   :: !PointType85Device4TVUnits                                               
,pointType85Device4TV                                        :: !PointType85Device4TV                                                                        
,pointType85Device4TVFailsafeOnResetValue                    :: !PointType85Device4TVFailsafeOnResetValue                                        
,pointType85Device4FVUnits                                   :: !PointType85Device4FVUnits                                                               
,pointType85Device4FV                                        :: !PointType85Device4FV                                                                         
,pointType85Device4FVFailsafeOnResetValue                    :: !PointType85Device4FVFailsafeOnResetValue                                                    
,pointType85Device4Slot0Assignment                           :: !PointType85Device4Slot0Assignment                                                             
,pointType85Device4Slot0Units                                :: !PointType85Device4Slot0Units                                                              
,pointType85Device4Slot0Variable                             :: !PointType85Device4Slot0Variable                                                     
,pointType85Device4Slot1Assignment                           :: !PointType85Device4Slot1Assignment                                                         
,pointType85Device4Slot1Units                                :: !PointType85Device4Slot1Units                                                                
,pointType85Device4Slot1Variable                             :: !PointType85Device4Slot1Variable                                                                  
,pointType85Device4Slot2Assignment                           :: !PointType85Device4Slot2Assignment                                                       
,pointType85Device4Slot2Units                                :: !PointType85Device4Slot2Units                                                                  
,pointType85Device4Slot2Variable                             :: !PointType85Device4Slot2Variable                                                                
,pointType85Device4Slot3Assignment                           :: !PointType85Device4Slot3Assignment                                                           
,pointType85Device4Slot3Units                                :: !PointType85Device4Slot3Units                                                               
,pointType85Device4Slot3Variable                             :: !PointType85Device4Slot3Variable                                                      
,pointType85Device4Message                                   :: !PointType85Device4Message                                                           
,pointType85Device4Descriptor                                :: !PointType85Device4Descriptor                                                                  
,pointType85Device4MfrIDDeviceID                             :: !PointType85Device4MfrIDDeviceID                                                            
,pointType85Device4SerialNum                                 :: !PointType85Device4SerialNum                                                                
,pointType85Device4IDNum                                     :: !PointType85Device4IDNum                                                                  
,pointType85Device4SensorUnits                               :: !PointType85Device4SensorUnits                                                       
,pointType85Device4UpperSensorLimit                          :: !PointType85Device4UpperSensorLimit                                                             
,pointType85Device4LowerSensorLimit                          :: !PointType85Device4LowerSensorLimit                                                      
,pointType85Device4MinSensorSpan                             :: !PointType85Device4MinSensorSpan                                                                
,pointType85Device4OutputUnits                               :: !PointType85Device4OutputUnits                                                       
,pointType85Device4UpperOutputLimit                          :: !PointType85Device4UpperOutputLimit                                                   
,pointType85Device4LowerOutputLimit                          :: !PointType85Device4LowerOutputLimit                                                         
,pointType85Device4DampingValue                              :: !PointType85Device4DampingValue                                                               
,pointType85Device5PollMode                                  :: !PointType85Device5PollMode                                                                                   
,pointType85Device5PollingAddress                            :: !PointType85Device5PollingAddress                                                                         
,pointType85Device5Status                                    :: !PointType85Device5Status                                                                         
,pointType85Device5ActualScanPeriod                          :: !PointType85Device5ActualScanPeriod                                                                         
,pointType85Device5Tag                                       :: !PointType85Device5Tag                                                                                       
,pointType85Device5ResponseCodeStatus                        :: !PointType85Device5ResponseCodeStatus                                                                          
,pointType85Device5ActiveAlarms                              :: !PointType85Device5ActiveAlarms                                                                       
,pointType85Device5Current                                   :: !PointType85Device5Current                                                                        
,pointType85Device5PercentOfRange                            :: !PointType85Device5PercentOfRange                                                                              
,pointType85Device5FaultValueEnable                          :: !PointType85Device5FaultValueEnable                                                                            
,pointType85Device5PVUnits                                   :: !PointType85Device5PVUnits                                                                                   
,pointType85Device5PV                                        :: !PointType85Device5PV                                                                                     
,pointType85Device5PVFailsafeOnResetValue                    :: !PointType85Device5PVFailsafeOnResetValue                                                             
,pointType85Device5SVUnits                                   :: !PointType85Device5SVUnits                                                                                   
,pointType85Device5SV                                        :: !PointType85Device5SV                                                                                      
,pointType85Device5SVFailsafeOnResetValue                    :: !PointType85Device5SVFailsafeOnResetValue                                                                        
,pointType85Device5TVUnits                                   :: !PointType85Device5TVUnits                                                                                
,pointType85Device5TV                                        :: !PointType85Device5TV                                                                                  
,pointType85Device5TVFailsafeOnResetValue                    :: !PointType85Device5TVFailsafeOnResetValue                                                                    
,pointType85Device5FVUnits                                   :: !PointType85Device5FVUnits                                                                                     
,pointType85Device5FV                                        :: !PointType85Device5FV                                                                                 
,pointType85Device5FVFailsafeOnResetValue                    :: !PointType85Device5FVFailsafeOnResetValue                                                         
,pointType85Device5Slot0Assignment                           :: !PointType85Device5Slot0Assignment                                                                             
,pointType85Device5Slot0Units                                :: !PointType85Device5Slot0Units                                                                                  
,pointType85Device5Slot0Variable                             :: !PointType85Device5Slot0Variable                                                                             
,pointType85Device5Slot1Assignment                           :: !PointType85Device5Slot1Assignment                                                                        
,pointType85Device5Slot1Units                                :: !PointType85Device5Slot1Units                                                                         
,pointType85Device5Slot1Variable                             :: !PointType85Device5Slot1Variable                                                                             
,pointType85Device5Slot2Assignment                           :: !PointType85Device5Slot2Assignment                                                                         
,pointType85Device5Slot2Units                                :: !PointType85Device5Slot2Units                                                                                    
,pointType85Device5Slot2Variable                             :: !PointType85Device5Slot2Variable                                                                          
,pointType85Device5Slot3Assignment                           :: !PointType85Device5Slot3Assignment                                                                     
,pointType85Device5Slot3Units                                :: !PointType85Device5Slot3Units                                                                         
,pointType85Device5Slot3Variable                             :: !PointType85Device5Slot3Variable                                                                  
,pointType85Device5Message                                   :: !PointType85Device5Message                                                                                     
,pointType85Device5Descriptor                                :: !PointType85Device5Descriptor                                                                                  
,pointType85Device5MfrIDDeviceID                             :: !PointType85Device5MfrIDDeviceID                                                                             
,pointType85Device5SerialNum                                 :: !PointType85Device5SerialNum                                                                              
,pointType85Device5IDNum                                     :: !PointType85Device5IDNum                                                                              
,pointType85Device5SensorUnits                               :: !PointType85Device5SensorUnits                                                                               
,pointType85Device5UpperSensorLimit                          :: !PointType85Device5UpperSensorLimit                                                                        
,pointType85Device5LowerSensorLimit                          :: !PointType85Device5LowerSensorLimit                                                                              
,pointType85Device5MinSensorSpan                             :: !PointType85Device5MinSensorSpan                                                                          
,pointType85Device5OutputUnits                               :: !PointType85Device5OutputUnits                                                                         
,pointType85Device5UpperOutputLimit                          :: !PointType85Device5UpperOutputLimit                                                                                         
,pointType85Device5LowerOutputLimit                          :: !PointType85Device5LowerOutputLimit                                    
,pointType85Device5DampingValue                              :: !PointType85Device5DampingValue                             
         
} deriving (Read,Eq, Show, Generic)                       
                                  
type PointType85ChannelTagId                                 = BS.ByteString                                                                            
type PointType85ChannelIOMode                                = Bool                                                                                               
type PointType85HARTCommMode                                 = Word8                                                                                     
type PointType85NumDevicesConnected                          = Word8                                                                                      
type PointType85HARTCommStatus                               = Word8                                                              
type PointType85AnalogMode                                   = Word8                                                                            
type PointType85ROCPrtclPassThruEnable                       = Word8                                                         
type PointType85HART1ResumePollTimeHART2IntrnlResister       = Word32                                                                
type PointType85EUValue                                      = Float                              
type PointType85FailsafeOnReset                              = Bool                                                                          
type PointType85FailsafeValue                                = Float                                                                                              
type PointType85ManualValue                                  = Float                                                                                      
type PointType85AutoValue                                    = Float                                                                                         
type PointType85PhysicalValue                                = Float                                                                                          
type PointType85PhysicalRawDAOutput                          = Word16                                                                                          
type PointType85CalibLiveValue                               = Float                                                                                                
type PointType85ZeroEUCalibValue                             = Float                                                                                       
type PointType85EUCalibValueSpan                             = Float                                                                                   
type PointType85RawEUValueInputorOutput                      = Word16                                                                                              
type PointType85ZeroRawEUCalibValue                          = Word16                                                                                               
type PointType85RawEUCalibValueSpan                          = Word16                                                                                     
type PointType85Device1PollMode                              = Word8                                                                                          
type PointType85Device1PollingAddress                        = Word8                                                                               
type PointType85Device1Status                                = Word8                                                                                                         
type PointType85Device1ActualScanPeriod                      = Float                                                                                                
type PointType85Device1Tag                                   = BS.ByteString                                                                                                              
type PointType85Device1ResponseCodeStatus                    = Word16                                                                                                    
type PointType85Device1ActiveAlarms                          = Word8                                                                                                    
type PointType85Device1Current                               = Float                                                                                                                
type PointType85Device1PercentOfRange                        = Float                                                                                                            
type PointType85Device1FaultValueEnable                      = Bool                                                                                                          
type PointType85Device1PVUnits                               = Word8                                                                                                
type PointType85Device1PV                                    = Float                                                                                                      
type PointType85Device1PVFailsafeOnResetValue                = Float                                                                                                            
type PointType85Device1SVUnits                               = Word8                                                                                                        
type PointType85Device1SV                                    = Float                                                                                                      
type PointType85Device1SVFailsafeOnResetValue                = Float                                                                                                          
type PointType85Device1TVUnits                               = Word8                                                                                                      
type PointType85Device1TV                                    = Float                                                                                                            
type PointType85Device1TVFailsafeOnResetValue                = Float                                                                                                            
type PointType85Device1FVUnits                               = Word8                                                                                                                  
type PointType85Device1FV                                    = Float                                                                                                  
type PointType85Device1FVFailsafeOnResetValue                = Float                                                                                                  
type PointType85Device1Slot0Assignment                       = Word8                                                                                                                
type PointType85Device1Slot0Units                            = Word8                                                                                                                
type PointType85Device1Slot0Variable                         = Float                                                                                                                
type PointType85Device1Slot1Assignment                       = Word8                                                                                                      
type PointType85Device1Slot1Units                            = Word8                                                                                                      
type PointType85Device1Slot1Variable                         = Float                                                                                                          
type PointType85Device1Slot2Assignment                       = Word8                                                                                                              
type PointType85Device1Slot2Units                            = Word8                                                                                                            
type PointType85Device1Slot2Variable                         = Float                                                                                                            
type PointType85Device1Slot3Assignment                       = Word8                                                                                                      
type PointType85Device1Slot3Units                            = Word8                                                                                                                  
type PointType85Device1Slot3Variable                         = Float                                                                                                            
type PointType85Device1Message                               = BS.ByteString                                                                                                              
type PointType85Device1Descriptor                            = BS.ByteString                                                                                                                
type PointType85Device1MfrIDDeviceID                         = Word16                                                                                        
type PointType85Device1SerialNum                             = Word32                                                                                                           
type PointType85Device1IDNum                                 = Word32                                                                                               
type PointType85Device1SensorUnits                           = Word8                                                                                               
type PointType85Device1UpperSensorLimit                      = Float                                                                                                             
type PointType85Device1LowerSensorLimit                      = Float                                                                                                           
type PointType85Device1MinSensorSpan                         = Float                                                                                                                 
type PointType85Device1OutputUnits                           = Word8                                                                                                        
type PointType85Device1UpperOutputLimit                      = Float                  
type PointType85Device1LowerOutputLimit                      = Float                  
type PointType85Device1DampingValue                          = Float                  
type PointType85Device2PollMode                              = Word8                                                                                              
type PointType85Device2PollingAddress                        = Word8                                                                                                    
type PointType85Device2Status                                = Word8                                                                                                         
type PointType85Device2ActualScanPeriod                      = Float                                                                                                           
type PointType85Device2Tag                                   = BS.ByteString                                                                                         
type PointType85Device2ResponseCodeStatus                    = Word16                                                                                             
type PointType85Device2ActiveAlarms                          = Word8                                                                                             
type PointType85Device2Current                               = Float                                                                                                         
type PointType85Device2PercentOfRange                        = Float                                                                                                    
type PointType85Device2FaultValueEnable                      = Bool                                                                                                  
type PointType85Device2PVUnits                               = Word8                                                                                                
type PointType85Device2PV                                    = Float                                                                                                           
type PointType85Device2PVFailsafeOnResetValue                = Float                                                                                                    
type PointType85Device2SVUnits                               = Word8                                                                                                    
type PointType85Device2SV                                    = Float                                                                                                     
type PointType85Device2SVFailsafeOnResetValue                = Float                                                                                                
type PointType85Device2TVUnits                               = Word8                                                                                                        
type PointType85Device2TV                                    = Float                                                                                                 
type PointType85Device2TVFailsafeOnResetValue                = Float                                                                                                           
type PointType85Device2FVUnits                               = Word8                                                                                                  
type PointType85Device2FV                                    = Float                                                                                               
type PointType85Device2FVFailsafeOnResetValue                = Float                                                                                                        
type PointType85Device2Slot0Assignment                       = Word8                                                                                                          
type PointType85Device2Slot0Units                            = Word8                                                                                                        
type PointType85Device2Slot0Variable                         = Float                                                                                                   
type PointType85Device2Slot1Assignment                       = Word8                                                                                               
type PointType85Device2Slot1Units                            = Word8                                                                                                        
type PointType85Device2Slot1Variable                         = Float                                                                                                          
type PointType85Device2Slot2Assignment                       = Word8                                                                                                        
type PointType85Device2Slot2Units                            = Word8                                                                                                   
type PointType85Device2Slot2Variable                         = Float                                                                                          
type PointType85Device2Slot3Assignment                       = Word8                                                                                                            
type PointType85Device2Slot3Units                            = Word8                                                                                                       
type PointType85Device2Slot3Variable                         = Float                                                                                                          
type PointType85Device2Message                               = BS.ByteString                                                                                            
type PointType85Device2Descriptor                            = BS.ByteString                                                                                           
type PointType85Device2MfrIDDeviceID                         = Word16                                                                                                     
type PointType85Device2SerialNum                             = Word32                                                                                                       
type PointType85Device2IDNum                                 = Word32                                                                                                            
type PointType85Device2SensorUnits                           = Word8                                                                                                      
type PointType85Device2UpperSensorLimit                      = Float                                                                                                
type PointType85Device2LowerSensorLimit                      = Float                                                                                                      
type PointType85Device2MinSensorSpan                         = Float                                                                                                  
type PointType85Device2OutputUnits                           = Word8                                                                                                   
type PointType85Device2UpperOutputLimit                      = Float                                                                                          
type PointType85Device2LowerOutputLimit                      = Float                                                                                                            
type PointType85Device2DampingValue                          = Float                                                                                                       
type PointType85Device3PollMode                              = Word8                                                                                                             
type PointType85Device3PollingAddress                        = Word8                                                                                                       
type PointType85Device3Status                                = Word8                                                                                                      
type PointType85Device3ActualScanPeriod                      = Float                                                                                                         
type PointType85Device3Tag                                   = BS.ByteString                                                                                                   
type PointType85Device3ResponseCodeStatus                    = Word16                                                                                                               
type PointType85Device3ActiveAlarms                          = Word8                                                                                                         
type PointType85Device3Current                               = Float                                                                                                   
type PointType85Device3PercentOfRange                        = Float                                                                                                         
type PointType85Device3FaultValueEnable                      = Bool                                                                                                      
type PointType85Device3PVUnits                               = Word8                                                                                    
type PointType85Device3PV                                    = Float                                                                                                       
type PointType85Device3PVFailsafeOnResetValue                = Float                                                                                             
type PointType85Device3SVUnits                               = Word8                                                                                              
type PointType85Device3SV                                    = Float                                                                      
type PointType85Device3SVFailsafeOnResetValue                = Float                                                                                    
type PointType85Device3TVUnits                               = Word8                                                                 
type PointType85Device3TV                                    = Float                                                                        
type PointType85Device3TVFailsafeOnResetValue                = Float                                      
type PointType85Device3FVUnits                               = Word8                                                                                  
type PointType85Device3FV                                    = Float                                                                                                      
type PointType85Device3FVFailsafeOnResetValue                = Float                                                                                              
type PointType85Device3Slot0Assignment                       = Word8                                                                                                 
type PointType85Device3Slot0Units                            = Word8                                                                                                  
type PointType85Device3Slot0Variable                         = Float                                                                                                  
type PointType85Device3Slot1Assignment                       = Word8                                                                                                        
type PointType85Device3Slot1Units                            = Word8                                                                                               
type PointType85Device3Slot1Variable                         = Float                                                                                           
type PointType85Device3Slot2Assignment                       = Word8                                                                                                      
type PointType85Device3Slot2Units                            = Word8                                                                                                       
type PointType85Device3Slot2Variable                         = Float                                                                                             
type PointType85Device3Slot3Assignment                       = Word8                                                                                       
type PointType85Device3Slot3Units                            = Word8                                                                            
type PointType85Device3Slot3Variable                         = Float                                                                                                      
type PointType85Device3Message                               = BS.ByteString                                                                                     
type PointType85Device3Descriptor                            = BS.ByteString                                                                                                   
type PointType85Device3MfrIDDeviceID                         = Word16                                                                                                
type PointType85Device3SerialNum                             = Word32                                                                                                
type PointType85Device3IDNum                                 = Word32                                                                                                            
type PointType85Device3SensorUnits                           = Word8                                                                                                         
type PointType85Device3UpperSensorLimit                      = Float                                                                                                       
type PointType85Device3LowerSensorLimit                      = Float                                                                                             
type PointType85Device3MinSensorSpan                         = Float                                                                                                   
type PointType85Device3OutputUnits                           = Word8                                                                                                         
type PointType85Device3UpperOutputLimit                      = Float                                                                                                     
type PointType85Device3LowerOutputLimit                      = Float                                                                                                   
type PointType85Device3DampingValue                          = Float                                                                                                       
type PointType85Device4PollMode                              = Word8                                                                                                       
type PointType85Device4PollingAddress                        = Word8                                                                                                             
type PointType85Device4Status                                = Word8                                                                                                             
type PointType85Device4ActualScanPeriod                      = Float                                                                                                                   
type PointType85Device4Tag                                   = BS.ByteString                                                                                           
type PointType85Device4ResponseCodeStatus                    = Word16                                                                                                  
type PointType85Device4ActiveAlarms                          = Word8                                                                                                                 
type PointType85Device4Current                               = Float                                                                                                                 
type PointType85Device4PercentOfRange                        = Float                                                                                                                 
type PointType85Device4FaultValueEnable                      = Bool                                                                                                       
type PointType85Device4PVUnits                               = Word8                                                                                                       
type PointType85Device4PV                                    = Float                                                                                                           
type PointType85Device4PVFailsafeOnResetValue                = Float                                                                                                               
type PointType85Device4SVUnits                               = Word8                                                                                                             
type PointType85Device4SV                                    = Float                                                                                                             
type PointType85Device4SVFailsafeOnResetValue                = Float                                                                                                       
type PointType85Device4TVUnits                               = Word8                                                                                                                   
type PointType85Device4TV                                    = Float                                                                                                             
type PointType85Device4TVFailsafeOnResetValue                = Float                                                                                                               
type PointType85Device4FVUnits                               = Word8                                                                                                                 
type PointType85Device4FV                                    = Float                                                                                         
type PointType85Device4FVFailsafeOnResetValue                = Float                                                                                                            
type PointType85Device4Slot0Assignment                       = Word8                                                                                                
type PointType85Device4Slot0Units                            = Word8                                                                                                
type PointType85Device4Slot0Variable                         = Float                                                                                                              
type PointType85Device4Slot1Assignment                       = Word8                                                                                                            
type PointType85Device4Slot1Units                            = Word8                                                                                                                  
type PointType85Device4Slot1Variable                         = Float                                                                                                         
type PointType85Device4Slot2Assignment                       = Word8                                                                                                     
type PointType85Device4Slot2Units                            = Word8                                                                                                           
type PointType85Device4Slot2Variable                         = Float                                                                                                                
type PointType85Device4Slot3Assignment                       = Word8                                                                                                                  
type PointType85Device4Slot3Units                            = Word8                                                                                                        
type PointType85Device4Slot3Variable                         = Float                                                                                                     
type PointType85Device4Message                               = BS.ByteString                                                                                            
type PointType85Device4Descriptor                            = BS.ByteString                                                                                                        
type PointType85Device4MfrIDDeviceID                         = Word16                                                                                                          
type PointType85Device4SerialNum                             = Word32                                                                                                        
type PointType85Device4IDNum                                 = Word32                                                                                                      
type PointType85Device4SensorUnits                           = Word8                                                                                                                  
type PointType85Device4UpperSensorLimit                      = Float                                                                                                           
type PointType85Device4LowerSensorLimit                      = Float                                                                                                           
type PointType85Device4MinSensorSpan                         = Float                                                                                                            
type PointType85Device4OutputUnits                           = Word8                                                                                                       
type PointType85Device4UpperOutputLimit                      = Float                                                                                                               
type PointType85Device4LowerOutputLimit                      = Float                                                                                                        
type PointType85Device4DampingValue                          = Float                             
type PointType85Device5PollMode                              = Word8                                                                                                     
type PointType85Device5PollingAddress                        = Word8                                                                                                   
type PointType85Device5Status                                = Word8                                                                                              
type PointType85Device5ActualScanPeriod                      = Float                                                                                          
type PointType85Device5Tag                                   = BS.ByteString                                                                                           
type PointType85Device5ResponseCodeStatus                    = Word16                                                                                                    
type PointType85Device5ActiveAlarms                          = Word8                                                                                                   
type PointType85Device5Current                               = Float                                                                                              
type PointType85Device5PercentOfRange                        = Float                                                                                     
type PointType85Device5FaultValueEnable                      = Bool                                                                                                       
type PointType85Device5PVUnits                               = Word8                                                                                                  
type PointType85Device5PV                                    = Float                                                                                                     
type PointType85Device5PVFailsafeOnResetValue                = Float                                                                                               
type PointType85Device5SVUnits                               = Word8                                                                                              
type PointType85Device5SV                                    = Float                                                                                                 
type PointType85Device5SVFailsafeOnResetValue                = Float                                                                                                   
type PointType85Device5TVUnits                               = Word8                                                                                                        
type PointType85Device5TV                                    = Float                                                                                                 
type PointType85Device5TVFailsafeOnResetValue                = Float                                                                                           
type PointType85Device5FVUnits                               = Word8                                                                                                 
type PointType85Device5FV                                    = Float                                                                                             
type PointType85Device5FVFailsafeOnResetValue                = Float                                                                                              
type PointType85Device5Slot0Assignment                       = Word8                                                                                     
type PointType85Device5Slot0Units                            = Word8                                                                                                       
type PointType85Device5Slot0Variable                         = Float                                                                                                  
type PointType85Device5Slot1Assignment                       = Word8                                                                                                     
type PointType85Device5Slot1Units                            = Word8                                                                                               
type PointType85Device5Slot1Variable                         = Float                                                                                              
type PointType85Device5Slot2Assignment                       = Word8                                                                                                 
type PointType85Device5Slot2Units                            = Word8                                                                                                   
type PointType85Device5Slot2Variable                         = Float                                                                                                        
type PointType85Device5Slot3Assignment                       = Word8                                                                                                 
type PointType85Device5Slot3Units                            = Word8                                                                                           
type PointType85Device5Slot3Variable                         = Float                                                                                                 
type PointType85Device5Message                               = BS.ByteString                                                                                        
type PointType85Device5Descriptor                            = BS.ByteString                                                                                               
type PointType85Device5MfrIDDeviceID                         = Word16                                                                                                 
type PointType85Device5SerialNum                             = Word32                                                                                                    
type PointType85Device5IDNum                                 = Word32                                                                                              
type PointType85Device5SensorUnits                           = Word8                                                                                                
type PointType85Device5UpperSensorLimit                      = Float                                                                                                   
type PointType85Device5LowerSensorLimit                      = Float                                                                                                     
type PointType85Device5MinSensorSpan                         = Float                                                                                                          
type PointType85Device5OutputUnits                           = Word8                                                                                                   
type PointType85Device5UpperOutputLimit                      = Float                                                                                             
type PointType85Device5LowerOutputLimit                      = Float                                                                                                   
type PointType85Device5DampingValue                          = Float                                                                                                    
                                                                                              
  
pointType85Parser :: Get PointType85
pointType85Parser = do 
  channelTagId <- getByteString 10    
  channelIOMode <- anyButNull                 
  hARTCommMode <- getWord8                    
  numDevicesConnected <- getWord8             
  hARTCommStatus <- getWord8                   
  analogMode <- getWord8                       
  rOCPrtclPassThruEnable <- getWord8                         
  hART1ResumePollTimeHART2IntrnlResister <- getWord32le             
  eUValue <- getIeeeFloat32                
  failsafeOnReset <- anyButNull                   
  failsafeValue <- getIeeeFloat32                      
  manualValue <- getIeeeFloat32                   
  autoValue <- getIeeeFloat32                   
  physicalValue <- getIeeeFloat32                      
  physicalRawDAOutput <- getWord16le                    
  calibLiveValue <- getIeeeFloat32                      
  zeroEUCalibValue <- getIeeeFloat32                      
  eUCalibValueSpan <- getIeeeFloat32                      
  rawEUValueInputorOutput <- getWord16le                
  zeroRawEUCalibValue <- getWord16le                    
  rawEUCalibValueSpan <- getWord16le                    
  device1PollMode <- getWord8                            
  device1PollingAddress <- getWord8                     
  device1Status <- getWord8                            
  device1ActualScanPeriod <- getIeeeFloat32              
  device1Tag <- getByteString 10                
  device1ResponseCodeStatus <- getWord16le              
  device1ActiveAlarms <- getWord8                        
  device1Current <- getIeeeFloat32                     
  device1PercentOfRange <- getIeeeFloat32                
  device1FaultValueEnable <- anyButNull                  
  device1PVUnits <- getWord8                            
  device1PV <- getIeeeFloat32                            
  device1PVFailsafeOnResetValue <- getIeeeFloat32                      
  device1SVUnits <- getWord8                            
  device1SV <- getIeeeFloat32                            
  device1SVFailsafeOnResetValue <- getIeeeFloat32                      
  device1TVUnits <- getWord8                            
  device1TV <- getIeeeFloat32                            
  device1TVFailsafeOnResetValue <- getIeeeFloat32                      
  device1FVUnits <- getWord8                            
  device1FV <- getIeeeFloat32                            
  device1FVFailsafeOnResetValue <- getIeeeFloat32                      
  device1Slot0Assignment <- getWord8                       
  device1Slot0Units <- getWord8                           
  device1Slot0Variable <- getIeeeFloat32                       
  device1Slot1Assignment <- getWord8                       
  device1Slot1Units <- getWord8                           
  device1Slot1Variable <- getIeeeFloat32                  
  device1Slot2Assignment <- getWord8                    
  device1Slot2Units <- getWord8                           
  device1Slot2Variable <- getIeeeFloat32                  
  device1Slot3Assignment <- getWord8                   
  device1Slot3Units <- getWord8                           
  device1Slot3Variable <- getIeeeFloat32                
  device1Message <- getByteString 40             
  device1Descriptor <- getByteString 20           
  device1MfrIDDeviceID <- getWord16le                    
  device1SerialNum <- getWord32le                       
  device1IDNum <- getWord32le                            
  device1SensorUnits <- getWord8                          
  device1UpperSensorLimit <- getIeeeFloat32              
  device1LowerSensorLimit <- getIeeeFloat32               
  device1MinSensorSpan <- getIeeeFloat32                  
  device1OutputUnits <- getWord8                          
  device1UpperOutputLimit <- getIeeeFloat32              
  device1LowerOutputLimit <- getIeeeFloat32               
  device1DampingValue <- getIeeeFloat32                     
  device2PollMode <- getWord8        
  device2PollingAddress <- getWord8        
  device2Status <- getWord8        
  device2ActualScanPeriod <- getIeeeFloat32  
  device2Tag <- getByteString 10
  device2ResponseCodeStatus <- getWord16le     
  device2ActiveAlarms <- getWord8        
  device2Current <- getIeeeFloat32  
  device2PercentOfRange <- getIeeeFloat32  
  device2FaultValueEnable <- anyButNull      
  device2PVUnits <- getWord8        
  device2PV <- getIeeeFloat32  
  device2PVFailsafeOnResetValue <- getIeeeFloat32  
  device2SVUnits <- getWord8        
  device2SV <- getIeeeFloat32  
  device2SVFailsafeOnResetValue <- getIeeeFloat32  
  device2TVUnits <- getWord8        
  device2TV <- getIeeeFloat32  
  device2TVFailsafeOnResetValue <- getIeeeFloat32  
  device2FVUnits <- getWord8        
  device2FV <- getIeeeFloat32  
  device2FVFailsafeOnResetValue <- getIeeeFloat32  
  device2Slot0Assignment <- getWord8        
  device2Slot0Units <- getWord8        
  device2Slot0Variable <- getIeeeFloat32  
  device2Slot1Assignment <- getWord8        
  device2Slot1Units <- getWord8        
  device2Slot1Variable <- getIeeeFloat32  
  device2Slot2Assignment <- getWord8        
  device2Slot2Units <- getWord8        
  device2Slot2Variable <- getIeeeFloat32  
  device2Slot3Assignment <- getWord8        
  device2Slot3Units <- getWord8        
  device2Slot3Variable <- getIeeeFloat32  
  device2Message <- getByteString 40
  device2Descriptor <- getByteString 20
  device2MfrIDDeviceID <- getWord16le     
  device2SerialNum <- getWord32le     
  device2IDNum <- getWord32le     
  device2SensorUnits <- getWord8        
  device2UpperSensorLimit <- getIeeeFloat32  
  device2LowerSensorLimit <- getIeeeFloat32  
  device2MinSensorSpan <- getIeeeFloat32  
  device2OutputUnits <- getWord8        
  device2UpperOutputLimit <- getIeeeFloat32  
  device2LowerOutputLimit <- getIeeeFloat32  
  device2DampingValue <- getIeeeFloat32  
  device3PollMode <- getWord8         
  device3PollingAddress <- getWord8         
  device3Status <- getWord8         
  device3ActualScanPeriod <- getIeeeFloat32   
  device3Tag <- getByteString 10 
  device3ResponseCodeStatus <- getWord16le      
  device3ActiveAlarms <- getWord8         
  device3Current <- getIeeeFloat32   
  device3PercentOfRange <- getIeeeFloat32   
  device3FaultValueEnable <- anyButNull       
  device3PVUnits <- getWord8         
  device3PV <- getIeeeFloat32   
  device3PVFailsafeOnResetValue <- getIeeeFloat32   
  device3SVUnits <- getWord8         
  device3SV <- getIeeeFloat32   
  device3SVFailsafeOnResetValue <- getIeeeFloat32   
  device3TVUnits <- getWord8         
  device3TV <- getIeeeFloat32   
  device3TVFailsafeOnResetValue <- getIeeeFloat32   
  device3FVUnits <- getWord8         
  device3FV <- getIeeeFloat32   
  device3FVFailsafeOnResetValue <- getIeeeFloat32   
  device3Slot0Assignment <- getWord8         
  device3Slot0Units <- getWord8         
  device3Slot0Variable <- getIeeeFloat32   
  device3Slot1Assignment <- getWord8         
  device3Slot1Units <- getWord8         
  device3Slot1Variable <- getIeeeFloat32   
  device3Slot2Assignment <- getWord8         
  device3Slot2Units <- getWord8         
  device3Slot2Variable <- getIeeeFloat32   
  device3Slot3Assignment <- getWord8         
  device3Slot3Units <- getWord8         
  device3Slot3Variable <- getIeeeFloat32   
  device3Message <- getByteString 40 
  device3Descriptor <- getByteString 20 
  device3MfrIDDeviceID <- getWord16le      
  device3SerialNum <- getWord32le      
  device3IDNum <- getWord32le      
  device3SensorUnits <- getWord8         
  device3UpperSensorLimit <- getIeeeFloat32   
  device3LowerSensorLimit <- getIeeeFloat32   
  device3MinSensorSpan <- getIeeeFloat32   
  device3OutputUnits <- getWord8         
  device3UpperOutputLimit <- getIeeeFloat32   
  device3LowerOutputLimit <- getIeeeFloat32   
  device3DampingValue <- getIeeeFloat32   
  device4PollMode <- getWord8        
  device4PollingAddress <- getWord8        
  device4Status <- getWord8        
  device4ActualScanPeriod <- getIeeeFloat32  
  device4Tag <- getByteString 10
  device4ResponseCodeStatus <- getWord16le     
  device4ActiveAlarms <- getWord8        
  device4Current <- getIeeeFloat32  
  device4PercentOfRange <- getIeeeFloat32  
  device4FaultValueEnable <- anyButNull      
  device4PVUnits <- getWord8        
  device4PV <- getIeeeFloat32  
  device4PVFailsafeOnResetValue <- getIeeeFloat32  
  device4SVUnits <- getWord8        
  device4SV <- getIeeeFloat32  
  device4SVFailsafeOnResetValue <- getIeeeFloat32  
  device4TVUnits <- getWord8        
  device4TV <- getIeeeFloat32  
  device4TVFailsafeOnResetValue <- getIeeeFloat32  
  device4FVUnits <- getWord8        
  device4FV <- getIeeeFloat32  
  device4FVFailsafeOnResetValue <- getIeeeFloat32  
  device4Slot0Assignment <- getWord8        
  device4Slot0Units <- getWord8        
  device4Slot0Variable <- getIeeeFloat32  
  device4Slot1Assignment <- getWord8        
  device4Slot1Units <- getWord8        
  device4Slot1Variable <- getIeeeFloat32  
  device4Slot2Assignment <- getWord8        
  device4Slot2Units <- getWord8        
  device4Slot2Variable <- getIeeeFloat32  
  device4Slot3Assignment <- getWord8        
  device4Slot3Units <- getWord8        
  device4Slot3Variable <- getIeeeFloat32  
  device4Message <- getByteString 40
  device4Descriptor <- getByteString 20
  device4MfrIDDeviceID <- getWord16le     
  device4SerialNum <- getWord32le     
  device4IDNum <- getWord32le     
  device4SensorUnits <- getWord8        
  device4UpperSensorLimit <- getIeeeFloat32  
  device4LowerSensorLimit <- getIeeeFloat32  
  device4MinSensorSpan <- getIeeeFloat32  
  device4OutputUnits <- getWord8        
  device4UpperOutputLimit <- getIeeeFloat32  
  device4LowerOutputLimit <- getIeeeFloat32  
  device4DampingValue <- getIeeeFloat32  
  device5PollMode <- getWord8        
  device5PollingAddress <- getWord8        
  device5Status <- getWord8        
  device5ActualScanPeriod <- getIeeeFloat32  
  device5Tag <- getByteString 10
  device5ResponseCodeStatus <- getWord16le     
  device5ActiveAlarms <- getWord8        
  device5Current <- getIeeeFloat32  
  device5PercentOfRange <- getIeeeFloat32  
  device5FaultValueEnable <- anyButNull      
  device5PVUnits <- getWord8        
  device5PV <- getIeeeFloat32  
  device5PVFailsafeOnResetValue <- getIeeeFloat32  
  device5SVUnits <- getWord8        
  device5SV <- getIeeeFloat32  
  device5SVFailsafeOnResetValue <- getIeeeFloat32  
  device5TVUnits <- getWord8        
  device5TV <- getIeeeFloat32  
  device5TVFailsafeOnResetValue <- getIeeeFloat32  
  device5FVUnits <- getWord8        
  device5FV <- getIeeeFloat32  
  device5FVFailsafeOnResetValue <- getIeeeFloat32  
  device5Slot0Assignment <- getWord8        
  device5Slot0Units <- getWord8        
  device5Slot0Variable <- getIeeeFloat32  
  device5Slot1Assignment <- getWord8        
  device5Slot1Units <- getWord8        
  device5Slot1Variable <- getIeeeFloat32  
  device5Slot2Assignment <- getWord8        
  device5Slot2Units <- getWord8        
  device5Slot2Variable <- getIeeeFloat32  
  device5Slot3Assignment <- getWord8        
  device5Slot3Units <- getWord8        
  device5Slot3Variable <- getIeeeFloat32  
  device5Message <- getByteString 40
  device5Descriptor <- getByteString 20
  device5MfrIDDeviceID <- getWord16le     
  device5SerialNum <- getWord32le     
  device5IDNum <- getWord32le     
  device5SensorUnits <- getWord8        
  device5UpperSensorLimit <- getIeeeFloat32  
  device5LowerSensorLimit <- getIeeeFloat32  
  device5MinSensorSpan <- getIeeeFloat32  
  device5OutputUnits <- getWord8        
  device5UpperOutputLimit <- getIeeeFloat32  
  device5LowerOutputLimit <- getIeeeFloat32  
  device5DampingValue <- getIeeeFloat32  

  return $ PointType85 channelTagId channelIOMode hARTCommMode numDevicesConnected hARTCommStatus analogMode rOCPrtclPassThruEnable hART1ResumePollTimeHART2IntrnlResister 
    eUValue failsafeOnReset failsafeValue manualValue autoValue physicalValue physicalRawDAOutput calibLiveValue zeroEUCalibValue eUCalibValueSpan rawEUValueInputorOutput 
    zeroRawEUCalibValue rawEUCalibValueSpan device1PollMode device1PollingAddress device1Status device1ActualScanPeriod device1Tag device1ResponseCodeStatus device1ActiveAlarms 
    device1Current device1PercentOfRange device1FaultValueEnable device1PVUnits device1PV device1PVFailsafeOnResetValue device1SVUnits device1SV device1SVFailsafeOnResetValue 
    device1TVUnits device1TV device1TVFailsafeOnResetValue device1FVUnits device1FV device1FVFailsafeOnResetValue device1Slot0Assignment device1Slot0Units device1Slot0Variable 
    device1Slot1Assignment device1Slot1Units device1Slot1Variable device1Slot2Assignment device1Slot2Units device1Slot2Variable device1Slot3Assignment device1Slot3Units 
    device1Slot3Variable device1Message device1Descriptor device1MfrIDDeviceID device1SerialNum device1IDNum device1SensorUnits device1UpperSensorLimit device1LowerSensorLimit 
    device1MinSensorSpan device1OutputUnits device1UpperOutputLimit device1LowerOutputLimit device1DampingValue device2PollMode device2PollingAddress device2Status 
    device2ActualScanPeriod device2Tag device2ResponseCodeStatus device2ActiveAlarms device2Current device2PercentOfRange device2FaultValueEnable device2PVUnits device2PV 
    device2PVFailsafeOnResetValue device2SVUnits device2SV device2SVFailsafeOnResetValue device2TVUnits device2TV device2TVFailsafeOnResetValue device2FVUnits device2FV 
    device2FVFailsafeOnResetValue device2Slot0Assignment device2Slot0Units device2Slot0Variable device2Slot1Assignment device2Slot1Units device2Slot1Variable device2Slot2Assignment 
    device2Slot2Units device2Slot2Variable device2Slot3Assignment device2Slot3Units device2Slot3Variable device2Message device2Descriptor device2MfrIDDeviceID device2SerialNum 
    device2IDNum device2SensorUnits device2UpperSensorLimit device2LowerSensorLimit device2MinSensorSpan device2OutputUnits device2UpperOutputLimit device2LowerOutputLimit 
    device2DampingValue device3PollMode device3PollingAddress device3Status device3ActualScanPeriod device3Tag device3ResponseCodeStatus device3ActiveAlarms device3Current 
    device3PercentOfRange device3FaultValueEnable device3PVUnits device3PV device3PVFailsafeOnResetValue device3SVUnits device3SV device3SVFailsafeOnResetValue device3TVUnits 
    device3TV device3TVFailsafeOnResetValue device3FVUnits device3FV device3FVFailsafeOnResetValue device3Slot0Assignment device3Slot0Units device3Slot0Variable device3Slot1Assignment 
    device3Slot1Units device3Slot1Variable device3Slot2Assignment device3Slot2Units device3Slot2Variable device3Slot3Assignment device3Slot3Units device3Slot3Variable device3Message 
    device3Descriptor device3MfrIDDeviceID device3SerialNum device3IDNum device3SensorUnits device3UpperSensorLimit device3LowerSensorLimit device3MinSensorSpan device3OutputUnits 
    device3UpperOutputLimit device3LowerOutputLimit device3DampingValue device4PollMode device4PollingAddress device4Status device4ActualScanPeriod device4Tag device4ResponseCodeStatus 
    device4ActiveAlarms device4Current device4PercentOfRange device4FaultValueEnable device4PVUnits device4PV device4PVFailsafeOnResetValue device4SVUnits device4SV 
    device4SVFailsafeOnResetValue device4TVUnits device4TV device4TVFailsafeOnResetValue device4FVUnits device4FV device4FVFailsafeOnResetValue device4Slot0Assignment device4Slot0Units 
    device4Slot0Variable device4Slot1Assignment device4Slot1Units device4Slot1Variable device4Slot2Assignment device4Slot2Units device4Slot2Variable device4Slot3Assignment 
    device4Slot3Units device4Slot3Variable device4Message device4Descriptor device4MfrIDDeviceID device4SerialNum device4IDNum device4SensorUnits device4UpperSensorLimit 
    device4LowerSensorLimit device4MinSensorSpan device4OutputUnits device4UpperOutputLimit device4LowerOutputLimit device4DampingValue device5PollMode device5PollingAddress 
    device5Status device5ActualScanPeriod device5Tag device5ResponseCodeStatus device5ActiveAlarms device5Current device5PercentOfRange device5FaultValueEnable device5PVUnits device5PV 
    device5PVFailsafeOnResetValue device5SVUnits device5SV device5SVFailsafeOnResetValue device5TVUnits device5TV device5TVFailsafeOnResetValue device5FVUnits device5FV 
    device5FVFailsafeOnResetValue device5Slot0Assignment device5Slot0Units device5Slot0Variable device5Slot1Assignment device5Slot1Units device5Slot1Variable device5Slot2Assignment 
    device5Slot2Units device5Slot2Variable device5Slot3Assignment device5Slot3Units device5Slot3Variable device5Message device5Descriptor device5MfrIDDeviceID device5SerialNum 
    device5IDNum device5SensorUnits device5UpperSensorLimit device5LowerSensorLimit device5MinSensorSpan device5OutputUnits device5UpperOutputLimit device5LowerOutputLimit 
    device5DampingValue