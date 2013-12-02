{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType80 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.Utils


data PointType80 = PointType80 {
  
 pointType80MACAddress                                   :: !PointType80MACAddress                                           
,pointType80IPAddress                                    :: !PointType80IPAddress                                                           
,pointType80SubnetAddress                                :: !PointType80SubnetAddress                                                      
,pointType80GatewayAddress                               :: !PointType80GatewayAddress                                                     
,pointType80ROCProtocolPortNum                           :: !PointType80ROCProtocolPortNum                                       
,pointType80NumActiveROCConnections                      :: !PointType80NumActiveROCConnections                                       
,pointType80ROCProtocolTimeout                           :: !PointType80ROCProtocolTimeout                                                   
,pointType80ROCProtocolConnection                        :: !PointType80ROCProtocolConnection                                       
,pointType80NotUsed1                                     :: !PointType80NotUsed1                                         
,pointType80ModbusPortNum                                :: !PointType80ModbusPortNum                                                    
,pointType80NumActiveModbusConnections                   :: !PointType80NumActiveModbusConnections                                                 
,pointType80ModbusTimeout                                :: !PointType80ModbusTimeout                                                                      
,pointType80ModbusConnection                             :: !PointType80ModbusConnection                                                            
,pointType80NotUsed2                                     :: !PointType80NotUsed2                                                               
,pointType80ROCModbusIPSelect                            :: !PointType80ROCModbusIPSelect                                                                     
,pointType80ModbusOverIPSlaveAddress                     :: !PointType80ModbusOverIPSlaveAddress                                                                    
,pointType80ModbusMasterTCPConnectionTimeout             :: !PointType80ModbusMasterTCPConnectionTimeout                                                                       
,pointType80ModbusMasterTCPCloseTimeout                  :: !PointType80ModbusMasterTCPCloseTimeout                                                             
,pointType80Reserved1                                    :: !PointType80Reserved1                                                            
,pointType80Reserved2                                    :: !PointType80Reserved2                                                                       
,pointType80MasterTable1ModbusMasterTCPOption            :: !PointType80MasterTable1ModbusMasterTCPOption                                                                      
,pointType80IPAddressTable1Server1                       :: !PointType80IPAddressTable1Server1                                                            
,pointType80PortNumTable1Server1                         :: !PointType80PortNumTable1Server1                                               
,pointType80IPAddressTable1Server2                       :: !PointType80IPAddressTable1Server2                                              
,pointType80PortNumTable1Server2                         :: !PointType80PortNumTable1Server2                                                            
,pointType80IPAddressTable1Server3                       :: !PointType80IPAddressTable1Server3                                                      
,pointType80PortNumTable1Server3                         :: !PointType80PortNumTable1Server3                                                                 
,pointType80IPAddressTable1Server4                       :: !PointType80IPAddressTable1Server4                                                         
,pointType80PortNumTable1Server4                         :: !PointType80PortNumTable1Server4                                                       
,pointType80IPAddressTable1Server5                       :: !PointType80IPAddressTable1Server5                                                                  
,pointType80PortNumTable1Server5                         :: !PointType80PortNumTable1Server5                                                               
,pointType80IPAddressTable1Server6                       :: !PointType80IPAddressTable1Server6                                                               
,pointType80PortNumTable1Server6                         :: !PointType80PortNumTable1Server6                                                       
,pointType80IPAddressTable1Server7                       :: !PointType80IPAddressTable1Server7                                                        
,pointType80PortNumTable1Server7                         :: !PointType80PortNumTable1Server7                                                                
,pointType80IPAddressTable1Server8                       :: !PointType80IPAddressTable1Server8                                                             
,pointType80PortNumTable1Server8                         :: !PointType80PortNumTable1Server8                                                             
,pointType80IPAddressTable1Server9                       :: !PointType80IPAddressTable1Server9                                                            
,pointType80PortNumTable1Server9                         :: !PointType80PortNumTable1Server9                                                        
,pointType80IPAddressTable1Server10                      :: !PointType80IPAddressTable1Server10                                                                
,pointType80PortNumTable1Server10                        :: !PointType80PortNumTable1Server10                                                               
,pointType80IPAddressTable1Server11                      :: !PointType80IPAddressTable1Server11                                                                   
,pointType80PortNumTable1Server11                        :: !PointType80PortNumTable1Server11                                                        
,pointType80IPAddressTable1Server12                      :: !PointType80IPAddressTable1Server12                                                      
,pointType80PortNumTable1Server12                        :: !PointType80PortNumTable1Server12                                                                  
,pointType80IPAddressTable1Server13                      :: !PointType80IPAddressTable1Server13                                                                 
,pointType80PortNumTable1Server13                        :: !PointType80PortNumTable1Server13                                                                  
,pointType80IPAddressTable1Server14                      :: !PointType80IPAddressTable1Server14                                                          
,pointType80PortNumTable1Server14                        :: !PointType80PortNumTable1Server14                                                        
,pointType80IPAddressTable1Server15                      :: !PointType80IPAddressTable1Server15                                                               
,pointType80PortNumTable1Server15                        :: !PointType80PortNumTable1Server15                                                                
,pointType80IPAddressTable1Server16                      :: !PointType80IPAddressTable1Server16                                                                 
,pointType80PortNumTable1Server16                        :: !PointType80PortNumTable1Server16                                                              
,pointType80IPAddressTable1Server17                      :: !PointType80IPAddressTable1Server17                                                         
,pointType80PortNumTable1Server17                        :: !PointType80PortNumTable1Server17                                                                    
,pointType80IPAddressTable1Server18                      :: !PointType80IPAddressTable1Server18                                                                
,pointType80PortNumTable1Server18                        :: !PointType80PortNumTable1Server18                                                                
,pointType80IPAddressTable1Server19                      :: !PointType80IPAddressTable1Server19                                                                 
,pointType80PortNumTable1Server19                        :: !PointType80PortNumTable1Server19                                             
,pointType80IPAddressTable1Server20                      :: !PointType80IPAddressTable1Server20                                                               
,pointType80PortNumTable1Server20                        :: !PointType80PortNumTable1Server20                                                     
,pointType80IPAddressTable1Server21                      :: !PointType80IPAddressTable1Server21                                                           
,pointType80PortNumTable1Server21                        :: !PointType80PortNumTable1Server21                                                                  
,pointType80IPAddressTable1Server22                      :: !PointType80IPAddressTable1Server22                                                               
,pointType80PortNumTable1Server22                        :: !PointType80PortNumTable1Server22                                                                   
,pointType80IPAddressTable1Server23                      :: !PointType80IPAddressTable1Server23                                                             
,pointType80PortNumTable1Server23                        :: !PointType80PortNumTable1Server23                                                         
,pointType80IPAddressTable1Server24                      :: !PointType80IPAddressTable1Server24                                                             
,pointType80PortNumTable1Server24                        :: !PointType80PortNumTable1Server24                                                                 
,pointType80IPAddressTable1Server25                      :: !PointType80IPAddressTable1Server25                                                                    
,pointType80PortNumTable1Server25                        :: !PointType80PortNumTable1Server25                                                             
,pointType80MasterTable2ModbusMasterTCPOption            :: !PointType80MasterTable2ModbusMasterTCPOption                                                       
,pointType80IPAddressTable2Server1                       :: !PointType80IPAddressTable2Server1                                                                   
,pointType80PortNumTable2Server1                         :: !PointType80PortNumTable2Server1                                                                  
,pointType80IPAddressTable2Server2                       :: !PointType80IPAddressTable2Server2                                                               
,pointType80PortNumTable2Server2                         :: !PointType80PortNumTable2Server2                                                           
,pointType80IPAddressTable2Server3                       :: !PointType80IPAddressTable2Server3                                                        
,pointType80PortNumTable2Server3                         :: !PointType80PortNumTable2Server3                                                                    
,pointType80IPAddressTable2Server4                       :: !PointType80IPAddressTable2Server4                                                               
,pointType80PortNumTable2Server4                         :: !PointType80PortNumTable2Server4                                                                 
,pointType80IPAddressTable2Server5                       :: !PointType80IPAddressTable2Server5                                                             
,pointType80PortNumTable2Server5                         :: !PointType80PortNumTable2Server5                                                          
,pointType80IPAddressTable2Server6                       :: !PointType80IPAddressTable2Server6                                                                   
,pointType80PortNumTable2Server6                         :: !PointType80PortNumTable2Server6                                                              
,pointType80IPAddressTable2Server7                       :: !PointType80IPAddressTable2Server7                                                                   
,pointType80PortNumTable2Server7                         :: !PointType80PortNumTable2Server7                                                          
,pointType80IPAddressTable2Server8                       :: !PointType80IPAddressTable2Server8                                                         
,pointType80PortNumTable2Server8                         :: !PointType80PortNumTable2Server8                                                                 
,pointType80IPAddressTable2Server9                       :: !PointType80IPAddressTable2Server9                                                                 
,pointType80PortNumTable2Server9                         :: !PointType80PortNumTable2Server9                                                                    
,pointType80IPAddressTable2Server10                      :: !PointType80IPAddressTable2Server10                                                             
,pointType80PortNumTable2Server10                        :: !PointType80PortNumTable2Server10                                                       
,pointType80IPAddressTable2Server11                      :: !PointType80IPAddressTable2Server11                                                               
,pointType80PortNumTable2Server11                        :: !PointType80PortNumTable2Server11                                                                  
,pointType80IPAddressTable2Server12                      :: !PointType80IPAddressTable2Server12                                                                  
,pointType80PortNumTable2Server12                        :: !PointType80PortNumTable2Server12                                                           
,pointType80IPAddressTable2Server13                      :: !PointType80IPAddressTable2Server13                                                     
,pointType80PortNumTable2Server13                        :: !PointType80PortNumTable2Server13                                                                    
,pointType80IPAddressTable2Server14                      :: !PointType80IPAddressTable2Server14                                                                  
,pointType80PortNumTable2Server14                        :: !PointType80PortNumTable2Server14                                                                  
,pointType80IPAddressTable2Server15                      :: !PointType80IPAddressTable2Server15                                                             
,pointType80PortNumTable2Server15                        :: !PointType80PortNumTable2Server15                                                           
,pointType80IPAddressTable2Server16                      :: !PointType80IPAddressTable2Server16                                                                
,pointType80PortNumTable2Server16                        :: !PointType80PortNumTable2Server16                                                                
,pointType80IPAddressTable2Server17                      :: !PointType80IPAddressTable2Server17                                                                    
,pointType80PortNumTable2Server17                        :: !PointType80PortNumTable2Server17                                                               
,pointType80IPAddressTable2Server18                      :: !PointType80IPAddressTable2Server18                                                          
,pointType80PortNumTable2Server18                        :: !PointType80PortNumTable2Server18                                                                  
,pointType80IPAddressTable2Server19                      :: !PointType80IPAddressTable2Server19                                                                  
,pointType80PortNumTable2Server19                        :: !PointType80PortNumTable2Server19                                                           
,pointType80IPAddressTable2Server20                      :: !PointType80IPAddressTable2Server20                                                     
,pointType80PortNumTable2Server20                        :: !PointType80PortNumTable2Server20                                                                    
,pointType80IPAddressTable2Server21                      :: !PointType80IPAddressTable2Server21                                                                  
,pointType80PortNumTable2Server21                        :: !PointType80PortNumTable2Server21                                                                  
,pointType80IPAddressTable2Server22                      :: !PointType80IPAddressTable2Server22                                                             
,pointType80PortNumTable2Server22                        :: !PointType80PortNumTable2Server22                                                           
,pointType80IPAddressTable2Server23                      :: !PointType80IPAddressTable2Server23                                                                
,pointType80PortNumTable2Server23                        :: !PointType80PortNumTable2Server23                                                                
,pointType80IPAddressTable2Server24                      :: !PointType80IPAddressTable2Server24                                                                    
,pointType80PortNumTable2Server24                        :: !PointType80PortNumTable2Server24                                                               
,pointType80IPAddressTable2Server25                      :: !PointType80IPAddressTable2Server25                                                          
,pointType80PortNumTable2Server25                        :: !PointType80PortNumTable2Server25                                                                                                  
  
} deriving (Read,Eq, Show, Generic)                       
                                  
type PointType80MACAddress                                           = BS.ByteString                                                                     
type PointType80IPAddress                                            = BS.ByteString                                                                                        
type PointType80SubnetAddress                                        = BS.ByteString                                                                              
type PointType80GatewayAddress                                       = BS.ByteString                                                                               
type PointType80ROCProtocolPortNum                                   = Word16                                                              
type PointType80NumActiveROCConnections                              = Word8                                                                             
type PointType80ROCProtocolTimeout                                   = Float                                                          
type PointType80ROCProtocolConnection                                = Bool                                                                  
type PointType80NotUsed1                                             = Word32                              
type PointType80ModbusPortNum                                        = Word16                                                                          
type PointType80NumActiveModbusConnections                           = Word8                                                                                               
type PointType80ModbusTimeout                                        = Float                                                                                       
type PointType80ModbusConnection                                     = Bool                                                                                           
type PointType80NotUsed2                                             = Word32                                                                                          
type PointType80ROCModbusIPSelect                                    = Word8                                                                                           
type PointType80ModbusOverIPSlaveAddress                             = Word8                                                                                                 
type PointType80ModbusMasterTCPConnectionTimeout                     = Word8                                                                                        
type PointType80ModbusMasterTCPCloseTimeout                          = Word8                                                                                    
type PointType80Reserved1                                            = Word8                                                                                               
type PointType80Reserved2                                            = Word8                                                                                                
type PointType80MasterTable1ModbusMasterTCPOption                    = Bool                                                                                       
type PointType80IPAddressTable1Server1                               = BS.ByteString                                                                        
type PointType80PortNumTable1Server1                                 = Word16                                                                    
type PointType80IPAddressTable1Server2                               = BS.ByteString                                                                                       
type PointType80PortNumTable1Server2                                 = Word16                                                                                     
type PointType80IPAddressTable1Server3                               = BS.ByteString                                                                                            
type PointType80PortNumTable1Server3                                 = Word16                                                                                         
type PointType80IPAddressTable1Server4                               = BS.ByteString                                                                                  
type PointType80PortNumTable1Server4                                 = Word16                                                                                                     
type PointType80IPAddressTable1Server5                               = BS.ByteString                                                                                          
type PointType80PortNumTable1Server5                                 = Word16                                                                                               
type PointType80IPAddressTable1Server6                               = BS.ByteString                                                                              
type PointType80PortNumTable1Server6                                 = Word16                                                                                           
type PointType80IPAddressTable1Server7                               = BS.ByteString                                                                                          
type PointType80PortNumTable1Server7                                 = Word16                                                                                             
type PointType80IPAddressTable1Server8                               = BS.ByteString                                                                                    
type PointType80PortNumTable1Server8                                 = Word16                                                                                               
type PointType80IPAddressTable1Server9                               = BS.ByteString                                                                                    
type PointType80PortNumTable1Server9                                 = Word16                                                                                                 
type PointType80IPAddressTable1Server10                              = BS.ByteString                                                                                          
type PointType80PortNumTable1Server10                                = Word16                                                                                                       
type PointType80IPAddressTable1Server11                              = BS.ByteString                                                                                
type PointType80PortNumTable1Server11                                = Word16                                                                                       
type PointType80IPAddressTable1Server12                              = BS.ByteString                                                                                              
type PointType80PortNumTable1Server12                                = Word16                                                                                                     
type PointType80IPAddressTable1Server13                              = BS.ByteString                                                                                              
type PointType80PortNumTable1Server13                                = Word16                                                                                           
type PointType80IPAddressTable1Server14                              = BS.ByteString                                                                                    
type PointType80PortNumTable1Server14                                = Word16                                                                                               
type PointType80IPAddressTable1Server15                              = BS.ByteString                                                                                            
type PointType80PortNumTable1Server15                                = Word16                                                                                                 
type PointType80IPAddressTable1Server16                              = BS.ByteString                                                                                          
type PointType80PortNumTable1Server16                                = Word16                                                                                           
type PointType80IPAddressTable1Server17                              = BS.ByteString                                                                                                
type PointType80PortNumTable1Server17                                = Word16                                                                                                 
type PointType80IPAddressTable1Server18                              = BS.ByteString                                                                                            
type PointType80PortNumTable1Server18                                = Word16                                                                                                     
type PointType80IPAddressTable1Server19                              = BS.ByteString                                                                      
type PointType80PortNumTable1Server19                                = Word16                                                                                                
type PointType80IPAddressTable1Server20                              = BS.ByteString                                                                             
type PointType80PortNumTable1Server20                                = Word16                                                                                    
type PointType80IPAddressTable1Server21                              = BS.ByteString                                                                                           
type PointType80PortNumTable1Server21                                = Word16                                                                                                
type PointType80IPAddressTable1Server22                              = BS.ByteString                                                                                               
type PointType80PortNumTable1Server22                                = Word16                                                                                             
type PointType80IPAddressTable1Server23                              = BS.ByteString                                                                                  
type PointType80PortNumTable1Server23                                = Word16                                                                                               
type PointType80IPAddressTable1Server24                              = BS.ByteString                                                                                             
type PointType80PortNumTable1Server24                                = Word16                                                                                                      
type PointType80IPAddressTable1Server25                              = BS.ByteString                                                                                     
type PointType80PortNumTable1Server25                                = Word16                                                                                         
type PointType80MasterTable2ModbusMasterTCPOption                    = Bool                                                                                          
type PointType80IPAddressTable2Server1                               = BS.ByteString                                                                                             
type PointType80PortNumTable2Server1                                 = Word16                                                                                               
type PointType80IPAddressTable2Server2                               = BS.ByteString                                                                                      
type PointType80PortNumTable2Server2                                 = Word16                                                                                           
type PointType80IPAddressTable2Server3                               = BS.ByteString                                                                                               
type PointType80PortNumTable2Server3                                 = Word16                                                                                               
type PointType80IPAddressTable2Server4                               = BS.ByteString                                                                                        
type PointType80PortNumTable2Server4                                 = Word16                                                                                                
type PointType80IPAddressTable2Server5                               = BS.ByteString                                                                                    
type PointType80PortNumTable2Server5                                 = Word16                                                                                                   
type PointType80IPAddressTable2Server6                               = BS.ByteString                                                                                     
type PointType80PortNumTable2Server6                                 = Word16                                                                                                      
type PointType80IPAddressTable2Server7                               = BS.ByteString                                                                                      
type PointType80PortNumTable2Server7                                 = Word16                                                                                          
type PointType80IPAddressTable2Server8                               = BS.ByteString                                                                                            
type PointType80PortNumTable2Server8                                 = Word16                                                                                                     
type PointType80IPAddressTable2Server9                               = BS.ByteString                                                                                            
type PointType80PortNumTable2Server9                                 = Word16                                                                                              
type PointType80IPAddressTable2Server10                              = BS.ByteString                                                                                   
type PointType80PortNumTable2Server10                                = Word16                                                                                                   
type PointType80IPAddressTable2Server11                              = BS.ByteString                                                                                              
type PointType80PortNumTable2Server11                                = Word16                                                                                                   
type PointType80IPAddressTable2Server12                              = BS.ByteString                                                                                       
type PointType80PortNumTable2Server12                                = Word16                                                                                     
type PointType80IPAddressTable2Server13                              = BS.ByteString                                                                                                
type PointType80PortNumTable2Server13                                = Word16                                                                                                  
type PointType80IPAddressTable2Server14                              = BS.ByteString                                                                                              
type PointType80PortNumTable2Server14                                = Word16                                                                                               
type PointType80IPAddressTable2Server15                              = BS.ByteString                                                                                       
type PointType80PortNumTable2Server15                                = Word16                                                                                                 
type PointType80IPAddressTable2Server16                              = BS.ByteString                                                                                            
type PointType80PortNumTable2Server16                                = Word16                                                                                                        
type PointType80IPAddressTable2Server17                              = BS.ByteString                                                                                          
type PointType80PortNumTable2Server17                                = Word16                                                                                           
type PointType80IPAddressTable2Server18                              = BS.ByteString                                                                                          
type PointType80PortNumTable2Server18                                = Word16                                                                                             
type PointType80IPAddressTable2Server19                              = BS.ByteString                                                                                       
type PointType80PortNumTable2Server19                                = Word16                                                                                     
type PointType80IPAddressTable2Server20                              = BS.ByteString                                                                                                
type PointType80PortNumTable2Server20                                = Word16                                                                                                  
type PointType80IPAddressTable2Server21                              = BS.ByteString                                                                                              
type PointType80PortNumTable2Server21                                = Word16                                                                                               
type PointType80IPAddressTable2Server22                              = BS.ByteString                                                                                       
type PointType80PortNumTable2Server22                                = Word16                                                                                                 
type PointType80IPAddressTable2Server23                              = BS.ByteString                                                                                            
type PointType80PortNumTable2Server23                                = Word16                                                                                                        
type PointType80IPAddressTable2Server24                              = BS.ByteString                                                                                          
type PointType80PortNumTable2Server24                                = Word16                                                                                           
type PointType80IPAddressTable2Server25                              = BS.ByteString                                                                                          
type PointType80PortNumTable2Server25                                = Word16                                                                                              
  
pointType80Parser :: Get PointType80
pointType80Parser = do 
                                  
  mACAddress <- getByteString 12                                                                                                               
  iPAddress <- getByteString 20                                                                                                                     
  subnetAddress <- getByteString 20                                                                                                                  
  gatewayAddress <- getByteString 20                                                                                                               
  rOCProtocolPortNum <- getWord16le                                                                                                              
  numActiveROCConnections <- getWord8                                                                                                                     
  rOCProtocolTimeout <- getIeeeFloat32                                                                                                                     
  rOCProtocolConnection <- anyButNull                                                                                                                
  notUsed1 <- getWord32le                                                                                                                               
  modbusPortNum <- getWord16le                                                                                                                          
  numActiveModbusConnections <- getWord8                                                                                                                   
  modbusTimeout <- getIeeeFloat32                                                                                                                         
  modbusConnection <- anyButNull                                                                                                                   
  notUsed2 <- getWord32le                                                                                                                                 
  rOCModbusIPSelect <- getWord8                                                                                                                        
  modbusOverIPSlaveAddress <-  getWord8                                                                                                                   
  modbusMasterTCPConnectionTimeout <- getWord8                                                                                                    
  modbusMasterTCPCloseTimeout <- getWord8                                                                                                         
  reserved1 <- getWord8                                                                                                                                 
  reserved2 <- getWord8                                                                                                                               
  masterTable1ModbusMasterTCPOption <- anyButNull                                                                                                          
  iPAddressTable1Server1 <- getByteString 20                                                                                               
  portNumTable1Server1 <- getWord16le                                                                                                
  iPAddressTable1Server2 <- getByteString 20                                                                                                  
  portNumTable1Server2 <- getWord16le                                                                                                    
  iPAddressTable1Server3 <- getByteString 20                                                                                                     
  portNumTable1Server3 <- getWord16le                                                                                                     
  iPAddressTable1Server4 <- getByteString 20                                                                                             
  portNumTable1Server4 <- getWord16le                                                                                                     
  iPAddressTable1Server5 <- getByteString 20                                                                                                 
  portNumTable1Server5 <- getWord16le                                                                                                           
  iPAddressTable1Server6 <- getByteString 20                                                                                                
  portNumTable1Server6 <- getWord16le                                                                                                 
  iPAddressTable1Server7 <- getByteString 20                                                                                                   
  portNumTable1Server7 <- getWord16le                                                                                                        
  iPAddressTable1Server8 <- getByteString 20                                                                                                      
  portNumTable1Server8 <- getWord16le                                                                                                   
  iPAddressTable1Server9 <- getByteString 20                                                                                              
  portNumTable1Server9 <- getWord16le                                                                                                         
  iPAddressTable1Server10 <- getByteString 20                                                                                                 
  portNumTable1Server10 <- getWord16le                                                                                                        
  iPAddressTable1Server11 <- getByteString 20                                                                                               
  portNumTable1Server11 <- getWord16le                                                                                                   
  iPAddressTable1Server12 <- getByteString 20                                                                                                  
  portNumTable1Server12 <- getWord16le                                                                                                    
  iPAddressTable1Server13 <- getByteString 20                                                                                                     
  portNumTable1Server13 <- getWord16le                                                                                                     
  iPAddressTable1Server14 <- getByteString 20                                                                                             
  portNumTable1Server14 <- getWord16le                                                                                                     
  iPAddressTable1Server15 <- getByteString 20                                                                                                 
  portNumTable1Server15 <- getWord16le                                                                                                           
  iPAddressTable1Server16 <- getByteString 20                                                                                               
  portNumTable1Server16 <- getWord16le                                                                                               
  iPAddressTable1Server17 <- getByteString 20                                                                                                 
  portNumTable1Server17 <- getWord16le                                                                                                      
  iPAddressTable1Server18 <- getByteString 20                                                                
  portNumTable1Server18 <- getWord16le                                                                     
  iPAddressTable1Server19 <- getByteString 20                                                      
  portNumTable1Server19 <- getWord16le                                                                       
  iPAddressTable1Server20 <- getByteString 20                                                            
  portNumTable1Server20 <- getWord16le                                                               
  iPAddressTable1Server21 <- getByteString 20                                                               
  portNumTable1Server21 <- getWord16le                                                                   
  iPAddressTable1Server22 <- getByteString 20                                                                  
  portNumTable1Server22 <- getWord16le                                                                 
  iPAddressTable1Server23 <- getByteString 20                                                          
  portNumTable1Server23 <- getWord16le                                                                    
  iPAddressTable1Server24 <- getByteString 20                                                              
  portNumTable1Server24 <- getWord16le                                                                       
  iPAddressTable1Server25 <- getByteString 20                                                            
  portNumTable1Server25 <- getWord16le                                                               
  masterTable2ModbusMasterTCPOption <- anyButNull                                                          
  iPAddressTable2Server1 <- getByteString 20                                                               
  portNumTable2Server1 <- getWord16le                                                                          
  iPAddressTable2Server2 <- getByteString 20                                                             
  portNumTable2Server2 <- getWord16le                                                                  
  iPAddressTable2Server3 <- getByteString 20                                                                
  portNumTable2Server3 <- getWord16le                                                                      
  iPAddressTable2Server4 <- getByteString 20                                                                   
  portNumTable2Server4 <- getWord16le                                                                    
  iPAddressTable2Server5 <- getByteString 20                                                           
  portNumTable2Server5 <- getWord16le                                                                       
  iPAddressTable2Server6 <- getByteString 20                                                               
  portNumTable2Server6 <- getWord16le                                                                          
  iPAddressTable2Server7 <- getByteString 20                                                             
  portNumTable2Server7 <- getWord16le                                                                  
  iPAddressTable2Server8 <- getByteString 20                                                                
  portNumTable2Server8 <- getWord16le                                                                      
  iPAddressTable2Server9 <- getByteString 20                                                                   
  portNumTable2Server9 <- getWord16le                                                                    
  iPAddressTable2Server10 <- getByteString 20                                                          
  portNumTable2Server10 <- getWord16le                                                                      
  iPAddressTable2Server11 <- getByteString 20                                                              
  portNumTable2Server11 <- getWord16le                                                                         
  iPAddressTable2Server12 <- getByteString 20                                                            
  portNumTable2Server12 <- getWord16le                                                                 
  iPAddressTable2Server13 <- getByteString 20                                                               
  portNumTable2Server13 <- getWord16le                                                                     
  iPAddressTable2Server14 <- getByteString 20                                                                  
  portNumTable2Server14 <- getWord16le                                                                   
  iPAddressTable2Server15 <- getByteString 20                                                          
  portNumTable2Server15 <- getWord16le                                                                      
  iPAddressTable2Server16 <- getByteString 20                                                              
  portNumTable2Server16 <- getWord16le                                                                         
  iPAddressTable2Server17 <- getByteString 20                                                            
  portNumTable2Server17 <- getWord16le                                                                 
  iPAddressTable2Server18 <- getByteString 20                                                               
  portNumTable2Server18 <- getWord16le                                                                     
  iPAddressTable2Server19 <- getByteString 20                                 
  portNumTable2Server19 <- getWord16le                                        
  iPAddressTable2Server20 <- getByteString 20                                 
  portNumTable2Server20 <- getWord16le                                        
  iPAddressTable2Server21 <- getByteString 20                                 
  portNumTable2Server21 <- getWord16le                                        
  iPAddressTable2Server22 <- getByteString 20                                 
  portNumTable2Server22 <- getWord16le                                        
  iPAddressTable2Server23 <- getByteString 20                                 
  portNumTable2Server23 <- getWord16le                                        
  iPAddressTable2Server24 <- getByteString 20                                 
  portNumTable2Server24 <- getWord16le                                        
  iPAddressTable2Server25 <- getByteString 20                                 
  portNumTable2Server25 <- getWord16le                                        
  
  return $ PointType80 mACAddress iPAddress subnetAddress gatewayAddress rOCProtocolPortNum numActiveROCConnections rOCProtocolTimeout rOCProtocolConnection notUsed1 modbusPortNum 
    numActiveModbusConnections modbusTimeout modbusConnection notUsed2 rOCModbusIPSelect modbusOverIPSlaveAddress modbusMasterTCPConnectionTimeout modbusMasterTCPCloseTimeout reserved1 
    reserved2 masterTable1ModbusMasterTCPOption iPAddressTable1Server1 portNumTable1Server1 iPAddressTable1Server2 portNumTable1Server2 iPAddressTable1Server3 portNumTable1Server3 
    iPAddressTable1Server4 portNumTable1Server4 iPAddressTable1Server5 portNumTable1Server5 iPAddressTable1Server6 portNumTable1Server6 iPAddressTable1Server7 portNumTable1Server7 
    iPAddressTable1Server8 portNumTable1Server8 iPAddressTable1Server9 portNumTable1Server9 iPAddressTable1Server10 portNumTable1Server10 iPAddressTable1Server11 portNumTable1Server11 
    iPAddressTable1Server12 portNumTable1Server12 iPAddressTable1Server13 portNumTable1Server13 iPAddressTable1Server14 portNumTable1Server14 iPAddressTable1Server15 portNumTable1Server15 
    iPAddressTable1Server16 portNumTable1Server16 iPAddressTable1Server17 portNumTable1Server17 iPAddressTable1Server18 portNumTable1Server18 iPAddressTable1Server19 portNumTable1Server19 
    iPAddressTable1Server20 portNumTable1Server20 iPAddressTable1Server21 portNumTable1Server21 iPAddressTable1Server22 portNumTable1Server22 iPAddressTable1Server23 portNumTable1Server23 
    iPAddressTable1Server24 portNumTable1Server24 iPAddressTable1Server25 portNumTable1Server25 masterTable2ModbusMasterTCPOption iPAddressTable2Server1 portNumTable2Server1 
    iPAddressTable2Server2 portNumTable2Server2 iPAddressTable2Server3 portNumTable2Server3 iPAddressTable2Server4 portNumTable2Server4 iPAddressTable2Server5 portNumTable2Server5 
    iPAddressTable2Server6 portNumTable2Server6 iPAddressTable2Server7 portNumTable2Server7 iPAddressTable2Server8 portNumTable2Server8 iPAddressTable2Server9 portNumTable2Server9 
    iPAddressTable2Server10 portNumTable2Server10 iPAddressTable2Server11 portNumTable2Server11 iPAddressTable2Server12 portNumTable2Server12 iPAddressTable2Server13 portNumTable2Server13 
    iPAddressTable2Server14 portNumTable2Server14 iPAddressTable2Server15 portNumTable2Server15 iPAddressTable2Server16 portNumTable2Server16 iPAddressTable2Server17 portNumTable2Server17 
    iPAddressTable2Server18 portNumTable2Server18 iPAddressTable2Server19 portNumTable2Server19 iPAddressTable2Server20 portNumTable2Server20 iPAddressTable2Server21 portNumTable2Server21 
    iPAddressTable2Server22 portNumTable2Server22 iPAddressTable2Server23 portNumTable2Server23 iPAddressTable2Server24 portNumTable2Server24 iPAddressTable2Server25 portNumTable2Server25



