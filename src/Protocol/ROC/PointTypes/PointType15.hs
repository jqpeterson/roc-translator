{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType15 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Utils
import Protocol.ROC.Float

data PointType15 = PointType15 {
 pointType15ROCAddress                                 :: !PointType15ROCAddress                                                
,pointType15ROCGroup                                   :: !PointType15ROCGroup                                                
,pointType15StationName                                :: !PointType15StationName                                                        
,pointType15ActvPIDs                                   :: !PointType15ActvPIDs                                                
,pointType15ActvAGAMtrRuns                             :: !PointType15ActvAGAMtrRuns                                               
,pointType15ActvTankorNumFSTorSysStatus                :: !PointType15ActvTankorNumFSTorSysStatus                                                
,pointType15NumBaseDataPntsorNumStdrdHistPnts          :: !PointType15NumBaseDataPntsorNumStdrdHistPnts                                            
,pointType15NumRAM1DataPntsorNumExtHistPnts            :: !PointType15NumRAM1DataPntsorNumExtHistPnts                                                
,pointType15NumRAM2DataPntsorHist3DataPnts             :: !PointType15NumRAM2DataPntsorHist3DataPnts                                                
,pointType15ForceEndOfDay                              :: !PointType15ForceEndOfDay                                               
,pointType15ContractHour                               :: !PointType15ContractHour                                                
,pointType15VersionName                                :: !PointType15VersionName                                                        
,pointType15MfgId                                      :: !PointType15MfgId                                                        
,pointType15TimeCreated                                :: !PointType15TimeCreated                                                    
,pointType15UnitSerialNum                              :: !PointType15UnitSerialNum                                                        
,pointType15CustomerName                               :: !PointType15CustomerName                                                       
,pointType15MaxNumPIDs                                 :: !PointType15MaxNumPIDs                                                
,pointType15MaxNumAGAMtrRuns                           :: !PointType15MaxNumAGAMtrRuns                                              
,pointType15MaxNumTanks                                :: !PointType15MaxNumTanks                                             
,pointType15MaxNumFSTs                                 :: !PointType15MaxNumFSTs                                             
,pointType15RAMInstMemAssign                           :: !PointType15RAMInstMemAssign                                           
,pointType15ROMInstMemAssign                           :: !PointType15ROMInstMemAssign                                             
,pointType15MPULoading                                 :: !PointType15MPULoading                                             
,pointType15Utilities                                  :: !PointType15Utilities                                             
,pointType15TypeOfROCorFloboss                         :: !PointType15TypeOfROCorFloboss                                              
,pointType15UnitsFlag                                  :: !PointType15UnitsFlag                                             


} deriving (Read,Eq, Show, Generic)                       

type PointType15ROCAddress                             = Word8                                                                       
type PointType15ROCGroup                               = Word8                                                                       
type PointType15StationName                            = BS.ByteString                                                               
type PointType15ActvPIDs                               = Word8                                                                       
type PointType15ActvAGAMtrRuns                         = Word8                                                                       
type PointType15ActvTankorNumFSTorSysStatus            = Word8                                                                       
type PointType15NumBaseDataPntsorNumStdrdHistPnts      = Word8                                                                       
type PointType15NumRAM1DataPntsorNumExtHistPnts        = Word8                                                                       
type PointType15NumRAM2DataPntsorHist3DataPnts         = Word8                                                                       
type PointType15ForceEndOfDay                          = Bool                                                                        
type PointType15ContractHour                           = Word8                                                                       
type PointType15VersionName                            = BS.ByteString                                                               
type PointType15MfgId                                  = BS.ByteString                                                               
type PointType15TimeCreated                            = BS.ByteString                                                               
type PointType15UnitSerialNum                          = BS.ByteString                                                               
type PointType15CustomerName                           = BS.ByteString                                                               
type PointType15MaxNumPIDs                             = Word8                                                                       
type PointType15MaxNumAGAMtrRuns                       = Word8                                                                       
type PointType15MaxNumTanks                            = Word8                                                                       
type PointType15MaxNumFSTs                             = Word8                                                                       
type PointType15RAMInstMemAssign                       = Word8                                                                       
type PointType15ROMInstMemAssign                       = Word8                                                                       
type PointType15MPULoading                             = Float                                                                       
type PointType15Utilities                              = Word8                                                                       
type PointType15TypeOfROCorFloboss                     = Word16                                                                      
type PointType15UnitsFlag                              = Word8                                                                                                     

pointType15Parser :: Get PointType15
pointType15Parser = do 
  
  rOCAddress <- getWord8
  rOCGroup <- getWord8
  stationName <- getByteString 20
  actvPIDs <- getWord8
  actvAGAMtrRuns <- getWord8
  actvTankorNumFSTorSysStatus <- getWord8
  numBaseDataPntsorNumStdrdHistPnts <- getWord8
  numRAM1DataPntsorNumExtHistPnts <- getWord8
  numRAM2DataPntsorHist3DataPnts <- getWord8
  forceEndOfDay <- anyButNull
  contractHour <- getWord8
  versionName <- getByteString 20
  mfgId <- getByteString 20
  timeCreated <- getByteString 20
  unitSerialNum <- getByteString 12
  customerName <- getByteString 20
  maxNumPIDs <- getWord8
  maxNumAGAMtrRuns <- getWord8
  maxNumTanks <- getWord8 
  maxNumFSTs <- getWord8 
  rAMInstMemAssign <- getWord8 
  rOMInstMemAssign <- getWord8
  mPULoading <- getIeeeFloat32
  utilities <- getWord8
  typeOfROCorFloboss <- getWord16le 
  unitsFlag <- getWord8
  
  
  return $ PointType15 rOCAddress rOCGroup stationName actvPIDs actvAGAMtrRuns actvTankorNumFSTorSysStatus numBaseDataPntsorNumStdrdHistPnts numRAM1DataPntsorNumExtHistPnts 
    numRAM2DataPntsorHist3DataPnts forceEndOfDay contractHour versionName mfgId timeCreated unitSerialNum customerName maxNumPIDs maxNumAGAMtrRuns maxNumTanks maxNumFSTs 
    rAMInstMemAssign rOMInstMemAssign mPULoading utilities typeOfROCorFloboss unitsFlag












