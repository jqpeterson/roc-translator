{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType20 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float

data PointType20 = PointType20 {
 
 pointType20ModType                      :: !PointType20ModType                    
,pointType20PhysicalModType              :: !PointType20PhysicalModType                    
,pointType20InstalledMod                 :: !PointType20InstalledMod                    
,pointType20ModAppRev                    :: !PointType20ModAppRev                    
,pointType20ModAppPartNum                :: !PointType20ModAppPartNum                    
,pointType20ModAppBuilDate               :: !PointType20ModAppBuilDate                    
,pointType20ModAppSerialNum              :: !PointType20ModAppSerialNum                    
,pointType20CompAlarmSummary             :: !PointType20CompAlarmSummary                    
,pointType20CompIntegrityStatus          :: !PointType20CompIntegrityStatus                    
,pointType20ModAlarmStatus               :: !PointType20ModAlarmStatus                    
,pointType20ModIntegrityStatus           :: !PointType20ModIntegrityStatus                    
,pointType20ModCFG                       :: !PointType20ModCFG                    
,pointType20ModSpecificCFG1              :: !PointType20ModSpecificCFG1                    
,pointType20ModSpecificCFG2              :: !PointType20ModSpecificCFG2                    
,pointType20CFGOrStatus1                 :: !PointType20CFGOrStatus1                    
,pointType20CFGOrStatus2                 :: !PointType20CFGOrStatus2                    
,pointType20CFGOrStatus3                 :: !PointType20CFGOrStatus3                    
,pointType20ModDiagnostic1               :: !PointType20ModDiagnostic1                    
,pointType20ModDiagnostic2               :: !PointType20ModDiagnostic2                    
,pointType20ModDiagnostic3               :: !PointType20ModDiagnostic3                    
,pointType20ModDiagnostic4               :: !PointType20ModDiagnostic4                    
,pointType20ModDiagnostic5               :: !PointType20ModDiagnostic5                    
,pointType20ModDiagnostic6               :: !PointType20ModDiagnostic6                    
,pointType20ModDiagnostic7               :: !PointType20ModDiagnostic7                    
,pointType20ModDiagnostic8               :: !PointType20ModDiagnostic8                    
,pointType20ModDiagnostic9               :: !PointType20ModDiagnostic9                    
,pointType20ModDiagnostic10              :: !PointType20ModDiagnostic10                    
,pointType20ModDiagnostic11              :: !PointType20ModDiagnostic11                    
,pointType20BootRevString                :: !PointType20BootRevString                    
,pointType20BootBuildDate                :: !PointType20BootBuildDate                    
,pointType20InstldModDescString          :: !PointType20InstldModDescString                    

} deriving (Read,Eq, Show, Generic)                       

type PointType20ModType                  = Word8                   
type PointType20PhysicalModType          = Word8                                                           
type PointType20InstalledMod             = BS.ByteString                                                          
type PointType20ModAppRev                = BS.ByteString                                                           
type PointType20ModAppPartNum            = BS.ByteString                                                           
type PointType20ModAppBuilDate           = BS.ByteString                                                           
type PointType20ModAppSerialNum          = BS.ByteString                                                           
type PointType20CompAlarmSummary         = Word32                                                           
type PointType20CompIntegrityStatus      = Word32                                                           
type PointType20ModAlarmStatus           = Word32                                                           
type PointType20ModIntegrityStatus       = Word32                                                           
type PointType20ModCFG                   = Word32                                                           
type PointType20ModSpecificCFG1          = Word32                                                           
type PointType20ModSpecificCFG2          = Word32                                                           
type PointType20CFGOrStatus1             = Word32                                                           
type PointType20CFGOrStatus2             = Word32                                                           
type PointType20CFGOrStatus3             = Word32                                                           
type PointType20ModDiagnostic1           = Float                                                           
type PointType20ModDiagnostic2           = Float                                                           
type PointType20ModDiagnostic3           = Float                                                           
type PointType20ModDiagnostic4           = Word32                                                           
type PointType20ModDiagnostic5           = Word32                                                           
type PointType20ModDiagnostic6           = Word32                                                           
type PointType20ModDiagnostic7           = Word32                                                           
type PointType20ModDiagnostic8           = Word32                                                           
type PointType20ModDiagnostic9           = BS.ByteString                                                           
type PointType20ModDiagnostic10          = BS.ByteString                                                           
type PointType20ModDiagnostic11          = BS.ByteString                                                           
type PointType20BootRevString            = BS.ByteString                                                           
type PointType20BootBuildDate            = BS.ByteString                                                           
type PointType20InstldModDescString      = BS.ByteString                                                           

  
pointType20Parser :: Get PointType20 
pointType20Parser = do 
  
  modType <- getWord8                  
  physicalModType <- getWord8          
  installedMod <- getByteString 20             
  modAppRev <- getByteString 10                
  modAppPartNum <- getByteString 20            
  modAppBuilDate <- getByteString 20           
  modAppSerialNum <- getByteString 30          
  compAlarmSummary <- getWord32le         
  compIntegrityStatus <- getWord32le      
  modAlarmStatus <- getWord32le           
  modIntegrityStatus <- getWord32le       
  modCFG <- getWord32le                   
  modSpecificCFG1 <- getWord32le          
  modSpecificCFG2 <- getWord32le          
  cFGOrStatus1 <- getWord32le             
  cFGOrStatus2 <- getWord32le             
  cFGOrStatus3 <- getWord32le             
  modDiagnostic1 <- getIeeeFloat32           
  modDiagnostic2 <- getIeeeFloat32           
  modDiagnostic3 <- getIeeeFloat32           
  modDiagnostic4 <- getWord32le           
  modDiagnostic5 <- getWord32le           
  modDiagnostic6 <- getWord32le           
  modDiagnostic7 <- getWord32le           
  modDiagnostic8 <- getWord32le           
  modDiagnostic9 <- getByteString 20           
  modDiagnostic10 <- getByteString 10          
  modDiagnostic11 <- getByteString 10          
  bootRevString <- getByteString 10            
  bootBuildDate <- getByteString 20            
  instldModDescString <- getByteString 20       
  
  
  return $ PointType20 modType physicalModType installedMod modAppRev modAppPartNum modAppBuilDate modAppSerialNum compAlarmSummary compIntegrityStatus modAlarmStatus 
    modIntegrityStatus modCFG modSpecificCFG1 modSpecificCFG2 cFGOrStatus1 cFGOrStatus2 cFGOrStatus3 modDiagnostic1 modDiagnostic2 modDiagnostic3 modDiagnostic4 modDiagnostic5 
    modDiagnostic6 modDiagnostic7 modDiagnostic8 modDiagnostic9 modDiagnostic10 modDiagnostic11 bootRevString bootBuildDate instldModDescString  
  
  
  
  