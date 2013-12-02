{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType4 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Int
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.Utils

data PointType4 = PointType4 {
 pointType4PointTag                      :: !PointType4PointTag                       
,pointType4Units                         :: !PointType4Units            
,pointType4AdjustedDA0                   :: !PointType4AdjustedDA0               
,pointType4AdjustedDA100                 :: !PointType4AdjustedDA100             
,pointType4LowReading                    :: !PointType4LowReading               
,pointType4HighReading                   :: !PointType4HighReading               
,pointType4EUValue                       :: !PointType4EUValue               
,pointType4ModeCFG                       :: !PointType4ModeCFG                   
,pointType4AlarmCode                     :: !PointType4AlarmCode                 
,pointType4RawDAOutput                   :: !PointType4RawDAOutput               
,pointType4ScanningMode                  :: !PointType4ScanningMode              
,pointType4ManualEU                      :: !PointType4ManualEU                  
,pointType4PhysicalEU                    :: !PointType4PhysicalEU                
                      
} deriving (Read,Eq, Show, Generic)                       

type PointType4PointTag                  = BS.ByteString    
type PointType4Units                     = BS.ByteString            
type PointType4AdjustedDA0               = Int16           
type PointType4AdjustedDA100             = Int16               
type PointType4LowReading                = Float             
type PointType4HighReading               = Float            
type PointType4EUValue                   = Float           
type PointType4ModeCFG                   = Word8          
type PointType4AlarmCode                 = Word8           
type PointType4RawDAOutput               = Int16           
type PointType4ScanningMode              = Bool            
type PointType4ManualEU                  = Float             
type PointType4PhysicalEU                = Float             
       

pointType4Parser :: Get PointType4
pointType4Parser = do 
  pointId <- getByteString 10
  units <- getByteString 10
  adjustedDA0 <- getInt16
  adjustedDA100 <- getInt16
  lowReading <- getIeeeFloat32
  highReading <- getIeeeFloat32
  euValue <- getIeeeFloat32
  modeCFG <- getWord8
  alarmCode <- getWord8
  rawDAOutput <- getInt16
  scanningMode <- anyButNull
  manualEU <- getIeeeFloat32
  physicalEU <- getIeeeFloat32
  
  return $ PointType4 pointId units adjustedDA0 adjustedDA100 lowReading highReading euValue modeCFG alarmCode rawDAOutput scanningMode manualEU physicalEU