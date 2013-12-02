{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType56 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Int
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.Utils

data PointType56 = PointType56 {
 
 pointType56PointTag                    :: !PointType56PointTag                                 
,pointType56CalibZeroADValue            :: !PointType56CalibZeroADValue                                                   
,pointType56CalibMdpnt1ADValue          :: !PointType56CalibMdpnt1ADValue                                                       
,pointType56CalibMdpnt2ADValue          :: !PointType56CalibMdpnt2ADValue                                                     
,pointType56CalibMdpnt3ADValue          :: !PointType56CalibMdpnt3ADValue                                                         
,pointType56CalibSpanADValue            :: !PointType56CalibSpanADValue                                                           
,pointType56CalibZeroEUValue            :: !PointType56CalibZeroEUValue                                                        
,pointType56CalibMdpnt1EUValue          :: !PointType56CalibMdpnt1EUValue                                                   
,pointType56CalibMdpnt2EUValue          :: !PointType56CalibMdpnt2EUValue                                                       
,pointType56CalibMdpnt3EUValue          :: !PointType56CalibMdpnt3EUValue                                                   
,pointType56CalibSpanEUValue            :: !PointType56CalibSpanEUValue                                                       
,pointType56Offset                      :: !PointType56Offset                                        
,pointType56CalibSetEUValue             :: !PointType56CalibSetEUValue                                                                          
,pointType56ManualEU                    :: !PointType56ManualEU                                                
,pointType56CalibTime                   :: !PointType56CalibTime                                          
,pointType56CalibMode                   :: !PointType56CalibMode                                              
,pointType56CalibType                   :: !PointType56CalibType                                          

} deriving (Read,Eq, Show, Generic)                       

type PointType56PointTag                 = BS.ByteString                                                                           
type PointType56CalibZeroADValue         = Int16                                                                        
type PointType56CalibMdpnt1ADValue       = Int16                                                                 
type PointType56CalibMdpnt2ADValue       = Int16                                                                   
type PointType56CalibMdpnt3ADValue       = Int16                                                                     
type PointType56CalibSpanADValue         = Int16                                                                         
type PointType56CalibZeroEUValue         = Int16                                                                 
type PointType56CalibMdpnt1EUValue       = Float                                                                   
type PointType56CalibMdpnt2EUValue       = Float                                                                   
type PointType56CalibMdpnt3EUValue       = Float                                                                         
type PointType56CalibSpanEUValue         = Float                                                                 
type PointType56Offset                   = Float                                                                   
type PointType56CalibSetEUValue          = Float                                                                     
type PointType56ManualEU                 = Float                                                                   
type PointType56CalibTime                = Word16                                                                   
type PointType56CalibMode                = Word8                                                                   
type PointType56CalibType                = Word8                                                                   
                                
pointType56Parser :: Get PointType56
pointType56Parser = do 

  pointTag <- getByteString 10                 
  calibZeroADValue <- getInt16         
  calibMdpnt1ADValue <- getInt16       
  calibMdpnt2ADValue <- getInt16       
  calibMdpnt3ADValue <- getInt16       
  calibSpanADValue <- getInt16         
  calibZeroEUValue <- getInt16         
  calibMdpnt1EUValue <- getIeeeFloat32       
  calibMdpnt2EUValue <- getIeeeFloat32       
  calibMdpnt3EUValue <- getIeeeFloat32       
  calibSpanEUValue <- getIeeeFloat32         
  offset <- getIeeeFloat32                   
  calibSetEUValue <- getIeeeFloat32          
  manualEU <- getIeeeFloat32                 
  calibTime <- getWord16le                
  calibMode <- getWord8                
  calibType <- getWord8                
  
  return $ PointType56 pointTag calibZeroADValue calibMdpnt1ADValue calibMdpnt2ADValue calibMdpnt3ADValue calibSpanADValue calibZeroEUValue calibMdpnt1EUValue calibMdpnt2EUValue 
    calibMdpnt3EUValue calibSpanEUValue offset calibSetEUValue manualEU calibTime calibMode calibType  
  
  
  
  
  
  
  