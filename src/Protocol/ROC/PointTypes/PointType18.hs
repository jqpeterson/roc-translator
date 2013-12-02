{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType18 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Int
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.Utils

data PointType18 = PointType18 {
 pointType18PointTag             :: !PointType18PointTag                    
,pointType18RawValue1            :: !PointType18RawValue1                  
,pointType18RawValue2            :: !PointType18RawValue2                  
,pointType18RawValue3            :: !PointType18RawValue3                  
,pointType18RawValue4            :: !PointType18RawValue4                  
,pointType18RawValue5            :: !PointType18RawValue5                   
,pointType18EUValue1             :: !PointType18EUValue1                 
,pointType18EUValue2             :: !PointType18EUValue2                 
,pointType18EUValue3             :: !PointType18EUValue3                 
,pointType18EUValue4             :: !PointType18EUValue4                 
,pointType18EUValue5             :: !PointType18EUValue5                 
,pointType18PressEffect          :: !PointType18PressEffect                    
,pointType18SetEUValue           :: !PointType18SetEUValue                   
,pointType18ManualEU             :: !PointType18ManualEU                 
,pointType18Timer                :: !PointType18Timer              
,pointType18Mode                 :: !PointType18Mode             
,pointType18Type                 :: !PointType18Type             

} deriving (Read,Eq, Show, Generic)                       

type PointType18PointTag         = BS.ByteString               
type PointType18RawValue1        = Int16              
type PointType18RawValue2        = Int16              
type PointType18RawValue3        = Int16              
type PointType18RawValue4        = Int16              
type PointType18RawValue5        = Int16               
type PointType18EUValue1         = Float            
type PointType18EUValue2         = Float            
type PointType18EUValue3         = Float            
type PointType18EUValue4         = Float            
type PointType18EUValue5         = Float            
type PointType18PressEffect      = Float                  
type PointType18SetEUValue       = Float                
type PointType18ManualEU         = Float            
type PointType18Timer            = Word16      
type PointType18Mode             = Word8    
type PointType18Type             = Word8    
  
pointType18Parser :: Get PointType18
pointType18Parser = do 

  pointTag <- getByteString 10 
  rawValue1 <- getInt16
  rawValue2 <- getInt16
  rawValue3 <- getInt16
  rawValue4 <- getInt16 
  rawValue5 <- getInt16 
  eUValue1 <- getIeeeFloat32
  eUValue2 <- getIeeeFloat32 
  eUValue3 <- getIeeeFloat32 
  eUValue4 <- getIeeeFloat32 
  eUValue5 <- getIeeeFloat32 
  pressEffect <- getIeeeFloat32 
  setEUValue <- getIeeeFloat32 
  manualEU <- getIeeeFloat32 
  timerAI <- getWord16le 
  modeAI <- getWord8 
  typeAI <- getWord8 
  
  return $ PointType18 pointTag rawValue1 rawValue2 rawValue3 rawValue4 rawValue5 eUValue1 eUValue2 eUValue3 eUValue4 eUValue5 pressEffect setEUValue manualEU timerAI modeAI typeAI











