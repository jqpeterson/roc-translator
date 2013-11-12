{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType19 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float

data PointType19 = PointType19 {
 pointType19PointTag                 :: !PointType19PointTag                           
,pointType19Archive                  :: !PointType19Archive                           
,pointType19PointType                :: !PointType19PointType                           
,pointType19PointLogicalNum          :: !PointType19PointLogicalNum                           
,pointType19ParamNum                 :: !PointType19ParamNum                           
,pointType19LastDailyValue           :: !PointType19LastDailyValue                           
,pointType19LastHoursTotal           :: !PointType19LastHoursTotal                           
,pointType19UserSpecText             :: !PointType19UserSpecText                           

} deriving (Read,Eq, Show, Generic)                       

type PointType19PointTag             = Float                           
type PointType19Archive              = Word8                  
type PointType19PointType            = Word8                  
type PointType19PointLogicalNum      = Word8                  
type PointType19ParamNum             = Word8                  
type PointType19LastDailyValue       = Float                   
type PointType19LastHoursTotal       = Float                
type PointType19UserSpecText         = BS.ByteString                
  
pointType19Parser :: Get PointType19
pointType19Parser = do 

  pointTag <- getIeeeFloat32            
  archive <- getWord8            
  pointType <- getWord8          
  pointLogicalNum <- getWord8    
  paramNum <- getWord8            
  lastDailyValue <- getIeeeFloat32     
  lastHoursTotal <- getIeeeFloat32     
  userSpecText <- getByteString 10       
  
  return $ PointType19 pointTag archive pointType pointLogicalNum paramNum lastDailyValue lastHoursTotal userSpecText



