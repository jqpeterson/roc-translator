{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType89 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.PointTypes.Utils

data PointType89 = PointType89 {
 
 pointType89ChartType                   :: !PointType89ChartType                       
,pointType89HistPntNumber               :: !PointType89HistPntNumber                       
,pointType89DynamicPointDataTLPRef      :: !PointType89DynamicPointDataTLPRef                       
,pointType89TextString                  :: !PointType89TextString                       
,pointType89UnitsString                 :: !PointType89UnitsString                       
,pointType89ScalingOption               :: !PointType89ScalingOption                       
,pointType89UserUpperScaleRange         :: !PointType89UserUpperScaleRange                       
,pointType89UserLowerScaleRange         :: !PointType89UserLowerScaleRange                       

} deriving (Read,Eq, Show, Generic)                       

type PointType89ChartType               = Word8                        
type PointType89HistPntNumber           = Word8                        
type PointType89DynamicPointDataTLPRef  = [Word8]                        
type PointType89TextString              = BS.ByteString                        
type PointType89UnitsString             = BS.ByteString                        
type PointType89ScalingOption           = Bool                                                        
type PointType89UserUpperScaleRange     = Float                        
type PointType89UserLowerScaleRange     = Float                        

pointType89Parser :: Get PointType89
pointType89Parser = do 

  chartType <- getWord8
  histPntNumber <- getWord8
  dynamicPointDataTLPRef <- getTLP
  textString <- getByteString 10 
  unitsString <- getByteString 10 
  scalingOption <- anyButNull 
  userUpperScaleRange <- getIeeeFloat32 
  userLowerScaleRange <- getIeeeFloat32 
  
  
  return $ PointType89 chartType histPntNumber dynamicPointDataTLPRef textString unitsString scalingOption userUpperScaleRange userLowerScaleRange  
  
  
  