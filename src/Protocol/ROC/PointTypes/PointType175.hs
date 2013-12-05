{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType175 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.Utils

data PointType175 = PointType175 {
 
 pointType175ImportedParameterTag      :: !PointType175ImportedParameterTag
,pointType175NetworkID                 :: !PointType175NetworkID
,pointType175UniqueIdForTLP            :: !PointType175UniqueIdForTLP
,pointType175ImportedCurrentValue      :: !PointType175ImportedCurrentValue
,pointType175ImportedValueStatus       :: !PointType175ImportedValueStatus
,pointType175FaultValue                :: !PointType175FaultValue
,pointType175FaultEnable               :: !PointType175FaultEnable
,pointType175Reserved                  :: !PointType175Reserved
,pointType175NetworkIDSourceRTU        :: !PointType175NetworkIDSourceRTU
,pointType175ForwardTLP                :: !PointType175ForwardTLP

} deriving (Eq, Show, Generic)                       

type PointType175ImportedParameterTag  = BS.ByteString
type PointType175NetworkID             = Word8
type PointType175UniqueIdForTLP        = Word16
type PointType175ImportedCurrentValue  = Float
type PointType175ImportedValueStatus   = Word8
type PointType175FaultValue            = Float
type PointType175FaultEnable           = Bool
type PointType175Reserved              = Word16
type PointType175NetworkIDSourceRTU    = Word8
type PointType175ForwardTLP            = [Word8]
  
pointType175Parser :: Get PointType175
pointType175Parser = do 

  importedParameterTag <- getByteString 10
  networkID <- getWord8
  uniqueIdForTLP <- getWord16le
  importedCurrentValue <- getIeeeFloat32
  importedValueStatus <- getWord8
  faultValue <- getIeeeFloat32
  faultEnable <- anyButNull
  reserved <- getWord16le
  networkIDSourceRTU <- getWord8
  forwardTLP <- getTLP
  
  return $ PointType175 importedParameterTag networkID uniqueIdForTLP importedCurrentValue importedValueStatus faultValue faultEnable reserved networkIDSourceRTU forwardTLP 