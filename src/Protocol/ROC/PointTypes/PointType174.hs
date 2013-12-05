{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType174 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.Utils

data PointType174 = PointType174 {
 
 pointType174ExportedParameterTag          :: !PointType174ExportedParameterTag
,pointType174ExportedParameterTLP          :: !PointType174ExportedParameterTLP
,pointType174NetworkID                     :: !PointType174NetworkID
,pointType174UniqueIdForTLP                :: !PointType174UniqueIdForTLP
,pointType174ExportedTLPCurrentValue       :: !PointType174ExportedTLPCurrentValue

} deriving (Eq, Show, Generic)                       

type PointType174ExportedParameterTag      = BS.ByteString
type PointType174ExportedParameterTLP      = [Word8]
type PointType174NetworkID                 = Word8
type PointType174UniqueIdForTLP            = Word16
type PointType174ExportedTLPCurrentValue   = Float
  
pointType174Parser :: Get PointType174
pointType174Parser = do 

  exportedParameterTag <- getByteString 10
  exportedParameterTLP <- getTLP
  networkID <- getWord8
  uniqueIdForTLP <- getWord16le
  exportedTLPCurrentValue <- getIeeeFloat32
  
  return $ PointType174 exportedParameterTag exportedParameterTLP networkID uniqueIdForTLP exportedTLPCurrentValue   