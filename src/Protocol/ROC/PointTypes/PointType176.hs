{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType176 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Utils

data PointType176 = PointType176 {
 
 pointType176DeviceTag                   :: !PointType176DeviceTag                    
,pointType176DeviceID                    :: !PointType176DeviceID                    
,pointType176ManufaturerID               :: !PointType176ManufaturerID                    
,pointType176DeviceType                  :: !PointType176DeviceType                    
,pointType176CommissionedListIndex       :: !PointType176CommissionedListIndex                    
,pointType176CommissionedFlag            :: !PointType176CommissionedFlag                    

} deriving (Eq, Show, Generic)                       

type PointType176DeviceTag               = BS.ByteString                      
type PointType176DeviceID                = Word32                      
type PointType176ManufaturerID           = Word16                      
type PointType176DeviceType              = Word16                      
type PointType176CommissionedListIndex   = Word8                      
type PointType176CommissionedFlag        = Bool                      
  
pointType176Parser :: Get PointType176
pointType176Parser = do 

  deviceTag <- getByteString 10  
  deviceID <- getWord32le  
  manufaturerID <- getWord16le  
  deviceType <- getWord16le  
  commissionedListIndex <- getWord8  
  commissionedFlag <- anyButNull  
  
  return $ PointType176 deviceTag deviceID manufaturerID deviceType commissionedListIndex commissionedFlag
