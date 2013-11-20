{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType58 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get

data PointType58 = PointType58 {
 
 pointType58DeviceFirmwareDesc               :: !PointType58DeviceFirmwareDesc                           
,pointType58PartNumber                       :: !PointType58PartNumber                                                         
,pointType58Version                          :: !PointType58Version                                                                   
,pointType58InformationPresentFlag           :: !PointType58InformationPresentFlag                                                                 

} deriving (Read,Eq, Show, Generic)                       

type PointType58DeviceFirmwareDesc           = BS.ByteString                                                                                                    
type PointType58PartNumber                   = BS.ByteString                                                                                 
type PointType58Version                      = BS.ByteString                                                                          
type PointType58InformationPresentFlag       = Word8                                                                            
                                
pointType58Parser :: Get PointType58
pointType58Parser = do

  deviceFirmwareDesc <- getByteString 20                                                       
  partNumber <- getByteString 10                                                 
  version <- getByteString 10                                                    
  informationPresentFlag <- getWord8                                
                         
  return $ PointType58 deviceFirmwareDesc partNumber version informationPresentFlag  
  
  
  
  