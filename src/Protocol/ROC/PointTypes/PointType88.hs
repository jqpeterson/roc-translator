{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType88 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.PointTypes.Utils

data PointType88 = PointType88 {
 
 pointType88TagID            :: !PointType88TagID                    
,pointType88UnitsString      :: !PointType88UnitsString                    
,pointType88DataTLP          :: !PointType88DataTLP                    

} deriving (Read,Eq, Show, Generic)                       

type PointType88TagID        = BS.ByteString           
type PointType88UnitsString  = BS.ByteString           
type PointType88DataTLP      = [Word8]           


pointType88Parser :: Get PointType88
pointType88Parser = do 

  tagID <- getByteString 10                   
  unitsString <- getByteString 10             
  dataTLP <- getTLP                  
  
  return $ PointType88 tagID unitsString dataTLP  