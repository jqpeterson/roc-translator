{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType21 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get

data PointType21 = PointType21 {
 pointType21PointTypeDesc        :: !PointType21PointTypeDesc                      
,pointType21TemplatePointer      :: !PointType21TemplatePointer                   
,pointType21NumParameters        :: !PointType21NumParameters                     
,pointType21DisplayNum           :: !PointType21DisplayNum                        

} deriving (Read,Eq, Show, Generic)                       

type PointType21PointTypeDesc    = BS.ByteString           
type PointType21TemplatePointer  = Word32          
type PointType21NumParameters    = Word8             
type PointType21DisplayNum       = Word8             
  
pointType21Parser :: Get PointType21
pointType21Parser = do 

  pointTypeDesc <- getByteString 20     
  templatePointer <- getWord32le   
  numParameters <- getWord8     
  displayNum <- getWord8        
  
  return $ PointType21 pointTypeDesc templatePointer numParameters displayNum


