{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType9 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.PointTypes.Utils

data PointType9 = PointType9 {
 pointType9Line1Text       :: !PointType9Line1Text                                            
,pointType9Line2Text       :: !PointType9Line2Text                              
,pointType9Line3Text       :: !PointType9Line3Text                                       
,pointType9Line1TLP        :: !PointType9Line1TLP                                    
,pointType9Line2TLP        :: !PointType9Line2TLP                                       
,pointType9Line3TLP        :: !PointType9Line3TLP                                      
                      
} deriving (Read,Eq, Show, Generic)                       

type PointType9Line1Text   = BS.ByteString    
type PointType9Line2Text   = BS.ByteString            
type PointType9Line3Text   = BS.ByteString           
type PointType9Line1TLP    = [Word8]               
type PointType9Line2TLP    = [Word8]             
type PointType9Line3TLP    = [Word8]            

pointType9Parser :: Get PointType9
pointType9Parser = do 
  line1Text <- getByteString 10
  line2Text <- getByteString 10
  line3Text <- getByteString 10
  line1TLP <- getTLP
  line2TLP <- getTLP
  line3TLP <- getTLP
  
  return $ PointType9 line1Text line2Text line3Text line1TLP line2TLP line3TLP                    