{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType17 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.Utils

data PointType17 = PointType17 {
 pointType17PointTag                  :: !PointType17PointTag
,pointType17IntFlag                   :: !PointType17IntFlag
,pointType17Data1                     :: !PointType17Data1
,pointType17Data2                     :: !PointType17Data2
,pointType17Data3                     :: !PointType17Data3
,pointType17Data4                     :: !PointType17Data4
,pointType17Data5                     :: !PointType17Data5
,pointType17Data6                     :: !PointType17Data6
,pointType17Data7                     :: !PointType17Data7
,pointType17Data8                     :: !PointType17Data8
,pointType17Data9                     :: !PointType17Data9
,pointType17Data10                    :: !PointType17Data10
,pointType17Data11                    :: !PointType17Data11
,pointType17Data12                    :: !PointType17Data12
,pointType17Data13                    :: !PointType17Data13
,pointType17Data14                    :: !PointType17Data14
,pointType17Data15                    :: !PointType17Data15
,pointType17Data16                    :: !PointType17Data16
,pointType17Data17                    :: !PointType17Data17
,pointType17Data18                    :: !PointType17Data18
,pointType17Data19                    :: !PointType17Data19
,pointType17Data20                    :: !PointType17Data20
,pointType17EnableSoftPntLog          :: !PointType17EnableSoftPntLog

} deriving (Read,Eq, Show, Generic)                       

type PointType17PointTag              = BS.ByteString    
type PointType17IntFlag               = Word16   
type PointType17Data1                 = Float 
type PointType17Data2                 = Float 
type PointType17Data3                 = Float 
type PointType17Data4                 = Float 
type PointType17Data5                 = Float 
type PointType17Data6                 = Float 
type PointType17Data7                 = Float 
type PointType17Data8                 = Float 
type PointType17Data9                 = Float 
type PointType17Data10                = Float  
type PointType17Data11                = Float  
type PointType17Data12                = Float  
type PointType17Data13                = Float  
type PointType17Data14                = Float  
type PointType17Data15                = Float  
type PointType17Data16                = Float  
type PointType17Data17                = Float  
type PointType17Data18                = Float  
type PointType17Data19                = Float  
type PointType17Data20                = Float  
type PointType17EnableSoftPntLog      = Bool            
  
pointType17Parser :: Get PointType17
pointType17Parser = do 

  pointTag <- getByteString 10
  intFlag <- getWord16le
  data1 <- getIeeeFloat32
  data2 <- getIeeeFloat32
  data3 <- getIeeeFloat32
  data4 <- getIeeeFloat32
  data5 <- getIeeeFloat32
  data6 <- getIeeeFloat32
  data7 <- getIeeeFloat32
  data8 <- getIeeeFloat32
  data9 <- getIeeeFloat32
  data10 <- getIeeeFloat32
  data11 <- getIeeeFloat32
  data12 <- getIeeeFloat32
  data13 <- getIeeeFloat32
  data14 <- getIeeeFloat32
  data15 <- getIeeeFloat32
  data16 <- getIeeeFloat32
  data17 <- getIeeeFloat32
  data18 <- getIeeeFloat32
  data19 <- getIeeeFloat32
  data20 <- getIeeeFloat32
  enableSoftPntLog <- anyButNull
  
  return $ PointType17 pointTag intFlag data1 data2 data3 data4 data5 data6 data7 data8 data9 data10 data11 data12 data13 data14 data15 data16 data17 data18 data19 data20 enableSoftPntLog  
  
  