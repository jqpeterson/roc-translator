{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType98 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Utils
import Protocol.ROC.Float

data PointType98 = PointType98 {
  
 pointType98IdTag                        :: !PointType98IdTag                           
,pointType98MiscStorage1                 :: !PointType98MiscStorage1                           
,pointType98MiscStorage2                 :: !PointType98MiscStorage2                           
,pointType98MiscStorage3                 :: !PointType98MiscStorage3                           
,pointType98MiscStorage4                 :: !PointType98MiscStorage4                           
,pointType98MiscStorage5                 :: !PointType98MiscStorage5                           
,pointType98MiscStorage6                 :: !PointType98MiscStorage6                           
,pointType98MiscStorage7                 :: !PointType98MiscStorage7                           
,pointType98MiscStorage8                 :: !PointType98MiscStorage8                           
,pointType98MiscStorage9                 :: !PointType98MiscStorage9                           
,pointType98MiscStorage10                :: !PointType98MiscStorage10                          
,pointType98MiscStorage11                :: !PointType98MiscStorage11                           
,pointType98MiscStorage12                :: !PointType98MiscStorage12                           
,pointType98MiscStorage13                :: !PointType98MiscStorage13                           
,pointType98MiscStorage14                :: !PointType98MiscStorage14                           
,pointType98MiscStorage15                :: !PointType98MiscStorage15                           
,pointType98MiscStorage16                :: !PointType98MiscStorage16                           
,pointType98MiscStorage17                :: !PointType98MiscStorage17                           
,pointType98MiscStorage18                :: !PointType98MiscStorage18                           
,pointType98MiscStorage19                :: !PointType98MiscStorage19                           
,pointType98MiscStorage20                :: !PointType98MiscStorage20                           
,pointType98MiscStorage21                :: !PointType98MiscStorage21                           
,pointType98MiscStorage22                :: !PointType98MiscStorage22                           
,pointType98MiscStorage23                :: !PointType98MiscStorage23                           
,pointType98MiscStorage24                :: !PointType98MiscStorage24                           
,pointType98MiscStorage25                :: !PointType98MiscStorage25                           
,pointType98MiscStorage26                :: !PointType98MiscStorage26                           
,pointType98MiscStorage27                :: !PointType98MiscStorage27                           
,pointType98MiscStorage28                :: !PointType98MiscStorage28                           
,pointType98MiscStorage29                :: !PointType98MiscStorage29                           
,pointType98MiscStorage30                :: !PointType98MiscStorage30                           
,pointType98MiscStorage31                :: !PointType98MiscStorage31                           
,pointType98MiscStorage32                :: !PointType98MiscStorage32                           
,pointType98MiscStorage33                :: !PointType98MiscStorage33                           
,pointType98MiscStorage34                :: !PointType98MiscStorage34                           
,pointType98MiscStorage35                :: !PointType98MiscStorage35                           
,pointType98MiscStorage36                :: !PointType98MiscStorage36                           
,pointType98MiscStorage37                :: !PointType98MiscStorage37                           
,pointType98MiscStorage38                :: !PointType98MiscStorage38                           
,pointType98MiscStorage39                :: !PointType98MiscStorage39                           
,pointType98MiscStorage40                :: !PointType98MiscStorage40                           
,pointType98MiscStorage41                :: !PointType98MiscStorage41                           
,pointType98MiscStorage42                :: !PointType98MiscStorage42                           
,pointType98EnableExtSoftPointLog        :: !PointType98EnableExtSoftPointLog                           
                             
  
} deriving (Read,Eq, Show, Generic)                       
                                  
type PointType98IdTag                    = BS.ByteString                                  
type PointType98MiscStorage1             = Float                                 
type PointType98MiscStorage2             = Float                                 
type PointType98MiscStorage3             = Float                                 
type PointType98MiscStorage4             = Float                                 
type PointType98MiscStorage5             = Float                                 
type PointType98MiscStorage6             = Float                                 
type PointType98MiscStorage7             = Float                                 
type PointType98MiscStorage8             = Float                                 
type PointType98MiscStorage9             = Float                                 
type PointType98MiscStorage10            = Float                                 
type PointType98MiscStorage11            = Float                                 
type PointType98MiscStorage12            = Float                                 
type PointType98MiscStorage13            = Float                                 
type PointType98MiscStorage14            = Float                                 
type PointType98MiscStorage15            = Float                                 
type PointType98MiscStorage16            = Float                                 
type PointType98MiscStorage17            = Float                                 
type PointType98MiscStorage18            = Float                                 
type PointType98MiscStorage19            = Float                                 
type PointType98MiscStorage20            = Float                                 
type PointType98MiscStorage21            = Word32                                 
type PointType98MiscStorage22            = Word32                                 
type PointType98MiscStorage23            = Word16                                 
type PointType98MiscStorage24            = Word16                                 
type PointType98MiscStorage25            = Word16                                 
type PointType98MiscStorage26            = Word16                                 
type PointType98MiscStorage27            = Word16                                 
type PointType98MiscStorage28            = Word16                                 
type PointType98MiscStorage29            = Word16                                 
type PointType98MiscStorage30            = Word16                                 
type PointType98MiscStorage31            = Word16                                 
type PointType98MiscStorage32            = Word16                                 
type PointType98MiscStorage33            = Word8                                 
type PointType98MiscStorage34            = Word8                                 
type PointType98MiscStorage35            = Word8                                 
type PointType98MiscStorage36            = Word8                                 
type PointType98MiscStorage37            = Word8                                 
type PointType98MiscStorage38            = Word8                                 
type PointType98MiscStorage39            = Word8                                 
type PointType98MiscStorage40            = Word8                                 
type PointType98MiscStorage41            = Word8                                 
type PointType98MiscStorage42            = Word8                                 
type PointType98EnableExtSoftPointLog    = Bool                                 

  
pointType98Parser :: Get PointType98
pointType98Parser = do 
 
  idTag <- getByteString 40                    
  miscStorage1 <- getIeeeFloat32             
  miscStorage2 <- getIeeeFloat32             
  miscStorage3 <- getIeeeFloat32             
  miscStorage4 <- getIeeeFloat32             
  miscStorage5 <- getIeeeFloat32             
  miscStorage6 <- getIeeeFloat32             
  miscStorage7 <- getIeeeFloat32             
  miscStorage8 <- getIeeeFloat32              
  miscStorage9 <- getIeeeFloat32              
  miscStorage10 <- getIeeeFloat32             
  miscStorage11 <- getIeeeFloat32             
  miscStorage12 <- getIeeeFloat32             
  miscStorage13 <- getIeeeFloat32             
  miscStorage14 <- getIeeeFloat32             
  miscStorage15 <- getIeeeFloat32             
  miscStorage16 <- getIeeeFloat32             
  miscStorage17 <- getIeeeFloat32             
  miscStorage18 <- getIeeeFloat32             
  miscStorage19 <- getIeeeFloat32             
  miscStorage20 <- getIeeeFloat32             
  miscStorage21 <- getWord32le             
  miscStorage22 <- getWord32le             
  miscStorage23 <- getWord16le             
  miscStorage24 <- getWord16le             
  miscStorage25 <- getWord16le             
  miscStorage26 <- getWord16le             
  miscStorage27 <- getWord16le             
  miscStorage28 <- getWord16le             
  miscStorage29 <- getWord16le             
  miscStorage30 <- getWord16le             
  miscStorage31 <- getWord16le             
  miscStorage32 <- getWord16le             
  miscStorage33 <- getWord8             
  miscStorage34 <- getWord8             
  miscStorage35 <- getWord8             
  miscStorage36 <- getWord8             
  miscStorage37 <- getWord8             
  miscStorage38 <- getWord8             
  miscStorage39 <- getWord8             
  miscStorage40 <- getWord8             
  miscStorage41 <- getWord8             
  miscStorage42 <- getWord8             
  enableExtSoftPointLog <- anyButNull     

  return $ PointType98 idTag miscStorage1 miscStorage2 miscStorage3 miscStorage4 miscStorage5 miscStorage6 miscStorage7 miscStorage8 miscStorage9 miscStorage10 miscStorage11 
    miscStorage12 miscStorage13 miscStorage14 miscStorage15 miscStorage16 miscStorage17 miscStorage18 miscStorage19 miscStorage20 miscStorage21 miscStorage22 miscStorage23  
    miscStorage24 miscStorage25 miscStorage26 miscStorage27 miscStorage28 miscStorage29 miscStorage30 miscStorage31 miscStorage32 miscStorage33 miscStorage34 miscStorage35 
    miscStorage36 miscStorage37 miscStorage38 miscStorage39 miscStorage40 miscStorage41 miscStorage42 enableExtSoftPointLog  
  
  
  