{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType118 where

import GHC.Generics
import Data.Word
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as BS
import Protocol.ROC.Utils


data PointType118 = PointType118 {
  
 pointType118TagID                           :: !PointType118TagID                                    
,pointType118StartRegister1                  :: !PointType118StartRegister1                                    
,pointType118EndRegister1                    :: !PointType118EndRegister1                                    
,pointType118ROCParametersRegRange1          :: !PointType118ROCParametersRegRange1                                    
,pointType118IndexingRegRange1               :: !PointType118IndexingRegRange1                                    
,pointType118ConversionCodeRegRange1         :: !PointType118ConversionCodeRegRange1                                    
,pointType118CommPortRegRange1               :: !PointType118CommPortRegRange1                                    
,pointType118StartRegister2                  :: !PointType118StartRegister2                                    
,pointType118EndRegister2                    :: !PointType118EndRegister2                                    
,pointType118ROCParametersRegRange2          :: !PointType118ROCParametersRegRange2                                    
,pointType118IndexingRegRange2               :: !PointType118IndexingRegRange2                                    
,pointType118ConversionCodeRegRange2         :: !PointType118ConversionCodeRegRange2                                    
,pointType118CommPortRegRange2               :: !PointType118CommPortRegRange2                                    
,pointType118StartRegister3                  :: !PointType118StartRegister3                                    
,pointType118EndRegister3                    :: !PointType118EndRegister3                                    
,pointType118ROCParametersRegRange3          :: !PointType118ROCParametersRegRange3                                    
,pointType118IndexingRegRange3               :: !PointType118IndexingRegRange3                                    
,pointType118ConversionCodeRegRange3         :: !PointType118ConversionCodeRegRange3                                    
,pointType118CommPortRegRange3               :: !PointType118CommPortRegRange3                                    
,pointType118StartRegister4                  :: !PointType118StartRegister4                                    
,pointType118EndRegister4                    :: !PointType118EndRegister4                                    
,pointType118ROCParametersRegRange4          :: !PointType118ROCParametersRegRange4                                    
,pointType118IndexingRegRange4               :: !PointType118IndexingRegRange4                                    
,pointType118ConversionCodeRegRange4         :: !PointType118ConversionCodeRegRange4                                    
,pointType118CommPortRegRange4               :: !PointType118CommPortRegRange4                                    
,pointType118StartRegister5                  :: !PointType118StartRegister5                                    
,pointType118EndRegister5                    :: !PointType118EndRegister5                                    
,pointType118ROCParametersRegRange5          :: !PointType118ROCParametersRegRange5                                    
,pointType118IndexingRegRange5               :: !PointType118IndexingRegRange5                                    
,pointType118ConversionCodeRegRange5         :: !PointType118ConversionCodeRegRange5                                    
,pointType118CommPortRegRange5               :: !PointType118CommPortRegRange5                                    
,pointType118StartRegister6                  :: !PointType118StartRegister6                                    
,pointType118EndRegister6                    :: !PointType118EndRegister6                                    
,pointType118ROCParametersRegRange6          :: !PointType118ROCParametersRegRange6                                    
,pointType118IndexingRegRange6               :: !PointType118IndexingRegRange6                                    
,pointType118ConversionCodeRegRange6         :: !PointType118ConversionCodeRegRange6                                    
,pointType118CommPortRegRange6               :: !PointType118CommPortRegRange6                                    
,pointType118StartRegister7                  :: !PointType118StartRegister7                                    
,pointType118EndRegister7                    :: !PointType118EndRegister7                                    
,pointType118ROCParametersRegRange7          :: !PointType118ROCParametersRegRange7                                    
,pointType118IndexingRegRange7               :: !PointType118IndexingRegRange7                                    
,pointType118ConversionCodeRegRange7         :: !PointType118ConversionCodeRegRange7                                    
,pointType118CommPortRegRange7               :: !PointType118CommPortRegRange7                                    
,pointType118StartRegister8                  :: !PointType118StartRegister8                                    
,pointType118EndRegister8                    :: !PointType118EndRegister8                                    
,pointType118ROCParametersRegRange8          :: !PointType118ROCParametersRegRange8                                    
,pointType118IndexingRegRange8               :: !PointType118IndexingRegRange8                                    
,pointType118ConversionCodeRegRange8         :: !PointType118ConversionCodeRegRange8                                    
,pointType118CommPortRegRange8               :: !PointType118CommPortRegRange8                                    
,pointType118StartRegister9                  :: !PointType118StartRegister9                                    
,pointType118EndRegister9                    :: !PointType118EndRegister9                                    
,pointType118ROCParametersRegRange9          :: !PointType118ROCParametersRegRange9                                    
,pointType118IndexingRegRange9               :: !PointType118IndexingRegRange9                                    
,pointType118ConversionCodeRegRange9         :: !PointType118ConversionCodeRegRange9                                    
,pointType118CommPortRegRange9               :: !PointType118CommPortRegRange9                                    
,pointType118StartRegister10                 :: !PointType118StartRegister10                                    
,pointType118EndRegister10                   :: !PointType118EndRegister10                                    
,pointType118ROCParametersRegRange10         :: !PointType118ROCParametersRegRange10                                    
,pointType118IndexingRegRange10              :: !PointType118IndexingRegRange10                                    
,pointType118ConversionCodeRegRange10        :: !PointType118ConversionCodeRegRange10                                    
,pointType118CommPortRegRange10              :: !PointType118CommPortRegRange10                                    
,pointType118StartRegister11                 :: !PointType118StartRegister11                                    
,pointType118EndRegister11                   :: !PointType118EndRegister11                                    
,pointType118ROCParametersRegRange11         :: !PointType118ROCParametersRegRange11                                    
,pointType118IndexingRegRange11              :: !PointType118IndexingRegRange11                                    
,pointType118ConversionCodeRegRange11        :: !PointType118ConversionCodeRegRange11                                    
,pointType118CommPortRegRange11              :: !PointType118CommPortRegRange11                                    
,pointType118StartRegister12                 :: !PointType118StartRegister12                                    
,pointType118EndRegister12                   :: !PointType118EndRegister12                                    
,pointType118ROCParametersRegRange12         :: !PointType118ROCParametersRegRange12                                    
,pointType118IndexingRegRange12              :: !PointType118IndexingRegRange12                                    
,pointType118ConversionCodeRegRange12        :: !PointType118ConversionCodeRegRange12                                    
,pointType118CommPortRegRange12              :: !PointType118CommPortRegRange12                                    
,pointType118StartRegister13                 :: !PointType118StartRegister13                                    
,pointType118EndRegister13                   :: !PointType118EndRegister13                                    
,pointType118ROCParametersRegRange13         :: !PointType118ROCParametersRegRange13                                    
,pointType118IndexingRegRange13              :: !PointType118IndexingRegRange13                                    
,pointType118ConversionCodeRegRange13        :: !PointType118ConversionCodeRegRange13                                    
,pointType118CommPortRegRange13              :: !PointType118CommPortRegRange13                                    
,pointType118StartRegister14                 :: !PointType118StartRegister14                                    
,pointType118EndRegister14                   :: !PointType118EndRegister14                                    
,pointType118ROCParametersRegRange14         :: !PointType118ROCParametersRegRange14                                    
,pointType118IndexingRegRange14              :: !PointType118IndexingRegRange14                                    
,pointType118ConversionCodeRegRange14        :: !PointType118ConversionCodeRegRange14                                    
,pointType118CommPortRegRange14              :: !PointType118CommPortRegRange14                                    
,pointType118StartRegister15                 :: !PointType118StartRegister15                                    
,pointType118EndRegister15                   :: !PointType118EndRegister15                                    
,pointType118ROCParametersRegRange15         :: !PointType118ROCParametersRegRange15                                    
,pointType118IndexingRegRange15              :: !PointType118IndexingRegRange15                                    
,pointType118ConversionCodeRegRange15        :: !PointType118ConversionCodeRegRange15                                    
,pointType118CommPortRegRange15              :: !PointType118CommPortRegRange15                                    
  
} deriving (Read,Eq, Show, Generic)                       
                                  
type PointType118TagID                       = BS.ByteString                              
type PointType118StartRegister1              = Word16                              
type PointType118EndRegister1                = Word16                              
type PointType118ROCParametersRegRange1      = [Word8]                              
type PointType118IndexingRegRange1           = Bool                              
type PointType118ConversionCodeRegRange1     = Word8                              
type PointType118CommPortRegRange1           = Word8                              
type PointType118StartRegister2              = Word16                               
type PointType118EndRegister2                = Word16                               
type PointType118ROCParametersRegRange2      = [Word8]                              
type PointType118IndexingRegRange2           = Bool                               
type PointType118ConversionCodeRegRange2     = Word8                               
type PointType118CommPortRegRange2           = Word8                               
type PointType118StartRegister3              = Word16                               
type PointType118EndRegister3                = Word16                               
type PointType118ROCParametersRegRange3      = [Word8]                              
type PointType118IndexingRegRange3           = Bool                               
type PointType118ConversionCodeRegRange3     = Word8                               
type PointType118CommPortRegRange3           = Word8                               
type PointType118StartRegister4              = Word16                               
type PointType118EndRegister4                = Word16                               
type PointType118ROCParametersRegRange4      = [Word8]                              
type PointType118IndexingRegRange4           = Bool                               
type PointType118ConversionCodeRegRange4     = Word8                               
type PointType118CommPortRegRange4           = Word8                               
type PointType118StartRegister5              = Word16                               
type PointType118EndRegister5                = Word16                               
type PointType118ROCParametersRegRange5      = [Word8]                              
type PointType118IndexingRegRange5           = Bool                               
type PointType118ConversionCodeRegRange5     = Word8                               
type PointType118CommPortRegRange5           = Word8                               
type PointType118StartRegister6              = Word16                               
type PointType118EndRegister6                = Word16                               
type PointType118ROCParametersRegRange6      = [Word8]                              
type PointType118IndexingRegRange6           = Bool                               
type PointType118ConversionCodeRegRange6     = Word8                               
type PointType118CommPortRegRange6           = Word8                               
type PointType118StartRegister7              = Word16                               
type PointType118EndRegister7                = Word16                               
type PointType118ROCParametersRegRange7      = [Word8]                              
type PointType118IndexingRegRange7           = Bool                               
type PointType118ConversionCodeRegRange7     = Word8                               
type PointType118CommPortRegRange7           = Word8                               
type PointType118StartRegister8              = Word16                               
type PointType118EndRegister8                = Word16                               
type PointType118ROCParametersRegRange8      = [Word8]                              
type PointType118IndexingRegRange8           = Bool                               
type PointType118ConversionCodeRegRange8     = Word8                               
type PointType118CommPortRegRange8           = Word8                               
type PointType118StartRegister9              = Word16                               
type PointType118EndRegister9                = Word16                               
type PointType118ROCParametersRegRange9      = [Word8]                              
type PointType118IndexingRegRange9           = Bool                               
type PointType118ConversionCodeRegRange9     = Word8                               
type PointType118CommPortRegRange9           = Word8                               
type PointType118StartRegister10             = Word16                               
type PointType118EndRegister10               = Word16                               
type PointType118ROCParametersRegRange10     = [Word8]                              
type PointType118IndexingRegRange10          = Bool                               
type PointType118ConversionCodeRegRange10    = Word8                               
type PointType118CommPortRegRange10          = Word8                               
type PointType118StartRegister11             = Word16                               
type PointType118EndRegister11               = Word16                               
type PointType118ROCParametersRegRange11     = [Word8]                              
type PointType118IndexingRegRange11          = Bool                               
type PointType118ConversionCodeRegRange11    = Word8                               
type PointType118CommPortRegRange11          = Word8                               
type PointType118StartRegister12             = Word16                               
type PointType118EndRegister12               = Word16                               
type PointType118ROCParametersRegRange12     = [Word8]                              
type PointType118IndexingRegRange12          = Bool                               
type PointType118ConversionCodeRegRange12    = Word8                               
type PointType118CommPortRegRange12          = Word8                               
type PointType118StartRegister13             = Word16                               
type PointType118EndRegister13               = Word16                               
type PointType118ROCParametersRegRange13     = [Word8]                              
type PointType118IndexingRegRange13          = Bool                               
type PointType118ConversionCodeRegRange13    = Word8                               
type PointType118CommPortRegRange13          = Word8                               
type PointType118StartRegister14             = Word16                               
type PointType118EndRegister14               = Word16                               
type PointType118ROCParametersRegRange14     = [Word8]                              
type PointType118IndexingRegRange14          = Bool                               
type PointType118ConversionCodeRegRange14    = Word8                               
type PointType118CommPortRegRange14          = Word8                               
type PointType118StartRegister15             = Word16                               
type PointType118EndRegister15               = Word16                               
type PointType118ROCParametersRegRange15     = [Word8]                              
type PointType118IndexingRegRange15          = Bool                               
type PointType118ConversionCodeRegRange15    = Word8                               
type PointType118CommPortRegRange15          = Word8                               

  
pointType118Parser :: Get PointType118
pointType118Parser = do 
                                                                        
  tagID <- getByteString 10
  startRegister1 <- getWord16le
  endRegister1 <- getWord16le
  rOCParametersRegRange1 <- getTLP
  indexingRegRange1 <- anyButNull
  conversionCodeRegRange1 <- getWord8
  commPortRegRange1 <- getWord8
  startRegister2 <- getWord16le
  endRegister2 <- getWord16le
  rOCParametersRegRange2 <- getTLP
  indexingRegRange2 <- anyButNull
  conversionCodeRegRange2 <- getWord8
  commPortRegRange2 <- getWord8
  startRegister3 <- getWord16le
  endRegister3 <- getWord16le
  rOCParametersRegRange3 <- getTLP
  indexingRegRange3 <- anyButNull
  conversionCodeRegRange3 <- getWord8
  commPortRegRange3 <- getWord8
  startRegister4 <- getWord16le
  endRegister4 <- getWord16le
  rOCParametersRegRange4 <- getTLP
  indexingRegRange4 <- anyButNull
  conversionCodeRegRange4 <- getWord8
  commPortRegRange4 <- getWord8
  startRegister5 <- getWord16le
  endRegister5 <- getWord16le
  rOCParametersRegRange5 <- getTLP
  indexingRegRange5 <- anyButNull
  conversionCodeRegRange5 <- getWord8
  commPortRegRange5 <- getWord8
  startRegister6 <- getWord16le
  endRegister6 <- getWord16le
  rOCParametersRegRange6 <- getTLP
  indexingRegRange6 <- anyButNull
  conversionCodeRegRange6 <- getWord8
  commPortRegRange6 <- getWord8
  startRegister7 <- getWord16le
  endRegister7 <- getWord16le
  rOCParametersRegRange7 <- getTLP
  indexingRegRange7 <- anyButNull
  conversionCodeRegRange7 <- getWord8
  commPortRegRange7 <- getWord8
  startRegister8 <- getWord16le
  endRegister8 <- getWord16le
  rOCParametersRegRange8 <- getTLP
  indexingRegRange8 <- anyButNull
  conversionCodeRegRange8 <- getWord8
  commPortRegRange8 <- getWord8
  startRegister9 <- getWord16le
  endRegister9 <- getWord16le
  rOCParametersRegRange9 <- getTLP
  indexingRegRange9 <- anyButNull
  conversionCodeRegRange9 <- getWord8
  commPortRegRange9 <- getWord8
  startRegister10 <- getWord16le
  endRegister10 <- getWord16le
  rOCParametersRegRange10 <- getTLP
  indexingRegRange10 <- anyButNull
  conversionCodeRegRange10 <- getWord8
  commPortRegRange10 <- getWord8
  startRegister11 <- getWord16le
  endRegister11 <- getWord16le
  rOCParametersRegRange11 <- getTLP
  indexingRegRange11 <- anyButNull
  conversionCodeRegRange11 <- getWord8
  commPortRegRange11 <- getWord8
  startRegister12 <- getWord16le
  endRegister12 <- getWord16le
  rOCParametersRegRange12 <- getTLP
  indexingRegRange12 <- anyButNull
  conversionCodeRegRange12 <- getWord8
  commPortRegRange12 <- getWord8
  startRegister13 <- getWord16le
  endRegister13 <- getWord16le
  rOCParametersRegRange13 <- getTLP
  indexingRegRange13 <- anyButNull
  conversionCodeRegRange13 <- getWord8
  commPortRegRange13 <- getWord8
  startRegister14 <- getWord16le
  endRegister14 <- getWord16le
  rOCParametersRegRange14 <- getTLP
  indexingRegRange14 <- anyButNull
  conversionCodeRegRange14 <- getWord8
  commPortRegRange14 <- getWord8
  startRegister15 <- getWord16le
  endRegister15 <- getWord16le
  rOCParametersRegRange15 <- getTLP
  indexingRegRange15 <- anyButNull
  conversionCodeRegRange15 <- getWord8
  commPortRegRange15 <- getWord8

  
  return $ PointType118 tagID startRegister1 endRegister1 rOCParametersRegRange1 indexingRegRange1 conversionCodeRegRange1 commPortRegRange1 startRegister2 endRegister2 
    rOCParametersRegRange2 indexingRegRange2 conversionCodeRegRange2 commPortRegRange2 startRegister3 endRegister3 rOCParametersRegRange3 indexingRegRange3 conversionCodeRegRange3 
    commPortRegRange3 startRegister4 endRegister4 rOCParametersRegRange4 indexingRegRange4 conversionCodeRegRange4 commPortRegRange4 startRegister5 endRegister5 
    rOCParametersRegRange5 indexingRegRange5 conversionCodeRegRange5 commPortRegRange5 startRegister6 endRegister6 rOCParametersRegRange6 indexingRegRange6 conversionCodeRegRange6 
    commPortRegRange6 startRegister7 endRegister7 rOCParametersRegRange7 indexingRegRange7 conversionCodeRegRange7 commPortRegRange7 startRegister8 endRegister8 
    rOCParametersRegRange8 indexingRegRange8 conversionCodeRegRange8 commPortRegRange8 startRegister9 endRegister9 rOCParametersRegRange9 indexingRegRange9 conversionCodeRegRange9 
    commPortRegRange9 startRegister10 endRegister10 rOCParametersRegRange10 indexingRegRange10 conversionCodeRegRange10 commPortRegRange10 startRegister11 endRegister11 
    rOCParametersRegRange11 indexingRegRange11 conversionCodeRegRange11 commPortRegRange11 startRegister12 endRegister12 rOCParametersRegRange12 indexingRegRange12 
    conversionCodeRegRange12 commPortRegRange12 startRegister13 endRegister13 rOCParametersRegRange13 indexingRegRange13 conversionCodeRegRange13 commPortRegRange13 startRegister14 
    endRegister14 rOCParametersRegRange14 indexingRegRange14 conversionCodeRegRange14 commPortRegRange14 startRegister15 endRegister15 rOCParametersRegRange15 indexingRegRange15 
    conversionCodeRegRange15 commPortRegRange15