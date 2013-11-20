{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType86 where

import GHC.Generics
import Data.Word
import Data.Binary
import Protocol.ROC.PointTypes.Utils


data PointType86 = PointType86 {
  
 pointType86MaxNumExtHistPnts             :: !PointType86MaxNumExtHistPnts                
,pointType86SampleLogInterval             :: !PointType86SampleLogInterval                
,pointType86PointTagIDTLP1                :: !PointType86PointTagIDTLP1                
,pointType86ExtHistLogPnt1                :: !PointType86ExtHistLogPnt1                
,pointType86ArchiveType1                  :: !PointType86ArchiveType1            
,pointType86AveragingorRateType1          :: !PointType86AveragingorRateType1                
,pointType86PointTagIDTLP2                :: !PointType86PointTagIDTLP2                
,pointType86ExtHistLogPnt2                :: !PointType86ExtHistLogPnt2                
,pointType86ArchiveType2                  :: !PointType86ArchiveType2            
,pointType86AveragingorRateType2          :: !PointType86AveragingorRateType2                
,pointType86PointTagIDTLP3                :: !PointType86PointTagIDTLP3                
,pointType86ExtHistLogPnt3                :: !PointType86ExtHistLogPnt3                
,pointType86ArchiveType3                  :: !PointType86ArchiveType3            
,pointType86AveragingorRateType3          :: !PointType86AveragingorRateType3                
,pointType86PointTagIDTLP4                :: !PointType86PointTagIDTLP4                
,pointType86ExtHistLogPnt4                :: !PointType86ExtHistLogPnt4                
,pointType86ArchiveType4                  :: !PointType86ArchiveType4            
,pointType86AveragingorRateType4          :: !PointType86AveragingorRateType4                
,pointType86PointTagIDTLP5                :: !PointType86PointTagIDTLP5                
,pointType86ExtHistLogPnt5                :: !PointType86ExtHistLogPnt5                
,pointType86ArchiveType5                  :: !PointType86ArchiveType5            
,pointType86AveragingorRateType5          :: !PointType86AveragingorRateType5                
,pointType86PointTagIDTLP6                :: !PointType86PointTagIDTLP6                
,pointType86ExtHistLogPnt6                :: !PointType86ExtHistLogPnt6                
,pointType86ArchiveType6                  :: !PointType86ArchiveType6            
,pointType86AveragingorRateType6          :: !PointType86AveragingorRateType6                
,pointType86PointTagIDTLP7                :: !PointType86PointTagIDTLP7                
,pointType86ExtHistLogPnt7                :: !PointType86ExtHistLogPnt7                
,pointType86ArchiveType7                  :: !PointType86ArchiveType7            
,pointType86AveragingorRateType7          :: !PointType86AveragingorRateType7                
,pointType86PointTagIDTLP8                :: !PointType86PointTagIDTLP8                
,pointType86ExtHistLogPnt8                :: !PointType86ExtHistLogPnt8                
,pointType86ArchiveType8                  :: !PointType86ArchiveType8            
,pointType86AveragingorRateType8          :: !PointType86AveragingorRateType8                
,pointType86PointTagIDTLP9                :: !PointType86PointTagIDTLP9                
,pointType86ExtHistLogPnt9                :: !PointType86ExtHistLogPnt9                
,pointType86ArchiveType9                  :: !PointType86ArchiveType9            
,pointType86AveragingorRateType9          :: !PointType86AveragingorRateType9                
,pointType86PointTagIDTLP10               :: !PointType86PointTagIDTLP10                
,pointType86ExtHistLogPnt10               :: !PointType86ExtHistLogPnt10                
,pointType86ArchiveType10                 :: !PointType86ArchiveType10                
,pointType86AveragingorRateType10         :: !PointType86AveragingorRateType10                
,pointType86PointTagIDTLP11               :: !PointType86PointTagIDTLP11                
,pointType86ExtHistLogPnt11               :: !PointType86ExtHistLogPnt11                
,pointType86ArchiveType11                 :: !PointType86ArchiveType11                
,pointType86AveragingorRateType11         :: !PointType86AveragingorRateType11                
,pointType86PointTagIDTLP12               :: !PointType86PointTagIDTLP12                
,pointType86ExtHistLogPnt12               :: !PointType86ExtHistLogPnt12                
,pointType86ArchiveType12                 :: !PointType86ArchiveType12                
,pointType86AveragingorRateType12         :: !PointType86AveragingorRateType12                
,pointType86PointTagIDTLP13               :: !PointType86PointTagIDTLP13                
,pointType86ExtHistLogPnt13               :: !PointType86ExtHistLogPnt13                
,pointType86ArchiveType13                 :: !PointType86ArchiveType13                
,pointType86AveragingorRateType13         :: !PointType86AveragingorRateType13                
,pointType86PointTagIDTLP14               :: !PointType86PointTagIDTLP14                
,pointType86ExtHistLogPnt14               :: !PointType86ExtHistLogPnt14                
,pointType86ArchiveType14                 :: !PointType86ArchiveType14                
,pointType86AveragingorRateType14         :: !PointType86AveragingorRateType14                
,pointType86PointTagIDTLP15               :: !PointType86PointTagIDTLP15                
,pointType86ExtHistLogPnt15               :: !PointType86ExtHistLogPnt15                
,pointType86ArchiveType15                 :: !PointType86ArchiveType15                
,pointType86AveragingorRateType15         :: !PointType86AveragingorRateType15                
,pointType86PointTagIDTLP16               :: !PointType86PointTagIDTLP16                
,pointType86ExtHistLogPnt16               :: !PointType86ExtHistLogPnt16                
,pointType86ArchiveType16                 :: !PointType86ArchiveType16                
,pointType86AveragingorRateType16         :: !PointType86AveragingorRateType16                
,pointType86PointTagIDTLP17               :: !PointType86PointTagIDTLP17                
,pointType86ExtHistLogPnt17               :: !PointType86ExtHistLogPnt17                
,pointType86ArchiveType17                 :: !PointType86ArchiveType17                
,pointType86AveragingorRateType17         :: !PointType86AveragingorRateType17                
,pointType86PointTagIDTLP18               :: !PointType86PointTagIDTLP18                
,pointType86ExtHistLogPnt18               :: !PointType86ExtHistLogPnt18                
,pointType86ArchiveType18                 :: !PointType86ArchiveType18                
,pointType86AveragingorRateType18         :: !PointType86AveragingorRateType18                
,pointType86PointTagIDTLP19               :: !PointType86PointTagIDTLP19                
,pointType86ExtHistLogPnt19               :: !PointType86ExtHistLogPnt19                
,pointType86ArchiveType19                 :: !PointType86ArchiveType19                
,pointType86AveragingorRateType19         :: !PointType86AveragingorRateType19                
,pointType86PointTagIDTLP20               :: !PointType86PointTagIDTLP20                
,pointType86ExtHistLogPnt20               :: !PointType86ExtHistLogPnt20                
,pointType86ArchiveType20                 :: !PointType86ArchiveType20                
,pointType86AveragingorRateType20         :: !PointType86AveragingorRateType20                
,pointType86PointTagIDTLP21               :: !PointType86PointTagIDTLP21                
,pointType86ExtHistLogPnt21               :: !PointType86ExtHistLogPnt21                
,pointType86ArchiveType21                 :: !PointType86ArchiveType21                
,pointType86AveragingorRateType21         :: !PointType86AveragingorRateType21                
,pointType86PointTagIDTLP22               :: !PointType86PointTagIDTLP22                
,pointType86ExtHistLogPnt22               :: !PointType86ExtHistLogPnt22                
,pointType86ArchiveType22                 :: !PointType86ArchiveType22                
,pointType86AveragingorRateType22         :: !PointType86AveragingorRateType22                
,pointType86PointTagIDTLP23               :: !PointType86PointTagIDTLP23                
,pointType86ExtHistLogPnt23               :: !PointType86ExtHistLogPnt23                
,pointType86ArchiveType23                 :: !PointType86ArchiveType23                
,pointType86AveragingorRateType23         :: !PointType86AveragingorRateType23                
,pointType86PointTagIDTLP24               :: !PointType86PointTagIDTLP24                
,pointType86ExtHistLogPnt24               :: !PointType86ExtHistLogPnt24                
,pointType86ArchiveType24                 :: !PointType86ArchiveType24                
,pointType86AveragingorRateType24         :: !PointType86AveragingorRateType24                
,pointType86PointTagIDTLP25               :: !PointType86PointTagIDTLP25                
,pointType86ExtHistLogPnt25               :: !PointType86ExtHistLogPnt25                
,pointType86ArchiveType25                 :: !PointType86ArchiveType25                
,pointType86AveragingorRateType25         :: !PointType86AveragingorRateType25                
  
} deriving (Read,Eq, Show, Generic)                       
                                  
type PointType86MaxNumExtHistPnts           = Word8                                 
type PointType86SampleLogInterval           = Word8                                 
type PointType86PointTagIDTLP1              = [Word8]                                 
type PointType86ExtHistLogPnt1              = [Word8]                                 
type PointType86ArchiveType1                = Word8                                 
type PointType86AveragingorRateType1        = Word8                                 
type PointType86PointTagIDTLP2              = [Word8]                                 
type PointType86ExtHistLogPnt2              = [Word8]                                 
type PointType86ArchiveType2                = Word8                                 
type PointType86AveragingorRateType2        = Word8                                 
type PointType86PointTagIDTLP3              = [Word8]                                 
type PointType86ExtHistLogPnt3              = [Word8]                                 
type PointType86ArchiveType3                = Word8                                 
type PointType86AveragingorRateType3        = Word8                                 
type PointType86PointTagIDTLP4              = [Word8]                                 
type PointType86ExtHistLogPnt4              = [Word8]                                 
type PointType86ArchiveType4                = Word8                                 
type PointType86AveragingorRateType4        = Word8                                 
type PointType86PointTagIDTLP5              = [Word8]                                 
type PointType86ExtHistLogPnt5              = [Word8]                                 
type PointType86ArchiveType5                = Word8                                 
type PointType86AveragingorRateType5        = Word8                                 
type PointType86PointTagIDTLP6              = [Word8]                                 
type PointType86ExtHistLogPnt6              = [Word8]                                 
type PointType86ArchiveType6                = Word8                                 
type PointType86AveragingorRateType6        = Word8                                 
type PointType86PointTagIDTLP7              = [Word8]                                 
type PointType86ExtHistLogPnt7              = [Word8]                                 
type PointType86ArchiveType7                = Word8                                 
type PointType86AveragingorRateType7        = Word8                                 
type PointType86PointTagIDTLP8              = [Word8]                                 
type PointType86ExtHistLogPnt8              = [Word8]                                 
type PointType86ArchiveType8                = Word8                                 
type PointType86AveragingorRateType8        = Word8                                 
type PointType86PointTagIDTLP9              = [Word8]                                 
type PointType86ExtHistLogPnt9              = [Word8]                                 
type PointType86ArchiveType9                = Word8                                 
type PointType86AveragingorRateType9        = Word8                                 
type PointType86PointTagIDTLP10             = [Word8]                                 
type PointType86ExtHistLogPnt10             = [Word8]                                 
type PointType86ArchiveType10               = Word8                                 
type PointType86AveragingorRateType10       = Word8                                 
type PointType86PointTagIDTLP11             = [Word8]                                 
type PointType86ExtHistLogPnt11             = [Word8]                                 
type PointType86ArchiveType11               = Word8                                 
type PointType86AveragingorRateType11       = Word8                                 
type PointType86PointTagIDTLP12             = [Word8]                                 
type PointType86ExtHistLogPnt12             = [Word8]                                 
type PointType86ArchiveType12               = Word8                                 
type PointType86AveragingorRateType12       = Word8                                 
type PointType86PointTagIDTLP13             = [Word8]                                 
type PointType86ExtHistLogPnt13             = [Word8]                                 
type PointType86ArchiveType13               = Word8                                 
type PointType86AveragingorRateType13       = Word8                                 
type PointType86PointTagIDTLP14             = [Word8]                                 
type PointType86ExtHistLogPnt14             = [Word8]                                 
type PointType86ArchiveType14               = Word8                                 
type PointType86AveragingorRateType14       = Word8                                 
type PointType86PointTagIDTLP15             = [Word8]                                 
type PointType86ExtHistLogPnt15             = [Word8]                                 
type PointType86ArchiveType15               = Word8                                 
type PointType86AveragingorRateType15       = Word8                                 
type PointType86PointTagIDTLP16             = [Word8]                                 
type PointType86ExtHistLogPnt16             = [Word8]                                 
type PointType86ArchiveType16               = Word8                                 
type PointType86AveragingorRateType16       = Word8                                 
type PointType86PointTagIDTLP17             = [Word8]                                 
type PointType86ExtHistLogPnt17             = [Word8]                                 
type PointType86ArchiveType17               = Word8                                 
type PointType86AveragingorRateType17       = Word8                                 
type PointType86PointTagIDTLP18             = [Word8]                                 
type PointType86ExtHistLogPnt18             = [Word8]                                 
type PointType86ArchiveType18               = Word8                                 
type PointType86AveragingorRateType18       = Word8                                 
type PointType86PointTagIDTLP19             = [Word8]                                 
type PointType86ExtHistLogPnt19             = [Word8]                                 
type PointType86ArchiveType19               = Word8                                 
type PointType86AveragingorRateType19       = Word8                                 
type PointType86PointTagIDTLP20             = [Word8]                                 
type PointType86ExtHistLogPnt20             = [Word8]                                 
type PointType86ArchiveType20               = Word8                                 
type PointType86AveragingorRateType20       = Word8                                 
type PointType86PointTagIDTLP21             = [Word8]                                 
type PointType86ExtHistLogPnt21             = [Word8]                                 
type PointType86ArchiveType21               = Word8                                 
type PointType86AveragingorRateType21       = Word8                                 
type PointType86PointTagIDTLP22             = [Word8]                                 
type PointType86ExtHistLogPnt22             = [Word8]                                 
type PointType86ArchiveType22               = Word8                                 
type PointType86AveragingorRateType22       = Word8                                 
type PointType86PointTagIDTLP23             = [Word8]                                 
type PointType86ExtHistLogPnt23             = [Word8]                                 
type PointType86ArchiveType23               = Word8                                 
type PointType86AveragingorRateType23       = Word8                                 
type PointType86PointTagIDTLP24             = [Word8]                                 
type PointType86ExtHistLogPnt24             = [Word8]                                 
type PointType86ArchiveType24               = Word8                                 
type PointType86AveragingorRateType24       = Word8                                 
type PointType86PointTagIDTLP25             = [Word8]                                 
type PointType86ExtHistLogPnt25             = [Word8]                                 
type PointType86ArchiveType25               = Word8                                 
type PointType86AveragingorRateType25       = Word8                                 
  
pointType86Parser :: Get PointType86
pointType86Parser = do 
                                                                        
  maxNumExtHistPnts <- getWord8
  sampleLogInterval <- getWord8
  pointTagIDTLP1 <- getTLP
  extHistLogPnt1 <- getTLP
  archiveType1 <- getWord8
  averagingorRateType1 <- getWord8
  pointTagIDTLP2 <- getTLP
  extHistLogPnt2 <- getTLP
  archiveType2 <- getWord8
  averagingorRateType2 <- getWord8
  pointTagIDTLP3 <- getTLP
  extHistLogPnt3 <- getTLP
  archiveType3 <- getWord8
  averagingorRateType3 <- getWord8
  pointTagIDTLP4 <- getTLP
  extHistLogPnt4 <- getTLP
  archiveType4 <- getWord8
  averagingorRateType4 <- getWord8
  pointTagIDTLP5 <- getTLP
  extHistLogPnt5 <- getTLP
  archiveType5 <- getWord8
  averagingorRateType5 <- getWord8
  pointTagIDTLP6 <- getTLP
  extHistLogPnt6 <- getTLP
  archiveType6 <- getWord8
  averagingorRateType6 <- getWord8
  pointTagIDTLP7 <- getTLP
  extHistLogPnt7 <- getTLP
  archiveType7 <- getWord8
  averagingorRateType7 <- getWord8
  pointTagIDTLP8 <- getTLP
  extHistLogPnt8 <- getTLP
  archiveType8 <- getWord8
  averagingorRateType8 <- getWord8
  pointTagIDTLP9 <- getTLP
  extHistLogPnt9 <- getTLP
  archiveType9 <- getWord8
  averagingorRateType9 <- getWord8
  pointTagIDTLP10 <- getTLP
  extHistLogPnt10 <- getTLP
  archiveType10 <- getWord8
  averagingorRateType10 <-getWord8 
  pointTagIDTLP11 <- getTLP
  extHistLogPnt11 <- getTLP
  archiveType11 <- getWord8
  averagingorRateType11 <- getWord8
  pointTagIDTLP12 <- getTLP
  extHistLogPnt12 <- getTLP
  archiveType12 <- getWord8
  averagingorRateType12 <- getWord8
  pointTagIDTLP13 <- getTLP
  extHistLogPnt13 <- getTLP
  archiveType13 <- getWord8
  averagingorRateType13 <- getWord8
  pointTagIDTLP14 <- getTLP
  extHistLogPnt14 <- getTLP
  archiveType14 <- getWord8
  averagingorRateType14 <- getWord8
  pointTagIDTLP15 <- getTLP
  extHistLogPnt15 <- getTLP
  archiveType15 <- getWord8
  averagingorRateType15 <- getWord8
  pointTagIDTLP16 <- getTLP
  extHistLogPnt16 <- getTLP
  archiveType16 <- getWord8
  averagingorRateType16 <- getWord8
  pointTagIDTLP17 <- getTLP
  extHistLogPnt17 <- getTLP
  archiveType17 <- getWord8
  averagingorRateType17 <- getWord8
  pointTagIDTLP18 <- getTLP
  extHistLogPnt18 <- getTLP
  archiveType18 <- getWord8
  averagingorRateType18 <- getWord8
  pointTagIDTLP19 <- getTLP
  extHistLogPnt19 <- getTLP
  archiveType19 <- getWord8
  averagingorRateType19 <- getWord8
  pointTagIDTLP20 <- getTLP
  extHistLogPnt20 <- getTLP
  archiveType20 <- getWord8
  averagingorRateType20 <- getWord8
  pointTagIDTLP21 <- getTLP
  extHistLogPnt21 <- getTLP
  archiveType21 <- getWord8
  averagingorRateType21 <- getWord8
  pointTagIDTLP22 <- getTLP
  extHistLogPnt22 <- getTLP
  archiveType22 <- getWord8
  averagingorRateType22 <- getWord8
  pointTagIDTLP23 <- getTLP
  extHistLogPnt23 <- getTLP
  archiveType23 <- getWord8
  averagingorRateType23 <- getWord8
  pointTagIDTLP24 <- getTLP
  extHistLogPnt24 <- getTLP
  archiveType24 <- getWord8
  averagingorRateType24 <- getWord8
  pointTagIDTLP25 <- getTLP
  extHistLogPnt25 <- getTLP
  archiveType25 <- getWord8
  averagingorRateType25 <- getWord8
  
  return $ PointType86 
    maxNumExtHistPnts sampleLogInterval pointTagIDTLP1 extHistLogPnt1 archiveType1 averagingorRateType1 pointTagIDTLP2 extHistLogPnt2 archiveType2 averagingorRateType2 
    pointTagIDTLP3 extHistLogPnt3 archiveType3 averagingorRateType3 pointTagIDTLP4 extHistLogPnt4 archiveType4 averagingorRateType4 pointTagIDTLP5 extHistLogPnt5 archiveType5 
    averagingorRateType5 pointTagIDTLP6 extHistLogPnt6 archiveType6 averagingorRateType6 pointTagIDTLP7 extHistLogPnt7 archiveType7 averagingorRateType7 pointTagIDTLP8 
    extHistLogPnt8 archiveType8 averagingorRateType8 pointTagIDTLP9 extHistLogPnt9 archiveType9 averagingorRateType9 pointTagIDTLP10 extHistLogPnt10 archiveType10 
    averagingorRateType10 pointTagIDTLP11 extHistLogPnt11 archiveType11 averagingorRateType11 pointTagIDTLP12 extHistLogPnt12 archiveType12 averagingorRateType12 pointTagIDTLP13 
    extHistLogPnt13 archiveType13 averagingorRateType13 pointTagIDTLP14 extHistLogPnt14 archiveType14 averagingorRateType14 pointTagIDTLP15 extHistLogPnt15 archiveType15 
    averagingorRateType15 pointTagIDTLP16 extHistLogPnt16 archiveType16 averagingorRateType16 pointTagIDTLP17 extHistLogPnt17 archiveType17 averagingorRateType17 pointTagIDTLP18 
    extHistLogPnt18 archiveType18 averagingorRateType18 pointTagIDTLP19 extHistLogPnt19 archiveType19 averagingorRateType19 pointTagIDTLP20 extHistLogPnt20 archiveType20 
    averagingorRateType20 pointTagIDTLP21 extHistLogPnt21 archiveType21 averagingorRateType21 pointTagIDTLP22 extHistLogPnt22 archiveType22 averagingorRateType22 pointTagIDTLP23 
    extHistLogPnt23 archiveType23 averagingorRateType23 pointTagIDTLP24 extHistLogPnt24 archiveType24 averagingorRateType24 pointTagIDTLP25 extHistLogPnt25 archiveType25 averagingorRateType25 