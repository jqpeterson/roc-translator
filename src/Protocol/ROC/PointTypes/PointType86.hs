{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType86 where

import GHC.Generics
import Data.Word
import Data.Binary
import Protocol.ROC.Utils


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
,pointType86PointTagIDTLP26               :: !PointType86PointTagIDTLP26                
,pointType86ExtHistLogPnt26               :: !PointType86ExtHistLogPnt26                
,pointType86ArchiveType26                 :: !PointType86ArchiveType26            
,pointType86AveragingorRateType26         :: !PointType86AveragingorRateType26                
,pointType86PointTagIDTLP27               :: !PointType86PointTagIDTLP27                
,pointType86ExtHistLogPnt27               :: !PointType86ExtHistLogPnt27                
,pointType86ArchiveType27                 :: !PointType86ArchiveType27            
,pointType86AveragingorRateType27         :: !PointType86AveragingorRateType27                
,pointType86PointTagIDTLP28               :: !PointType86PointTagIDTLP28                
,pointType86ExtHistLogPnt28               :: !PointType86ExtHistLogPnt28                
,pointType86ArchiveType28                 :: !PointType86ArchiveType28            
,pointType86AveragingorRateType28         :: !PointType86AveragingorRateType28                
,pointType86PointTagIDTLP29               :: !PointType86PointTagIDTLP29                
,pointType86ExtHistLogPnt29               :: !PointType86ExtHistLogPnt29                
,pointType86ArchiveType29                 :: !PointType86ArchiveType29            
,pointType86AveragingorRateType29         :: !PointType86AveragingorRateType29                
,pointType86PointTagIDTLP30               :: !PointType86PointTagIDTLP30               
,pointType86ExtHistLogPnt30               :: !PointType86ExtHistLogPnt30               
,pointType86ArchiveType30                 :: !PointType86ArchiveType30           
,pointType86AveragingorRateType30         :: !PointType86AveragingorRateType30               
,pointType86PointTagIDTLP31               :: !PointType86PointTagIDTLP31                
,pointType86ExtHistLogPnt31               :: !PointType86ExtHistLogPnt31                
,pointType86ArchiveType31                 :: !PointType86ArchiveType31            
,pointType86AveragingorRateType31         :: !PointType86AveragingorRateType31                
,pointType86PointTagIDTLP32               :: !PointType86PointTagIDTLP32                
,pointType86ExtHistLogPnt32               :: !PointType86ExtHistLogPnt32                
,pointType86ArchiveType32                 :: !PointType86ArchiveType32            
,pointType86AveragingorRateType32         :: !PointType86AveragingorRateType32                
,pointType86PointTagIDTLP33               :: !PointType86PointTagIDTLP33                
,pointType86ExtHistLogPnt33               :: !PointType86ExtHistLogPnt33                
,pointType86ArchiveType33                 :: !PointType86ArchiveType33            
,pointType86AveragingorRateType33         :: !PointType86AveragingorRateType33                
,pointType86PointTagIDTLP34               :: !PointType86PointTagIDTLP34                
,pointType86ExtHistLogPnt34               :: !PointType86ExtHistLogPnt34                
,pointType86ArchiveType34                 :: !PointType86ArchiveType34            
,pointType86AveragingorRateType34         :: !PointType86AveragingorRateType34                
,pointType86PointTagIDTLP35               :: !PointType86PointTagIDTLP35                
,pointType86ExtHistLogPnt35               :: !PointType86ExtHistLogPnt35                
,pointType86ArchiveType35                 :: !PointType86ArchiveType35                
,pointType86AveragingorRateType35         :: !PointType86AveragingorRateType35                
,pointType86PointTagIDTLP36               :: !PointType86PointTagIDTLP36                
,pointType86ExtHistLogPnt36               :: !PointType86ExtHistLogPnt36                
,pointType86ArchiveType36                 :: !PointType86ArchiveType36                
,pointType86AveragingorRateType36         :: !PointType86AveragingorRateType36                
,pointType86PointTagIDTLP37               :: !PointType86PointTagIDTLP37                
,pointType86ExtHistLogPnt37               :: !PointType86ExtHistLogPnt37                
,pointType86ArchiveType37                 :: !PointType86ArchiveType37                
,pointType86AveragingorRateType37         :: !PointType86AveragingorRateType37                
,pointType86PointTagIDTLP38               :: !PointType86PointTagIDTLP38                
,pointType86ExtHistLogPnt38               :: !PointType86ExtHistLogPnt38                
,pointType86ArchiveType38                 :: !PointType86ArchiveType38                
,pointType86AveragingorRateType38         :: !PointType86AveragingorRateType38                
,pointType86PointTagIDTLP39               :: !PointType86PointTagIDTLP39                
,pointType86ExtHistLogPnt39               :: !PointType86ExtHistLogPnt39                
,pointType86ArchiveType39                 :: !PointType86ArchiveType39                
,pointType86AveragingorRateType39         :: !PointType86AveragingorRateType39                
,pointType86PointTagIDTLP40               :: !PointType86PointTagIDTLP40                
,pointType86ExtHistLogPnt40               :: !PointType86ExtHistLogPnt40                
,pointType86ArchiveType40                 :: !PointType86ArchiveType40                
,pointType86AveragingorRateType40         :: !PointType86AveragingorRateType40                
,pointType86PointTagIDTLP41               :: !PointType86PointTagIDTLP41                
,pointType86ExtHistLogPnt41               :: !PointType86ExtHistLogPnt41                
,pointType86ArchiveType41                 :: !PointType86ArchiveType41                
,pointType86AveragingorRateType41         :: !PointType86AveragingorRateType41                
,pointType86PointTagIDTLP42               :: !PointType86PointTagIDTLP42                
,pointType86ExtHistLogPnt42               :: !PointType86ExtHistLogPnt42                
,pointType86ArchiveType42                 :: !PointType86ArchiveType42                
,pointType86AveragingorRateType42         :: !PointType86AveragingorRateType42                
,pointType86PointTagIDTLP43               :: !PointType86PointTagIDTLP43                
,pointType86ExtHistLogPnt43               :: !PointType86ExtHistLogPnt43                
,pointType86ArchiveType43                 :: !PointType86ArchiveType43                
,pointType86AveragingorRateType43         :: !PointType86AveragingorRateType43                
,pointType86PointTagIDTLP44               :: !PointType86PointTagIDTLP44                
,pointType86ExtHistLogPnt44               :: !PointType86ExtHistLogPnt44                
,pointType86ArchiveType44                 :: !PointType86ArchiveType44                
,pointType86AveragingorRateType44         :: !PointType86AveragingorRateType44                
,pointType86PointTagIDTLP45               :: !PointType86PointTagIDTLP45                
,pointType86ExtHistLogPnt45               :: !PointType86ExtHistLogPnt45                
,pointType86ArchiveType45                 :: !PointType86ArchiveType45                
,pointType86AveragingorRateType45         :: !PointType86AveragingorRateType45                
,pointType86PointTagIDTLP46               :: !PointType86PointTagIDTLP46                
,pointType86ExtHistLogPnt46               :: !PointType86ExtHistLogPnt46                
,pointType86ArchiveType46                 :: !PointType86ArchiveType46                
,pointType86AveragingorRateType46         :: !PointType86AveragingorRateType46                
,pointType86PointTagIDTLP47               :: !PointType86PointTagIDTLP47                
,pointType86ExtHistLogPnt47               :: !PointType86ExtHistLogPnt47                
,pointType86ArchiveType47                 :: !PointType86ArchiveType47                
,pointType86AveragingorRateType47         :: !PointType86AveragingorRateType47                
,pointType86PointTagIDTLP48               :: !PointType86PointTagIDTLP48                
,pointType86ExtHistLogPnt48               :: !PointType86ExtHistLogPnt48                
,pointType86ArchiveType48                 :: !PointType86ArchiveType48                
,pointType86AveragingorRateType48         :: !PointType86AveragingorRateType48                
,pointType86PointTagIDTLP49               :: !PointType86PointTagIDTLP49                
,pointType86ExtHistLogPnt49               :: !PointType86ExtHistLogPnt49                
,pointType86ArchiveType49                 :: !PointType86ArchiveType49                
,pointType86AveragingorRateType49         :: !PointType86AveragingorRateType49                
,pointType86PointTagIDTLP50               :: !PointType86PointTagIDTLP50                
,pointType86ExtHistLogPnt50               :: !PointType86ExtHistLogPnt50                
,pointType86ArchiveType50                 :: !PointType86ArchiveType50                
,pointType86AveragingorRateType50         :: !PointType86AveragingorRateType50

  
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
type PointType86PointTagIDTLP26             = [Word8]                                 
type PointType86ExtHistLogPnt26             = [Word8]                                 
type PointType86ArchiveType26               = Word8                                 
type PointType86AveragingorRateType26       = Word8                                 
type PointType86PointTagIDTLP27             = [Word8]                                 
type PointType86ExtHistLogPnt27             = [Word8]                                 
type PointType86ArchiveType27               = Word8                                 
type PointType86AveragingorRateType27       = Word8                                 
type PointType86PointTagIDTLP28             = [Word8]                                 
type PointType86ExtHistLogPnt28             = [Word8]                                 
type PointType86ArchiveType28               = Word8                                 
type PointType86AveragingorRateType28       = Word8                                 
type PointType86PointTagIDTLP29             = [Word8]                                 
type PointType86ExtHistLogPnt29             = [Word8]                                 
type PointType86ArchiveType29               = Word8                                 
type PointType86AveragingorRateType29       = Word8                                 
type PointType86PointTagIDTLP30             = [Word8]                                 
type PointType86ExtHistLogPnt30             = [Word8]                                 
type PointType86ArchiveType30               = Word8                                 
type PointType86AveragingorRateType30       = Word8                                 
type PointType86PointTagIDTLP31             = [Word8]                                 
type PointType86ExtHistLogPnt31             = [Word8]                                 
type PointType86ArchiveType31               = Word8                                 
type PointType86AveragingorRateType31       = Word8                                 
type PointType86PointTagIDTLP32             = [Word8]                                 
type PointType86ExtHistLogPnt32             = [Word8]                                 
type PointType86ArchiveType32               = Word8                                 
type PointType86AveragingorRateType32       = Word8                                 
type PointType86PointTagIDTLP33             = [Word8]                                 
type PointType86ExtHistLogPnt33             = [Word8]                                 
type PointType86ArchiveType33               = Word8                                 
type PointType86AveragingorRateType33       = Word8                                 
type PointType86PointTagIDTLP34             = [Word8]                                 
type PointType86ExtHistLogPnt34             = [Word8]                                 
type PointType86ArchiveType34               = Word8                                 
type PointType86AveragingorRateType34       = Word8                                 
type PointType86PointTagIDTLP35             = [Word8]                                 
type PointType86ExtHistLogPnt35             = [Word8]                                 
type PointType86ArchiveType35               = Word8                                 
type PointType86AveragingorRateType35       = Word8                                 
type PointType86PointTagIDTLP36             = [Word8]                                 
type PointType86ExtHistLogPnt36             = [Word8]                                 
type PointType86ArchiveType36               = Word8                                 
type PointType86AveragingorRateType36       = Word8                                 
type PointType86PointTagIDTLP37             = [Word8]                                 
type PointType86ExtHistLogPnt37             = [Word8]                                 
type PointType86ArchiveType37               = Word8                                 
type PointType86AveragingorRateType37       = Word8                                 
type PointType86PointTagIDTLP38             = [Word8]                                 
type PointType86ExtHistLogPnt38             = [Word8]                                 
type PointType86ArchiveType38               = Word8                                 
type PointType86AveragingorRateType38       = Word8                                 
type PointType86PointTagIDTLP39             = [Word8]                                 
type PointType86ExtHistLogPnt39             = [Word8]                                 
type PointType86ArchiveType39               = Word8                                 
type PointType86AveragingorRateType39       = Word8                                 
type PointType86PointTagIDTLP40             = [Word8]                                 
type PointType86ExtHistLogPnt40             = [Word8]                                 
type PointType86ArchiveType40               = Word8                                 
type PointType86AveragingorRateType40       = Word8                                 
type PointType86PointTagIDTLP41             = [Word8]                                 
type PointType86ExtHistLogPnt41             = [Word8]                                 
type PointType86ArchiveType41               = Word8                                 
type PointType86AveragingorRateType41       = Word8                                 
type PointType86PointTagIDTLP42             = [Word8]                                 
type PointType86ExtHistLogPnt42             = [Word8]                                 
type PointType86ArchiveType42               = Word8                                 
type PointType86AveragingorRateType42       = Word8                                 
type PointType86PointTagIDTLP43             = [Word8]                                 
type PointType86ExtHistLogPnt43             = [Word8]                                 
type PointType86ArchiveType43               = Word8                                 
type PointType86AveragingorRateType43       = Word8                                 
type PointType86PointTagIDTLP44             = [Word8]                                 
type PointType86ExtHistLogPnt44             = [Word8]                                 
type PointType86ArchiveType44               = Word8                                 
type PointType86AveragingorRateType44       = Word8                                 
type PointType86PointTagIDTLP45             = [Word8]                                 
type PointType86ExtHistLogPnt45             = [Word8]                                 
type PointType86ArchiveType45               = Word8                                 
type PointType86AveragingorRateType45       = Word8                                 
type PointType86PointTagIDTLP46             = [Word8]                                 
type PointType86ExtHistLogPnt46             = [Word8]                                 
type PointType86ArchiveType46               = Word8                                 
type PointType86AveragingorRateType46       = Word8                                 
type PointType86PointTagIDTLP47             = [Word8]                                 
type PointType86ExtHistLogPnt47             = [Word8]                                 
type PointType86ArchiveType47               = Word8                                 
type PointType86AveragingorRateType47       = Word8                                 
type PointType86PointTagIDTLP48             = [Word8]                                 
type PointType86ExtHistLogPnt48             = [Word8]                                 
type PointType86ArchiveType48               = Word8                                 
type PointType86AveragingorRateType48       = Word8                                 
type PointType86PointTagIDTLP49             = [Word8]                                 
type PointType86ExtHistLogPnt49             = [Word8]                                 
type PointType86ArchiveType49               = Word8                                 
type PointType86AveragingorRateType49       = Word8                                 
type PointType86PointTagIDTLP50             = [Word8]                                 
type PointType86ExtHistLogPnt50             = [Word8]                                 
type PointType86ArchiveType50               = Word8                                 
type PointType86AveragingorRateType50       = Word8
  
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
  pointTagIDTLP26 <- getTLP
  extHistLogPnt26 <- getTLP
  archiveType26 <- getWord8
  averagingorRateType26 <- getWord8
  pointTagIDTLP27 <- getTLP
  extHistLogPnt27 <- getTLP
  archiveType27 <- getWord8
  averagingorRateType27 <- getWord8
  pointTagIDTLP28 <- getTLP
  extHistLogPnt28 <- getTLP
  archiveType28 <- getWord8
  averagingorRateType28 <- getWord8
  pointTagIDTLP29 <- getTLP
  extHistLogPnt29 <- getTLP
  archiveType29 <- getWord8
  averagingorRateType29 <- getWord8
  pointTagIDTLP30 <- getTLP
  extHistLogPnt30 <- getTLP
  archiveType30 <- getWord8
  averagingorRateType30 <- getWord8
  pointTagIDTLP31 <- getTLP
  extHistLogPnt31 <- getTLP
  archiveType31 <- getWord8
  averagingorRateType31 <- getWord8
  pointTagIDTLP32 <- getTLP
  extHistLogPnt32 <- getTLP
  archiveType32 <- getWord8
  averagingorRateType32 <- getWord8
  pointTagIDTLP33 <- getTLP
  extHistLogPnt33 <- getTLP
  archiveType33 <- getWord8
  averagingorRateType33 <- getWord8
  pointTagIDTLP34 <- getTLP
  extHistLogPnt34 <- getTLP
  archiveType34 <- getWord8
  averagingorRateType34 <- getWord8
  pointTagIDTLP35 <- getTLP
  extHistLogPnt35 <- getTLP
  archiveType35 <- getWord8
  averagingorRateType35 <-getWord8 
  pointTagIDTLP36 <- getTLP
  extHistLogPnt36 <- getTLP
  archiveType36 <- getWord8
  averagingorRateType36 <- getWord8
  pointTagIDTLP37 <- getTLP
  extHistLogPnt37 <- getTLP
  archiveType37 <- getWord8
  averagingorRateType37 <- getWord8
  pointTagIDTLP38 <- getTLP
  extHistLogPnt38 <- getTLP
  archiveType38 <- getWord8
  averagingorRateType38 <- getWord8
  pointTagIDTLP39 <- getTLP
  extHistLogPnt39 <- getTLP
  archiveType39 <- getWord8
  averagingorRateType39 <- getWord8
  pointTagIDTLP40 <- getTLP
  extHistLogPnt40 <- getTLP
  archiveType40 <- getWord8
  averagingorRateType40 <- getWord8
  pointTagIDTLP41 <- getTLP
  extHistLogPnt41 <- getTLP
  archiveType41 <- getWord8
  averagingorRateType41 <- getWord8
  pointTagIDTLP42 <- getTLP
  extHistLogPnt42 <- getTLP
  archiveType42 <- getWord8
  averagingorRateType42 <- getWord8
  pointTagIDTLP43 <- getTLP
  extHistLogPnt43 <- getTLP
  archiveType43 <- getWord8
  averagingorRateType43 <- getWord8
  pointTagIDTLP44 <- getTLP
  extHistLogPnt44 <- getTLP
  archiveType44 <- getWord8
  averagingorRateType44 <- getWord8
  pointTagIDTLP45 <- getTLP
  extHistLogPnt45 <- getTLP
  archiveType45 <- getWord8
  averagingorRateType45 <- getWord8
  pointTagIDTLP46 <- getTLP
  extHistLogPnt46 <- getTLP
  archiveType46 <- getWord8
  averagingorRateType46 <- getWord8
  pointTagIDTLP47 <- getTLP
  extHistLogPnt47 <- getTLP
  archiveType47 <- getWord8
  averagingorRateType47 <- getWord8
  pointTagIDTLP48 <- getTLP
  extHistLogPnt48 <- getTLP
  archiveType48 <- getWord8
  averagingorRateType48 <- getWord8
  pointTagIDTLP49 <- getTLP
  extHistLogPnt49 <- getTLP
  archiveType49 <- getWord8
  averagingorRateType49 <- getWord8
  pointTagIDTLP50 <- getTLP
  extHistLogPnt50 <- getTLP
  archiveType50 <- getWord8
  averagingorRateType50 <- getWord8  
  
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
    extHistLogPnt23 archiveType23 averagingorRateType23 pointTagIDTLP24 extHistLogPnt24 archiveType24 averagingorRateType24 pointTagIDTLP25 extHistLogPnt25 archiveType25 
    averagingorRateType25 pointTagIDTLP26 extHistLogPnt26 archiveType26 averagingorRateType26 pointTagIDTLP27 extHistLogPnt27 archiveType27 averagingorRateType27 pointTagIDTLP28 
    extHistLogPnt28 archiveType28 averagingorRateType28 pointTagIDTLP29 extHistLogPnt29 archiveType29 averagingorRateType29 pointTagIDTLP30 extHistLogPnt30 archiveType30 
    averagingorRateType30 pointTagIDTLP31 extHistLogPnt31 archiveType31 averagingorRateType31 pointTagIDTLP32 extHistLogPnt32 archiveType32 averagingorRateType32 pointTagIDTLP33  
    extHistLogPnt33 archiveType33 averagingorRateType33 pointTagIDTLP34 extHistLogPnt34 archiveType34 averagingorRateType34 pointTagIDTLP35 extHistLogPnt35 archiveType35 
    averagingorRateType35 pointTagIDTLP36 extHistLogPnt36 archiveType36 averagingorRateType36 pointTagIDTLP37 extHistLogPnt37 archiveType37 averagingorRateType37 pointTagIDTLP38 
    extHistLogPnt38 archiveType38 averagingorRateType38 pointTagIDTLP39 extHistLogPnt39 archiveType39 averagingorRateType39 pointTagIDTLP40 extHistLogPnt40 archiveType40 
    averagingorRateType40 pointTagIDTLP41 extHistLogPnt41 archiveType41 averagingorRateType41 pointTagIDTLP42 extHistLogPnt42 archiveType42 averagingorRateType42 pointTagIDTLP43  
    extHistLogPnt43 archiveType43 averagingorRateType43 pointTagIDTLP44 extHistLogPnt44 archiveType44 averagingorRateType44 pointTagIDTLP45 extHistLogPnt45 archiveType45 
    averagingorRateType45 pointTagIDTLP46 extHistLogPnt46 archiveType46 averagingorRateType46 pointTagIDTLP47 extHistLogPnt47 archiveType47 averagingorRateType47 pointTagIDTLP48 
    extHistLogPnt48 archiveType48 averagingorRateType48 pointTagIDTLP49 extHistLogPnt49 archiveType49 averagingorRateType49 pointTagIDTLP50 extHistLogPnt50 archiveType50 averagingorRateType50