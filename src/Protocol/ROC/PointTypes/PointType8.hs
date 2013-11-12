{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType8 where

import GHC.Generics
import Data.Word
import Data.Binary
import Protocol.ROC.PointTypes.Utils

data PointType8 = PointType8 {
 pointType8PtTagTLP1          :: !PointType8PtTagTLP1 
,pointType8HistLogPtNum1      :: !PointType8HistLogPtNum1        
,pointType8ArchiveType1       :: !PointType8ArchiveType1         
,pointType8AvgOrRateType1     :: !PointType8AvgOrRateType1                
,pointType8PtTagTLP2          :: !PointType8PtTagTLP2 
,pointType8HistLogPtNum2      :: !PointType8HistLogPtNum2        
,pointType8ArchiveType2       :: !PointType8ArchiveType2         
,pointType8AvgOrRateType2     :: !PointType8AvgOrRateType2
,pointType8PtTagTLP3          :: !PointType8PtTagTLP3 
,pointType8HistLogPtNum3      :: !PointType8HistLogPtNum3        
,pointType8ArchiveType3       :: !PointType8ArchiveType3         
,pointType8AvgOrRateType3     :: !PointType8AvgOrRateType3                
,pointType8PtTagTLP4          :: !PointTypePtTagTLP4 
,pointType8HistLogPtNum4      :: !PointType8HistLogPtNum4        
,pointType8ArchiveType4       :: !PointType8ArchiveType4         
,pointType8AvgOrRateType4     :: !PointType8AvgOrRateType4                
,pointType8PtTagTLP5          :: !PointType8PtTagTLP5 
,pointType8HistLogPtNum5      :: !PointType8HistLogPtNum5        
,pointType8ArchiveType5       :: !PointType8ArchiveType5         
,pointType8AvgOrRateType5     :: !PointType8AvgOrRateType5                
,pointType8PtTagTLP6          :: !PointType8PtTagTLP6 
,pointType8HistLogPtNum6      :: !PointType8HistLogPtNum6        
,pointType8ArchiveType6       :: !PointType8ArchiveType6         
,pointType8AvgOrRateType6     :: !PointType8AvgOrRateType6                
,pointType8PtTagTLP7          :: !PointType8PtTagTLP7 
,pointType8HistLogPtNum7      :: !PointType8HistLogPtNum7        
,pointType8ArchiveType7       :: !PointType8ArchiveType7         
,pointType8AvgOrRateType7     :: !PointType8AvgOrRateType7                
,pointType8PtTagTLP8          :: !PointType8PtTagTLP8 
,pointType8HistLogPtNum8      :: !PointType8HistLogPtNum8        
,pointType8ArchiveType8       :: !PointType8ArchiveType8         
,pointType8AvgOrRateType8     :: !PointType8AvgOrRateType8                
,pointType8PtTagTLP9          :: !PointType8PtTagTLP9 
,pointType8HistLogPtNum9      :: !PointType8HistLogPtNum9        
,pointType8ArchiveType9       :: !PointType8ArchiveType9         
,pointType8AvgOrRateType9     :: !PointType8AvgOrRateType9                
,pointType8PtTagTLP10         :: !PointType8PtTagTLP10 
,pointType8HistLogPtNum10     :: !PointType8HistLogPtNum10        
,pointType8ArchiveType10      :: !PointType8ArchiveType10         
,pointType8AvgOrRateType10    :: !PointType8AvgOrRateType10                
,pointType8PtTagTLP11         :: !PointType8PtTagTLP11 
,pointType8HistLogPtNum11     :: !PointType8HistLogPtNum11        
,pointType8ArchiveType11      :: !PointType8ArchiveType11         
,pointType8AvgOrRateType11    :: !PointType8AvgOrRateType11                
,pointType8PtTagTLP12         :: !PointType8PtTagTLP12 
,pointType8HistLogPtNum12     :: !PointType8HistLogPtNum12        
,pointType8ArchiveType12      :: !PointType8ArchiveType12         
,pointType8AvgOrRateType12    :: !PointType8AvgOrRateType12                
,pointType8PtTagTLP13         :: !PointType8PtTagTLP13 
,pointType8HistLogPtNum13     :: !PointType8HistLogPtNum13        
,pointType8ArchiveType13      :: !PointType8ArchiveType13         
,pointType8AvgOrRateType13    :: !PointType8AvgOrRateType13                
,pointType8PtTagTLP14         :: !PointType8PtTagTLP14 
,pointType8HistLogPtNum14     :: !PointType8HistLogPtNum14        
,pointType8ArchiveType14      :: !PointType8ArchiveType14         
,pointType8AvgOrRateType14    :: !PointType8AvgOrRateType14                
,pointType8PtTagTLP15         :: !PointType8PtTagTLP15 
,pointType8HistLogPtNum15     :: !PointType8HistLogPtNum15        
,pointType8ArchiveType15      :: !PointType8ArchiveType15         
,pointType8AvgOrRateType15    :: !PointType8AvgOrRateType15                                         
} deriving (Read,Eq, Show, Generic)                       

type PointType8PtTagTLP1           = [Word8]                           
type PointType8HistLogPtNum1       = [Word8]                          
type PointType8ArchiveType1        = Word8                          
type PointType8AvgOrRateType1      = Word8                          
type PointType8PtTagTLP2           = [Word8]                           
type PointType8HistLogPtNum2       = [Word8]                           
type PointType8ArchiveType2        = Word8                             
type PointType8AvgOrRateType2      = Word8                             
type PointType8PtTagTLP3           = [Word8]                           
type PointType8HistLogPtNum3       = [Word8]                           
type PointType8ArchiveType3        = Word8                             
type PointType8AvgOrRateType3      = Word8                             
type PointTypePtTagTLP4            = [Word8]                           
type PointType8HistLogPtNum4       = [Word8]                           
type PointType8ArchiveType4        = Word8                          
type PointType8AvgOrRateType4      = Word8                            
type PointType8PtTagTLP5           = [Word8]                     
type PointType8HistLogPtNum5       = [Word8]                         
type PointType8ArchiveType5        = Word8                          
type PointType8AvgOrRateType5      = Word8                            
type PointType8PtTagTLP6           = [Word8]                     
type PointType8HistLogPtNum6       = [Word8]                         
type PointType8ArchiveType6        = Word8                          
type PointType8AvgOrRateType6      = Word8                            
type PointType8PtTagTLP7           = [Word8]                     
type PointType8HistLogPtNum7       = [Word8]                         
type PointType8ArchiveType7        = Word8                          
type PointType8AvgOrRateType7      = Word8                            
type PointType8PtTagTLP8           = [Word8]                     
type PointType8HistLogPtNum8       = [Word8]                         
type PointType8ArchiveType8        = Word8                          
type PointType8AvgOrRateType8      = Word8                            
type PointType8PtTagTLP9           = [Word8]                     
type PointType8HistLogPtNum9       = [Word8]                         
type PointType8ArchiveType9        = Word8                          
type PointType8AvgOrRateType9      = Word8                            
type PointType8PtTagTLP10          = [Word8]                     
type PointType8HistLogPtNum10      = [Word8]                         
type PointType8ArchiveType10       = Word8                          
type PointType8AvgOrRateType10     = Word8                            
type PointType8PtTagTLP11          = [Word8]                      
type PointType8HistLogPtNum11      = [Word8]                          
type PointType8ArchiveType11       = Word8                           
type PointType8AvgOrRateType11     = Word8                             
type PointType8PtTagTLP12          = [Word8]                      
type PointType8HistLogPtNum12      = [Word8]                          
type PointType8ArchiveType12       = Word8                           
type PointType8AvgOrRateType12     = Word8                             
type PointType8PtTagTLP13          = [Word8]                      
type PointType8HistLogPtNum13      = [Word8]                          
type PointType8ArchiveType13       = Word8                           
type PointType8AvgOrRateType13     = Word8                             
type PointType8PtTagTLP14          = [Word8]                      
type PointType8HistLogPtNum14      = [Word8]                          
type PointType8ArchiveType14       = Word8                           
type PointType8AvgOrRateType14     = Word8                             
type PointType8PtTagTLP15          = [Word8]                      
type PointType8HistLogPtNum15      = [Word8]                          
type PointType8ArchiveType15       = Word8                           
type PointType8AvgOrRateType15     = Word8                             

  
pointType8Parser :: Get PointType8 
pointType8Parser = do 
  ptTagTLP1 <- getTLP                 
  histLogPtNum1 <- getTLP             
  archiveType1 <- getWord8          
  avgOrRateType1 <- getWord8      
  ptTagTLP2 <- getTLP                 
  histLogPtNum2 <- getTLP             
  archiveType2 <- getWord8          
  avgOrRateType2 <- getWord8        
  ptTagTLP3 <- getTLP                 
  histLogPtNum3 <- getTLP             
  archiveType3 <- getWord8          
  avgOrRateType3 <- getWord8      
  ptTagTLP4 <- getTLP                 
  histLogPtNum4 <- getTLP             
  archiveType4 <- getWord8          
  avgOrRateType4 <- getWord8      
  ptTagTLP5 <- getTLP                 
  histLogPtNum5 <- getTLP             
  archiveType5 <- getWord8          
  avgOrRateType5 <- getWord8      
  ptTagTLP6 <- getTLP                 
  histLogPtNum6 <- getTLP             
  archiveType6 <- getWord8          
  avgOrRateType6 <- getWord8      
  ptTagTLP7 <- getTLP                 
  histLogPtNum7 <- getTLP             
  archiveType7 <- getWord8          
  avgOrRateType7 <- getWord8      
  ptTagTLP8 <- getTLP                 
  histLogPtNum8 <- getTLP             
  archiveType8 <- getWord8          
  avgOrRateType8 <- getWord8      
  ptTagTLP9 <- getTLP                 
  histLogPtNum9 <- getTLP             
  archiveType9 <- getWord8          
  avgOrRateType9 <- getWord8      
  ptTagTLP10 <- getTLP                 
  histLogPtNum10 <- getTLP             
  archiveType10 <- getWord8          
  avgOrRateType10 <- getWord8      
  ptTagTLP11 <- getTLP                 
  histLogPtNum11 <- getTLP             
  archiveType11 <- getWord8          
  avgOrRateType11 <- getWord8      
  ptTagTLP12 <- getTLP                 
  histLogPtNum12 <- getTLP             
  archiveType12 <- getWord8          
  avgOrRateType12 <- getWord8      
  ptTagTLP13 <- getTLP                 
  histLogPtNum13 <- getTLP             
  archiveType13 <- getWord8          
  avgOrRateType13 <- getWord8      
  ptTagTLP14 <- getTLP                 
  histLogPtNum14 <- getTLP             
  archiveType14 <- getWord8          
  avgOrRateType14 <- getWord8      
  ptTagTLP15 <- getTLP                 
  histLogPtNum15 <- getTLP             
  archiveType15 <- getWord8          
  avgOrRateType15 <- getWord8      
  
  return $ PointType8 ptTagTLP1 histLogPtNum1 archiveType1 avgOrRateType1 ptTagTLP2 histLogPtNum2 archiveType2 avgOrRateType2 ptTagTLP3 histLogPtNum3 archiveType3 avgOrRateType3
    ptTagTLP4 histLogPtNum4 archiveType4 avgOrRateType4 ptTagTLP5 histLogPtNum5 archiveType5 avgOrRateType5 ptTagTLP6 histLogPtNum6 archiveType6 avgOrRateType6 ptTagTLP7 histLogPtNum7 
    archiveType7 avgOrRateType7 ptTagTLP8 histLogPtNum8 archiveType8 avgOrRateType8 ptTagTLP9 histLogPtNum9 archiveType9 avgOrRateType9 ptTagTLP10 histLogPtNum10 archiveType10 
    avgOrRateType10 ptTagTLP11 histLogPtNum11 archiveType11 avgOrRateType11 ptTagTLP12 histLogPtNum12 archiveType12 avgOrRateType12 ptTagTLP13 histLogPtNum13 archiveType13 avgOrRateType13 
    ptTagTLP14 histLogPtNum14 archiveType14 avgOrRateType14 ptTagTLP15 histLogPtNum15 archiveType15 avgOrRateType15