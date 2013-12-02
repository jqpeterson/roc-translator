{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType55 where

import GHC.Generics
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Utils

data PointType55 = PointType55 {
  
 pointType55EventAlarmRgstr                :: !PointType55EventAlarmRgstr                                   
,pointType55PeriodicHistIndexRgstr         :: !PointType55PeriodicHistIndexRgstr                                                
,pointType55DailyHistIndexRgstr            :: !PointType55DailyHistIndexRgstr                                           
,pointType55ExtHistIndexRgstr              :: !PointType55ExtHistIndexRgstr                                           
,pointType55HistFormat                     :: !PointType55HistFormat                                    
,pointType55HistArchiveRgsr1               :: !PointType55HistArchiveRgsr1                                          
,pointType55StartHistPnt1                  :: !PointType55StartHistPnt1                                 
,pointType55EndHistPnt1                    :: !PointType55EndHistPnt1                                     
,pointType55HistArchiveType1               :: !PointType55HistArchiveType1                                           
,pointType55ConversionCode1                :: !PointType55ConversionCode1                                         
,pointType55HistArchiveRgsr2               :: !PointType55HistArchiveRgsr2                                                             
,pointType55StartHistPnt2                  :: !PointType55StartHistPnt2                                                      
,pointType55EndHistPnt2                    :: !PointType55EndHistPnt2                                                           
,pointType55HistArchiveType2               :: !PointType55HistArchiveType2                                                            
,pointType55ConversionCode2                :: !PointType55ConversionCode2                                                            
,pointType55HistArchiveRgsr3               :: !PointType55HistArchiveRgsr3                                                              
,pointType55StartHistPnt3                  :: !PointType55StartHistPnt3                                                       
,pointType55EndHistPnt3                    :: !PointType55EndHistPnt3                                                        
,pointType55HistArchiveType3               :: !PointType55HistArchiveType3                                                              
,pointType55ConversionCode3                :: !PointType55ConversionCode3                                                              
,pointType55HistArchiveRgsr4               :: !PointType55HistArchiveRgsr4                                                   
,pointType55StartHistPnt4                  :: !PointType55StartHistPnt4                                         
,pointType55EndHistPnt4                    :: !PointType55EndHistPnt4                                          
,pointType55HistArchiveType4               :: !PointType55HistArchiveType4                                                   
,pointType55ConversionCode4                :: !PointType55ConversionCode4                                              
,pointType55HistArchiveRgsr5               :: !PointType55HistArchiveRgsr5                                                        
,pointType55StartHistPnt5                  :: !PointType55StartHistPnt5                                                   
,pointType55EndHistPnt5                    :: !PointType55EndHistPnt5                                                   
,pointType55HistArchiveType5               :: !PointType55HistArchiveType5                                                         
,pointType55ConversionCode5                :: !PointType55ConversionCode5                                                       
,pointType55HistArchiveRgsr6               :: !PointType55HistArchiveRgsr6                                                      
,pointType55StartHistPnt6                  :: !PointType55StartHistPnt6                                                 
,pointType55EndHistPnt6                    :: !PointType55EndHistPnt6                                                    
,pointType55HistArchiveType6               :: !PointType55HistArchiveType6                                                       
,pointType55ConversionCode6                :: !PointType55ConversionCode6                                                     
,pointType55HistArchiveRgsr7               :: !PointType55HistArchiveRgsr7                                                    
,pointType55StartHistPnt7                  :: !PointType55StartHistPnt7                                                      
,pointType55EndHistPnt7                    :: !PointType55EndHistPnt7                                                    
,pointType55HistArchiveType7               :: !PointType55HistArchiveType7                                                       
,pointType55ConversionCode7                :: !PointType55ConversionCode7                                                       
,pointType55HistArchiveRgsr8               :: !PointType55HistArchiveRgsr8                                                          
,pointType55StartHistPnt8                  :: !PointType55StartHistPnt8                                                  
,pointType55EndHistPnt8                    :: !PointType55EndHistPnt8                                                  
,pointType55HistArchiveType8               :: !PointType55HistArchiveType8                                                         
,pointType55ConversionCode8                :: !PointType55ConversionCode8                                                         
,pointType55HistArchiveRgsr9               :: !PointType55HistArchiveRgsr9                                                         
,pointType55StartHistPnt9                  :: !PointType55StartHistPnt9                                                    
,pointType55EndHistPnt9                    :: !PointType55EndHistPnt9                                                    
,pointType55HistArchiveType9               :: !PointType55HistArchiveType9                                                      
,pointType55ConversionCode9                :: !PointType55ConversionCode9                                                        
,pointType55HistArchiveRgsr10              :: !PointType55HistArchiveRgsr10                                                       
,pointType55StartHistPnt10                 :: !PointType55StartHistPnt10                                                       
,pointType55EndHistPnt10                   :: !PointType55EndHistPnt10                                                    
,pointType55HistArchiveType10              :: !PointType55HistArchiveType10                                                          
,pointType55ConversionCode10               :: !PointType55ConversionCode10                                                       
,pointType55DateAccessRgstr                :: !PointType55DateAccessRgstr                                                        
,pointType55TimeAccessRgstr                :: !PointType55TimeAccessRgstr                                                         
,pointType55EFMExt                         :: !PointType55EFMExt                                              
,pointType55HistArchiveRgsr11              :: !PointType55HistArchiveRgsr11                                                     
,pointType55StartHistPnt11                 :: !PointType55StartHistPnt11                                              
,pointType55EndHistPnt11                   :: !PointType55EndHistPnt11                                                      
,pointType55HistArchiveType11              :: !PointType55HistArchiveType11                                                        
,pointType55ConversionCode11               :: !PointType55ConversionCode11                                                      
,pointType55HistArchiveRgsr12              :: !PointType55HistArchiveRgsr12                                                         
,pointType55StartHistPnt12                 :: !PointType55StartHistPnt12                                                      
,pointType55EndHistPnt12                   :: !PointType55EndHistPnt12                                                    
,pointType55HistArchiveType12              :: !PointType55HistArchiveType12                                                   
,pointType55ConversionCode12               :: !PointType55ConversionCode12                                                        
,pointType55HistArchiveRgsr13              :: !PointType55HistArchiveRgsr13                                                          
,pointType55StartHistPnt13                 :: !PointType55StartHistPnt13                                                      
,pointType55EndHistPnt13                   :: !PointType55EndHistPnt13                                                  
,pointType55HistArchiveType13              :: !PointType55HistArchiveType13                                                         
,pointType55ConversionCode13               :: !PointType55ConversionCode13                                                         
,pointType55HistArchiveRgsr14              :: !PointType55HistArchiveRgsr14                                                     
,pointType55StartHistPnt14                 :: !PointType55StartHistPnt14                                                    
,pointType55EndHistPnt14                   :: !PointType55EndHistPnt14                                                   
,pointType55HistArchiveType14              :: !PointType55HistArchiveType14                                                          
,pointType55ConversionCode14               :: !PointType55ConversionCode14                                                      
,pointType55HistArchiveRgsr15              :: !PointType55HistArchiveRgsr15                                                       
,pointType55StartHistPnt15                 :: !PointType55StartHistPnt15                                                      
,pointType55EndHistPnt15                   :: !PointType55EndHistPnt15                                                     
,pointType55HistArchiveType15              :: !PointType55HistArchiveType15                                                         
,pointType55ConversionCode15               :: !PointType55ConversionCode15                                                     
,pointType55HistArchiveRgsr16              :: !PointType55HistArchiveRgsr16                                                         
,pointType55StartHistPnt16                 :: !PointType55StartHistPnt16                                                   
,pointType55EndHistPnt16                   :: !PointType55EndHistPnt16                                                    
,pointType55HistArchiveType16              :: !PointType55HistArchiveType16                                                       
,pointType55ConversionCode16               :: !PointType55ConversionCode16                                                        
,pointType55HistArchiveRgsr17              :: !PointType55HistArchiveRgsr17                                                          
,pointType55StartHistPnt17                 :: !PointType55StartHistPnt17                                                      
,pointType55EndHistPnt17                   :: !PointType55EndHistPnt17                                                  
,pointType55HistArchiveType17              :: !PointType55HistArchiveType17                                                     
,pointType55ConversionCode17               :: !PointType55ConversionCode17                                                         
,pointType55HistArchiveRgsr18              :: !PointType55HistArchiveRgsr18                                                        
,pointType55StartHistPnt18                 :: !PointType55StartHistPnt18                                                    
,pointType55EndHistPnt18                   :: !PointType55EndHistPnt18                                                
,pointType55HistArchiveType18              :: !PointType55HistArchiveType18                                                          
,pointType55ConversionCode18               :: !PointType55ConversionCode18                                                         
,pointType55HistArchiveRgsr19              :: !PointType55HistArchiveRgsr19                                                        
,pointType55StartHistPnt19                 :: !PointType55StartHistPnt19                                                      
,pointType55EndHistPnt19                   :: !PointType55EndHistPnt19                                                      
,pointType55HistArchiveType19              :: !PointType55HistArchiveType19                                                      
,pointType55ConversionCode19               :: !PointType55ConversionCode19                                                       
,pointType55HistArchiveRgsr20              :: !PointType55HistArchiveRgsr20                                                          
,pointType55StartHistPnt20                 :: !PointType55StartHistPnt20                                                        
,pointType55EndHistPnt20                   :: !PointType55EndHistPnt20                                                     
,pointType55HistArchiveType20              :: !PointType55HistArchiveType20                                                        
,pointType55ConversionCode20               :: !PointType55ConversionCode20                                                         
  
} deriving (Read,Eq, Show, Generic)                       
                                  
type PointType55EventAlarmRgstr             = Word16                                                                           
type PointType55PeriodicHistIndexRgstr      = Word16                                                                                              
type PointType55DailyHistIndexRgstr         = Word16                                                                                    
type PointType55ExtHistIndexRgstr           = Word16                                                                                     
type PointType55HistFormat                  = Word8                                                                     
type PointType55HistArchiveRgsr1            = Word16                                                                                   
type PointType55StartHistPnt1               = Word8                                                                 
type PointType55EndHistPnt1                 = Word8                                                                         
type PointType55HistArchiveType1            = Word8                                                                                     
type PointType55ConversionCode1             = Word8                                                                                 
type PointType55HistArchiveRgsr2            = Word16                                                                                                    
type PointType55StartHistPnt2               = Word8                                                                                             
type PointType55EndHistPnt2                 = Word8                                                                                                 
type PointType55HistArchiveType2            = Word8                                                                                                  
type PointType55ConversionCode2             = Word8                                                                                                   
type PointType55HistArchiveRgsr3            = Word16                                                                                                      
type PointType55StartHistPnt3               = Word8                                                                                               
type PointType55EndHistPnt3                 = Word8                                                                                           
type PointType55HistArchiveType3            = Word8                                                                                                      
type PointType55ConversionCode3             = Word8                                                                                                       
type PointType55HistArchiveRgsr4            = Word16                                                                                             
type PointType55StartHistPnt4               = Word8                                                                                
type PointType55EndHistPnt4                 = Word8                                                                            
type PointType55HistArchiveType4            = Word8                                                                                             
type PointType55ConversionCode4             = Word8                                                                                    
type PointType55HistArchiveRgsr5            = Word16                                                                                                 
type PointType55StartHistPnt5               = Word8                                                                                        
type PointType55EndHistPnt5                 = Word8                                                                                        
type PointType55HistArchiveType5            = Word8                                                                                                    
type PointType55ConversionCode5             = Word8                                                                                                
type PointType55HistArchiveRgsr6            = Word16                                                                                             
type PointType55StartHistPnt6               = Word8                                                                                    
type PointType55EndHistPnt6                 = Word8                                                                                          
type PointType55HistArchiveType6            = Word8                                                                                                
type PointType55ConversionCode6             = Word8                                                                                            
type PointType55HistArchiveRgsr7            = Word16                                                                                         
type PointType55StartHistPnt7               = Word8                                                                                              
type PointType55EndHistPnt7                 = Word8                                                                                          
type PointType55HistArchiveType7            = Word8                                                                                                
type PointType55ConversionCode7             = Word8                                                                                                
type PointType55HistArchiveRgsr8            = Word16                                                                                                     
type PointType55StartHistPnt8               = Word8                                                                                      
type PointType55EndHistPnt8                 = Word8                                                                                      
type PointType55HistArchiveType8            = Word8                                                                                                    
type PointType55ConversionCode8             = Word8                                                                                                    
type PointType55HistArchiveRgsr9            = Word16                                                                                                   
type PointType55StartHistPnt9               = Word8                                                                                          
type PointType55EndHistPnt9                 = Word8                                                                                          
type PointType55HistArchiveType9            = Word8                                                                                              
type PointType55ConversionCode9             = Word8                                                                                                  
type PointType55HistArchiveRgsr10           = Word16                                                                                               
type PointType55StartHistPnt10              = Word8                                                                                                
type PointType55EndHistPnt10                = Word8                                                                                          
type PointType55HistArchiveType10           = Word8                                                                                                      
type PointType55ConversionCode10            = Word8                                                                                                
type PointType55DateAccessRgstr             = Word16                                                                                                 
type PointType55TimeAccessRgstr             = Word16                                                                                                   
type PointType55EFMExt                      = Bool                                                                             
type PointType55HistArchiveRgsr11           = Word16                                                                                              
type PointType55StartHistPnt11              = Word8                                                                                   
type PointType55EndHistPnt11                = Word8                                                                                   
type PointType55HistArchiveType11           = Word8                                                                                                 
type PointType55ConversionCode11            = Word8                                                                                               
type PointType55HistArchiveRgsr12           = Word16                                                                                                    
type PointType55StartHistPnt12              = Word8                                                                                            
type PointType55EndHistPnt12                = Word8                                                                                        
type PointType55HistArchiveType12           = Word8                                                                                              
type PointType55ConversionCode12            = Word8                                                                                                   
type PointType55HistArchiveRgsr13           = Word16                                                                                                    
type PointType55StartHistPnt13              = Word8                                                                                           
type PointType55EndHistPnt13                = Word8                                                                                        
type PointType55HistArchiveType13           = Word8                                                                                                 
type PointType55ConversionCode13            = Word8                                                                                                   
type PointType55HistArchiveRgsr14           = Word16                                                                                             
type PointType55StartHistPnt14              = Word8                                                                                            
type PointType55EndHistPnt14                = Word8                                                                                          
type PointType55HistArchiveType14           = Word8                                                                                                     
type PointType55ConversionCode14            = Word8                                                                                              
type PointType55HistArchiveRgsr15           = Word16                                                                                             
type PointType55StartHistPnt15              = Word8                                                                                               
type PointType55EndHistPnt15                = Word8                                                                                          
type PointType55HistArchiveType15           = Word8                                                                                                  
type PointType55ConversionCode15            = Word8                                                                                           
type PointType55HistArchiveRgsr16           = Word16                                                                                                    
type PointType55StartHistPnt16              = Word8                                                                                            
type PointType55EndHistPnt16                = Word8                                                                                         
type PointType55HistArchiveType16           = Word8                                                                                                  
type PointType55ConversionCode16            = Word8                                                                                                    
type PointType55HistArchiveRgsr17           = Word16                                                                                                 
type PointType55StartHistPnt17              = Word8                                                                                             
type PointType55EndHistPnt17                = Word8                                                                                         
type PointType55HistArchiveType17           = Word8                                                                                                  
type PointType55ConversionCode17            = Word8                                                                                                    
type PointType55HistArchiveRgsr18           = Word16                                                                                                 
type PointType55StartHistPnt18              = Word8                                                                                             
type PointType55EndHistPnt18                = Word8                                                                                    
type PointType55HistArchiveType18           = Word8                                                                                                      
type PointType55ConversionCode18            = Word8                                                                                                 
type PointType55HistArchiveRgsr19           = Word16                                                                                                   
type PointType55StartHistPnt19              = Word8                                                                                              
type PointType55EndHistPnt19                = Word8                                                                                             
type PointType55HistArchiveType19           = Word8                                                                                                
type PointType55ConversionCode19            = Word8                                                                                                  
type PointType55HistArchiveRgsr20           = Word16                                                                                                      
type PointType55StartHistPnt20              = Word8                                                                                                
type PointType55EndHistPnt20                = Word8                                                                                          
type PointType55HistArchiveType20           = Word8                                                                                                
type PointType55ConversionCode20            = Word8                                                                                            
  
pointType55Parser :: Get PointType55
pointType55Parser = do 
                                  
  eventAlarmRgstr <- getWord16le                                   
  periodicHistIndexRgstr <- getWord16le                                     
  dailyHistIndexRgstr <- getWord16le                                      
  extHistIndexRgstr <- getWord16le                                     
  histFormat <- getWord8                                   
  histArchiveRgsr1 <- getWord16le                                     
  startHistPnt1 <- getWord8                                      
  endHistPnt1 <- getWord8                                     
  histArchiveType1 <- getWord8                                   
  conversionCode1 <- getWord8                                     
  histArchiveRgsr2 <- getWord16le                                      
  startHistPnt2 <- getWord8                                     
  endHistPnt2 <- getWord8                                   
  histArchiveType2 <- getWord8                                     
  conversionCode2 <- getWord8                                      
  histArchiveRgsr3 <- getWord16le                                     
  startHistPnt3 <- getWord8                                   
  endHistPnt3 <- getWord8                                     
  histArchiveType3 <- getWord8                                      
  conversionCode3 <- getWord8                                     
  histArchiveRgsr4 <- getWord16le                                   
  startHistPnt4 <- getWord8                                     
  endHistPnt4 <- getWord8                                      
  histArchiveType4 <- getWord8                                     
  conversionCode4 <- getWord8                                   
  histArchiveRgsr5 <- getWord16le                                     
  startHistPnt5 <- getWord8                                      
  endHistPnt5 <- getWord8                                     
  histArchiveType5 <- getWord8                                   
  conversionCode5 <- getWord8                                     
  histArchiveRgsr6 <- getWord16le                                      
  startHistPnt6 <- getWord8                                      
  endHistPnt6 <- getWord8                                    
  histArchiveType6 <- getWord8                                      
  conversionCode6 <- getWord8                                       
  histArchiveRgsr7 <- getWord16le                                      
  startHistPnt7 <- getWord8                                    
  endHistPnt7 <- getWord8                                      
  histArchiveType7 <- getWord8                                       
  conversionCode7 <- getWord8                                      
  histArchiveRgsr8 <- getWord16le                                    
  startHistPnt8 <- getWord8                                      
  endHistPnt8 <- getWord8                                       
  histArchiveType8 <- getWord8                                      
  conversionCode8 <- getWord8                                    
  histArchiveRgsr9 <- getWord16le                                      
  startHistPnt9 <- getWord8                                       
  endHistPnt9 <- getWord8                                      
  histArchiveType9 <- getWord8                                    
  conversionCode9 <- getWord8                                      
  histArchiveRgsr10 <- getWord16le                                      
  startHistPnt10 <- getWord8                                     
  endHistPnt10 <- getWord8                                  
  histArchiveType10 <- getWord8                                    
  conversionCode10 <- getWord8                                     
  dateAccessRgstr <- getWord16le  
  timeAccessRgstr <- getWord16le  
  eFMExt <- anyButNull  
  histArchiveRgsr11 <- getWord16le  
  startHistPnt11 <- getWord8  
  endHistPnt11 <- getWord8  
  histArchiveType11 <- getWord8  
  conversionCode11 <- getWord8  
  histArchiveRgsr12 <- getWord16le  
  startHistPnt12 <- getWord8  
  endHistPnt12 <- getWord8  
  histArchiveType12 <- getWord8  
  conversionCode12 <- getWord8  
  histArchiveRgsr13 <- getWord16le  
  startHistPnt13 <- getWord8  
  endHistPnt13 <- getWord8  
  histArchiveType13 <- getWord8  
  conversionCode13 <- getWord8  
  histArchiveRgsr14 <- getWord16le  
  startHistPnt14 <- getWord8  
  endHistPnt14 <- getWord8  
  histArchiveType14 <- getWord8  
  conversionCode14 <- getWord8  
  histArchiveRgsr15 <- getWord16le  
  startHistPnt15 <- getWord8  
  endHistPnt15 <- getWord8  
  histArchiveType15 <- getWord8  
  conversionCode15 <- getWord8  
  histArchiveRgsr16 <- getWord16le  
  startHistPnt16 <- getWord8  
  endHistPnt16 <- getWord8  
  histArchiveType16 <- getWord8  
  conversionCode16 <- getWord8  
  histArchiveRgsr17 <- getWord16le  
  startHistPnt17 <- getWord8  
  endHistPnt17 <- getWord8  
  histArchiveType17 <- getWord8  
  conversionCode17 <- getWord8  
  histArchiveRgsr18 <- getWord16le  
  startHistPnt18 <- getWord8  
  endHistPnt18 <- getWord8  
  histArchiveType18 <- getWord8  
  conversionCode18 <- getWord8  
  histArchiveRgsr19 <- getWord16le  
  startHistPnt19 <- getWord8  
  endHistPnt19 <- getWord8  
  histArchiveType19 <- getWord8  
  conversionCode19 <- getWord8  
  histArchiveRgsr20 <- getWord16le  
  startHistPnt20 <- getWord8  
  endHistPnt20 <- getWord8  
  histArchiveType20 <- getWord8  
  conversionCode20 <- getWord8  
  
  return $ PointType55 eventAlarmRgstr periodicHistIndexRgstr dailyHistIndexRgstr extHistIndexRgstr histFormat histArchiveRgsr1 startHistPnt1 endHistPnt1 histArchiveType1 conversionCode1 
    histArchiveRgsr2 startHistPnt2 endHistPnt2 histArchiveType2 conversionCode2 histArchiveRgsr3 startHistPnt3 endHistPnt3 histArchiveType3 conversionCode3 histArchiveRgsr4 startHistPnt4 
    endHistPnt4 histArchiveType4 conversionCode4 histArchiveRgsr5 startHistPnt5 endHistPnt5 histArchiveType5 conversionCode5 histArchiveRgsr6 startHistPnt6 endHistPnt6 histArchiveType6 
    conversionCode6 histArchiveRgsr7 startHistPnt7 endHistPnt7 histArchiveType7 conversionCode7 histArchiveRgsr8 startHistPnt8 endHistPnt8 histArchiveType8 conversionCode8 histArchiveRgsr9 
    startHistPnt9 endHistPnt9 histArchiveType9 conversionCode9 histArchiveRgsr10 startHistPnt10 endHistPnt10 histArchiveType10 conversionCode10 dateAccessRgstr timeAccessRgstr eFMExt 
    histArchiveRgsr11 startHistPnt11 endHistPnt11 histArchiveType11 conversionCode11 histArchiveRgsr12 startHistPnt12 endHistPnt12 histArchiveType12 conversionCode12 histArchiveRgsr13 
    startHistPnt13 endHistPnt13 histArchiveType13 conversionCode13 histArchiveRgsr14 startHistPnt14 endHistPnt14 histArchiveType14 conversionCode14 histArchiveRgsr15 startHistPnt15 
    endHistPnt15 histArchiveType15 conversionCode15 histArchiveRgsr16 startHistPnt16 endHistPnt16 histArchiveType16 conversionCode16 histArchiveRgsr17 startHistPnt17 endHistPnt17 
    histArchiveType17 conversionCode17 histArchiveRgsr18 startHistPnt18 endHistPnt18 histArchiveType18 conversionCode18 histArchiveRgsr19 startHistPnt19 endHistPnt19 histArchiveType19 
    conversionCode19 histArchiveRgsr20 startHistPnt20 endHistPnt20 histArchiveType20 conversionCode20









