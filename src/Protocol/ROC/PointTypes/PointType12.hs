{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType12 where

import GHC.Generics
import Data.Word
import Data.Binary
import Protocol.ROC.Utils

data PointType12 = PointType12 {
 pointType12Seconds                :: !PointType12Seconds                                     
,pointType12Minutes                :: !PointType12Minutes                          
,pointType12Hours                  :: !PointType12Hours                             
,pointType12Day                    :: !PointType12Day                           
,pointType12Month                  :: !PointType12Month                             
,pointType12Year                   :: !PointType12Year                             
,pointType12LeapYear               :: !PointType12LeapYear                             
,pointType12DayofWeek              :: !PointType12DayofWeek                                 
,pointType12TimeSMHDMY            :: !PointType12TimeSMHDMY                                                              
,pointType12Century                :: !PointType12Century                             
,pointType12EnableDaySavTime       :: !PointType12EnableDaySavTime                            
                      
} deriving (Read,Eq, Show, Generic)                       

type PointType12Seconds            = Word8    
type PointType12Minutes            = Word8            
type PointType12Hours              = Word8 
type PointType12Day                = Word8     
type PointType12Month              = Word8   
type PointType12Year               = Word8  
type PointType12LeapYear           = Word8 
type PointType12DayofWeek          = Word8
type PointType12TimeSMHDMY         = [Word8] 
type PointType12Century            = Word8 
type PointType12EnableDaySavTime   = Bool  

pointType12Parser :: Get PointType12
pointType12Parser = do 

  seconds <- getWord8  
  minutes <- getWord8  
  hours <- getWord8  
  day <- getWord8  
  month <- getWord8  
  year <- getWord8  
  leapYear <- getWord8  
  dayOfWeek <- getWord8  
  timeSMHDMY <- getTime  
  century <- getWord8  
  enableDaySavTime <- anyButNull  
  
  return $ PointType12 seconds minutes hours day month year leapYear dayOfWeek timeSMHDMY century enableDaySavTime  
  