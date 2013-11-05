{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType5 where

import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Builder as LBB
import Data.Word
import Data.Binary
import Data.Bool
import Data.Int
import Data.Binary.Get
import Numeric
import Control.Applicative
import Protocol.ROC.Float

data PointType5 = PointType5 {
 pointType5PointTag                      :: !PointType5PointTag                                     
,pointType5Units                         :: !PointType5Units                          
,pointType5RateFlag                      :: !PointType5RateFlag                                
,pointType5RatePeriod                    :: !PointType5RatePeriod                           
,pointType5FilterTime                    :: !PointType5FilterTime                              
,pointType5ScanPeriod                    :: !PointType5ScanPeriod                             
,pointType5ConversionFactor              :: !PointType5ConversionFactor                        
,pointType5LowAlarm                      :: !PointType5LowAlarm                            
,pointType5HighAlarm                     :: !PointType5HighAlarm                               
,pointType5LowLowAlarm                   :: !PointType5LowLowAlarm                             
,pointType5HighHighAlarm                 :: !PointType5HighHighAlarm                           
,pointType5RateAlarm                     :: !PointType5RateAlarm                               
,pointType5AlarmDeadband                 :: !PointType5AlarmDeadband                           
,pointType5EUValue                       :: !PointType5EUValue                                 
,pointType5ModeCFG                       :: !PointType5ModeCFG                    
,pointType5AlarmCode                     :: !PointType5AlarmCode                  
,pointType5AccumulatedValue              :: !PointType5AccumulatedValue           
,pointType5CurrentRates                  :: !PointType5CurrentRates               
,pointType5TodaysTotal                   :: !PointType5TodaysTotal                
,pointType5YesterdaysTotal               :: !PointType5YesterdaysTotal            
,pointType5PulsesForDay                  :: !PointType5PulsesForDay               
,pointType5FreqHertz                     :: !PointType5FreqHertz                  

                      
} deriving (Read,Eq, Show, Generic)                       

type PointType5PointTag                  = BS.ByteString    
type PointType5Units                     = BS.ByteString            
type PointType5RateFlag                  = Word8           
type PointType5RatePeriod                = Word8               
type PointType5FilterTime                = Word8             
type PointType5ScanPeriod                = Word16            
type PointType5ConversionFactor          = Float           
type PointType5LowAlarm                  = Float          
type PointType5HighAlarm                 = Float           
type PointType5LowLowAlarm               = Float           
type PointType5HighHighAlarm             = Float            
type PointType5RateAlarm                 = Float             
type PointType5AlarmDeadband             = Float             
type PointType5EUValue                   = Float       
type PointType5ModeCFG                   = Word8    
type PointType5AlarmCode                 = Word8             
type PointType5AccumulatedValue          = Word32            
type PointType5CurrentRates              = Float        
type PointType5TodaysTotal               = Float        
type PointType5YesterdaysTotal           = Float        
type PointType5PulsesForDay              = Word32        
type PointType5FreqHertz                 = Float           


pointType5Parser :: Get PointType5
pointType5Parser = do 
  id <- getByteString 10
  units <- getByteString 10
  rateFlat <- getWord8
  ratePeriod <- getWord8
  filterTime <- getWord8
  scanPeriod <- getWord16le
  conversionFactor <- getIeeeFloat32
  lowAlarm <- getIeeeFloat32
  highAlarm <- getIeeeFloat32
  lowLowAlarm <- getIeeeFloat32
  highHighAlarm <- getIeeeFloat32
  rateAlarm <- getIeeeFloat32
  alarmDeadband <- getIeeeFloat32
  euValue <- getIeeeFloat32
  modeCFG <- getWord8
  alarmCode <- getWord8
  accumulatedValue <- getWord32le
  currentRates <- getIeeeFloat32
  todaysTotal <- getIeeeFloat32
  yesterdaysTotal <- getIeeeFloat32
  pulsesForDay <- getWord32le
  freqHertz <- getIeeeFloat32
  
  return $ PointType5 id units rateFlat ratePeriod filterTime scanPeriod conversionFactor lowAlarm highAlarm lowLowAlarm highHighAlarm rateAlarm alarmDeadband 
                   euValue modeCFG alarmCode accumulatedValue currentRates todaysTotal yesterdaysTotal pulsesForDay freqHertz