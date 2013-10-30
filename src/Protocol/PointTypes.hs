{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes where

-- newtype PointType1
data PointType1 = PointType1 
{
 pointType1PointTag             :: PointType1PointTag          
,pointType1Filter               :: PointType1Filter                   
,pointType1Status               :: PointType1Status                    
,pointType1BitfieldHigh         :: PointType1BitfieldHigh                   
,pointType1BitfieldLow          :: PointType1BitfieldLow                   
,pointType1AccumulatedValues    :: PointType1AccumulatedValues                   
,pointType1OnCounter            :: PointType1OnCounter                   
,pointType1OffCounter           :: PointType1OffCounter                   
,pointType1PulseWidth0          :: PointType1PulseWidth0                   
,pointType1PulseWidth100        :: PointType1PulseWidth100                   
,pointType1MaxTimePulse         :: PointType1MaxTimePulse                   
,pointType1Units                :: PointType1Units                   
,pointType1ScanPeriod           :: PointType1ScanPeriod                   
,pointType1LowReading           :: PointType1LowReading                   
,pointType1HighReading          :: PointType1HighReading                   
,pointType1LowAlarm             :: PointType1LowAlarm                   
,pointType1HighAlarm            :: PointType1HighAlarm                   
,pointType1LowLowAlarm          :: PointType1LowLowAlarm                   
,pointType1HiHiAlarm            :: PointType1HiHiAlarm                   
,pointType1RateAlarm            :: PointType1RateAlarm                   
,pointType1AlarmDeadband        :: PointType1AlarmDeadband                   
,pointType1EUValue              :: PointType1EUValue                   
,pointType1TDICount             :: PointType1TDICount                    
} deriving (Read,Eq, Show, Generic)                       
                       
newtype PointType1PointTag           = PointType1PointTag         
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1Filter             = PointType1Filter             
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1Status             = PointType1Status             
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1BitfieldHigh       = PointType1BitfieldHigh       
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1BitfieldLow        = PointType1BitfieldLow        
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1AccumulatedValues  = PointType1AccumulatedValues  
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1OnCounter          = PointType1OnCounter          
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1OffCounter         = PointType1OffCounter         
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1PulseWidth0        = PointType1PulseWidth0        
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1PulseWidth100      = PointType1PulseWidth100      
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1MaxTimePulse       = PointType1MaxTimePulse       
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1Units              = PointType1Units              
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1ScanPeriod         = PointType1ScanPeriod         
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1LowReading         = PointType1LowReading         
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1HighReading        = PointType1HighReading        
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1LowAlarm           = PointType1LowAlarm           
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1HighAlarm          = PointType1HighAlarm          
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1LowLowAlarm        = PointType1LowLowAlarm        
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1HiHiAlarm          = PointType1HiHiAlarm          
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1RateAlarm          = PointType1RateAlarm          
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1AlarmDeadband      = PointType1AlarmDeadband      
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1EUValue            = PointType1EUValue            
  deriving (Read,Eq, Show, Generic)                                 
newtype PointType1TDICount           = PointType1TDICount           
  deriving (Read,Eq, Show, Generic)                       
                       
                       
                       
                       
                       
                       
                       
                       













































                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       