{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType117 where

import GHC.Generics
import Data.Int
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Utils
import Protocol.ROC.Float


data PointType117 = PointType117 {
  
 pointType117TransmissionMode                :: !PointType117TransmissionMode                       
,pointType117ByteOrder                       :: !PointType117ByteOrder                       
,pointType117EventLogEnable                  :: !PointType117EventLogEnable                       
,pointType117SlaveExceptionStatus            :: !PointType117SlaveExceptionStatus                       
,pointType117MasterPollRequestTrigger        :: !PointType117MasterPollRequestTrigger                       
,pointType117MasterStartingRequetNum         :: !PointType117MasterStartingRequetNum                       
,pointType117MasterNumRequests               :: !PointType117MasterNumRequests                       
,pointType117MasterContPollingEnable         :: !PointType117MasterContPollingEnable                       
,pointType117MasterPollRequestDelay          :: !PointType117MasterPollRequestDelay                       
,pointType117ModbusMode                      :: !PointType117ModbusMode                       
,pointType117LowScalingInteger               :: !PointType117LowScalingInteger                       
,pointType117HighScalingInteger              :: !PointType117HighScalingInteger                       
,pointType117LowFloatScale1                  :: !PointType117LowFloatScale1                       
,pointType117HighFloatScale1                 :: !PointType117HighFloatScale1                       
,pointType117LowFloatScale2                  :: !PointType117LowFloatScale2                       
,pointType117HighFloatScale2                 :: !PointType117HighFloatScale2                       
,pointType117LowFloatScale3                  :: !PointType117LowFloatScale3                       
,pointType117HighFloatScale3                 :: !PointType117HighFloatScale3                       
,pointType117LowFloatScale4                  :: !PointType117LowFloatScale4                       
,pointType117HighFloatScale4                 :: !PointType117HighFloatScale4                       
,pointType117LowFloatScale5                  :: !PointType117LowFloatScale5                       
,pointType117HighFloatScale5                 :: !PointType117HighFloatScale5                       
,pointType117LowFloatScale6                  :: !PointType117LowFloatScale6                       
,pointType117HighFloatScale6                 :: !PointType117HighFloatScale6                       
,pointType117LowFloatScale7                  :: !PointType117LowFloatScale7                       
,pointType117HighFloatScale7                 :: !PointType117HighFloatScale7                       
,pointType117LowFloatScale8                  :: !PointType117LowFloatScale8                       
,pointType117HighFloatScale8                 :: !PointType117HighFloatScale8                       
,pointType117MasterPollTimeout               :: !PointType117MasterPollTimeout                       
,pointType117MasterPollNumRetries            :: !PointType117MasterPollNumRetries                       
                             
  
} deriving (Read,Eq, Show, Generic)                       
                                  
type PointType117TransmissionMode            = Bool                             
type PointType117ByteOrder                   = Bool                             
type PointType117EventLogEnable              = Bool                             
type PointType117SlaveExceptionStatus        = Word8                             
type PointType117MasterPollRequestTrigger    = Bool                             
type PointType117MasterStartingRequetNum     = Word16                             
type PointType117MasterNumRequests           = Word16                             
type PointType117MasterContPollingEnable     = Bool                             
type PointType117MasterPollRequestDelay      = Float                             
type PointType117ModbusMode                  = Bool                             
type PointType117LowScalingInteger           = Int16                             
type PointType117HighScalingInteger          = Int16                             
type PointType117LowFloatScale1              = Float                             
type PointType117HighFloatScale1             = Float                             
type PointType117LowFloatScale2              = Float                             
type PointType117HighFloatScale2             = Float                             
type PointType117LowFloatScale3              = Float                             
type PointType117HighFloatScale3             = Float                             
type PointType117LowFloatScale4              = Float                             
type PointType117HighFloatScale4             = Float                             
type PointType117LowFloatScale5              = Float                             
type PointType117HighFloatScale5             = Float                             
type PointType117LowFloatScale6              = Float                             
type PointType117HighFloatScale6             = Float                             
type PointType117LowFloatScale7              = Float                             
type PointType117HighFloatScale7             = Float                             
type PointType117LowFloatScale8              = Float                             
type PointType117HighFloatScale8             = Float                             
type PointType117MasterPollTimeout           = Word8                             
type PointType117MasterPollNumRetries        = Word8                             

  
pointType117Parser :: Get PointType117
pointType117Parser = do 
 
  transmissionMode <- anyButNull
  byteOrder <- anyButNull
  eventLogEnable <- anyButNull
  slaveExceptionStatus <- getWord8
  masterPollRequestTrigger <- anyButNull
  masterStartingRequetNum <- getWord16le
  masterNumRequests <- getWord16le
  masterContPollingEnable <- anyButNull
  masterPollRequestDelay <- getIeeeFloat32
  modbusMode <- anyButNull
  lowScalingInteger <- getInt16
  highScalingInteger <- getInt16
  lowFloatScale1 <- getIeeeFloat32
  highFloatScale1 <- getIeeeFloat32
  lowFloatScale2 <- getIeeeFloat32
  highFloatScale2 <- getIeeeFloat32
  lowFloatScale3 <- getIeeeFloat32
  highFloatScale3 <- getIeeeFloat32
  lowFloatScale4 <- getIeeeFloat32
  highFloatScale4 <- getIeeeFloat32
  lowFloatScale5 <- getIeeeFloat32
  highFloatScale5 <- getIeeeFloat32
  lowFloatScale6 <- getIeeeFloat32
  highFloatScale6 <- getIeeeFloat32
  lowFloatScale7 <- getIeeeFloat32
  highFloatScale7 <- getIeeeFloat32
  lowFloatScale8 <- getIeeeFloat32
  highFloatScale8 <- getIeeeFloat32
  masterPollTimeout <- getWord8
  masterPollNumRetries <- getWord8
     

  return $ PointType117 transmissionMode byteOrder eventLogEnable slaveExceptionStatus masterPollRequestTrigger masterStartingRequetNum masterNumRequests masterContPollingEnable 
    masterPollRequestDelay modbusMode lowScalingInteger highScalingInteger lowFloatScale1 highFloatScale1 lowFloatScale2 highFloatScale2 lowFloatScale3 highFloatScale3 lowFloatScale4 
    highFloatScale4 lowFloatScale5 highFloatScale5 lowFloatScale6 highFloatScale6 lowFloatScale7 highFloatScale7 lowFloatScale8 highFloatScale8 masterPollTimeout masterPollNumRetries  
  