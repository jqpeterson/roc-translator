{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType177 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Utils
import Protocol.ROC.Float

data PointType177 = PointType177 {
  
 pointType177DeviceTag                             :: !PointType177DeviceTag
,pointType177DeviceMessage                         :: !PointType177DeviceMessage
,pointType177DeviceDescriptor                      :: !PointType177DeviceDescriptor
,pointType177TransducerSerialNum                   :: !PointType177TransducerSerialNum
,pointType177DeviceID                              :: !PointType177DeviceID
,pointType177ManufacturerID                        :: !PointType177ManufacturerID
,pointType177DeviceType                            :: !PointType177DeviceType
,pointType177AdapterID                             :: !PointType177AdapterID
,pointType177AdapterType                           :: !PointType177AdapterType
,pointType177DecommissionFlag                      :: !PointType177DecommissionFlag
,pointType177DaysOfBatteryLife                     :: !PointType177DaysOfBatteryLife
,pointType177ResponseStatus                        :: !PointType177ResponseStatus
,pointType177PollingMode                           :: !PointType177PollingMode
,pointType177WirelessTransBurstRate                :: !PointType177WirelessTransBurstRate
,pointType177CommStatus                            :: !PointType177CommStatus
,pointType177DeviceLoopCurrent                     :: !PointType177DeviceLoopCurrent
,pointType177PrimVarValue                          :: !PointType177PrimVarValue
,pointType177PrimVarHARTUnitsCode                  :: !PointType177PrimVarHARTUnitsCode
,pointType177SecVarValue                           :: !PointType177SecVarValue
,pointType177SecVarHartUnitsCode                   :: !PointType177SecVarHartUnitsCode
,pointType177TertiaryVarValue                      :: !PointType177TertiaryVarValue
,pointType177TertiaryVarHartUnitsCode              :: !PointType177TertiaryVarHartUnitsCode
,pointType177QuatemaryVarValue                     :: !PointType177QuatemaryVarValue
,pointType177QuatemaryVArHartUnitsCode             :: !PointType177QuatemaryVArHartUnitsCode
,pointType177Reserved                              :: !PointType177Reserved
,pointType177Slot0SlotVarNum                       :: !PointType177Slot0SlotVarNum
,pointType177Slot0HARTUnitsCode                    :: !PointType177Slot0HARTUnitsCode
,pointType177Slot0Value                            :: !PointType177Slot0Value
,pointType177Slot1SlotVarNum                       :: !PointType177Slot1SlotVarNum
,pointType177Slot1HARTUnitsCode                    :: !PointType177Slot1HARTUnitsCode
,pointType177Slot1Value                            :: !PointType177Slot1Value
,pointType177Slot2SlotVarNum                       :: !PointType177Slot2SlotVarNum
,pointType177Slot2HARTUnitsCode                    :: !PointType177Slot2HARTUnitsCode
,pointType177Slot2Value                            :: !PointType177Slot2Value
,pointType177Slot3SlotVarNu                        :: !PointType177Slot3SlotVarNu
,pointType177Slot3HARTUnitsCode                    :: !PointType177Slot3HARTUnitsCode
,pointType177Slot3Value                            :: !PointType177Slot3Value
-- ,pointType177NumOfDiscreteChannels                 :: !PointType177NumOfDiscreteChannels
-- ,pointType177DisChan1StpntClassification           :: !PointType177DisChan1StpntClassification
-- ,pointType177DisChan1LiveValueClassification       :: !PointType177DisChan1LiveValueClassification
-- ,pointType177DisChan1Stpnt                         :: !PointType177DisChan1Stpnt
-- ,pointType177DisChan1LiveValue                     :: !PointType177DisChan1LiveValue
-- ,pointType177DisChan2StpntClassification           :: !PointType177DisChan2StpntClassification
-- ,pointType177DisChan2LiveValueClassification       :: !PointType177DisChan2LiveValueClassification
-- ,pointType177DisChan2Stpnt                         :: !PointType177DisChan2Stpnt
-- ,pointType177DisChan2LiveValue                     :: !PointType177DisChan2LiveValue
-- ,pointType177DisChan3StpntClassification           :: !PointType177DisChan3StpntClassification
-- ,pointType177DisChan3LiveValueClassification       :: !PointType177DisChan3LiveValueClassification
-- ,pointType177DisChan3Stpnt                         :: !PointType177DisChan3Stpnt
-- ,pointType177DisChan3LiveValue                     :: !PointType177DisChan3LiveValue
-- ,pointType177DisChan4StpntClassification           :: !PointType177DisChan4StpntClassification
-- ,pointType177DisChan4LiveValueClassification       :: !PointType177DisChan4LiveValueClassification
-- ,pointType177DisChan4Stpnt                         :: !PointType177DisChan4Stpnt
-- ,pointType177DisChan4LiveValue                     :: !PointType177DisChan4LiveValue
-- ,pointType177DeviceFailSafeMode                    :: !PointType177DeviceFailSafeMode
-- ,pointType177PVFaultValue                          :: !PointType177PVFaultValue
-- ,pointType177SVFaultValue                          :: !PointType177SVFaultValue
-- ,pointType177TVFaultValue                          :: !PointType177TVFaultValue
-- ,pointType177QVFaultValue                          :: !PointType177QVFaultValue
-- ,pointType177ProcessVarNaNFlags                    :: !PointType177ProcessVarNaNFlags

  
} deriving (Read,Eq, Show, Generic)                       
                                  
type PointType177DeviceTag                         = BS.ByteString
type PointType177DeviceMessage                     = BS.ByteString
type PointType177DeviceDescriptor                  = BS.ByteString
type PointType177TransducerSerialNum               = Word32
type PointType177DeviceID                          = Word32
type PointType177ManufacturerID                    = Word16
type PointType177DeviceType                        = Word16
type PointType177AdapterID                         = Word32
type PointType177AdapterType                       = Word16
type PointType177DecommissionFlag                  = Bool
type PointType177DaysOfBatteryLife                 = Word16
type PointType177ResponseStatus                    = Word8
type PointType177PollingMode                       = Bool
type PointType177WirelessTransBurstRate            = Word16
type PointType177CommStatus                        = Bool
type PointType177DeviceLoopCurrent                 = Float
type PointType177PrimVarValue                      = Float
type PointType177PrimVarHARTUnitsCode              = Word8
type PointType177SecVarValue                       = Float
type PointType177SecVarHartUnitsCode               = Word8
type PointType177TertiaryVarValue                  = Float
type PointType177TertiaryVarHartUnitsCode          = Word8
type PointType177QuatemaryVarValue                 = Float
type PointType177QuatemaryVArHartUnitsCode         = Word8
type PointType177Reserved                          = Word8
type PointType177Slot0SlotVarNum                   = Word8
type PointType177Slot0HARTUnitsCode                = Word8
type PointType177Slot0Value                        = Float
type PointType177Slot1SlotVarNum                   = Word8
type PointType177Slot1HARTUnitsCode                = Word8
type PointType177Slot1Value                        = Float
type PointType177Slot2SlotVarNum                   = Word8
type PointType177Slot2HARTUnitsCode                = Word8
type PointType177Slot2Value                        = Float
type PointType177Slot3SlotVarNu                    = Word8
type PointType177Slot3HARTUnitsCode                = Word8
type PointType177Slot3Value                        = Float
-- type PointType177NumOfDiscreteChannels             = Word8
-- type PointType177DisChan1StpntClassification       = Word16
-- type PointType177DisChan1LiveValueClassification   = Word16
-- type PointType177DisChan1Stpnt                     = Word16
-- type PointType177DisChan1LiveValue                 = Word16
-- type PointType177DisChan2StpntClassification       = Word16
-- type PointType177DisChan2LiveValueClassification   = Word16
-- type PointType177DisChan2Stpnt                     = Word16
-- type PointType177DisChan2LiveValue                 = Word16
-- type PointType177DisChan3StpntClassification       = Word16
-- type PointType177DisChan3LiveValueClassification   = Word16
-- type PointType177DisChan3Stpnt                     = Word16
-- type PointType177DisChan3LiveValue                 = Word16
-- type PointType177DisChan4StpntClassification       = Word16
-- type PointType177DisChan4LiveValueClassification   = Word16
-- type PointType177DisChan4Stpnt                     = Word16
-- type PointType177DisChan4LiveValue                 = Word16
-- type PointType177DeviceFailSafeMode                = Bool
-- type PointType177PVFaultValue                      = Float
-- type PointType177SVFaultValue                      = Float
-- type PointType177TVFaultValue                      = Float
-- type PointType177QVFaultValue                      = Float
-- type PointType177ProcessVarNaNFlags                = Word8
  
pointType177Parser :: Get PointType177
pointType177Parser = do 
 
  deviceTag <- getByteString 40
  deviceMessage <- getByteString 40
  deviceDescriptor <- getByteString 20
  transducerSerialNum <- getWord32le
  deviceID <- getWord32le
  manufacturerID <- getWord16le
  deviceType <- getWord16le
  adapterID <- getWord32le
  adapterType <- getWord16le
  decommissionFlag <- anyButNull
  daysOfBatteryLife <- getWord16le
  responseStatus <- getWord8
  pollingMode <- anyButNull
  wirelessTransBurstRate <- getWord16le
  commStatus <- anyButNull
  deviceLoopCurrent <- getIeeeFloat32
  primVarValue <- getIeeeFloat32
  primVarHARTUnitsCode <- getWord8
  secVarValue <- getIeeeFloat32
  secVarHartUnitsCode <- getWord8
  tertiaryVarValue <- getIeeeFloat32
  tertiaryVarHartUnitsCode <- getWord8
  quatemaryVarValue <- getIeeeFloat32
  quatemaryVArHartUnitsCode <- getWord8
  reserved <- getWord8
  slot0SlotVarNum <- getWord8
  slot0HARTUnitsCode <- getWord8
  slot0Value <- getIeeeFloat32
  slot1SlotVarNum <- getWord8
  slot1HARTUnitsCode <- getWord8
  slot1Value <- getIeeeFloat32
  slot2SlotVarNum <- getWord8
  slot2HARTUnitsCode <- getWord8
  slot2Value <- getIeeeFloat32
  slot3SlotVarNu <- getWord8
  slot3HARTUnitsCode <- getWord8
  slot3Value <- getIeeeFloat32
  -- numOfDiscreteChannels <- getWord8
  -- disChan1StpntClassification <- getWord16le
  -- disChan1LiveValueClassification <- getWord16le
  -- disChan1Stpnt <- getWord16le
  -- disChan1LiveValue <- getWord16le
  -- disChan2StpntClassification <- getWord16le
  -- disChan2LiveValueClassification <- getWord16le
  -- disChan2Stpnt <- getWord16le
  -- disChan2LiveValue <- getWord16le
  -- disChan3StpntClassification <- getWord16le
  -- disChan3LiveValueClassification <- getWord16le
  -- disChan3Stpnt <- getWord16le
  -- disChan3LiveValue <- getWord16le
  -- disChan4StpntClassification <- getWord16le
  -- disChan4LiveValueClassification <- getWord16le
  -- disChan4Stpnt <- getWord16le
  -- disChan4LiveValue <- getWord16le
  -- deviceFailSafeMode <- anyButNull
  -- pVFaultValue <- getIeeeFloat32
  -- sVFaultValue <- getIeeeFloat32
  -- tVFaultValue <- getIeeeFloat32
  -- qVFaultValue <- getIeeeFloat32
  -- processVarNaNFlags <- getWord8
  
  

  return $ PointType177 deviceTag deviceMessage deviceDescriptor transducerSerialNum deviceID manufacturerID deviceType adapterID adapterType decommissionFlag daysOfBatteryLife 
    responseStatus pollingMode wirelessTransBurstRate commStatus deviceLoopCurrent primVarValue primVarHARTUnitsCode secVarValue secVarHartUnitsCode tertiaryVarValue 
    tertiaryVarHartUnitsCode quatemaryVarValue quatemaryVArHartUnitsCode reserved slot0SlotVarNum slot0HARTUnitsCode slot0Value slot1SlotVarNum slot1HARTUnitsCode slot1Value 
    slot2SlotVarNum slot2HARTUnitsCode slot2Value slot3SlotVarNu slot3HARTUnitsCode slot3Value -- numOfDiscreteChannels disChan1StpntClassification disChan1LiveValueClassification 
    -- disChan1Stpnt disChan1LiveValue disChan2StpntClassification disChan2LiveValueClassification disChan2Stpnt disChan2LiveValue disChan3StpntClassification 
    -- disChan3LiveValueClassification disChan3Stpnt disChan3LiveValue disChan4StpntClassification disChan4LiveValueClassification disChan4Stpnt disChan4LiveValue deviceFailSafeMode 
    -- pVFaultValue sVFaultValue tVFaultValue qVFaultValue processVarNaNFlags