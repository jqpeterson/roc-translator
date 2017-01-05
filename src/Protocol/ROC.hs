{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Protocol.ROC where

import System.Hardware.Serialport
import CRC16.Calculator
import Data.Serialize
import qualified Data.ByteString as BS
import Control.Monad
--import qualified Data.ByteString.Lazy as LB
--import Protocol.ROC.Utils
--import Protocol.ROC.PointTypes
--import Protocol.ROC.FullyDefinedPointType 
import Protocol.ROC.ROCConfig
import Protocol.ROC.OpCodes
--import Protocol.ROC.RocSerialize
-- import Control.Applicative
import Numeric                                            
-- import Data.Int
-- import Data.ByteString.Builder
import Data.Word

-- getPointType :: RocConfig -> DefaultPointType -> PointNumber -> IO ()
-- getPointType cfg fdpt pn = do
--   let fdataBytes = fdptRxProtocol fdpt
--       ptid = fdptPointTypeID fdpt
--       pc = fdptParameterCount fdpt
--       sp = fdptStartParameter fdpt
--   dataBytes <- fdataBytes cfg pn ptid pc sp    
--   let fetchedPointType = fetchPointType ptid (LB.fromStrict dataBytes)
--   print fetchedPointType

-- writePointType :: RocSerialize a => RocConfig -> DefaultPointType -> PointNumber -> ParameterNumber -> a -> IO ()
-- writePointType cfg fdpt pn prn pdata = do
--   let port = rocConfigPort cfg
--       commRate = rocCommSpeed cfg
--       ptid = fdptPointTypeID fdpt
--       pt = decodePTID ptid
--       databytes = BS.append (opCode166 pt pn prn pdata cfg) (lzyBSto16BScrc.pack8to16 $ BS.unpack $ opCode166 pt pn prn pdata cfg)
--   s <- openSerial port defaultSerialSettings { commSpeed = commRate } 
--   print $ showInt <$> BS.unpack databytes <*> [""]
--   _ <- send s databytes
--   receivebs <- recvAllBytes s 255
--   closeSerial s
--   print $ showInt <$> BS.unpack receivebs <*> [""]

loginToROC :: String -> Word16 ->  IO (Either String Float)
loginToROC userName passWrd = do
  responsebytes <- runOpCodeRaw (testRocConfig{rocLogin = userName, rocPassword = passWrd}) opCode17
  if checkCRC16 (IStrictBS responsebytes) standardConfig
  then case runGet parseResponse responsebytes of
         Left err -> return $ Left err
         Right responseinfo -> return $ parseResponseInfo responseinfo
  else return $ Left "CRC check failed"

getMinutesToday :: IO (Either String Float)
getMinutesToday = runGetDataPoint 47 0 41

getHWUncorrectedFlowRate :: IO (Either String Float)
getHWUncorrectedFlowRate = runGetDataPoint 46 0 51

getPFStaticPressure :: IO (Either String Float)
getPFStaticPressure = runGetDataPoint 46 0 52

getTFTemperature :: IO (Either String Float)
getTFTemperature = runGetDataPoint 46 0 53

getIMVBMV :: IO (Either String Float)
getIMVBMV = runGetDataPoint 47 0 16

getPressureExtensionUncorrectedFlowRate :: IO (Either String Float)
getPressureExtensionUncorrectedFlowRate = runGetDataPoint 47 0 4

getFlowRatePerDay :: IO (Either String Float)
getFlowRatePerDay = runGetDataPoint 47 0 0

getEnergyRatePerDay :: IO (Either String Float)
getEnergyRatePerDay = runGetDataPoint 47 0 1
                   

runGetDataPoint :: PointType -> PointNumber -> ParameterNumber -> IO (Either String Float)
runGetDataPoint pointtype pointnumber parameternumber = do
  responsebytes <- runOpCodeRaw testRocConfig (opCode167 pointtype pointnumber 1 parameternumber)
  if checkCRC16 (IStrictBS responsebytes) standardConfig
  then case runGet parseResponse responsebytes of
         Left err -> return $ Left err
         Right responseinfo -> do
           print $ showHex <$> BS.unpack (dataByteString responseinfo) <*> [""]
           return $ parseResponseInfo responseinfo
  else return $ Left "CRC check failed"
                   
parseResponse :: Get ResponseInfo
parseResponse = do
  responseLocalHostUnitId <- getWord8
  responseLocalHostGroupId <- getWord8
  responseDeviceUnitId <- getWord8
  responseDeviceGroupId <- getWord8
  responseOpcodeId <- getWord8
  bytesLength <- getWord8
  responseDataBytes <- getByteString $ fromIntegral bytesLength
  skip 2
  return $ ResponseInfo responseLocalHostUnitId
                        responseLocalHostGroupId
                        responseDeviceUnitId
                        responseDeviceGroupId
                        responseOpcodeId
                        responseDataBytes

parseResponseInfo :: ResponseInfo -> Either String Float
parseResponseInfo (ResponseInfo _ _ _ _ 167 databytes) = runGet parseOutFloat databytes
parseResponseInfo (ResponseInfo _ _ _ _ 17  databytes) = Left "Login Successful" 
parseResponseInfo (ResponseInfo _ _ _ _ 255 databytes) = runGet parseError databytes
parseResponseInfo response = Left $ "Unknown Response " ++ show response

parseError :: Get Float
parseError = do
  errorcode <- getWord8
  opcodeWithError <- getWord8
  byteWithError <- getWord8
  let errorInfo = " for Opcode " ++ show opcodeWithError ++ " at byte number " ++ show byteWithError
  case errorcode of
     1 -> fail $ "Invalid Opcode request" ++ errorInfo
     2 -> fail $ "Invalid Parameter Number" ++ errorInfo
     3 -> fail $ "Invalid Logical Number/Point Number" ++ errorInfo
     4 -> fail $ "Invalid Point Type" ++ errorInfo
     5 -> fail $ "Received too many data bytes" ++ errorInfo
     6 -> fail $ "Received too few data bytes" ++ errorInfo
     7 -> fail $ "Did not receive 1 data byte" ++ errorInfo
     8 -> fail $ "Did not receive 2 data byte" ++ errorInfo
     9 -> fail $ "Did not receive 3 data byte" ++ errorInfo
     10 -> fail $ "Did not receive 4 data byte" ++ errorInfo
     11 -> fail $ "Did not receive 5 data byte" ++ errorInfo
     12 -> fail $ "Did not receive 16 data" ++ errorInfo
     13 -> fail $ "Outside valid address range" ++ errorInfo
     14 -> fail $ "Invalid history request" ++ errorInfo
     15 -> fail $ "Invalid FST request" ++ errorInfo
     16 -> fail $ "Invalid event entry" ++ errorInfo
     17 -> fail $ "Requested too many alarms" ++ errorInfo
     18 -> fail $ "Requested too many events" ++ errorInfo
     19 -> fail $ "Write to read only parameter" ++ errorInfo
     20 -> fail $ "Security error" ++ errorInfo
     21 -> fail $ "Invalid security logon" ++ errorInfo
     22 -> fail $ "Invalid store and forward path" ++ errorInfo
     23 -> fail $ "Flash programming error" ++ errorInfo
     24 -> fail $ "History configuration in progress" ++ errorInfo
     63 -> fail $ "Requested security level too hight" ++ errorInfo
     _ -> fail $ "Unknown error" ++ errorInfo

-- class (RocGet a) => RocType a where

-- class RocGet a where
    

-- data Response a = Response {responseValue :: a} deriving (Show,Eq,Ord)
          
parseOutFloat :: Get Float
parseOutFloat = do
  pointtype <- getWord8
  pointnumber <- getWord8
  parametercount <- getWord8
  parameterStart <- getWord8
  dataFloat <- getFloat32le
  remainderFloats <- replicateM (fromIntegral parametercount - 1) getFloat32le
  return dataFloat
          
runOpCodeRaw :: RocConfig -> (RocConfig -> BS.ByteString) -> IO BS.ByteString
runOpCodeRaw cfg opCode = do
  let port = rocConfigPort cfg
      commRate = rocCommSpeed cfg            
  s <- openSerial port defaultSerialSettings { commSpeed = commRate }
  _ <- send s $ appendCRC16 (IStrictBS $ opCode cfg) standardConfig
  receivebs <- recvAllBytes s 255
  closeSerial s
  print $ showHex <$> BS.unpack receivebs <*> [""]
  return receivebs
         
data ResponseInfo = ResponseInfo { hostUnitId     :: Word8
                                 , hostGroupId    :: Word8
                                 , deviceUnitId   :: Word8
                                 , deviceGroupId  :: Word8
                                 , opcodeId       :: Word8
                                 , dataByteString :: BS.ByteString
                                 } deriving (Eq,Ord,Show)
                                   
testRocConfig :: RocConfig
testRocConfig = RocConfig "/dev/ttyUSB0" [240,240] [1,3] CS19200 "LOI" 1000


