{-# LANGUAGE RankNTypes, OverloadedStrings, BangPatterns #-}
module Protocol.ROC.ROCConfig where

import System.Hardware.Serialport
import Data.Word
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as LB
import Protocol.ROC.PointTypes

type BlockNumber  = Word8                                                 
type RocAddress   = [Word8]

data RocConfig = RocConfig {  rocConfigPort         :: FilePath
                             ,rocConfigRocAddress   :: RocAddress
                             ,rocConfigHostAddress  :: RocAddress                              
                             ,rocCommSpeed          :: CommSpeed 
                             ,rocLogin              :: String 
                             ,rocPassword           :: Word16
                             }
                 

data FullyDefinedPointType cfg pn rt pt pc sp rp = FDPT { fdptROCType                   :: rt
                                                         ,fdptPointTypeID               :: pt
                                                         ,fdptParameterCount            :: pc                                                                               
                                                         ,fdptStartParameter            :: sp                                                 
                                                         ,fdptRxProtocol                :: (cfg -> pn -> pt -> pc -> sp -> rp)
                                                        } 
                                       

type ROCType                 = Word8               
type PointTypeID a           = PointTypes a
type PointNumber             = Word8 -- Maybe Int         
type ParameterCount          = Word          
type StartParameter          = Word8                

type DefaultPointType = FullyDefinedPointType RocConfig Word8 ROCType (PointTypes ()) ParameterCount StartParameter (IO BS.ByteString)
                      