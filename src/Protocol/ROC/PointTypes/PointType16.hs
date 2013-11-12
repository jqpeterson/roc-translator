{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType16 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float

data PointType16 = PointType16 {
 pointType16PointTag                :: !PointType16PointTag                                                   
,pointType16ResultRegister          :: !PointType16ResultRegister                                                   
,pointType16Register1               :: !PointType16Register1                                                   
,pointType16Register2               :: !PointType16Register2                                                   
,pointType16Register3               :: !PointType16Register3                                                   
,pointType16Register4               :: !PointType16Register4                                                   
,pointType16Register5               :: !PointType16Register5                                                   
,pointType16Register6               :: !PointType16Register6                                                   
,pointType16Register7               :: !PointType16Register7                                                   
,pointType16Register8               :: !PointType16Register8                                                   
,pointType16Register9               :: !PointType16Register9                                                   
,pointType16Register10              :: !PointType16Register10                                                   
,pointType16Timer1                  :: !PointType16Timer1                                                   
,pointType16Timer2                  :: !PointType16Timer2                                                   
,pointType16Timer3                  :: !PointType16Timer3                                                   
,pointType16Timer4                  :: !PointType16Timer4                                                   
,pointType16Message1                :: !PointType16Message1                                                   
,pointType16Message2                :: !PointType16Message2                                                   
,pointType16MessageData             :: !PointType16MessageData                                                   
,pointType16Misc1                   :: !PointType16Misc1                                                   
,pointType16Misc2                   :: !PointType16Misc2                                                   
,pointType16Misc3                   :: !PointType16Misc3                                                   
,pointType16Misc4                   :: !PointType16Misc4                                                   
,pointType16CompareFlagSVD          :: !PointType16CompareFlagSVD                                                   
,pointType16RunFlag                 :: !PointType16RunFlag                                                   
,pointType16CodeSize                :: !PointType16CodeSize                                                   
,pointType16InstrctnPntr            :: !PointType16InstrctnPntr                                                   
,pointType16ExecDelay               :: !PointType16ExecDelay                                                   

} deriving (Read,Eq, Show, Generic)                       

type PointType16PointTag            = BS.ByteString                                      
type PointType16ResultRegister      = Float                                      
type PointType16Register1           = Float                                      
type PointType16Register2           = Float                                      
type PointType16Register3           = Float                                      
type PointType16Register4           = Float                                      
type PointType16Register5           = Float                                      
type PointType16Register6           = Float                                      
type PointType16Register7           = Float                                      
type PointType16Register8           = Float                                      
type PointType16Register9           = Float                                      
type PointType16Register10          = Float                                      
type PointType16Timer1              = Word32                                      
type PointType16Timer2              = Word32                                      
type PointType16Timer3              = Word32                                     
type PointType16Timer4              = Word32                                      
type PointType16Message1            = BS.ByteString                                      
type PointType16Message2            = BS.ByteString                                      
type PointType16MessageData         = BS.ByteString                                      
type PointType16Misc1               = Word8                                      
type PointType16Misc2               = Word8                                      
type PointType16Misc3               = Word8                                      
type PointType16Misc4               = Word8                                      
type PointType16CompareFlagSVD      = Word8                                      
type PointType16RunFlag             = Word8                                      
type PointType16CodeSize            = Word8                                      
type PointType16InstrctnPntr        = Word16                                      
type PointType16ExecDelay           = Word32                                       
  
pointType16Parser :: Get PointType16
pointType16Parser = do 
  
  pointTag <- getByteString 10
  resultRegister <- getIeeeFloat32
  register1 <- getIeeeFloat32
  register2 <- getIeeeFloat32
  register3 <- getIeeeFloat32
  register4 <- getIeeeFloat32
  register5 <- getIeeeFloat32
  register6 <- getIeeeFloat32
  register7 <- getIeeeFloat32
  register8 <- getIeeeFloat32
  register9 <- getIeeeFloat32
  register10 <- getIeeeFloat32
  timer1 <- getWord32le
  timer2 <- getWord32le
  timer3 <- getWord32le
  timer4 <- getWord32le
  message1 <- getByteString 30
  message2 <- getByteString 30
  messageData <- getByteString 9
  misc1 <- getWord8
  misc2 <- getWord8
  misc3 <- getWord8
  misc4 <- getWord8
  compareFlagSVD <- getWord8
  runFlag <- getWord8
  codeSize <- getWord8
  instrctnPntr <- getWord16le
  execDelay <- getWord32le
  
  
  return $ PointType16 pointTag resultRegister register1 register2 register3 register4 register5 register6 register7 register8 register9 register10 timer1 timer2 timer3 timer4 
    message1 message2 messageData misc1 misc2 misc3 misc4 compareFlagSVD runFlag codeSize instrctnPntr execDelay  
  
  
  
  
  
  
  
  
  
  
  
  
  