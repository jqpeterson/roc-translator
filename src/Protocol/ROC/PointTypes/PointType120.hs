{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType120 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get

data PointType120 = PointType120 {
 
 pointType120TagID                       :: !PointType120TagID                       
,pointType120FirstAddress                :: !PointType120FirstAddress                       
,pointType120FirstConnectCommand         :: !PointType120FirstConnectCommand                       
,pointType120SecondAddress               :: !PointType120SecondAddress                       
,pointType120SecondConnectCommand        :: !PointType120SecondConnectCommand                       
,pointType120ThirdAddress                :: !PointType120ThirdAddress                       
,pointType120ThirdConnectCommand         :: !PointType120ThirdConnectCommand                       
,pointType120FourthAddress               :: !PointType120FourthAddress                       
,pointType120FourthConnectCommand        :: !PointType120FourthConnectCommand                       
,pointType120FifthAddress                :: !PointType120FifthAddress                       
,pointType120FifthConnectCommand         :: !PointType120FifthConnectCommand                       
,pointType120SixthAddress                :: !PointType120SixthAddress                       
,pointType120SixthConnectCommand         :: !PointType120SixthConnectCommand                       

} deriving (Eq, Show, Generic)                       

type PointType120TagID                   = BS.ByteString                       
type PointType120FirstAddress            = Word8                       
type PointType120FirstConnectCommand     = BS.ByteString                       
type PointType120SecondAddress           = Word8                       
type PointType120SecondConnectCommand    = BS.ByteString                       
type PointType120ThirdAddress            = Word8                       
type PointType120ThirdConnectCommand     = BS.ByteString                       
type PointType120FourthAddress           = Word8                       
type PointType120FourthConnectCommand    = BS.ByteString                       
type PointType120FifthAddress            = Word8                       
type PointType120FifthConnectCommand     = BS.ByteString                       
type PointType120SixthAddress            = Word8                       
type PointType120SixthConnectCommand     = BS.ByteString                         
  
pointType120Parser :: Get PointType120
pointType120Parser = do 

  tagID <- getByteString 10  
  firstAddress <- getWord8
  firstConnectCommand <- getByteString 30
  secondAddress <- getWord8
  secondConnectCommand <- getByteString 30
  thirdAddress <- getWord8
  thirdConnectCommand <- getByteString 30
  fourthAddress <- getWord8
  fourthConnectCommand <- getByteString 30
  fifthAddress <- getWord8
  fifthConnectCommand <- getByteString 30
  sixthAddress <- getWord8
  sixthConnectCommand <- getByteString 30
  
  return $ PointType120 tagID firstAddress firstConnectCommand secondAddress secondConnectCommand thirdAddress thirdConnectCommand fourthAddress fourthConnectCommand fifthAddress 
    fifthConnectCommand sixthAddress sixthConnectCommand  
  