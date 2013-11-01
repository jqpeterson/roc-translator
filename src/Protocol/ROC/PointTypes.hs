{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes where

import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Builder as LBB
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Word
import Data.Binary
import Data.Bool
import Data.Int
import Data.Binary.Get
import Numeric
import Control.Applicative
import Protocol.ROC.Float
import Protocol.ROC.PointTypes.PointType1 as PointTypes
import Protocol.ROC.PointTypes.PointType2 as PointTypes

data PointTypes a = PTID1 (Either a PointType1) | PTID2 (Either a PointType2) 
          deriving (Read,Eq,Show)
                   
pt1 = PTID1 $ Left ()
pt2 = PTID2 $ Left ()                   

decodePTID :: PointTypes a -> Word8
decodePTID (PTID1 _) = 0x01
decodePTID (PTID2 _) = 0x02
--------------------------------------------------
--data PointTypeTest = PointTypeTest { 
--pointTypeTestLowRead :: !PointTypeTestLowRead
--}

--type PointTypeTestLowRead = Float

--pointTypeTestParser :: Get PointTypeTestLowRead
--pointTypeTestParser = get

--fetchPointTypeTest :: LB.ByteString -> Decoder PointTypeTestLowRead 
--fetchPointTypeTest bs = runGetIncremental pointTypeTestParser `pushChunks` bs
 
fetchPointType :: PointTypes a -> LB.ByteString -> PointTypes LB.ByteString 
fetchPointType  (PTID1 _ ) bs = PTID1 $ decodeToEither $ runGetIncremental pointType1Parser `pushChunks` bs 
fetchPointType  (PTID2 _ ) bs = PTID2 $ decodeToEither $ runGetIncremental pointType2Parser `pushChunks` bs 
  

decodeToEither :: (Show a) => Decoder a -> Either LB.ByteString a
decodeToEither (Fail _ _ s) = Left $ C8.append "decoder Failed with"  (C8.pack s)
decodeToEither (Done _ _ a) = Right a
decodeToEither _ = Left "incomplete parsing SHOULD NOT HAPPEN!"
 
debugDecoderPointType :: (Show a) => Decoder a -> IO () 
debugDecoderPointType (Fail _ _ s) = print $ "decoder Failed with" ++ s 
debugDecoderPointType (Done _ _ pt) = print "Point type finished" >> print pt
debugDecoderPointType _ = print "incomplete parsing SHOULD NOT HAPPEN!"