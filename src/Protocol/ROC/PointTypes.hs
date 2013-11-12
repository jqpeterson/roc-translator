{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes (module PointTypes
                               ,decodePTID
                               ,fetchPointType
                               ,pt1
                               ,pt2
                               ,pt3
                               ,pt4
                               ,pt5
                               ,pt6
                               ,pt7
                               ,pt8
                               ,pt9
                               ,pt10
                              -- ,pt11  
                               ,pt12
                               ,pt13
                               ,pt14
                               ,pt15
                               ,pt16
                               ,pt17
                               ,pt18
                               ,pt19
                               ,pt20
--                               ,pt21
--                               ,pt22
--                               ,pt23
                               ,pt47
                               ) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Word
import Data.Binary.Get
--import Protocol.ROC.PointTypes.PointType0 as PointTypes
import Protocol.ROC.PointTypes.PointType1 as PointTypes
import Protocol.ROC.PointTypes.PointType2 as PointTYpes
import Protocol.ROC.PointTypes.PointType3 as PointTYpes
import Protocol.ROC.PointTypes.PointType4 as PointTYpes
import Protocol.ROC.PointTypes.PointType5 as PointTypes
import Protocol.ROC.PointTypes.PointType6 as PointTypes
import Protocol.ROC.PointTypes.PointType7 as PointTypes
import Protocol.ROC.PointTypes.PointType8 as PointTypes
import Protocol.ROC.PointTypes.PointType9 as PointTypes
import Protocol.ROC.PointTypes.PointType10 as PointTypes
--import Protocol.ROC.PointTypes.PointType11 as PointTYpes
import Protocol.ROC.PointTypes.PointType12 as PointTYpes
import Protocol.ROC.PointTypes.PointType13 as PointTYpes
import Protocol.ROC.PointTypes.PointType14 as PointTypes
import Protocol.ROC.PointTypes.PointType15 as PointTypes
import Protocol.ROC.PointTypes.PointType16 as PointTypes
import Protocol.ROC.PointTypes.PointType17 as PointTypes
import Protocol.ROC.PointTypes.PointType18 as PointTypes
import Protocol.ROC.PointTypes.PointType19 as PointTypes
import Protocol.ROC.PointTypes.PointType20 as PointTYpes
--import Protocol.ROC.PointTypes.PointType21 as PointTYpes
--import Protocol.ROC.PointTypes.PointType22 as PointTYpes
--import Protocol.ROC.PointTypes.PointType23 as PointTypes
--import Protocol.ROC.PointTypes.PointType24 as PointTypes
--import Protocol.ROC.PointTypes.PointType25 as PointTypes
--import Protocol.ROC.PointTypes.PointType26 as PointTypes
import Protocol.ROC.PointTypes.PointType47 as PointTypes

data PointTypes a = PTID1 (Either a PointType1)
                  | PTID2 (Either a PointType2)
                  | PTID3 (Either a PointType3)
                  | PTID4 (Either a PointType4)
                  | PTID5 (Either a PointType5)
                  | PTID6 (Either a PointType6)
                  | PTID7 (Either a PointType7)                
                  | PTID8 (Either a PointType8)
                  | PTID9 (Either a PointType9)
                  | PTID10 (Either a PointType10) 
--                  | PTID11 (Either a PointType11) 
                  | PTID12 (Either a PointType12)
                  | PTID13 (Either a PointType13)
                  | PTID14 (Either a PointType14)                                      
                  | PTID15 (Either a PointType15) 
                  | PTID16 (Either a PointType16) 
                  | PTID17 (Either a PointType17) 
                  | PTID18 (Either a PointType18)
                  | PTID19 (Either a PointType19)
                  | PTID20 (Either a PointType20)
--                  | PTID21 (Either a PointType21)
                  | PTID47 (Either a PointType47)                  
                  deriving (Read,Eq,Show)

pt1 :: PointTypes () 
pt1 = PTID1 $ Left ()                   
pt2 :: PointTypes ()
pt2 = PTID2 $ Left ()
pt3 :: PointTypes () 
pt3 = PTID3 $ Left ()
pt4 :: PointTypes () 
pt4 = PTID4 $ Left ()
pt5 :: PointTypes () 
pt5 = PTID5 $ Left ()
pt6 :: PointTypes () 
pt6 = PTID6 $ Left ()
pt7 :: PointTypes () 
pt7 = PTID7 $ Left ()
pt8 :: PointTypes () 
pt8 = PTID8 $ Left ()
pt9 :: PointTypes () 
pt9 = PTID9 $ Left ()                   
pt10 :: PointTypes () 
pt10 = PTID10 $ Left ()
--pt11 :: PointTypes () 
--pt11 = PTID11 $ Left ()
pt12 :: PointTypes () 
pt12 = PTID12 $ Left ()
pt13 :: PointTypes () 
pt13 = PTID13 $ Left ()
pt14 :: PointTypes () 
pt14 = PTID14 $ Left ()
pt15 :: PointTypes () 
pt15 = PTID15 $ Left ()
pt16 :: PointTypes () 
pt16 = PTID16 $ Left ()
pt17 :: PointTypes () 
pt17 = PTID17 $ Left ()                   
pt18 :: PointTypes () 
pt18 = PTID18 $ Left ()
pt19 :: PointTypes () 
pt19 = PTID19 $ Left ()
pt20 :: PointTypes () 
pt20 = PTID20 $ Left ()
--pt21 :: PointTypes () 
--pt21 = PTID21 $ Left ()
--pt22 :: PointTypes () 
--pt22 = PTID22 $ Left ()
--pt23 :: PointTypes () 
--pt23 = PTID23 $ Left ()
pt47 :: PointTypes () 
pt47 = PTID47 $ Left ()

decodePTID :: PointTypes a -> Word8
decodePTID (PTID1 _) = 1
decodePTID (PTID2 _) = 2
decodePTID (PTID3 _) = 3
decodePTID (PTID4 _) = 4
decodePTID (PTID5 _) = 5
decodePTID (PTID6 _) = 6
decodePTID (PTID7 _) = 7
decodePTID (PTID8 _) = 8
decodePTID (PTID9 _) = 9
decodePTID (PTID10 _) = 10
--decodePTID (PTID11 _) = 11
decodePTID (PTID12 _) = 12
decodePTID (PTID13 _) = 13
decodePTID (PTID14 _) = 14
decodePTID (PTID15 _) = 15
decodePTID (PTID16 _) = 16
decodePTID (PTID17 _) = 17
decodePTID (PTID18 _) = 18
decodePTID (PTID19 _) = 19
decodePTID (PTID20 _) = 20
--decodePTID (PTID21 _) = 21
--decodePTID (PTID22 _) = 22
--decodePTID (PTID23 _) = 23
decodePTID (PTID47 _) = 47

-----------------------------------------------------------------------------------
--data PointTypeTest = PointTypeTest { 
--pointTypeTestLowRead :: !PointTypeTestLowRead
--}

--type PointTypeTestLowRead = Float

--pointTypeTestParser :: Get PointTypeTestLowRead
--pointTypeTestParser = get

--fetchPointTypeTest :: LB.ByteString -> Decoder PointTypeTestLowRead 
--fetchPointTypeTest bs = runGetIncremental pointTypeTestParser `pushChunks` bs
------------------------------------------------------------------------------------ 
fetchPointType :: PointTypes a -> LB.ByteString -> PointTypes LB.ByteString 
fetchPointType  (PTID1 _ ) bs = PTID1 $ decodeToEither $ runGetIncremental pointType1Parser `pushChunks` bs 
fetchPointType  (PTID2 _ ) bs = PTID2 $ decodeToEither $ runGetIncremental pointType2Parser `pushChunks` bs   
fetchPointType  (PTID3 _ ) bs = PTID3 $ decodeToEither $ runGetIncremental pointType3Parser `pushChunks` bs   
fetchPointType  (PTID4 _ ) bs = PTID4 $ decodeToEither $ runGetIncremental pointType4Parser `pushChunks` bs   
fetchPointType  (PTID5 _ ) bs = PTID5 $ decodeToEither $ runGetIncremental pointType5Parser `pushChunks` bs  
fetchPointType  (PTID6 _ ) bs = PTID6 $ decodeToEither $ runGetIncremental pointType6Parser `pushChunks` bs  
fetchPointType  (PTID7 _ ) bs = PTID7 $ decodeToEither $ runGetIncremental pointType7Parser `pushChunks` bs  
fetchPointType  (PTID8 _ ) bs = PTID8 $ decodeToEither $ runGetIncremental pointType8Parser `pushChunks` bs 
fetchPointType  (PTID9 _ ) bs = PTID9 $ decodeToEither $ runGetIncremental pointType9Parser `pushChunks` bs 
fetchPointType  (PTID10 _ ) bs = PTID10 $ decodeToEither $ runGetIncremental pointType10Parser `pushChunks` bs   
--fetchPointType  (PTID11 _ ) bs = PTID11 $ decodeToEither $ runGetIncremental pointType11Parser `pushChunks` bs   
fetchPointType  (PTID12 _ ) bs = PTID12 $ decodeToEither $ runGetIncremental pointType12Parser `pushChunks` bs   
fetchPointType  (PTID13 _ ) bs = PTID13 $ decodeToEither $ runGetIncremental pointType13Parser `pushChunks` bs  
fetchPointType  (PTID14 _ ) bs = PTID14 $ decodeToEither $ runGetIncremental pointType14Parser `pushChunks` bs  
fetchPointType  (PTID15 _ ) bs = PTID15 $ decodeToEither $ runGetIncremental pointType15Parser `pushChunks` bs  
fetchPointType  (PTID16 _ ) bs = PTID16 $ decodeToEither $ runGetIncremental pointType16Parser `pushChunks` bs 
fetchPointType  (PTID17 _ ) bs = PTID17 $ decodeToEither $ runGetIncremental pointType17Parser `pushChunks` bs 
fetchPointType  (PTID18 _ ) bs = PTID18 $ decodeToEither $ runGetIncremental pointType18Parser `pushChunks` bs   
fetchPointType  (PTID19 _ ) bs = PTID19 $ decodeToEither $ runGetIncremental pointType19Parser `pushChunks` bs   
fetchPointType  (PTID20 _ ) bs = PTID20 $ decodeToEither $ runGetIncremental pointType20Parser `pushChunks` bs   
--fetchPointType  (PTID21 _ ) bs = PTID21 $ decodeToEither $ runGetIncremental pointType21Parser `pushChunks` bs  
--fetchPointType  (PTID22 _ ) bs = PTID22 $ decodeToEither $ runGetIncremental pointType22Parser `pushChunks` bs  
--fetchPointType  (PTID23 _ ) bs = PTID23 $ decodeToEither $ runGetIncremental pointType23Parser `pushChunks` bs  
fetchPointType  (PTID47 _ ) bs = PTID47 $ decodeToEither $ runGetIncremental pointType47Parser `pushChunks` bs  

decodeToEither :: (Show a) => Decoder a -> Either LB.ByteString a
decodeToEither (Fail _ _ s) = Left $ C8.append "decoder Failed with"  (C8.pack s)
decodeToEither (Done _ _ a) = Right a
decodeToEither _ = Left "incomplete parsing SHOULD NOT HAPPEN!"

--debugDecoderPointType :: (Show a) => Decoder a -> IO () 
--debugDecoderPointType (Fail _ _ s) = print $ "decoder Failed with" ++ s 
--debugDecoderPointType (Done _ _ pt) = print "Point type finished" >> print pt
--debugDecoderPointType _ = print "incomplete parsing SHOULD NOT HAPPEN!"