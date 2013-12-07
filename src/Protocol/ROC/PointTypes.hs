{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes (module PointTypes
                               ,decodePTID
                               ,fetchPointType
                               ,pt0
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
                               ,pt12
                               ,pt13
                               ,pt14
                               ,pt15
                               ,pt16
                               ,pt17
                               ,pt18
                               ,pt19
                               ,pt20
                               ,pt21
                               ,pt40
                               ,pt41
                               ,pt42
                               ,pt43
                               ,pt44
                               ,pt45
                               ,pt46
                               ,pt47
                               ,pt48
                               ,pt52 
                               ,pt53 
                               ,pt54 
                               ,pt55 
                               ,pt56 
                               ,pt57 
                               ,pt58 
                               ,pt59 
                               ,pt80
                               ,pt81
                               ,pt85 
                               ,pt86 
                               ,pt88 
                               ,pt89                                
                               ,pt93                                                               
                               ,pt94                                                                
                               ,pt98                                                                
                               ,pt117 
                               ,pt118 
                               ,pt120
                               ,pt121 
                               ,pt122
                               ,pt172
                               ,pt173 
                               ,pt174 
                               ,pt175 
                               ,pt176 
                               ,pt177
                               ,PointTypes (..)
                               ) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Word
import Data.Binary.Get
import Protocol.ROC.PointTypes.PointType0 as PointTypes
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
import Protocol.ROC.PointTypes.PointType12 as PointTYpes
import Protocol.ROC.PointTypes.PointType13 as PointTYpes
import Protocol.ROC.PointTypes.PointType14 as PointTypes
import Protocol.ROC.PointTypes.PointType15 as PointTypes
import Protocol.ROC.PointTypes.PointType16 as PointTypes
import Protocol.ROC.PointTypes.PointType17 as PointTypes
import Protocol.ROC.PointTypes.PointType18 as PointTypes
import Protocol.ROC.PointTypes.PointType19 as PointTypes
import Protocol.ROC.PointTypes.PointType20 as PointTYpes
import Protocol.ROC.PointTypes.PointType21 as PointTYpes
import Protocol.ROC.PointTypes.PointType40 as PointTYpes
import Protocol.ROC.PointTypes.PointType41 as PointTypes
import Protocol.ROC.PointTypes.PointType42 as PointTypes
import Protocol.ROC.PointTypes.PointType43 as PointTypes
import Protocol.ROC.PointTypes.PointType44 as PointTYpes
import Protocol.ROC.PointTypes.PointType45 as PointTypes
import Protocol.ROC.PointTypes.PointType46 as PointTypes
import Protocol.ROC.PointTypes.PointType47 as PointTypes
import Protocol.ROC.PointTypes.PointType48 as PointTYpes
import Protocol.ROC.PointTypes.PointType52 as PointTypes
import Protocol.ROC.PointTypes.PointType53 as PointTypes
import Protocol.ROC.PointTypes.PointType54 as PointTypes
import Protocol.ROC.PointTypes.PointType55 as PointTypes
import Protocol.ROC.PointTypes.PointType56 as PointTypes
import Protocol.ROC.PointTypes.PointType57 as PointTypes
import Protocol.ROC.PointTypes.PointType58 as PointTypes
import Protocol.ROC.PointTypes.PointType59 as PointTypes
import Protocol.ROC.PointTypes.PointType80 as PointTypes
import Protocol.ROC.PointTypes.PointType81 as PointTYpes
import Protocol.ROC.PointTypes.PointType85 as PointTypes
import Protocol.ROC.PointTypes.PointType86 as PointTypes
import Protocol.ROC.PointTypes.PointType88 as PointTypes
import Protocol.ROC.PointTypes.PointType89 as PointTypes
import Protocol.ROC.PointTypes.PointType93 as PointTypes
import Protocol.ROC.PointTypes.PointType94 as PointTypes
import Protocol.ROC.PointTypes.PointType98 as PointTypes
import Protocol.ROC.PointTypes.PointType117 as PointTypes
import Protocol.ROC.PointTypes.PointType118 as PointTypes
import Protocol.ROC.PointTypes.PointType120 as PointTypes
import Protocol.ROC.PointTypes.PointType121 as PointTypes
import Protocol.ROC.PointTypes.PointType122 as PointTypes
import Protocol.ROC.PointTypes.PointType172 as PointTypes
import Protocol.ROC.PointTypes.PointType173 as PointTypes
import Protocol.ROC.PointTypes.PointType174 as PointTypes
import Protocol.ROC.PointTypes.PointType175 as PointTypes
import Protocol.ROC.PointTypes.PointType176 as PointTypes
import Protocol.ROC.PointTypes.PointType177 as PointTypes


data PointTypes a = PTID0 (Either a PointType0)
                  | PTID1 (Either a PointType1)
                  | PTID2 (Either a PointType2)
                  | PTID3 (Either a PointType3)
                  | PTID4 (Either a PointType4)
                  | PTID5 (Either a PointType5)
                  | PTID6 (Either a PointType6)
                  | PTID7 (Either a PointType7)
                  | PTID8 (Either a PointType8)
                  | PTID9 (Either a PointType9)
                  | PTID10 (Either a PointType10)
                  | PTID12 (Either a PointType12)
                  | PTID13 (Either a PointType13)
                  | PTID14 (Either a PointType14)
                  | PTID15 (Either a PointType15)
                  | PTID16 (Either a PointType16)
                  | PTID17 (Either a PointType17)
                  | PTID18 (Either a PointType18)
                  | PTID19 (Either a PointType19)
                  | PTID20 (Either a PointType20)
                  | PTID21 (Either a PointType21)
                  | PTID40 (Either a PointType40)
                  | PTID41 (Either a PointType41)
                  | PTID42 (Either a PointType42)
                  | PTID43 (Either a PointType43)
                  | PTID44 (Either a PointType44)
                  | PTID45 (Either a PointType45)
                  | PTID46 (Either a PointType46)
                  | PTID47 (Either a PointType47)
                  | PTID48 (Either a PointType48)
                  | PTID52 (Either a PointType52)
                  | PTID53 (Either a PointType53)
                  | PTID54 (Either a PointType54)
                  | PTID55 (Either a PointType55)
                  | PTID56 (Either a PointType56)
                  | PTID57 (Either a PointType57)
                  | PTID58 (Either a PointType58)
                  | PTID59 (Either a PointType59)
                  | PTID80 (Either a PointType80)
                  | PTID81 (Either a PointType81)
                  | PTID85 (Either a PointType85)
                  | PTID86 (Either a PointType86)
                  | PTID88 (Either a PointType88)
                  | PTID89 (Either a PointType89)
                  | PTID93 (Either a PointType93)
                  | PTID94 (Either a PointType94)
                  | PTID98 (Either a PointType98)
                  | PTID117 (Either a PointType117)
                  | PTID118 (Either a PointType118)
                  | PTID120 (Either a PointType120)                  
                  | PTID121 (Either a PointType121)
                  | PTID122 (Either a PointType122)
                  | PTID172 (Either a PointType172)                  
                  | PTID173 (Either a PointType173)                    
                  | PTID174 (Either a PointType174)                    
                  | PTID175 (Either a PointType175)                    
                  | PTID176 (Either a PointType176)
                  | PTID177 (Either a PointType177)  

                  deriving (Eq,Show)

pt0 :: PointTypes ()
pt0 = PTID0 $ Left ()
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
pt21 :: PointTypes ()
pt21 = PTID21 $ Left ()
pt40 :: PointTypes ()
pt40 = PTID40 $ Left ()
pt41 :: PointTypes ()
pt41 = PTID41 $ Left ()
pt42 :: PointTypes ()
pt42 = PTID42 $ Left ()
pt43 :: PointTypes ()
pt43 = PTID43 $ Left ()
pt44 :: PointTypes ()
pt44 = PTID44 $ Left ()
pt45 :: PointTypes ()
pt45 = PTID45 $ Left ()
pt46 :: PointTypes ()
pt46 = PTID46 $ Left ()
pt47 :: PointTypes ()
pt47 = PTID47 $ Left ()
pt48 :: PointTypes ()
pt48 = PTID48 $ Left ()
pt52 :: PointTypes ()
pt52 = PTID52 $ Left ()
pt53 :: PointTypes ()
pt53 = PTID53 $ Left ()
pt54 :: PointTypes ()
pt54 = PTID54 $ Left ()
pt55 :: PointTypes ()
pt55 = PTID55 $ Left ()
pt56 :: PointTypes ()
pt56 = PTID56 $ Left ()
pt57 :: PointTypes ()
pt57 = PTID57 $ Left ()
pt58 :: PointTypes ()
pt58 = PTID58 $ Left ()
pt59 :: PointTypes ()
pt59 = PTID59 $ Left ()
pt80 :: PointTypes ()
pt80 = PTID80 $ Left ()
pt81 :: PointTypes ()
pt81 = PTID81 $ Left ()
pt85 :: PointTypes ()
pt85 = PTID85 $ Left ()
pt86 :: PointTypes ()
pt86 = PTID86 $ Left ()
pt88 :: PointTypes ()
pt88 = PTID88 $ Left ()
pt89 :: PointTypes ()
pt89 = PTID89 $ Left ()
pt93 :: PointTypes ()
pt93 = PTID93 $ Left ()
pt94 :: PointTypes ()
pt94 = PTID94 $ Left ()
pt98 :: PointTypes ()
pt98 = PTID98 $ Left ()
pt117 :: PointTypes ()
pt117 = PTID117 $ Left ()
pt118 :: PointTypes ()
pt118 = PTID118 $ Left ()
pt120 :: PointTypes ()
pt120 = PTID120 $ Left ()
pt121 :: PointTypes ()
pt121 = PTID121 $ Left ()
pt122 :: PointTypes ()
pt122 = PTID122 $ Left ()
pt172 :: PointTypes ()
pt172 = PTID172 $ Left ()
pt173 :: PointTypes ()
pt173 = PTID173 $ Left ()
pt174 :: PointTypes ()
pt174 = PTID174 $ Left ()
pt175 :: PointTypes ()
pt175 = PTID175 $ Left ()
pt176 :: PointTypes ()
pt176 = PTID176 $ Left ()
pt177 :: PointTypes ()
pt177 = PTID177 $ Left ()


decodePTID :: PointTypes a -> Word8
decodePTID (PTID0 _) = 0
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
decodePTID (PTID12 _) = 12
decodePTID (PTID13 _) = 13
decodePTID (PTID14 _) = 14
decodePTID (PTID15 _) = 15
decodePTID (PTID16 _) = 16
decodePTID (PTID17 _) = 17
decodePTID (PTID18 _) = 18
decodePTID (PTID19 _) = 19
decodePTID (PTID20 _) = 20
decodePTID (PTID21 _) = 21
decodePTID (PTID40 _) = 40
decodePTID (PTID41 _) = 41
decodePTID (PTID42 _) = 42
decodePTID (PTID43 _) = 43
decodePTID (PTID44 _) = 44
decodePTID (PTID45 _) = 45
decodePTID (PTID46 _) = 46
decodePTID (PTID47 _) = 47
decodePTID (PTID48 _) = 48
decodePTID (PTID52 _) = 52
decodePTID (PTID53 _) = 53
decodePTID (PTID54 _) = 54
decodePTID (PTID55 _) = 55
decodePTID (PTID56 _) = 56
decodePTID (PTID57 _) = 57
decodePTID (PTID58 _) = 58
decodePTID (PTID59 _) = 59
decodePTID (PTID80 _) = 80
decodePTID (PTID81 _) = 81
decodePTID (PTID85 _) = 85
decodePTID (PTID86 _) = 86
decodePTID (PTID88 _) = 88
decodePTID (PTID89 _) = 89
decodePTID (PTID93 _) = 93
decodePTID (PTID94 _) = 94
decodePTID (PTID98 _) = 98
decodePTID (PTID117 _) = 117
decodePTID (PTID118 _) = 118
decodePTID (PTID120 _) = 120
decodePTID (PTID121 _) = 121
decodePTID (PTID122 _) = 122
decodePTID (PTID172 _) = 172
decodePTID (PTID173 _) = 173
decodePTID (PTID174 _) = 174
decodePTID (PTID175 _) = 175
decodePTID (PTID176 _) = 176
decodePTID (PTID177 _) = 177
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
fetchPointType  (PTID0 _ ) bs = PTID0 $ decodeToEither $ runGetIncremental pointType0Parser `pushChunks` bs
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
fetchPointType  (PTID12 _ ) bs = PTID12 $ decodeToEither $ runGetIncremental pointType12Parser `pushChunks` bs
fetchPointType  (PTID13 _ ) bs = PTID13 $ decodeToEither $ runGetIncremental pointType13Parser `pushChunks` bs
fetchPointType  (PTID14 _ ) bs = PTID14 $ decodeToEither $ runGetIncremental pointType14Parser `pushChunks` bs
fetchPointType  (PTID15 _ ) bs = PTID15 $ decodeToEither $ runGetIncremental pointType15Parser `pushChunks` bs
fetchPointType  (PTID16 _ ) bs = PTID16 $ decodeToEither $ runGetIncremental pointType16Parser `pushChunks` bs
fetchPointType  (PTID17 _ ) bs = PTID17 $ decodeToEither $ runGetIncremental pointType17Parser `pushChunks` bs
fetchPointType  (PTID18 _ ) bs = PTID18 $ decodeToEither $ runGetIncremental pointType18Parser `pushChunks` bs
fetchPointType  (PTID19 _ ) bs = PTID19 $ decodeToEither $ runGetIncremental pointType19Parser `pushChunks` bs
fetchPointType  (PTID20 _ ) bs = PTID20 $ decodeToEither $ runGetIncremental pointType20Parser `pushChunks` bs
fetchPointType  (PTID21 _ ) bs = PTID21 $ decodeToEither $ runGetIncremental pointType21Parser `pushChunks` bs
fetchPointType  (PTID40 _ ) bs = PTID40 $ decodeToEither $ runGetIncremental pointType40Parser `pushChunks` bs
fetchPointType  (PTID41 _ ) bs = PTID41 $ decodeToEither $ runGetIncremental pointType41Parser `pushChunks` bs
fetchPointType  (PTID42 _ ) bs = PTID42 $ decodeToEither $ runGetIncremental pointType42Parser `pushChunks` bs
fetchPointType  (PTID43 _ ) bs = PTID43 $ decodeToEither $ runGetIncremental pointType43Parser `pushChunks` bs
fetchPointType  (PTID44 _ ) bs = PTID44 $ decodeToEither $ runGetIncremental pointType44Parser `pushChunks` bs
fetchPointType  (PTID45 _ ) bs = PTID45 $ decodeToEither $ runGetIncremental pointType45Parser `pushChunks` bs
fetchPointType  (PTID46 _ ) bs = PTID46 $ decodeToEither $ runGetIncremental pointType46Parser `pushChunks` bs
fetchPointType  (PTID47 _ ) bs = PTID47 $ decodeToEither $ runGetIncremental pointType47Parser `pushChunks` bs
fetchPointType  (PTID48 _ ) bs = PTID48 $ decodeToEither $ runGetIncremental pointType48Parser `pushChunks` bs
fetchPointType  (PTID52 _ ) bs = PTID52 $ decodeToEither $ runGetIncremental pointType52Parser `pushChunks` bs
fetchPointType  (PTID53 _ ) bs = PTID53 $ decodeToEither $ runGetIncremental pointType53Parser `pushChunks` bs
fetchPointType  (PTID54 _ ) bs = PTID54 $ decodeToEither $ runGetIncremental pointType54Parser `pushChunks` bs
fetchPointType  (PTID55 _ ) bs = PTID55 $ decodeToEither $ runGetIncremental pointType55Parser `pushChunks` bs
fetchPointType  (PTID56 _ ) bs = PTID56 $ decodeToEither $ runGetIncremental pointType56Parser `pushChunks` bs
fetchPointType  (PTID57 _ ) bs = PTID57 $ decodeToEither $ runGetIncremental pointType57Parser `pushChunks` bs
fetchPointType  (PTID58 _ ) bs = PTID58 $ decodeToEither $ runGetIncremental pointType58Parser `pushChunks` bs
fetchPointType  (PTID59 _ ) bs = PTID59 $ decodeToEither $ runGetIncremental pointType59Parser `pushChunks` bs
fetchPointType  (PTID80 _ ) bs = PTID80 $ decodeToEither $ runGetIncremental pointType80Parser `pushChunks` bs
fetchPointType  (PTID81 _ ) bs = PTID81 $ decodeToEither $ runGetIncremental pointType81Parser `pushChunks` bs
fetchPointType  (PTID85 _ ) bs = PTID85 $ decodeToEither $ runGetIncremental pointType85Parser `pushChunks` bs
fetchPointType  (PTID86 _ ) bs = PTID86 $ decodeToEither $ runGetIncremental pointType86Parser `pushChunks` bs
fetchPointType  (PTID88 _ ) bs = PTID88 $ decodeToEither $ runGetIncremental pointType88Parser `pushChunks` bs
fetchPointType  (PTID89 _ ) bs = PTID89 $ decodeToEither $ runGetIncremental pointType89Parser `pushChunks` bs
fetchPointType  (PTID93 _ ) bs = PTID93 $ decodeToEither $ runGetIncremental pointType93Parser `pushChunks` bs
fetchPointType  (PTID94 _ ) bs = PTID94 $ decodeToEither $ runGetIncremental pointType94Parser `pushChunks` bs
fetchPointType  (PTID98 _ ) bs = PTID98 $ decodeToEither $ runGetIncremental pointType98Parser `pushChunks` bs
fetchPointType  (PTID117 _ ) bs = PTID117 $ decodeToEither $ runGetIncremental pointType117Parser `pushChunks` bs
fetchPointType  (PTID118 _ ) bs = PTID118 $ decodeToEither $ runGetIncremental pointType118Parser `pushChunks` bs
fetchPointType  (PTID120 _ ) bs = PTID120 $ decodeToEither $ runGetIncremental pointType120Parser `pushChunks` bs
fetchPointType  (PTID121 _ ) bs = PTID121 $ decodeToEither $ runGetIncremental pointType121Parser `pushChunks` bs
fetchPointType  (PTID122 _ ) bs = PTID122 $ decodeToEither $ runGetIncremental pointType122Parser `pushChunks` bs
fetchPointType  (PTID172 _ ) bs = PTID172 $ decodeToEither $ runGetIncremental pointType172Parser `pushChunks` bs
fetchPointType  (PTID173 _ ) bs = PTID173 $ decodeToEither $ runGetIncremental pointType173Parser `pushChunks` bs
fetchPointType  (PTID174 _ ) bs = PTID174 $ decodeToEither $ runGetIncremental pointType174Parser `pushChunks` bs
fetchPointType  (PTID175 _ ) bs = PTID175 $ decodeToEither $ runGetIncremental pointType175Parser `pushChunks` bs
fetchPointType  (PTID176 _ ) bs = PTID176 $ decodeToEither $ runGetIncremental pointType176Parser `pushChunks` bs
fetchPointType  (PTID177 _ ) bs = PTID177 $ decodeToEither $ runGetIncremental pointType177Parser `pushChunks` bs

decodeToEither :: (Show a) => Decoder a -> Either LB.ByteString a
decodeToEither (Fail _ _ s) = Left $ C8.append "decoder Failed with"  (C8.pack s)
decodeToEither (Done _ _ a) = Right a
decodeToEither _ = Left "incomplete parsing SHOULD NOT HAPPEN!"

--debugDecoderPointType :: (Show a) => Decoder a -> IO () 
--debugDecoderPointType (Fail _ _ s) = print $ "decoder Failed with" ++ s 
--debugDecoderPointType (Done _ _ pt) = print "Point type finished" >> print pt
--debugDecoderPointType _ = print "incomplete parsing SHOULD NOT HAPPEN!"