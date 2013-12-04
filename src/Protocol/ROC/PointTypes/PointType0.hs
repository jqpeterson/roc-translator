{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType0 where

import GHC.Generics
import Data.Word
import Data.Binary
import Protocol.ROC.Utils
import Protocol.ROC.Float

data PointType0 = PointType0 {
  
 pointType0SequenceRevisionNum         :: !PointType0SequenceRevisionNum                         
,pointType0TLPData1                    :: !PointType0TLPData1                         
,pointType0TLPData2                    :: !PointType0TLPData2                         
,pointType0TLPData3                    :: !PointType0TLPData3                         
,pointType0TLPData4                    :: !PointType0TLPData4                         
,pointType0TLPData5                    :: !PointType0TLPData5                         
,pointType0TLPData6                    :: !PointType0TLPData6                         
,pointType0TLPData7                    :: !PointType0TLPData7                         
,pointType0TLPData8                    :: !PointType0TLPData8                         
,pointType0TLPData9                    :: !PointType0TLPData9                         
,pointType0TLPData10                   :: !PointType0TLPData10                         
,pointType0TLPData11                   :: !PointType0TLPData11                         
,pointType0TLPData12                   :: !PointType0TLPData12                         
,pointType0TLPData13                   :: !PointType0TLPData13                         
,pointType0TLPData14                   :: !PointType0TLPData14                         
,pointType0TLPData15                   :: !PointType0TLPData15                         
,pointType0TLPData16                   :: !PointType0TLPData16                         
,pointType0TLPData17                   :: !PointType0TLPData17                         
,pointType0TLPData18                   :: !PointType0TLPData18                         
,pointType0TLPData19                   :: !PointType0TLPData19                         
,pointType0TLPData20                   :: !PointType0TLPData20                         
,pointType0TLPData21                   :: !PointType0TLPData21                         
,pointType0TLPData22                   :: !PointType0TLPData22                         
,pointType0TLPData23                   :: !PointType0TLPData23                         
,pointType0TLPData24                   :: !PointType0TLPData24                         
,pointType0TLPData25                   :: !PointType0TLPData25                         
,pointType0TLPData26                   :: !PointType0TLPData26                         
,pointType0TLPData27                   :: !PointType0TLPData27                         
,pointType0TLPData28                   :: !PointType0TLPData28                         
,pointType0TLPData29                   :: !PointType0TLPData29                         
,pointType0TLPData30                   :: !PointType0TLPData30                         
,pointType0TLPData31                   :: !PointType0TLPData31                         
,pointType0TLPData32                   :: !PointType0TLPData32                         
,pointType0TLPData33                   :: !PointType0TLPData33                         
,pointType0TLPData34                   :: !PointType0TLPData34                         
,pointType0TLPData35                   :: !PointType0TLPData35                         
,pointType0TLPData36                   :: !PointType0TLPData36                         
,pointType0TLPData37                   :: !PointType0TLPData37                         
,pointType0TLPData38                   :: !PointType0TLPData38                         
,pointType0TLPData39                   :: !PointType0TLPData39                         
,pointType0TLPData40                   :: !PointType0TLPData40                         
,pointType0TLPData41                   :: !PointType0TLPData41                         
,pointType0TLPData42                   :: !PointType0TLPData42                         
,pointType0TLPData43                   :: !PointType0TLPData43                         
,pointType0TLPData44                   :: !PointType0TLPData44                               
  
} deriving (Read,Eq, Show, Generic)                       
                                  
type PointType0SequenceRevisionNum     = Float     
type PointType0TLPData1                = [Word8]     
type PointType0TLPData2                = [Word8]     
type PointType0TLPData3                = [Word8]     
type PointType0TLPData4                = [Word8]     
type PointType0TLPData5                = [Word8]     
type PointType0TLPData6                = [Word8]     
type PointType0TLPData7                = [Word8]     
type PointType0TLPData8                = [Word8]     
type PointType0TLPData9                = [Word8]     
type PointType0TLPData10               = [Word8]     
type PointType0TLPData11               = [Word8]     
type PointType0TLPData12               = [Word8]     
type PointType0TLPData13               = [Word8]     
type PointType0TLPData14               = [Word8]     
type PointType0TLPData15               = [Word8]     
type PointType0TLPData16               = [Word8]     
type PointType0TLPData17               = [Word8]     
type PointType0TLPData18               = [Word8]     
type PointType0TLPData19               = [Word8]     
type PointType0TLPData20               = [Word8]     
type PointType0TLPData21               = [Word8]     
type PointType0TLPData22               = [Word8]     
type PointType0TLPData23               = [Word8]     
type PointType0TLPData24               = [Word8]     
type PointType0TLPData25               = [Word8]     
type PointType0TLPData26               = [Word8]     
type PointType0TLPData27               = [Word8]     
type PointType0TLPData28               = [Word8]     
type PointType0TLPData29               = [Word8]     
type PointType0TLPData30               = [Word8]     
type PointType0TLPData31               = [Word8]     
type PointType0TLPData32               = [Word8]     
type PointType0TLPData33               = [Word8]     
type PointType0TLPData34               = [Word8]     
type PointType0TLPData35               = [Word8]     
type PointType0TLPData36               = [Word8]     
type PointType0TLPData37               = [Word8]     
type PointType0TLPData38               = [Word8]     
type PointType0TLPData39               = [Word8]     
type PointType0TLPData40               = [Word8]     
type PointType0TLPData41               = [Word8]     
type PointType0TLPData42               = [Word8]     
type PointType0TLPData43               = [Word8]     
type PointType0TLPData44               = [Word8]     
  
pointType0Parser :: Get PointType0
pointType0Parser = do 
 
  sequenceRevisionNum <- getIeeeFloat32
  tLPData1 <- getTLP 
  tLPData2 <- getTLP 
  tLPData3 <- getTLP 
  tLPData4 <- getTLP 
  tLPData5 <- getTLP 
  tLPData6 <- getTLP 
  tLPData7 <- getTLP 
  tLPData8 <- getTLP 
  tLPData9 <- getTLP 
  tLPData10 <- getTLP 
  tLPData11 <- getTLP 
  tLPData12 <- getTLP 
  tLPData13 <- getTLP 
  tLPData14 <- getTLP 
  tLPData15 <- getTLP 
  tLPData16 <- getTLP 
  tLPData17 <- getTLP 
  tLPData18 <- getTLP 
  tLPData19 <- getTLP 
  tLPData20 <- getTLP 
  tLPData21 <- getTLP 
  tLPData22 <- getTLP 
  tLPData23 <- getTLP 
  tLPData24 <- getTLP 
  tLPData25 <- getTLP 
  tLPData26 <- getTLP 
  tLPData27 <- getTLP 
  tLPData28 <- getTLP 
  tLPData29 <- getTLP 
  tLPData30 <- getTLP 
  tLPData31 <- getTLP 
  tLPData32 <- getTLP 
  tLPData33 <- getTLP 
  tLPData34 <- getTLP 
  tLPData35 <- getTLP 
  tLPData36 <- getTLP 
  tLPData37 <- getTLP 
  tLPData38 <- getTLP 
  tLPData39 <- getTLP 
  tLPData40 <- getTLP 
  tLPData41 <- getTLP 
  tLPData42 <- getTLP 
  tLPData43 <- getTLP 
  tLPData44 <- getTLP 

  return $ PointType0 sequenceRevisionNum tLPData1 tLPData2 tLPData3 tLPData4 tLPData5 tLPData6 tLPData7 tLPData8 tLPData9 tLPData10 tLPData11 tLPData12 tLPData13 tLPData14 
    tLPData15 tLPData16 tLPData17 tLPData18 tLPData19 tLPData20 tLPData21 tLPData22 tLPData23 tLPData24 tLPData25 tLPData26 tLPData27 tLPData28 tLPData29 tLPData30 tLPData31 
    tLPData32 tLPData33 tLPData34 tLPData35 tLPData36 tLPData37 tLPData38 tLPData39 tLPData40 tLPData41 tLPData42 tLPData43 tLPData44