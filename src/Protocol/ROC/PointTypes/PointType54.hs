{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType54 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.PointTypes.Utils

data PointType54 = PointType54 {
 pointType54PointTag                            :: !PointType54PointTag                             
,pointType54StartRegister1                      :: !PointType54StartRegister1                             
,pointType54EndRegister1                        :: !PointType54EndRegister1                             
,pointType54ROCParameters1                      :: !PointType54ROCParameters1                             
,pointType54ConversionCode1                     :: !PointType54ConversionCode1                            
,pointType54StartRegister2                      :: !PointType54StartRegister2                             
,pointType54EndRegister2                        :: !PointType54EndRegister2                         
,pointType54ROCParameters2                      :: !PointType54ROCParameters2                             
,pointType54ConversionCode2                     :: !PointType54ConversionCode2                             
,pointType54StartRegister3                      :: !PointType54StartRegister3                             
,pointType54EndRegister3                        :: !PointType54EndRegister3                             
,pointType54ROCParameters3                      :: !PointType54ROCParameters3                             
,pointType54ConversionCode3                     :: !PointType54ConversionCode3                             
,pointType54StartRegister4                      :: !PointType54StartRegister4                         
,pointType54EndRegister4                        :: !PointType54EndRegister4                             
,pointType54ROCParameters4                      :: !PointType54ROCParameters4                            
,pointType54ConversionCode4                     :: !PointType54ConversionCode4                             
,pointType54StartRegister5                      :: !PointType54StartRegister5                           
,pointType54EndRegister5                        :: !PointType54EndRegister5                             
,pointType54ROCParameters5                      :: !PointType54ROCParameters5                             
,pointType54ConversionCode5                     :: !PointType54ConversionCode5                             
,pointType54StartRegister6                      :: !PointType54StartRegister6                             
,pointType54EndRegister6                        :: !PointType54EndRegister6                            
,pointType54ROCParameters6                      :: !PointType54ROCParameters6                             
,pointType54ConversionCode6                     :: !PointType54ConversionCode6                         
,pointType54StartRegister7                      :: !PointType54StartRegister7                             
,pointType54EndRegister7                        :: !PointType54EndRegister7                             
,pointType54ROCParameters7                      :: !PointType54ROCParameters7                             
,pointType54ConversionCode7                     :: !PointType54ConversionCode7                             
,pointType54StartRegister8                      :: !PointType54StartRegister8                             
,pointType54EndRegister8                        :: !PointType54EndRegister8                             
,pointType54ROCParameters8                      :: !PointType54ROCParameters8                         
,pointType54ConversionCode8                     :: !PointType54ConversionCode8                             
,pointType54StartRegister9                      :: !PointType54StartRegister9                            
,pointType54EndRegister9                        :: !PointType54EndRegister9                             
,pointType54ROCParameters9                      :: !PointType54ROCParameters9                         
,pointType54ConversionCode9                     :: !PointType54ConversionCode9                             
,pointType54StartRegister10                     :: !PointType54StartRegister10                             
,pointType54EndRegister10                       :: !PointType54EndRegister10                             
,pointType54ROCParameters10                     :: !PointType54ROCParameters10                            
,pointType54ConversionCode10                    :: !PointType54ConversionCode10                             
,pointType54StartRegister11                     :: !PointType54StartRegister11                         
,pointType54EndRegister11                       :: !PointType54EndRegister11                             
,pointType54ROCParameters11                     :: !PointType54ROCParameters11                             
,pointType54ConversionCode11                    :: !PointType54ConversionCode11                             
,pointType54StartRegister12                     :: !PointType54StartRegister12                             
,pointType54EndRegister12                       :: !PointType54EndRegister12                             
,pointType54ROCParameters12                     :: !PointType54ROCParameters12                             
,pointType54ConversionCode12                    :: !PointType54ConversionCode12                         
,pointType54StartRegister13                     :: !PointType54StartRegister13                             
,pointType54EndRegister13                       :: !PointType54EndRegister13                            
,pointType54ROCParameters13                     :: !PointType54ROCParameters13                             
,pointType54ConversionCode13                    :: !PointType54ConversionCode13                           
,pointType54StartRegister14                     :: !PointType54StartRegister14                             
,pointType54EndRegister14                       :: !PointType54EndRegister14                             
,pointType54ROCParameters14                     :: !PointType54ROCParameters14                             
,pointType54ConversionCode14                    :: !PointType54ConversionCode14                             
,pointType54StartRegister15                     :: !PointType54StartRegister15                            
,pointType54EndRegister15                       :: !PointType54EndRegister15                             
,pointType54ROCParameters15                     :: !PointType54ROCParameters15                         
,pointType54ConversionCode15                    :: !PointType54ConversionCode15                             
  
} deriving (Read,Eq, Show, Generic)                       
                                  
type PointType54PointTag                         = BS.ByteString                                                       
type PointType54StartRegister1                   = Word16                                                             
type PointType54EndRegister1                     = Word16                                                           
type PointType54ROCParameters1                   = [Word8]                                                             
type PointType54ConversionCode1                  = Word8                                                             
type PointType54StartRegister2                   = Word16                                                              
type PointType54EndRegister2                     = Word16                                                        
type PointType54ROCParameters2                   = [Word8]                                                             
type PointType54ConversionCode2                  = Word8                                                                
type PointType54StartRegister3                   = Word16                                                              
type PointType54EndRegister3                     = Word16                                                            
type PointType54ROCParameters3                   = [Word8]                                                             
type PointType54ConversionCode3                  = Word8                                                                
type PointType54StartRegister4                   = Word16                                                          
type PointType54EndRegister4                     = Word16                                                            
type PointType54ROCParameters4                   = [Word8]                                                            
type PointType54ConversionCode4                  = Word8                                                                
type PointType54StartRegister5                   = Word16                                                            
type PointType54EndRegister5                     = Word16                                                            
type PointType54ROCParameters5                   = [Word8]                                                             
type PointType54ConversionCode5                  = Word8                                                                
type PointType54StartRegister6                   = Word16                                                              
type PointType54EndRegister6                     = Word16                                                           
type PointType54ROCParameters6                   = [Word8]                                                             
type PointType54ConversionCode6                  = Word8                                                            
type PointType54StartRegister7                   = Word16                                                              
type PointType54EndRegister7                     = Word16                                                            
type PointType54ROCParameters7                   = [Word8]                                                             
type PointType54ConversionCode7                  = Word8                                                                
type PointType54StartRegister8                   = Word16                                                              
type PointType54EndRegister8                     = Word16                                                            
type PointType54ROCParameters8                   = [Word8]                                                         
type PointType54ConversionCode8                  = Word8                                                                
type PointType54StartRegister9                   = Word16                                                             
type PointType54EndRegister9                     = Word16                                                            
type PointType54ROCParameters9                   = [Word8]                                                         
type PointType54ConversionCode9                  = Word8                                                                
type PointType54StartRegister10                  = Word16                                                               
type PointType54EndRegister10                    = Word16                                                             
type PointType54ROCParameters10                  = [Word8]                                                             
type PointType54ConversionCode10                 = Word8                                                                 
type PointType54StartRegister11                  = Word16                                                           
type PointType54EndRegister11                    = Word16                                                             
type PointType54ROCParameters11                  = [Word8]                                                              
type PointType54ConversionCode11                 = Word8                                                                 
type PointType54StartRegister12                  = Word16                                                               
type PointType54EndRegister12                    = Word16                                                             
type PointType54ROCParameters12                  = [Word8]                                                              
type PointType54ConversionCode12                 = Word8                                                             
type PointType54StartRegister13                  = Word16                                                               
type PointType54EndRegister13                    = Word16                                                            
type PointType54ROCParameters13                  = [Word8]                                                              
type PointType54ConversionCode13                 = Word8                                                               
type PointType54StartRegister14                  = Word16                                                               
type PointType54EndRegister14                    = Word16                                                             
type PointType54ROCParameters14                  = [Word8]                                                              
type PointType54ConversionCode14                 = Word8                                                                 
type PointType54StartRegister15                  = Word16                                                              
type PointType54EndRegister15                    = Word16                                                             
type PointType54ROCParameters15                  = [Word8]                                                          
type PointType54ConversionCode15                 = Word8                                                      
  
pointType54Parser :: Get PointType54
pointType54Parser = do 

  pointTag <- getByteString 20                                       
  startRegister1 <- getWord16le                                       
  endRegister1 <- getWord16le                                       
  rOCParameters1 <- getTLP                                       
  conversionCode1 <- getWord8                                         
  startRegister2 <- getWord16le                                         
  endRegister2 <- getWord16le                                         
  rOCParameters2 <- getTLP                                              
  conversionCode2 <- getWord8                                            
  startRegister3 <- getWord16le                                         
  endRegister3 <- getWord16le                                         
  rOCParameters3 <- getTLP                                              
  conversionCode3 <- getWord8                                            
  startRegister4 <- getWord16le                                         
  endRegister4 <- getWord16le                                         
  rOCParameters4 <- getTLP                                              
  conversionCode4 <- getWord8                                            
  startRegister5 <- getWord16le                                         
  endRegister5 <- getWord16le                                         
  rOCParameters5 <- getTLP                                              
  conversionCode5 <- getWord8                                            
  startRegister6 <- getWord16le                                         
  endRegister6 <- getWord16le                                         
  rOCParameters6 <- getTLP                                              
  conversionCode6 <- getWord8                                            
  startRegister7 <- getWord16le                                         
  endRegister7 <- getWord16le                                         
  rOCParameters7 <- getTLP                                              
  conversionCode7 <- getWord8                                            
  startRegister8 <- getWord16le                                         
  endRegister8 <- getWord16le                                         
  rOCParameters8 <- getTLP                                              
  conversionCode8 <- getWord8                                            
  startRegister9 <- getWord16le                                         
  endRegister9 <- getWord16le                                         
  rOCParameters9 <- getTLP                                              
  conversionCode9 <- getWord8                                            
  startRegister10 <- getWord16le                                         
  endRegister10 <- getWord16le                                         
  rOCParameters10 <- getTLP                                              
  conversionCode10 <- getWord8                                            
  startRegister11 <- getWord16le                                         
  endRegister11 <- getWord16le                                         
  rOCParameters11 <- getTLP                                              
  conversionCode11 <- getWord8                                            
  startRegister12 <- getWord16le                                         
  endRegister12 <- getWord16le                                         
  rOCParameters12 <- getTLP                                              
  conversionCode12 <- getWord8                                            
  startRegister13 <- getWord16le                                         
  endRegister13 <- getWord16le                                         
  rOCParameters13 <- getTLP                                              
  conversionCode13 <- getWord8                                            
  startRegister14 <- getWord16le                                         
  endRegister14 <- getWord16le                                         
  rOCParameters14 <- getTLP                                              
  conversionCode14 <- getWord8                                           
  startRegister15 <- getWord16le                                        
  endRegister15 <- getWord16le                                       
  rOCParameters15 <- getTLP                                            
  conversionCode15 <- getWord8                                          
  
  return $ PointType54 pointTag startRegister1 endRegister1 rOCParameters1 conversionCode1 startRegister2 endRegister2 rOCParameters2 conversionCode2 startRegister3 endRegister3 
    rOCParameters3 conversionCode3 startRegister4 endRegister4 rOCParameters4 conversionCode4 startRegister5 endRegister5 rOCParameters5 conversionCode5 startRegister6 endRegister6 
    rOCParameters6 conversionCode6 startRegister7 endRegister7 rOCParameters7 conversionCode7 startRegister8 endRegister8 rOCParameters8 conversionCode8 startRegister9 endRegister9 
    rOCParameters9 conversionCode9 startRegister10 endRegister10 rOCParameters10 conversionCode10 startRegister11 endRegister11 rOCParameters11 conversionCode11 startRegister12 
    endRegister12 rOCParameters12 conversionCode12 startRegister13 endRegister13 rOCParameters13 conversionCode13 startRegister14 endRegister14 rOCParameters14 conversionCode14 
    startRegister15 endRegister15 rOCParameters15 conversionCode15