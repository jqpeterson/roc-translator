{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType41 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.PointTypes.Utils

data PointType41 = PointType41 {
  
 pointType41PointTag                      :: !PointType41PointTag                     
,pointType41ATMPress                      :: !PointType41ATMPress                                
,pointType41CalcMethodII                  :: !PointType41CalcMethodII                             
,pointType41NotUse1                       :: !PointType41NotUse1                                 
,pointType41PipeRefTemp                   :: !PointType41PipeRefTemp                                    
,pointType41PipeMaterial                  :: !PointType41PipeMaterial                                           
,pointType41NotUse2                       :: !PointType41NotUse2                                  
,pointType41MtrTypeOrCdTbFtm              :: !PointType41MtrTypeOrCdTbFtm                                    
,pointType41FrOrReynoldsNum               :: !PointType41FrOrReynoldsNum                                 
,pointType41OrExpFactorTbFpm              :: !PointType41OrExpFactorTbFpm                                              
,pointType41FpbFactor                     :: !PointType41FpbFactor                                             
,pointType41FtpFactor                     :: !PointType41FtpFactor                               
,pointType41FtfFactor                     :: !PointType41FtfFactor                                        
,pointType41FgrFactor                     :: !PointType41FgrFactor                                      
,pointType41FpvFactor                     :: !PointType41FpvFactor                                      
,pointType41HistPnt1                      :: !PointType41HistPnt1                                   
,pointType41RollUp1                       :: !PointType41RollUp1                                    
,pointType41TLPParamArchived1             :: !PointType41TLPParamArchived1                                       
,pointType41ConvRollUp1                   :: !PointType41ConvRollUp1                                     
,pointType41HistPnt2                      :: !PointType41HistPnt2                                    
,pointType41RollUp2                       :: !PointType41RollUp2                                        
,pointType41TLPParamArchived2             :: !PointType41TLPParamArchived2                                 
,pointType41ConvRollUp2                   :: !PointType41ConvRollUp2                                     
,pointType41HistPnt3                      :: !PointType41HistPnt3                                  
,pointType41RollUp3                       :: !PointType41RollUp3                                 
,pointType41TLPParamArchived3             :: !PointType41TLPParamArchived3                                 
,pointType41ConvRollUp3                   :: !PointType41ConvRollUp3                                 
,pointType41HistPnt4                      :: !PointType41HistPnt4                                  
,pointType41RollUp4                       :: !PointType41RollUp4                               
,pointType41TLPParamArchived4             :: !PointType41TLPParamArchived4                            
,pointType41ConvRollUp4                   :: !PointType41ConvRollUp4                                
,pointType41HistPnt5                      :: !PointType41HistPnt5                                                        
,pointType41RollUp5                       :: !PointType41RollUp5                                                     
,pointType41TLPParamArchived5             :: !PointType41TLPParamArchived5                                                  
,pointType41ConvRollUp5                   :: !PointType41ConvRollUp5                                                    
,pointType41HistPnt6                      :: !PointType41HistPnt6                                                           
,pointType41RollUp6                       :: !PointType41RollUp6                                                       
,pointType41TLPParamArchived6             :: !PointType41TLPParamArchived6                                                     
,pointType41ConvRollUp6                   :: !PointType41ConvRollUp6                                                        
,pointType41HistPnt7                      :: !PointType41HistPnt7                                                                 
,pointType41RollUp7                       :: !PointType41RollUp7                                                   
,pointType41TLPParamArchived7             :: !PointType41TLPParamArchived7                                                     
,pointType41ConvRollUp7                   :: !PointType41ConvRollUp7                                                        
,pointType41HistPnt8                      :: !PointType41HistPnt8                              
,pointType41RollUp8                       :: !PointType41RollUp8                             
,pointType41TLPParamArchived8             :: !PointType41TLPParamArchived8                                    
,pointType41ConvRollUp8                   :: !PointType41ConvRollUp8                         
,pointType41HistPnt9                      :: !PointType41HistPnt9                              
,pointType41RollUp9                       :: !PointType41RollUp9                             
,pointType41TLPParamArchived9             :: !PointType41TLPParamArchived9                              
,pointType41ConvRollUp9                   :: !PointType41ConvRollUp9                             
,pointType41HistPnt10                     :: !PointType41HistPnt10                               
,pointType41RollUp10                      :: !PointType41RollUp10                         
,pointType41TLPParamArchived10            :: !PointType41TLPParamArchived10                           
,pointType41ConvRollUp10                  :: !PointType41ConvRollUp10                         
--,pointType41HistPnt11                     :: !PointType41HistPnt11                         
--,pointType41RollUp11                      :: !PointType41RollUp11                         
--,pointType41TLPParamArchived11            :: !PointType41TLPParamArchived11                         
--,pointType41ConvRollUp11                  :: !PointType41ConvRollUp11                         
--,pointType41HistPnt12                     :: !PointType41HistPnt12                         
--,pointType41RollUp12                      :: !PointType41RollUp12                         
--,pointType41TLPParamArchived12            :: !PointType41TLPParamArchived12                         
--,pointType41ConvRollUp12                  :: !PointType41ConvRollUp12                         
--,pointType41HistPnt13                     :: !PointType41HistPnt13                         
--,pointType41RollUp13                      :: !PointType41RollUp13                         
--,pointType41TLPParamArchived13            :: !PointType41TLPParamArchived13                         
--,pointType41ConvRollUp13                  :: !PointType41ConvRollUp13                         
--,pointType41HistPnt14                     :: !PointType41HistPnt14                         
--,pointType41RollUp14                      :: !PointType41RollUp14                         
--,pointType41TLPParamArchived14            :: !PointType41TLPParamArchived14                         
--,pointType41ConvRollUp14                  :: !PointType41ConvRollUp14                         
--,pointType41HistPnt15                     :: !PointType41HistPnt15                         
--,pointType41RollUp15                      :: !PointType41RollUp15                         
--,pointType41TLPParamArchived15            :: !PointType41TLPParamArchived15                         
--,pointType41ConvRollUp15                  :: !PointType41ConvRollUp15                         
--,pointType41HistPnt16                     :: !PointType41HistPnt16                                            
--,pointType41RollUp16                      :: !PointType41RollUp16                                             
--,pointType41TLPParamArchived16            :: !PointType41TLPParamArchived16                                   
--,pointType41ConvRollUp16                  :: !PointType41ConvRollUp16                                         

} deriving (Read,Eq, Show, Generic)                       

type PointType41PointTag                 = BS.ByteString                                       
type PointType41ATMPress                 = Float                              
type PointType41CalcMethodII             = Word8                              
type PointType41NotUse1                  = [Word8]                              
type PointType41PipeRefTemp              = Float                              
type PointType41PipeMaterial             = Word8                              
type PointType41NotUse2                  = Word8                             
type PointType41MtrTypeOrCdTbFtm         = Float                              
type PointType41FrOrReynoldsNum          = Float                              
type PointType41OrExpFactorTbFpm         = Float                              
type PointType41FpbFactor                = Float                              
type PointType41FtpFactor                = Float                              
type PointType41FtfFactor                = Float                              
type PointType41FgrFactor                = Float                              
type PointType41FpvFactor                = Float                              
type PointType41HistPnt1                 = Word8                              
type PointType41RollUp1                  = Word8                              
type PointType41TLPParamArchived1        = [Word8]                              
type PointType41ConvRollUp1              = Float                              
type PointType41HistPnt2                 = Word8                                
type PointType41RollUp2                  = Word8                                
type PointType41TLPParamArchived2        = [Word8]                              
type PointType41ConvRollUp2              = Float                                
type PointType41HistPnt3                 = Word8                                                       
type PointType41RollUp3                  = Word8                                  
type PointType41TLPParamArchived3        = [Word8]                                
type PointType41ConvRollUp3              = Float                                  
type PointType41HistPnt4                 = Word8                                  
type PointType41RollUp4                  = Word8                                  
type PointType41TLPParamArchived4        = [Word8]                                
type PointType41ConvRollUp4              = Float                                  
type PointType41HistPnt5                 = Word8                                  
type PointType41RollUp5                  = Word8                                  
type PointType41TLPParamArchived5        = [Word8]                                
type PointType41ConvRollUp5              = Float                                  
type PointType41HistPnt6                 = Word8                                  
type PointType41RollUp6                  = Word8                                  
type PointType41TLPParamArchived6        = [Word8]                                
type PointType41ConvRollUp6              = Float                                  
type PointType41HistPnt7                 = Word8                                  
type PointType41RollUp7                  = Word8                                 
type PointType41TLPParamArchived7        = [Word8]                                
type PointType41ConvRollUp7              = Float                                  
type PointType41HistPnt8                 = Word8                                  
type PointType41RollUp8                  = Word8                                  
type PointType41TLPParamArchived8        = [Word8]                                
type PointType41ConvRollUp8              = Float                                 
type PointType41HistPnt9                 = Word8                                 
type PointType41RollUp9                  = Word8                                 
type PointType41TLPParamArchived9        = [Word8]                               
type PointType41ConvRollUp9              = Float                                 
type PointType41HistPnt10                = Word8                                  
type PointType41RollUp10                 = Word8                                  
type PointType41TLPParamArchived10       = [Word8]                            
type PointType41ConvRollUp10             = Float                                  
--type PointType41HistPnt11                = Word8                                  
--type PointType41RollUp11                 = Word8                                  
--type PointType41TLPParamArchived11       = [Word8]                                
--type PointType41ConvRollUp11             = Float                                  
--type PointType41HistPnt12                = Word8                                  
--type PointType41RollUp12                 = Word8                                  
--type PointType41TLPParamArchived12       = [Word8]                                
--type PointType41ConvRollUp12             = Float                                  
--type PointType41HistPnt13                = Word8                                  
--type PointType41RollUp13                 = Word8                                  
--type PointType41TLPParamArchived13       = [Word8]                               
--type PointType41ConvRollUp13             = Float                                  
--type PointType41HistPnt14                = Word8                                  
--type PointType41RollUp14                 = Word8                                  
--type PointType41TLPParamArchived14       = [Word8]                                
--type PointType41ConvRollUp14             = Float                                  
--type PointType41HistPnt15                = Word8                                 
--type PointType41RollUp15                 = Word8                                  
--type PointType41TLPParamArchived15       = [Word8]                                
--type PointType41ConvRollUp15             = Float                                  
--type PointType41HistPnt16                = Word8                                  
--type PointType41RollUp16                 = Word8                                  
--type PointType41TLPParamArchived16       = [Word8]                                
--type PointType41ConvRollUp16             = Float                                
 
  
pointType41Parser :: Get PointType41 
pointType41Parser = do   
  pointTag <- getByteString 10                        
  aTMPress <- getIeeeFloat32                        
  calcMethodII <- getWord8                        
  notUse1 <- getTLP                        
  pipeRefTemp <- getIeeeFloat32                        
  pipeMaterial <- getWord8                        
  notUse2 <- getWord8                        
  mtrTypeOrCdTbFtm <- getIeeeFloat32                        
  frOrReynoldsNum <- getIeeeFloat32                        
  orExpFactorTbFpm <- getIeeeFloat32                        
  fpbFactor <- getIeeeFloat32                        
  ftpFactor <- getIeeeFloat32                        
  ftfFactor <- getIeeeFloat32                        
  fgrFactor <- getIeeeFloat32                        
  fpvFactor <- getIeeeFloat32                        
  histPnt1 <- getWord8                        
  rollUp1 <- getWord8                        
  tLPParamArchived1 <- getTLP                        
  convRollUp1 <- getIeeeFloat32                        
  histPnt2 <- getWord8                        
  rollUp2 <- getWord8                        
  tLPParamArchived2 <- getTLP                        
  convRollUp2 <- getIeeeFloat32                        
  histPnt3 <- getWord8                        
  rollUp3 <- getWord8                        
  tLPParamArchived3 <- getTLP                        
  convRollUp3 <- getIeeeFloat32                        
  histPnt4 <- getWord8                        
  rollUp4 <- getWord8                        
  tLPParamArchived4 <- getTLP                        
  convRollUp4 <- getIeeeFloat32                        
  histPnt5 <- getWord8                        
  rollUp5 <- getWord8                        
  tLPParamArchived5 <- getTLP                        
  convRollUp5 <- getIeeeFloat32                        
  histPnt6 <- getWord8                        
  rollUp6 <- getWord8                        
  tLPParamArchived6 <- getTLP                        
  convRollUp6 <- getIeeeFloat32                        
  histPnt7 <- getWord8                        
  rollUp7 <- getWord8                        
  tLPParamArchived7 <- getTLP                        
  convRollUp7 <- getIeeeFloat32                        
  histPnt8 <- getWord8                        
  rollUp8 <- getWord8                        
  tLPParamArchived8 <- getTLP                        
  convRollUp8 <- getIeeeFloat32                        
  histPnt9 <- getWord8                       
  rollUp9 <- getWord8                       
  tLPParamArchived9 <- getTLP                       
  convRollUp9 <- getIeeeFloat32                       
  histPnt10 <- getWord8                        
  rollUp10 <- getWord8                        
  tLPParamArchived10 <- getTLP                        
  convRollUp10 <- getIeeeFloat32                        
--  histPnt11 <- getWord8                        
--  rollUp11 <- getWord8                        
--  tLPParamArchived11 <- getTLP                        
--  convRollUp11 <- getIeeeFloat32                        
--  histPnt12 <- getWord8                        
--  rollUp12 <- getWord8                        
--  tLPParamArchived12 <- getTLP                        
--  convRollUp12 <- getIeeeFloat32                        
--  histPnt13 <- getWord8                        
--  rollUp13 <- getWord8                        
--  tLPParamArchived13 <- getTLP                        
--  convRollUp13 <- getIeeeFloat32                        
--  histPnt14 <- getWord8                        
--  rollUp14 <- getWord8                        
--  tLPParamArchived14 <- getTLP                        
--  convRollUp14 <- getIeeeFloat32                        
--  histPnt15 <- getWord8                        
--  rollUp15 <- getWord8                        
--  tLPParamArchived15 <- getTLP                        
--  convRollUp15 <- getIeeeFloat32                        
--  histPnt16 <- getWord8                        
--  rollUp16 <- getWord8                        
--  tLPParamArchived16 <- getTLP                        
--  convRollUp16 <- getIeeeFloat32                        
  
  return $ PointType41 pointTag aTMPress calcMethodII notUse1 pipeRefTemp pipeMaterial notUse2 mtrTypeOrCdTbFtm frOrReynoldsNum orExpFactorTbFpm fpbFactor ftpFactor ftfFactor 
    fgrFactor fpvFactor histPnt1 rollUp1 tLPParamArchived1 convRollUp1 histPnt2 rollUp2 tLPParamArchived2 convRollUp2 histPnt3 rollUp3 tLPParamArchived3 convRollUp3 histPnt4 
    rollUp4 tLPParamArchived4 convRollUp4 histPnt5 rollUp5 tLPParamArchived5 convRollUp5 histPnt6 rollUp6 tLPParamArchived6 convRollUp6 histPnt7 rollUp7 tLPParamArchived7 
    convRollUp7 histPnt8 rollUp8 tLPParamArchived8 convRollUp8 histPnt9 rollUp9 tLPParamArchived9 convRollUp9 histPnt10 rollUp10 tLPParamArchived10 convRollUp10 --histPnt11 
--    rollUp11 tLPParamArchived11 convRollUp11 histPnt12 rollUp12 tLPParamArchived12 convRollUp12 histPnt13 rollUp13 tLPParamArchived13 convRollUp13 histPnt14 rollUp14 
--    tLPParamArchived14 convRollUp14 histPnt15 rollUp15 tLPParamArchived15 convRollUp15 histPnt16 rollUp16 tLPParamArchived16 convRollUp16  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  