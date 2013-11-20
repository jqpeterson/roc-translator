{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType59 where

import GHC.Generics
import Data.Int
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.PointTypes.Utils

data PointType59 = PointType59 {
 
 pointType59OperationProgressFlag           :: !PointType59OperationProgressFlag                            
,pointType59StatusCode                      :: !PointType59StatusCode                                                      
,pointType59ServiceRequest                  :: !PointType59ServiceRequest                                                            
,pointType59ServiceTimer                    :: !PointType59ServiceTimer                                                          
,pointType59CopyIndexFlobossCRC             :: !PointType59CopyIndexFlobossCRC                                                              
,pointType59UpdateSector                    :: !PointType59UpdateSector                                                              
,pointType59FlashPointer                    :: !PointType59FlashPointer                                                           
,pointType59RAMPointer                      :: !PointType59RAMPointer                                                        
,pointType59CmdPointerTotalBytesRcvd        :: !PointType59CmdPointerTotalBytesRcvd                                                            
,pointType59UpdateSectorAddress             :: !PointType59UpdateSectorAddress                                                        
,pointType59CopyIndex                       :: !PointType59CopyIndex                                                          
,pointType59NotUsed1                        :: !PointType59NotUsed1                                 
,pointType59NotUsed2                        :: !PointType59NotUsed2                                                                            

} deriving (Read,Eq, Show, Generic)                       

type PointType59OperationProgressFlag       =  Word8                                                                                     
type PointType59StatusCode                  =  Int8                                                                          
type PointType59ServiceRequest              =  Word16                                                                   
type PointType59ServiceTimer                =  Word16                                                                     
type PointType59CopyIndexFlobossCRC         =  Word16                                                                       
type PointType59UpdateSector                =  Word16                                                                           
type PointType59FlashPointer                =  Word32                                                                   
type PointType59RAMPointer                  =  Word32                                                                     
type PointType59CmdPointerTotalBytesRcvd    =  Word32                                                                     
type PointType59UpdateSectorAddress         =  Word32                                                                           
type PointType59CopyIndex                   =  Word16                                                                   
type PointType59NotUsed1                    =  Word8                                                                     
type PointType59NotUsed2                    =  Word8                                                                       
                                
pointType59Parser :: Get PointType59
pointType59Parser = do 

  operationProgressFlag <- getWord8                                  
  statusCode <- getInt8                                     
  serviceRequest <- getWord16le                                 
  serviceTimer <- getWord16le                                   
  copyIndexFlobossCRC <- getWord16le                            
  updateSector <- getWord16le                                   
  flashPointer <- getWord32le                                   
  rAMPointer <- getWord32le                                           
  cmdPointerTotalBytesRcvd <- getWord32le                             
  updateSectorAddress <- getWord32le                                  
  copyIndex <- getWord16le                                            
  notUsed1 <- getWord8                                              
  notUsed2 <- getWord8                                              

  
  return $ PointType59 operationProgressFlag statusCode serviceRequest serviceTimer copyIndexFlobossCRC updateSector flashPointer rAMPointer cmdPointerTotalBytesRcvd 
    updateSectorAddress copyIndex notUsed1 notUsed2  
  
  