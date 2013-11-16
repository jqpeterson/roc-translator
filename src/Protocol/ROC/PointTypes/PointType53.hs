{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType53 where

import GHC.Generics
import Data.Int
import Data.Word
import Data.Binary
import Protocol.ROC.Float
import Protocol.ROC.PointTypes.Utils

data PointType53 = PointType53 {
 
 pointType53Options                    :: !PointType53Options                                     
,pointType53ModbusStatus               :: !PointType53ModbusStatus                                          
,pointType53HighIntegerScale           :: !PointType53HighIntegerScale                                        
,pointType53LowIntegerScale            :: !PointType53LowIntegerScale                                       
,pointType53HighFloatScale1            :: !PointType53HighFloatScale1                                             
,pointType53LowFloatScale1             :: !PointType53LowFloatScale1                                                
,pointType53HighFloatScale2            :: !PointType53HighFloatScale2                                            
,pointType53LowFloatScale2             :: !PointType53LowFloatScale2                                     
,pointType53HighFloatScale3            :: !PointType53HighFloatScale3                                         
,pointType53LowFloatScale3             :: !PointType53LowFloatScale3                                     
,pointType53HighFloatScale4            :: !PointType53HighFloatScale4                                           
,pointType53LowFloatScale4             :: !PointType53LowFloatScale4                                
,pointType53HighFloatScale5            :: !PointType53HighFloatScale5                                     
,pointType53LowFloatScale5             :: !PointType53LowFloatScale5                                            
,pointType53HighFloatScale6            :: !PointType53HighFloatScale6                                     
,pointType53LowFloatScale6             :: !PointType53LowFloatScale6                                         
,pointType53HighFloatScale7            :: !PointType53HighFloatScale7                                     
,pointType53LowFloatScale7             :: !PointType53LowFloatScale7                                           
,pointType53HighFloatScale8            :: !PointType53HighFloatScale8                                
,pointType53LowFloatScale8             :: !PointType53LowFloatScale8                                     

} deriving (Read,Eq, Show, Generic)                       

type PointType53Options                = Word8                                                            
type PointType53ModbusStatus           = Bool                                                          
type PointType53HighIntegerScale       = Int16                                                  
type PointType53LowIntegerScale        = Int16                                                    
type PointType53HighFloatScale1        = Float                                                      
type PointType53LowFloatScale1         = Float                                                          
type PointType53HighFloatScale2        = Float                                                  
type PointType53LowFloatScale2         = Float                                                    
type PointType53HighFloatScale3        = Float                                                    
type PointType53LowFloatScale3         = Float                                                          
type PointType53HighFloatScale4        = Float                                                  
type PointType53LowFloatScale4         = Float                                                    
type PointType53HighFloatScale5        = Float                                                      
type PointType53LowFloatScale5         = Float                                                    
type PointType53HighFloatScale6        = Float                                                    
type PointType53LowFloatScale6         = Float                                                    
type PointType53HighFloatScale7        = Float                                                    
type PointType53LowFloatScale7         = Float                                                    
type PointType53HighFloatScale8        = Float                                                    
type PointType53LowFloatScale8         = Float                                                    
                                
pointType53Parser :: Get PointType53
pointType53Parser = do 

  options <- getWord8                                                                        
  modbusStatus <- anyButNull                                                            
  highIntegerScale <- getInt16                                                        
  lowIntegerScale <- getInt16                                                         
  highFloatScale1 <- getIeeeFloat32                                                         
  lowFloatScale1 <- getIeeeFloat32                                                          
  highFloatScale2 <- getIeeeFloat32                                                         
  lowFloatScale2 <- getIeeeFloat32                                                    
  highFloatScale3 <- getIeeeFloat32                                                   
  lowFloatScale3 <- getIeeeFloat32                                                    
  highFloatScale4 <- getIeeeFloat32                                                   
  lowFloatScale4 <- getIeeeFloat32                                                   
  highFloatScale5 <- getIeeeFloat32                                                   
  lowFloatScale5 <- getIeeeFloat32                            
  highFloatScale6 <- getIeeeFloat32                           
  lowFloatScale6 <- getIeeeFloat32                            
  highFloatScale7 <- getIeeeFloat32                           
  lowFloatScale7 <- getIeeeFloat32                            
  highFloatScale8 <- getIeeeFloat32                           
  lowFloatScale8 <- getIeeeFloat32                            
  
  return $ PointType53 options modbusStatus highIntegerScale lowIntegerScale highFloatScale1 lowFloatScale1 highFloatScale2 lowFloatScale2 highFloatScale3 lowFloatScale3 
    highFloatScale4 lowFloatScale4 highFloatScale5 lowFloatScale5 highFloatScale6 lowFloatScale6 highFloatScale7 lowFloatScale7 highFloatScale8 lowFloatScale8  
  
  