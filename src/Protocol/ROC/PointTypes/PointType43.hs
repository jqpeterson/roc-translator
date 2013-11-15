{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType43 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.PointTypes.Utils

data PointType43 = PointType43 {
 pointType43Text1                     :: !PointType43Text1                                   
,pointType43Text2                     :: !PointType43Text2                                            
,pointType43Text3                     :: !PointType43Text3                                                 
,pointType43Text4                     :: !PointType43Text4                                                    
,pointType43Text5                     :: !PointType43Text5                                         
,pointType43Text6                     :: !PointType43Text6                                            
,pointType43Text7                     :: !PointType43Text7                                                
,pointType43Text8                     :: !PointType43Text8                                          
,pointType43Text9                     :: !PointType43Text9                                        
,pointType43Text10                    :: !PointType43Text10                                                   
,pointType43Text11                    :: !PointType43Text11                                                
,pointType43Text12                    :: !PointType43Text12                                                      
,pointType43Text13                    :: !PointType43Text13                                           
,pointType43Text14                    :: !PointType43Text14                                                                              
,pointType43Text15                    :: !PointType43Text15                                        
,pointType43Text16                    :: !PointType43Text16                     
,pointType43Data1                     :: !PointType43Data1                             
,pointType43Data2                     :: !PointType43Data2                                 
,pointType43Data3                     :: !PointType43Data3                                    
,pointType43Data4                     :: !PointType43Data4                         
,pointType43Data5                     :: !PointType43Data5                            
,pointType43Data6                     :: !PointType43Data6                                 
,pointType43Data7                     :: !PointType43Data7                                            
,pointType43Data8                     :: !PointType43Data8                       
,pointType43Data9                     :: !PointType43Data9                                   
,pointType43Data10                    :: !PointType43Data10                                 
,pointType43Data11                    :: !PointType43Data11                                    
,pointType43Data12                    :: !PointType43Data12                                  
,pointType43Data13                    :: !PointType43Data13                                
,pointType43Data14                    :: !PointType43Data14                                   
,pointType43Data15                    :: !PointType43Data15                                            
,pointType43Data16                    :: !PointType43Data16                                                 
,pointType43AuxUnitsString1           :: !PointType43AuxUnitsString1                                                    
,pointType43AuxUnitsString2           :: !PointType43AuxUnitsString2                                         
,pointType43AuxUnitsString3           :: !PointType43AuxUnitsString3                                            
,pointType43AuxUnitsString4           :: !PointType43AuxUnitsString4                                                
,pointType43AuxUnitsString5           :: !PointType43AuxUnitsString5                                          
,pointType43AuxUnitsString6           :: !PointType43AuxUnitsString6                                        
,pointType43AuxUnitsString7           :: !PointType43AuxUnitsString7                                                   
,pointType43AuxUnitsString8           :: !PointType43AuxUnitsString8                                                
,pointType43AuxUnitsString9           :: !PointType43AuxUnitsString9                                                      
,pointType43AuxUnitsString10          :: !PointType43AuxUnitsString10                                          
,pointType43AuxUnitsString11          :: !PointType43AuxUnitsString11                                                                              
,pointType43AuxUnitsString12          :: !PointType43AuxUnitsString12                                        
,pointType43AuxUnitsString13          :: !PointType43AuxUnitsString13                     
,pointType43AuxUnitsString14          :: !PointType43AuxUnitsString14                            
,pointType43AuxUnitsString15          :: !PointType43AuxUnitsString15                                 
,pointType43AuxUnitsString16          :: !PointType43AuxUnitsString16                                    
,pointType43UserListTitle             :: !PointType43UserListTitle                         
,pointType43ScrollTimeinSec           :: !PointType43ScrollTimeinSec                            
                                    

} deriving (Read,Eq, Show, Generic)                       

type PointType43Text1                  = BS.ByteString                                              
type PointType43Text2                  = BS.ByteString                                     
type PointType43Text3                  = BS.ByteString                                    
type PointType43Text4                  = BS.ByteString                                    
type PointType43Text5                  = BS.ByteString                                      
type PointType43Text6                  = BS.ByteString                                      
type PointType43Text7                  = BS.ByteString                                       
type PointType43Text8                  = BS.ByteString                                       
type PointType43Text9                  = BS.ByteString                                      
type PointType43Text10                 = BS.ByteString                                      
type PointType43Text11                 = BS.ByteString                                       
type PointType43Text12                 = BS.ByteString                                       
type PointType43Text13                 = BS.ByteString                                      
type PointType43Text14                 = BS.ByteString                                      
type PointType43Text15                 = BS.ByteString                                    
type PointType43Text16                 = BS.ByteString                                      
type PointType43Data1                  = [Word8]                             
type PointType43Data2                  = [Word8]                                 
type PointType43Data3                  = [Word8]                                 
type PointType43Data4                  = [Word8]                                   
type PointType43Data5                  = [Word8]                             
type PointType43Data6                  = [Word8]                                 
type PointType43Data7                  = [Word8]                                 
type PointType43Data8                  = [Word8]                                   
type PointType43Data9                  = [Word8]                             
type PointType43Data10                 = [Word8]                                 
type PointType43Data11                 = [Word8]                                 
type PointType43Data12                 = [Word8]                                   
type PointType43Data13                 = [Word8]                             
type PointType43Data14                 = [Word8]                                           
type PointType43Data15                 = [Word8]                                  
type PointType43Data16                 = [Word8]                                 
type PointType43AuxUnitsString1        = BS.ByteString                                   
type PointType43AuxUnitsString2        = BS.ByteString                                      
type PointType43AuxUnitsString3        = BS.ByteString                                      
type PointType43AuxUnitsString4        = BS.ByteString                                       
type PointType43AuxUnitsString5        = BS.ByteString                                       
type PointType43AuxUnitsString6        = BS.ByteString                                      
type PointType43AuxUnitsString7        = BS.ByteString                                      
type PointType43AuxUnitsString8        = BS.ByteString                                       
type PointType43AuxUnitsString9        = BS.ByteString                                       
type PointType43AuxUnitsString10       = BS.ByteString                                      
type PointType43AuxUnitsString11       = BS.ByteString                                      
type PointType43AuxUnitsString12       = BS.ByteString                                    
type PointType43AuxUnitsString13       = BS.ByteString                                      
type PointType43AuxUnitsString14       = BS.ByteString                                
type PointType43AuxUnitsString15       = BS.ByteString                                    
type PointType43AuxUnitsString16       = BS.ByteString                                    
type PointType43UserListTitle          = BS.ByteString                                      
type PointType43ScrollTimeinSec        = Word8                                  

pointType43Parser :: Get PointType43 
pointType43Parser = do 
         
  text1 <- getByteString 10                                                  
  text2 <- getByteString 10                                              
  text3 <- getByteString 10                                            
  text4 <- getByteString 10                                                
  text5 <- getByteString 10                                                
  text6 <- getByteString 10                                              
  text7 <- getByteString 10                                              
  text8 <- getByteString 10                                                
  text9 <- getByteString 10                                                
  text10 <- getByteString 10                                             
  text11 <- getByteString 10                                           
  text12 <- getByteString 10                                               
  text13 <- getByteString 10                                               
  text14 <- getByteString 10                                             
  text15 <- getByteString 10                                           
  text16 <- getByteString 10                                               
  data1 <- getTLP                                                
  data2 <- getTLP                                              
  data3 <- getTLP                                            
  data4 <- getTLP                                                
  data5 <- getTLP                                                
  data6 <- getTLP                                              
  data7 <- getTLP                                            
  data8 <- getTLP                                                
  data9 <- getTLP                                                
  data10 <- getTLP                                             
  data11 <- getTLP                                           
  data12 <- getTLP                                               
  data13 <- getTLP                                               
  data14 <- getTLP                 
  data15 <- getTLP                 
  data16 <- getTLP                 
  auxUnitsString1 <- getByteString 10        
  auxUnitsString2 <- getByteString 10        
  auxUnitsString3 <- getByteString 10        
  auxUnitsString4 <- getByteString 10        
  auxUnitsString5 <- getByteString 10        
  auxUnitsString6 <- getByteString 10        
  auxUnitsString7 <- getByteString 10        
  auxUnitsString8 <- getByteString 10        
  auxUnitsString9 <- getByteString 10        
  auxUnitsString10 <- getByteString 10       
  auxUnitsString11 <- getByteString 10       
  auxUnitsString12 <- getByteString 10       
  auxUnitsString13 <- getByteString 10       
  auxUnitsString14 <- getByteString 10       
  auxUnitsString15 <- getByteString 10       
  auxUnitsString16 <- getByteString 10       
  userListTitle <- getByteString 10          
  scrollTimeinSec <- getWord8        
       
  return $ PointType43 text1 text2 text3 text4 text5 text6 text7 text8 text9 text10 text11 text12 text13 text14 text15 text16 data1 data2 data3 data4 data5 data6 data7 data8
    data9 data10 data11 data12 data13 data14 data15 data16 auxUnitsString1 auxUnitsString2 auxUnitsString3 auxUnitsString4 auxUnitsString5 auxUnitsString6 auxUnitsString7
    auxUnitsString8 auxUnitsString9 auxUnitsString10 auxUnitsString11 auxUnitsString12 auxUnitsString13 auxUnitsString14 auxUnitsString15 auxUnitsString16 userListTitle scrollTimeinSec  