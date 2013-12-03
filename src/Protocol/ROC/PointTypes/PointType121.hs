{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType121 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get

data PointType121 = PointType121 {
  
 pointType121TagID                      :: !PointType121TagID                      
,pointType121SlaveAddress1              :: !PointType121SlaveAddress1                      
,pointType121FunctionCode1              :: !PointType121FunctionCode1                      
,pointType121SlaveRegister1             :: !PointType121SlaveRegister1                      
,pointType121MasterRegister1            :: !PointType121MasterRegister1                      
,pointType121NumOfRegisters1            :: !PointType121NumOfRegisters1                      
,pointType121CommStatus1                :: !PointType121CommStatus1                      
,pointType121SlaveAddress2              :: !PointType121SlaveAddress2                  
,pointType121FunctionCode2              :: !PointType121FunctionCode2                  
,pointType121SlaveRegister2             :: !PointType121SlaveRegister2                  
,pointType121MasterRegister2            :: !PointType121MasterRegister2                  
,pointType121NumOfRegisters2            :: !PointType121NumOfRegisters2                  
,pointType121CommStatus2                :: !PointType121CommStatus2                  
,pointType121SlaveAddress3              :: !PointType121SlaveAddress3                      
,pointType121FunctionCode3              :: !PointType121FunctionCode3                      
,pointType121SlaveRegister3             :: !PointType121SlaveRegister3                      
,pointType121MasterRegister3            :: !PointType121MasterRegister3                      
,pointType121NumOfRegisters3            :: !PointType121NumOfRegisters3                      
,pointType121CommStatus3                :: !PointType121CommStatus3                      
,pointType121SlaveAddress4              :: !PointType121SlaveAddress4                      
,pointType121FunctionCode4              :: !PointType121FunctionCode4                      
,pointType121SlaveRegister4             :: !PointType121SlaveRegister4                      
,pointType121MasterRegister4            :: !PointType121MasterRegister4                      
,pointType121NumOfRegisters4            :: !PointType121NumOfRegisters4                      
,pointType121CommStatus4                :: !PointType121CommStatus4                      
,pointType121SlaveAddress5              :: !PointType121SlaveAddress5                      
,pointType121FunctionCode5              :: !PointType121FunctionCode5                      
,pointType121SlaveRegister5             :: !PointType121SlaveRegister5                      
,pointType121MasterRegister5            :: !PointType121MasterRegister5                      
,pointType121NumOfRegisters5            :: !PointType121NumOfRegisters5                      
,pointType121CommStatus5                :: !PointType121CommStatus5                      
,pointType121SlaveAddress6              :: !PointType121SlaveAddress6                      
,pointType121FunctionCode6              :: !PointType121FunctionCode6                      
,pointType121SlaveRegister6             :: !PointType121SlaveRegister6                      
,pointType121MasterRegister6            :: !PointType121MasterRegister6                      
,pointType121NumOfRegisters6            :: !PointType121NumOfRegisters6                      
,pointType121CommStatus6                :: !PointType121CommStatus6                      
,pointType121SlaveAddress7              :: !PointType121SlaveAddress7                      
,pointType121FunctionCode7              :: !PointType121FunctionCode7                      
,pointType121SlaveRegister7             :: !PointType121SlaveRegister7                      
,pointType121MasterRegister7            :: !PointType121MasterRegister7                      
,pointType121NumOfRegisters7            :: !PointType121NumOfRegisters7                      
,pointType121CommStatus7                :: !PointType121CommStatus7                      
,pointType121SlaveAddress8              :: !PointType121SlaveAddress8                      
,pointType121FunctionCode8              :: !PointType121FunctionCode8                      
,pointType121SlaveRegister8             :: !PointType121SlaveRegister8                      
,pointType121MasterRegister8            :: !PointType121MasterRegister8                      
,pointType121NumOfRegisters8            :: !PointType121NumOfRegisters8                      
,pointType121CommStatus8                :: !PointType121CommStatus8                      
,pointType121SlaveAddress9              :: !PointType121SlaveAddress9                      
,pointType121FunctionCode9              :: !PointType121FunctionCode9                      
,pointType121SlaveRegister9             :: !PointType121SlaveRegister9                      
,pointType121MasterRegister9            :: !PointType121MasterRegister9                      
,pointType121NumOfRegisters9            :: !PointType121NumOfRegisters9                      
,pointType121CommStatus9                :: !PointType121CommStatus9                      
,pointType121SlaveAddress10             :: !PointType121SlaveAddress10                      
,pointType121FunctionCode10             :: !PointType121FunctionCode10                      
,pointType121SlaveRegister10            :: !PointType121SlaveRegister10                      
,pointType121MasterRegister10           :: !PointType121MasterRegister10                      
,pointType121NumOfRegisters10           :: !PointType121NumOfRegisters10                      
,pointType121CommStatus10               :: !PointType121CommStatus10                      
,pointType121SlaveAddress11             :: !PointType121SlaveAddress11                      
,pointType121FunctionCode11             :: !PointType121FunctionCode11                      
,pointType121SlaveRegister11            :: !PointType121SlaveRegister11                      
,pointType121MasterRegister11           :: !PointType121MasterRegister11                      
,pointType121NumOfRegisters11           :: !PointType121NumOfRegisters11                      
,pointType121CommStatus11               :: !PointType121CommStatus11                      
,pointType121SlaveAddress12             :: !PointType121SlaveAddress12                      
,pointType121FunctionCode12             :: !PointType121FunctionCode12                      
,pointType121SlaveRegister12            :: !PointType121SlaveRegister12                      
,pointType121MasterRegister12           :: !PointType121MasterRegister12                      
,pointType121NumOfRegisters12           :: !PointType121NumOfRegisters12                      
,pointType121CommStatus12               :: !PointType121CommStatus12                      
,pointType121SlaveAddress13             :: !PointType121SlaveAddress13                      
,pointType121FunctionCode13             :: !PointType121FunctionCode13                      
,pointType121SlaveRegister13            :: !PointType121SlaveRegister13                      
,pointType121MasterRegister13           :: !PointType121MasterRegister13                      
,pointType121NumOfRegisters13           :: !PointType121NumOfRegisters13                      
,pointType121CommStatus13               :: !PointType121CommStatus13                      
,pointType121SlaveAddress14             :: !PointType121SlaveAddress14                      
,pointType121FunctionCode14             :: !PointType121FunctionCode14                      
,pointType121SlaveRegister14            :: !PointType121SlaveRegister14                      
,pointType121MasterRegister14           :: !PointType121MasterRegister14                      
,pointType121NumOfRegisters14           :: !PointType121NumOfRegisters14                      
,pointType121CommStatus14               :: !PointType121CommStatus14                      
,pointType121SlaveAddress15             :: !PointType121SlaveAddress15                      
,pointType121FunctionCode15             :: !PointType121FunctionCode15                      
,pointType121SlaveRegister15            :: !PointType121SlaveRegister15                      
,pointType121MasterRegister15           :: !PointType121MasterRegister15                      
,pointType121NumOfRegisters15           :: !PointType121NumOfRegisters15                      
,pointType121CommStatus15               :: !PointType121CommStatus15                      
,pointType121SlaveAddress16             :: !PointType121SlaveAddress16                      
,pointType121FunctionCode16             :: !PointType121FunctionCode16                      
,pointType121SlaveRegister16            :: !PointType121SlaveRegister16                      
,pointType121MasterRegister16           :: !PointType121MasterRegister16                      
,pointType121NumOfRegisters16           :: !PointType121NumOfRegisters16                      
,pointType121CommStatus16               :: !PointType121CommStatus16                      
,pointType121SlaveAddress17             :: !PointType121SlaveAddress17                      
,pointType121FunctionCode17             :: !PointType121FunctionCode17                      
,pointType121SlaveRegister17            :: !PointType121SlaveRegister17                      
,pointType121MasterRegister17           :: !PointType121MasterRegister17                      
,pointType121NumOfRegisters17           :: !PointType121NumOfRegisters17                      
,pointType121CommStatus17               :: !PointType121CommStatus17                      
,pointType121SlaveAddress18             :: !PointType121SlaveAddress18                      
,pointType121FunctionCode18             :: !PointType121FunctionCode18                      
,pointType121SlaveRegister18            :: !PointType121SlaveRegister18                      
,pointType121MasterRegister18           :: !PointType121MasterRegister18                      
,pointType121NumOfRegisters18           :: !PointType121NumOfRegisters18                      
,pointType121CommStatus18               :: !PointType121CommStatus18                      
,pointType121SlaveAddress19             :: !PointType121SlaveAddress19                      
,pointType121FunctionCode19             :: !PointType121FunctionCode19                      
,pointType121SlaveRegister19            :: !PointType121SlaveRegister19                      
,pointType121MasterRegister19           :: !PointType121MasterRegister19                      
,pointType121NumOfRegisters19           :: !PointType121NumOfRegisters19                      
,pointType121CommStatus19               :: !PointType121CommStatus19                      
,pointType121SlaveAddress20             :: !PointType121SlaveAddress20                      
,pointType121FunctionCode20             :: !PointType121FunctionCode20                      
,pointType121SlaveRegister20            :: !PointType121SlaveRegister20                      
,pointType121MasterRegister20           :: !PointType121MasterRegister20                      
,pointType121NumOfRegisters20           :: !PointType121NumOfRegisters20                      
,pointType121CommStatus20               :: !PointType121CommStatus20                      
,pointType121SlaveAddress21             :: !PointType121SlaveAddress21                      
,pointType121FunctionCode21             :: !PointType121FunctionCode21                      
,pointType121SlaveRegister21            :: !PointType121SlaveRegister21                      
,pointType121MasterRegister21           :: !PointType121MasterRegister21                      
,pointType121NumOfRegisters21           :: !PointType121NumOfRegisters21                      
,pointType121CommStatus21               :: !PointType121CommStatus21                      
,pointType121SlaveAddress22             :: !PointType121SlaveAddress22                      
,pointType121FunctionCode22             :: !PointType121FunctionCode22                      
,pointType121SlaveRegister22            :: !PointType121SlaveRegister22                      
,pointType121MasterRegister22           :: !PointType121MasterRegister22                      
,pointType121NumOfRegisters22           :: !PointType121NumOfRegisters22                      
,pointType121CommStatus22               :: !PointType121CommStatus22                      
,pointType121SlaveAddress23             :: !PointType121SlaveAddress23                      
,pointType121FunctionCode23             :: !PointType121FunctionCode23                      
,pointType121SlaveRegister23            :: !PointType121SlaveRegister23                      
,pointType121MasterRegister23           :: !PointType121MasterRegister23                      
,pointType121NumOfRegisters23           :: !PointType121NumOfRegisters23                      
,pointType121CommStatus23               :: !PointType121CommStatus23                      
,pointType121SlaveAddress24             :: !PointType121SlaveAddress24                      
,pointType121FunctionCode24             :: !PointType121FunctionCode24                      
,pointType121SlaveRegister24            :: !PointType121SlaveRegister24                      
,pointType121MasterRegister24           :: !PointType121MasterRegister24                      
,pointType121NumOfRegisters24           :: !PointType121NumOfRegisters24                      
,pointType121CommStatus24               :: !PointType121CommStatus24                      
,pointType121SlaveAddress25             :: !PointType121SlaveAddress25                      
,pointType121FunctionCode25             :: !PointType121FunctionCode25                      
,pointType121SlaveRegister25            :: !PointType121SlaveRegister25                      
,pointType121MasterRegister25           :: !PointType121MasterRegister25                      
,pointType121NumOfRegisters25           :: !PointType121NumOfRegisters25                      
,pointType121CommStatus25               :: !PointType121CommStatus25                      

  
} deriving (Read,Eq, Show, Generic)                       
                                  



type PointType121TagID              = BS.ByteString       
type PointType121SlaveAddress1      = Word8       
type PointType121FunctionCode1      = Word8       
type PointType121SlaveRegister1     = Word16       
type PointType121MasterRegister1    = Word16       
type PointType121NumOfRegisters1    = Word8       
type PointType121CommStatus1        = Word8       
type PointType121SlaveAddress2      = Word8        
type PointType121FunctionCode2      = Word8        
type PointType121SlaveRegister2     = Word16       
type PointType121MasterRegister2    = Word16       
type PointType121NumOfRegisters2    = Word8        
type PointType121CommStatus2        = Word8        
type PointType121SlaveAddress3      = Word8        
type PointType121FunctionCode3      = Word8        
type PointType121SlaveRegister3     = Word16       
type PointType121MasterRegister3    = Word16       
type PointType121NumOfRegisters3    = Word8        
type PointType121CommStatus3        = Word8        
type PointType121SlaveAddress4      = Word8        
type PointType121FunctionCode4      = Word8        
type PointType121SlaveRegister4     = Word16       
type PointType121MasterRegister4    = Word16       
type PointType121NumOfRegisters4    = Word8        
type PointType121CommStatus4        = Word8        
type PointType121SlaveAddress5      = Word8        
type PointType121FunctionCode5      = Word8        
type PointType121SlaveRegister5     = Word16       
type PointType121MasterRegister5    = Word16       
type PointType121NumOfRegisters5    = Word8        
type PointType121CommStatus5        = Word8        
type PointType121SlaveAddress6      = Word8        
type PointType121FunctionCode6      = Word8        
type PointType121SlaveRegister6     = Word16       
type PointType121MasterRegister6    = Word16       
type PointType121NumOfRegisters6    = Word8        
type PointType121CommStatus6        = Word8        
type PointType121SlaveAddress7      = Word8        
type PointType121FunctionCode7      = Word8        
type PointType121SlaveRegister7     = Word16       
type PointType121MasterRegister7    = Word16       
type PointType121NumOfRegisters7    = Word8        
type PointType121CommStatus7        = Word8        
type PointType121SlaveAddress8      = Word8        
type PointType121FunctionCode8      = Word8        
type PointType121SlaveRegister8     = Word16       
type PointType121MasterRegister8    = Word16       
type PointType121NumOfRegisters8    = Word8        
type PointType121CommStatus8        = Word8        
type PointType121SlaveAddress9      = Word8        
type PointType121FunctionCode9      = Word8        
type PointType121SlaveRegister9     = Word16       
type PointType121MasterRegister9    = Word16       
type PointType121NumOfRegisters9    = Word8        
type PointType121CommStatus9        = Word8        
type PointType121SlaveAddress10     = Word8        
type PointType121FunctionCode10     = Word8        
type PointType121SlaveRegister10    = Word16       
type PointType121MasterRegister10   = Word16       
type PointType121NumOfRegisters10   = Word8        
type PointType121CommStatus10       = Word8        
type PointType121SlaveAddress11     = Word8        
type PointType121FunctionCode11     = Word8        
type PointType121SlaveRegister11    = Word16       
type PointType121MasterRegister11   = Word16       
type PointType121NumOfRegisters11   = Word8        
type PointType121CommStatus11       = Word8        
type PointType121SlaveAddress12     = Word8        
type PointType121FunctionCode12     = Word8        
type PointType121SlaveRegister12    = Word16       
type PointType121MasterRegister12   = Word16       
type PointType121NumOfRegisters12   = Word8        
type PointType121CommStatus12       = Word8        
type PointType121SlaveAddress13     = Word8        
type PointType121FunctionCode13     = Word8        
type PointType121SlaveRegister13    = Word16       
type PointType121MasterRegister13   = Word16       
type PointType121NumOfRegisters13   = Word8        
type PointType121CommStatus13       = Word8        
type PointType121SlaveAddress14     = Word8        
type PointType121FunctionCode14     = Word8        
type PointType121SlaveRegister14    = Word16       
type PointType121MasterRegister14   = Word16       
type PointType121NumOfRegisters14   = Word8        
type PointType121CommStatus14       = Word8        
type PointType121SlaveAddress15     = Word8        
type PointType121FunctionCode15     = Word8        
type PointType121SlaveRegister15    = Word16       
type PointType121MasterRegister15   = Word16       
type PointType121NumOfRegisters15   = Word8        
type PointType121CommStatus15       = Word8        
type PointType121SlaveAddress16     = Word8        
type PointType121FunctionCode16     = Word8        
type PointType121SlaveRegister16    = Word16       
type PointType121MasterRegister16   = Word16       
type PointType121NumOfRegisters16   = Word8        
type PointType121CommStatus16       = Word8        
type PointType121SlaveAddress17     = Word8        
type PointType121FunctionCode17     = Word8        
type PointType121SlaveRegister17    = Word16       
type PointType121MasterRegister17   = Word16       
type PointType121NumOfRegisters17   = Word8        
type PointType121CommStatus17       = Word8        
type PointType121SlaveAddress18     = Word8        
type PointType121FunctionCode18     = Word8        
type PointType121SlaveRegister18    = Word16       
type PointType121MasterRegister18   = Word16       
type PointType121NumOfRegisters18   = Word8        
type PointType121CommStatus18       = Word8        
type PointType121SlaveAddress19     = Word8        
type PointType121FunctionCode19     = Word8        
type PointType121SlaveRegister19    = Word16       
type PointType121MasterRegister19   = Word16       
type PointType121NumOfRegisters19   = Word8        
type PointType121CommStatus19       = Word8        
type PointType121SlaveAddress20     = Word8        
type PointType121FunctionCode20     = Word8        
type PointType121SlaveRegister20    = Word16       
type PointType121MasterRegister20   = Word16       
type PointType121NumOfRegisters20   = Word8        
type PointType121CommStatus20       = Word8        
type PointType121SlaveAddress21     = Word8        
type PointType121FunctionCode21     = Word8        
type PointType121SlaveRegister21    = Word16       
type PointType121MasterRegister21   = Word16       
type PointType121NumOfRegisters21   = Word8        
type PointType121CommStatus21       = Word8        
type PointType121SlaveAddress22     = Word8        
type PointType121FunctionCode22     = Word8        
type PointType121SlaveRegister22    = Word16       
type PointType121MasterRegister22   = Word16       
type PointType121NumOfRegisters22   = Word8        
type PointType121CommStatus22       = Word8        
type PointType121SlaveAddress23     = Word8        
type PointType121FunctionCode23     = Word8        
type PointType121SlaveRegister23    = Word16       
type PointType121MasterRegister23   = Word16       
type PointType121NumOfRegisters23   = Word8        
type PointType121CommStatus23       = Word8        
type PointType121SlaveAddress24     = Word8        
type PointType121FunctionCode24     = Word8        
type PointType121SlaveRegister24    = Word16       
type PointType121MasterRegister24   = Word16       
type PointType121NumOfRegisters24   = Word8        
type PointType121CommStatus24       = Word8        
type PointType121SlaveAddress25     = Word8        
type PointType121FunctionCode25     = Word8        
type PointType121SlaveRegister25    = Word16       
type PointType121MasterRegister25   = Word16       
type PointType121NumOfRegisters25   = Word8        
type PointType121CommStatus25       = Word8        


pointType121Parser :: Get PointType121
pointType121Parser = do 
                                  
  tagID <- getByteString 10               
  slaveAddress1 <- getWord8       
  functionCode1 <- getWord8       
  slaveRegister1 <- getWord16le      
  masterRegister1 <- getWord16le     
  numOfRegisters1 <- getWord8     
  commStatus1 <- getWord8         
  slaveAddress2 <- getWord8       
  functionCode2 <- getWord8       
  slaveRegister2 <- getWord16le      
  masterRegister2 <- getWord16le     
  numOfRegisters2 <- getWord8     
  commStatus2 <- getWord8         
  slaveAddress3 <- getWord8       
  functionCode3 <- getWord8       
  slaveRegister3 <- getWord16le      
  masterRegister3 <- getWord16le     
  numOfRegisters3 <- getWord8     
  commStatus3 <- getWord8         
  slaveAddress4 <- getWord8       
  functionCode4 <- getWord8       
  slaveRegister4 <- getWord16le      
  masterRegister4 <- getWord16le     
  numOfRegisters4 <- getWord8     
  commStatus4 <- getWord8         
  slaveAddress5 <- getWord8       
  functionCode5 <- getWord8       
  slaveRegister5 <- getWord16le      
  masterRegister5 <- getWord16le     
  numOfRegisters5 <- getWord8     
  commStatus5 <- getWord8         
  slaveAddress6 <- getWord8       
  functionCode6 <- getWord8       
  slaveRegister6 <- getWord16le      
  masterRegister6 <- getWord16le     
  numOfRegisters6 <- getWord8     
  commStatus6 <- getWord8         
  slaveAddress7 <- getWord8       
  functionCode7 <- getWord8       
  slaveRegister7 <- getWord16le      
  masterRegister7 <- getWord16le     
  numOfRegisters7 <- getWord8     
  commStatus7 <- getWord8         
  slaveAddress8 <- getWord8       
  functionCode8 <- getWord8       
  slaveRegister8 <- getWord16le      
  masterRegister8 <- getWord16le     
  numOfRegisters8 <- getWord8     
  commStatus8 <- getWord8         
  slaveAddress9 <- getWord8       
  functionCode9 <- getWord8       
  slaveRegister9 <- getWord16le      
  masterRegister9 <- getWord16le     
  numOfRegisters9 <- getWord8     
  commStatus9 <- getWord8         
  slaveAddress10 <- getWord8      
  functionCode10 <- getWord8      
  slaveRegister10 <- getWord16le     
  masterRegister10 <- getWord16le    
  numOfRegisters10 <- getWord8    
  commStatus10 <- getWord8        
  slaveAddress11 <- getWord8      
  functionCode11 <- getWord8      
  slaveRegister11 <- getWord16le     
  masterRegister11 <- getWord16le    
  numOfRegisters11 <- getWord8    
  commStatus11 <- getWord8        
  slaveAddress12 <- getWord8      
  functionCode12 <- getWord8      
  slaveRegister12 <- getWord16le     
  masterRegister12 <- getWord16le    
  numOfRegisters12 <- getWord8    
  commStatus12 <- getWord8        
  slaveAddress13 <- getWord8      
  functionCode13 <- getWord8      
  slaveRegister13 <- getWord16le     
  masterRegister13 <- getWord16le    
  numOfRegisters13 <- getWord8    
  commStatus13 <- getWord8        
  slaveAddress14 <- getWord8      
  functionCode14 <- getWord8      
  slaveRegister14 <- getWord16le     
  masterRegister14 <- getWord16le    
  numOfRegisters14 <- getWord8    
  commStatus14 <- getWord8        
  slaveAddress15 <- getWord8      
  functionCode15 <- getWord8      
  slaveRegister15 <- getWord16le     
  masterRegister15 <- getWord16le    
  numOfRegisters15 <- getWord8    
  commStatus15 <- getWord8        
  slaveAddress16 <- getWord8      
  functionCode16 <- getWord8      
  slaveRegister16 <- getWord16le     
  masterRegister16 <- getWord16le    
  numOfRegisters16 <- getWord8    
  commStatus16 <- getWord8        
  slaveAddress17 <- getWord8      
  functionCode17 <- getWord8      
  slaveRegister17 <- getWord16le     
  masterRegister17 <- getWord16le    
  numOfRegisters17 <- getWord8    
  commStatus17 <- getWord8        
  slaveAddress18 <- getWord8      
  functionCode18 <- getWord8      
  slaveRegister18 <- getWord16le     
  masterRegister18 <- getWord16le    
  numOfRegisters18 <- getWord8    
  commStatus18 <- getWord8        
  slaveAddress19 <- getWord8      
  functionCode19 <- getWord8      
  slaveRegister19 <- getWord16le     
  masterRegister19 <- getWord16le    
  numOfRegisters19 <- getWord8    
  commStatus19 <- getWord8        
  slaveAddress20 <- getWord8      
  functionCode20 <- getWord8      
  slaveRegister20 <- getWord16le     
  masterRegister20 <- getWord16le    
  numOfRegisters20 <- getWord8    
  commStatus20 <- getWord8        
  slaveAddress21 <- getWord8      
  functionCode21 <- getWord8      
  slaveRegister21 <- getWord16le     
  masterRegister21 <- getWord16le    
  numOfRegisters21 <- getWord8    
  commStatus21 <- getWord8        
  slaveAddress22 <- getWord8      
  functionCode22 <- getWord8      
  slaveRegister22 <- getWord16le     
  masterRegister22 <- getWord16le    
  numOfRegisters22 <- getWord8    
  commStatus22 <- getWord8        
  slaveAddress23 <- getWord8      
  functionCode23 <- getWord8      
  slaveRegister23 <- getWord16le     
  masterRegister23 <- getWord16le    
  numOfRegisters23 <- getWord8    
  commStatus23 <- getWord8        
  slaveAddress24 <- getWord8      
  functionCode24 <- getWord8      
  slaveRegister24 <- getWord16le     
  masterRegister24 <- getWord16le    
  numOfRegisters24 <- getWord8    
  commStatus24 <- getWord8        
  slaveAddress25 <- getWord8      
  functionCode25 <- getWord8      
  slaveRegister25 <- getWord16le     
  masterRegister25 <- getWord16le    
  numOfRegisters25 <- getWord8    
  commStatus25 <- getWord8        
  
  
  return $ PointType121 tagID slaveAddress1 functionCode1 slaveRegister1 masterRegister1 numOfRegisters1 commStatus1 slaveAddress2 functionCode2 slaveRegister2 masterRegister2 
    numOfRegisters2 commStatus2 slaveAddress3 functionCode3 slaveRegister3 masterRegister3 numOfRegisters3 commStatus3 slaveAddress4 functionCode4 slaveRegister4 masterRegister4 
    numOfRegisters4 commStatus4 slaveAddress5 functionCode5 slaveRegister5 masterRegister5 numOfRegisters5 commStatus5 slaveAddress6 functionCode6 slaveRegister6 masterRegister6 
    numOfRegisters6 commStatus6 slaveAddress7 functionCode7 slaveRegister7 masterRegister7 numOfRegisters7 commStatus7 slaveAddress8 functionCode8 slaveRegister8 masterRegister8 
    numOfRegisters8 commStatus8 slaveAddress9 functionCode9 slaveRegister9 masterRegister9 numOfRegisters9 commStatus9 slaveAddress10 functionCode10 slaveRegister10 masterRegister10 
    numOfRegisters10 commStatus10 slaveAddress11 functionCode11 slaveRegister11 masterRegister11 numOfRegisters11 commStatus11 slaveAddress12 functionCode12 slaveRegister12 
    masterRegister12 numOfRegisters12 commStatus12 slaveAddress13 functionCode13 slaveRegister13 masterRegister13 numOfRegisters13 commStatus13 slaveAddress14 functionCode14 
    slaveRegister14 masterRegister14 numOfRegisters14 commStatus14 slaveAddress15 functionCode15 slaveRegister15 masterRegister15 numOfRegisters15 commStatus15 slaveAddress16 
    functionCode16 slaveRegister16 masterRegister16 numOfRegisters16 commStatus16 slaveAddress17 functionCode17 slaveRegister17 masterRegister17 numOfRegisters17 commStatus17 
    slaveAddress18 functionCode18 slaveRegister18 masterRegister18 numOfRegisters18 commStatus18 slaveAddress19 functionCode19 slaveRegister19 masterRegister19 numOfRegisters19 
    commStatus19 slaveAddress20 functionCode20 slaveRegister20 masterRegister20 numOfRegisters20 commStatus20 slaveAddress21 functionCode21 slaveRegister21 masterRegister21 
    numOfRegisters21 commStatus21 slaveAddress22 functionCode22 slaveRegister22 masterRegister22 numOfRegisters22 commStatus22 slaveAddress23 functionCode23 slaveRegister23 
    masterRegister23 numOfRegisters23 commStatus23 slaveAddress24 functionCode24 slaveRegister24 masterRegister24 numOfRegisters24 commStatus24 slaveAddress25 functionCode25 
    slaveRegister25 masterRegister25 numOfRegisters25 commStatus25  