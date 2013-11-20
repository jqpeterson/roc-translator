{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType81 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Float

data PointType81 = PointType81 {
 
 pointType81TagId                             :: !PointType81TagId                                  
,pointType81TravelOperatorStatus              :: !PointType81TravelOperatorStatus                                                 
,pointType81InletPressOperatorStatus          :: !PointType81InletPressOperatorStatus                                                           
,pointType81OutletPressOperatorStatus         :: !PointType81OutletPressOperatorStatus                                                       
,pointType81AuxPressOperatorStatus            :: !PointType81AuxPressOperatorStatus                                                                  
,pointType81FlowOperatorStatus                :: !PointType81FlowOperatorStatus                                                           
,pointType81TravelValue                       :: !PointType81TravelValue                                                        
,pointType81InletPressValue                   :: !PointType81InletPressValue                                                   
,pointType81OutletPressValue                  :: !PointType81OutletPressValue                                                                     
,pointType81AuxPressValue                     :: !PointType81AuxPressValue                                                            
,pointType81FlowValue                         :: !PointType81FlowValue                                                    
,pointType81Mode                              :: !PointType81Mode                                    
,pointType81Description                       :: !PointType81Description                                                                     

} deriving (Read,Eq, Show, Generic)                       

type  PointType81TagId                        = BS.ByteString                                                                                           
type  PointType81TravelOperatorStatus         = Float                                                                               
type  PointType81InletPressOperatorStatus     = Float                                                                          
type  PointType81OutletPressOperatorStatus    = Float                                                                            
type  PointType81AuxPressOperatorStatus       = Float                                                                              
type  PointType81FlowOperatorStatus           = Float                                                                                  
type  PointType81TravelValue                  = Float                                                                          
type  PointType81InletPressValue              = Float                                                                            
type  PointType81OutletPressValue             = Float                                                                            
type  PointType81AuxPressValue                = Float                                                                                  
type  PointType81FlowValue                    = Float                                                                          
type  PointType81Mode                         = Word16                                                                           
type  PointType81Description                  = BS.ByteString                                                                             
                                
pointType81Parser :: Get PointType81
pointType81Parser = do 

  tagId <- getByteString 10                                                                    
  travelOperatorStatus <- getIeeeFloat32                                            
  inletPressOperatorStatus <- getIeeeFloat32                                            
  outletPressOperatorStatus <- getIeeeFloat32                                           
  auxPressOperatorStatus <- getIeeeFloat32                                              
  flowOperatorStatus <- getIeeeFloat32                                                  
  travelValue <- getIeeeFloat32                                                         
  inletPressValue <- getIeeeFloat32                                                           
  outletPressValue <- getIeeeFloat32                                                          
  auxPressValue <- getIeeeFloat32                                                             
  flowValue <- getIeeeFloat32                                                                 
  mode <- getWord16le                                                                    
  description <- getByteString 20                                                             

  
  return $ PointType81 tagId travelOperatorStatus inletPressOperatorStatus outletPressOperatorStatus auxPressOperatorStatus flowOperatorStatus travelValue inletPressValue 
    outletPressValue auxPressValue flowValue mode description  
  
  