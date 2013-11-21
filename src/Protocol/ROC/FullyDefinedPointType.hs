{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.FullyDefinedPointType where

--data FullyDefinedPointType = FDPT { fdptROCType                  :: ROCType 
--                                  ,fdptConfigurationBytes        :: ConfigurationBytes
--                                  ,fdptPointTypeID               :: PointTypeID                                   
--                                  ,fdptPointNumSet               :: SetOfPointNumbers                                   
--                                  ,fdptNumberOfParameters        :: NumberOfParameters         
--                                  ,fdptStartingParameterNumber   :: StartingParameterNumber
--                                  ,fdptRxProtocol                :: (i,j,p# -> PointTypes)
--                                  } deriving (Eq,Read,Show)
 


data FullyDefinedPointType u i j id s p = FDPT { fdptROCType                   :: u
                                                ,fdptStartingParameterNumber   :: i
                                                ,fdptNumberOfParameters        :: j
                                                ,fdptPointTypeID               :: id                                                     
                                                ,fdptSetOfPointNumbers         :: s                                   
                                                ,fdptRxProtocol                :: (u -> i ->  j -> id -> s -> p)
                                                } deriving (Eq,Read,Show)
  


type ROCType                 = Word8               
type StartingParameterNumber = Word8                
type NumberOfParameters      = Word8          
type PointTypeID             = Word8          
type SetOfPointNumbers       = Maybe Int --[Word8]         


type DefaultPointType = FullyDefinedPointType ROCType StartingParameterNumber NumberOfParameters PointTypeID SetOfPointNumbers ByteString
                      
                        
type FB107PT = DefaultPointType
something = undefined
fbUnit107PT15 :: FB107PT31
fBUnit107PT15 = FDPT 107 0 26 15 Nothing something  
                
--pt31RXProtocol = defaultRXfnctn
                
--getPointType (FDPT
--            ""
--            startingParameter
--            numberOfParameters
--            pointID
--            {Valid} (Point #) 
--                              
--                              = check (Point #)(set)
--                                rxFnctn i j p# 
--                                
--defaultPTget ::  -> ByteString                                
--defaultPTget = 