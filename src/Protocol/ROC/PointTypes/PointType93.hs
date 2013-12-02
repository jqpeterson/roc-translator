{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType93 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
import Data.Binary.Get
import Protocol.ROC.Utils
import Data.Time.Clock.POSIX

data PointType93 = PointType93 {
 
 pointType93LicenseInstallationStatus                :: !PointType93LicenseInstallationStatus                         
,pointType93LicenseNumber                            :: !PointType93LicenseNumber                             
,pointType93ApplicationName                          :: !PointType93ApplicationName                                      
,pointType93ApplicationProvider                      :: !PointType93ApplicationProvider                          
,pointType93ApplicationCode                          :: !PointType93ApplicationCode                           
,pointType93ApplicationVersion                       :: !PointType93ApplicationVersion                             
,pointType93QuantityTotal                            :: !PointType93QuantityTotal                                   
,pointType93QuantityRemaining                        :: !PointType93QuantityRemaining                                   
,pointType93ExpirationData                           :: !PointType93ExpirationData                             
,pointType93LicenseValidityState                     :: !PointType93LicenseValidityState                                   
,pointType93LiceseCreationDate                       :: !PointType93LiceseCreationDate                                   

} deriving (Eq, Show, Generic)                       

type PointType93LicenseInstallationStatus            = Bool                  
type PointType93LicenseNumber                        = Word8                  
type PointType93ApplicationName                      = BS.ByteString                    
type PointType93ApplicationProvider                  = BS.ByteString                          
type PointType93ApplicationCode                      = Word16                          
type PointType93ApplicationVersion                   = BS.ByteString                                                 
type PointType93QuantityTotal                        = Word8                  
type PointType93QuantityRemaining                    = Word8                  
type PointType93ExpirationData                       = POSIXTime                                                 
type PointType93LicenseValidityState                 = Word8                  
type PointType93LiceseCreationDate                   = POSIXTime                    

pointType93Parser :: Get PointType93
pointType93Parser = do 

  licenseInstallationStatus <- anyButNull
  licenseNumber <- getWord8 
  applicationName <- getByteString 20
  applicationProvider <- getByteString 20
  applicationCode <- getWord16le
  applicationVersion <- getByteString 10 
  quantityTotal <- getWord8 
  quantityRemaining <- getWord8 
  expirationData <- getPosixTime 
  licenseValidityState <- getWord8 
  liceseCreationDate <- getPosixTime 
  
  
  return $ PointType93 licenseInstallationStatus licenseNumber applicationName applicationProvider applicationCode applicationVersion quantityTotal quantityRemaining 
    expirationData licenseValidityState liceseCreationDate  