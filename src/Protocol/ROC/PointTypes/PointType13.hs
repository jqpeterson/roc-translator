{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType13 where

import GHC.Generics
import Data.Word
import Data.Binary
import Protocol.ROC.PointTypes.Utils

data PointType13 = PointType13 {
 pointType13CRCCheck                    :: !PointType13CRCCheck                                  
,pointType13DiPi                        :: !PointType13DiPi                               
,pointType13EnableLCDCalc2Prog          :: !PointType13EnableLCDCalc2Prog                               
,pointType13EnableLOICalc2Prog          :: !PointType13EnableLOICalc2Prog                               
,pointType13ClearFSTDisplay             :: !PointType13ClearFSTDisplay                               
,pointType13EnableComm1UserProg         :: !PointType13EnableComm1UserProg                               
,pointType13EnableComm2UserProg         :: !PointType13EnableComm2UserProg                               
,pointType13EnableUserCalcProg          :: !PointType13EnableUserCalcProg                               
,pointType13LOIRTSTetSeconds            :: !PointType13LOIRTSTetSeconds                              
,pointType13Comm1RTSTestSeconds         :: !PointType13Comm1RTSTestSeconds                               
,pointType13Comm2RTSTestSeconds         :: !PointType13Comm2RTSTestSeconds                               
,pointType13ClearCFGMem                 :: !PointType13ClearCFGMem                               
,pointType13EnableScanIO                :: !PointType13EnableScanIO                               
,pointType13EnableAuxOut2               :: !PointType13EnableAuxOut2                               
,pointType13AuxOut1OnOrPlusTVolt        :: !PointType13AuxOut1OnOrPlusTVolt                               
,pointType13ColdStartOption             :: !PointType13ColdStartOption                               
,pointType13WarmStart                   :: !PointType13WarmStart                               
,pointType13ReadInOut                   :: !PointType13ReadInOut                               
,pointType13WriteCFGMem                 :: !PointType13WriteCFGMem                               
,pointType13CFGMemWriteCmpl             :: !PointType13CFGMemWriteCmpl                               
,pointType13EnableEventLogorInitHist    :: !PointType13EnableEventLogorInitHist                               
,pointType13MngLOISec                   :: !PointType13MngLOISec                               
,pointType13MngCommPort1Sec             :: !PointType13MngCommPort1Sec                               
,pointType13MngCommPort2Sec             :: !PointType13MngCommPort2Sec                               
,pointType13TermTypeInstdOrMngLCDSec    :: !PointType13TermTypeInstdOrMngLCDSec                               
,pointType13CommPassThroughMode         :: !PointType13CommPassThroughMode                               
,pointType13Mng6PointIOSetupFlag        :: !PointType13Mng6PointIOSetupFlag                               
,pointType13MngCommPort3Sec             :: !PointType13MngCommPort3Sec                               
,pointType13Comm3RTSTestSeconds         :: !PointType13Comm3RTSTestSeconds                               
,pointType13MngCFGNumDailyHistLogs      :: !PointType13MngCFGNumDailyHistLogs                               
,pointType13MngHistTImeStampOpt         :: !PointType13MngHistTImeStampOpt                               
,pointType13MngCFGNumDailyHitLogs       :: !PointType13MngCFGNumDailyHitLogs                               


} deriving (Read,Eq, Show, Generic)                       

type PointType13CRCCheck                  = Word8                
type PointType13DiPi                      = Word8         
type PointType13EnableLCDCalc2Prog        = Bool                       
type PointType13EnableLOICalc2Prog        = Bool                       
type PointType13ClearFSTDisplay           = Bool                    
type PointType13EnableComm1UserProg       = Bool                        
type PointType13EnableComm2UserProg       = Bool                        
type PointType13EnableUserCalcProg        = Bool                       
type PointType13LOIRTSTetSeconds          = Word8                     
type PointType13Comm1RTSTestSeconds       = Word8                        
type PointType13Comm2RTSTestSeconds       = Word8                        
type PointType13ClearCFGMem               = Bool                
type PointType13EnableScanIO              = Bool                 
type PointType13EnableAuxOut2             = Bool                  
type PointType13AuxOut1OnOrPlusTVolt      = Bool                         
type PointType13ColdStartOption           = Word8                    
type PointType13WarmStart                 = Bool              
type PointType13ReadInOut                 = Bool           
type PointType13WriteCFGMem               = Bool                
type PointType13CFGMemWriteCmpl           = Bool                    
type PointType13EnableEventLogorInitHist  = Bool                             
type PointType13MngLOISec                 = Word8                
type PointType13MngCommPort1Sec           = Word8                      
type PointType13MngCommPort2Sec           = Word8                      
type PointType13TermTypeInstdOrMngLCDSec  = Word8                               
type PointType13CommPassThroughMode       = Word8                          
type PointType13Mng6PointIOSetupFlag      = Word8                           
type PointType13MngCommPort3Sec           = Word8                      
type PointType13Comm3RTSTestSeconds       = Word8                          
type PointType13MngCFGNumDailyHistLogs    = Bool                             
type PointType13MngHistTImeStampOpt       = Bool                          
type PointType13MngCFGNumDailyHitLogs     = Word8                               
  
pointType13Parser :: Get PointType13
pointType13Parser = do 

  cRCCheck <- getWord8  
  diPi <- getWord8  
  enableLCDCalc2Prog <- anyButNull  
  enableLOICalc2Prog <- anyButNull  
  clearFSTDisplay <- anyButNull  
  enableComm1UserProg <- anyButNull  
  enableComm2UserProg <- anyButNull  
  enableUserCalcProg <- anyButNull  
  lOIRTSTetSeconds <- getWord8
  comm1RTSTestSeconds <- getWord8
  comm2RTSTestSeconds <- getWord8
  clearCFGMem <- anyButNull  
  enableScanIO <- anyButNull  
  enableAuxOut2 <- anyButNull  
  auxOut1OnOrPlusTVolt <- anyButNull  
  coldStartOption <- getWord8
  warmStart <- anyButNull
  readInOut <- anyButNull  
  writeCFGMem <- anyButNull  
  cFGMemWriteCmpl <- anyButNull  
  enableEventLogorInitHist <- anyButNull  
  mngLOISec <- getWord8  
  mngCommPort1Sec <- getWord8  
  mngCommPort2Sec <- getWord8  
  termTypeInstdOrMngLCDSec <- getWord8  
  commPassThroughMode <- getWord8  
  mng6PointIOSetupFlag <- getWord8  
  mngCommPort3Sec <- getWord8  
  comm3RTSTestSeconds <- getWord8  
  mngCFGNumDailyHistLogs <- anyButNull  
  mngHistTImeStampOpt <- anyButNull  
  mngCFGNumDailyHitLogs <- getWord8  
  
  return $ PointType13 cRCCheck diPi enableLCDCalc2Prog enableLOICalc2Prog clearFSTDisplay enableComm1UserProg enableComm2UserProg enableUserCalcProg lOIRTSTetSeconds 
    comm1RTSTestSeconds comm2RTSTestSeconds clearCFGMem enableScanIO enableAuxOut2 auxOut1OnOrPlusTVolt coldStartOption warmStart readInOut writeCFGMem cFGMemWriteCmpl 
    enableEventLogorInitHist mngLOISec mngCommPort1Sec mngCommPort2Sec termTypeInstdOrMngLCDSec commPassThroughMode mng6PointIOSetupFlag mngCommPort3Sec comm3RTSTestSeconds 
    mngCFGNumDailyHistLogs mngHistTImeStampOpt mngCFGNumDailyHitLogs






