{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.PointType47 where

import GHC.Generics
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary
--import Data.Int
import Data.Binary.Get
import Protocol.ROC.Float
import Protocol.ROC.Utils

data PointType47 = PointType47 {
 pointType47FlowRatePerDay                        :: !PointType47FlowRatePerDay    
,pointType47EnergyRatePerDay                      :: !PointType47EnergyRatePerDay    
,pointType47FlowRatePerHour                       :: !PointType47FlowRatePerHour    
,pointType47EnergyRatePerHour                     :: !PointType47EnergyRatePerHour    
,pointType47OrPressExtLinMeterUncrtdFlow          :: !PointType47OrPressExtLinMeterUncrtdFlow   
,pointType47OrExpFactorTbFpm                      :: !PointType47OrExpFactorTbFpm    
,pointType47OrCdFT                                :: !PointType47OrCdFT
,pointType47OrFmTbFtm                             :: !PointType47OrFmTbFtm    
,pointType47Fpb                                   :: !PointType47Fpb    
,pointType47Ftb                                   :: !PointType47Ftb    
,pointType47Ftf                                   :: !PointType47Ftf    
,pointType47Fgr                                   :: !PointType47Fgr    
,pointType47Fpv                                   :: !PointType47Fpv    
,pointType47ZsCompStndCond                        :: !PointType47ZsCompStndCond
,pointType47ZbCompBaseCond                        :: !PointType47ZbCompBaseCond    
,pointType47Zf1CompFlowingCond                    :: !PointType47Zf1CompFlowingCond   
,pointType47OrIntegMultValueTbBaseMultValue       :: !PointType47OrIntegMultValueTbBaseMultValue    
,pointType47OrPlateBoreDiamFlowingCond            :: !PointType47OrPlateBoreDiamFlowingCond  
,pointType47MeterTubeIntDiamFlowingCond           :: !PointType47MeterTubeIntDiamFlowingCond    
,pointType47DiamRatioBeta                         :: !PointType47DiamRatioBeta    
,pointType47VelocityOfApproachEV                  :: !PointType47VelocityOfApproachEV    
,pointType47OrAvghLinMeterTotalCountsLastBMP      :: !PointType47OrAvghLinMeterTotalCountsLastBMP    
,pointType47PfAvgFlowingPress                     :: !PointType47PfAvgFlowingPress   
,pointType47TfAvgFlowingTemp                      :: !PointType47TfAvgFlowingTemp    
,pointType47FlowingDensity                        :: !PointType47FlowingDensity
,pointType47BaseDensity                           :: !PointType47BaseDensity    
,pointType47ReynoldsNum                           :: !PointType47ReynoldsNum    
,pointType47UpStrmStaticPress                     :: !PointType47UpStrmStaticPress    
,pointType47MolecularWeight                       :: !PointType47MolecularWeight    
,pointType47Fam                                   :: !PointType47Fam    
,pointType47Fwt                                   :: !PointType47Fwt    
,pointType47Fwl                                   :: !PointType47Fwl
,pointType47LocalGravCrctStaticPress              :: !PointType47LocalGravCrctStaticPress    
,pointType47LocalGravCrctDP                       :: !PointType47LocalGravCrctDP   
,pointType47Fhgm                                  :: !PointType47Fhgm    
,pointType47Fhgt                                  :: !PointType47Fhgt
,pointType47FlowToday                             :: !PointType47FlowToday    
,pointType47FlowYesterday                         :: !PointType47FlowYesterday    
,pointType47FlowMonth                             :: !PointType47FlowMonth    
,pointType47FlowPrvsMonth                         :: !PointType47FlowPrvsMonth   
,pointType47FlowAccum                             :: !PointType47FlowAccum    
,pointType47MinutesToday                          :: !PointType47MinutesToday
,pointType47MinutesYesterday                      :: !PointType47MinutesYesterday    
,pointType47MinutesMonth                          :: !PointType47MinutesMonth    
,pointType47MinutesPrvsMonth                      :: !PointType47MinutesPrvsMonth    
,pointType47MinutesAccum                          :: !PointType47MinutesAccum    
,pointType47EnergyToday                           :: !PointType47EnergyToday    
,pointType47EnergyYesterday                       :: !PointType47EnergyYesterday    
,pointType47EnergyMonth                           :: !PointType47EnergyMonth
,pointType47EnergyPrvsMonth                       :: !PointType47EnergyPrvsMonth    
,pointType47EnergyAccum                           :: !PointType47EnergyAccum   
,pointType47UncrtdToday                           :: !PointType47UncrtdToday    
,pointType47UncrtdYesterday                       :: !PointType47UncrtdYesterday  
,pointType47UncrtdMonth                           :: !PointType47UncrtdMonth    
,pointType47UncrtdPrvsMonth                       :: !PointType47UncrtdPrvsMonth    
,pointType47UncrtdAccum                           :: !PointType47UncrtdAccum    
,pointType47PartialRecalcFlag                     :: !PointType47PartialRecalcFlag    
,pointType47RedundantFlowRatePerDay               :: !PointType47RedundantFlowRatePerDay   
,pointType47RedundantTotalCounts                  :: !PointType47RedundantTotalCounts    
,pointType47LinMeterRawPulses                     :: !PointType47LinMeterRawPulses
,pointType47MeterFlowingStatus                    :: !PointType47MeterFlowingStatus    
,pointType47DailyMassFlowRate                     :: !PointType47DailyMassFlowRate    
,pointType47HourlyMassFlowRate                    :: !PointType47HourlyMassFlowRate    
,pointType47MassFlowToday                         :: !PointType47MassFlowToday    
,pointType47MassFlowYesterday                     :: !PointType47MassFlowYesterday    
,pointType47MassFlowCurrentMonth                  :: !PointType47MassFlowCurrentMonth    
,pointType47MassFlowPrvsMonth                     :: !PointType47MassFlowPrvsMonth
,pointType47MassFlowAccumLastReset                :: !PointType47MassFlowAccumLastReset    
,pointType47FlowCalcCFG                           :: !PointType47FlowCalcCFG   
,pointType47FlowCalcAGA7PressMult                 :: !PointType47FlowCalcAGA7PressMult    
,pointType47FlowCalcAGA7TempMult                  :: !PointType47FlowCalcAGA7TempMult
,pointType47FlowCalcAGA7CompMult                  :: !PointType47FlowCalcAGA7CompMult    
,pointType47DescActiveFlowCalc                    :: !PointType47DescActiveFlowCalc    
,pointType47DescActivePropCalc                    :: !PointType47DescActivePropCalc    
,pointType47UpStrmFlowingTemp                     :: !PointType47UpStrmFlowingTemp   
  
} deriving (Read,Eq, Show, Generic)                       

type PointType47FlowRatePerDay                    = Float           
type PointType47EnergyRatePerDay                  = Float           
type PointType47FlowRatePerHour                   = Float           
type PointType47EnergyRatePerHour                 = Float           
type PointType47OrPressExtLinMeterUncrtdFlow      = Float           
type PointType47OrExpFactorTbFpm                  = Float           
type PointType47OrCdFT                            = Float           
type PointType47OrFmTbFtm                         = Float           
type PointType47Fpb                               = Float           
type PointType47Ftb                               = Float           
type PointType47Ftf                               = Float           
type PointType47Fgr                               = Float           
type PointType47Fpv                               = Float           
type PointType47ZsCompStndCond                    = Float           
type PointType47ZbCompBaseCond                    = Float           
type PointType47Zf1CompFlowingCond                = Float           
type PointType47OrIntegMultValueTbBaseMultValue   = Float           
type PointType47OrPlateBoreDiamFlowingCond        = Float           
type PointType47MeterTubeIntDiamFlowingCond       = Float           
type PointType47DiamRatioBeta                     = Float           
type PointType47VelocityOfApproachEV              = Float           
type PointType47OrAvghLinMeterTotalCountsLastBMP  = Float           
type PointType47PfAvgFlowingPress                 = Float           
type PointType47TfAvgFlowingTemp                  = Float           
type PointType47FlowingDensity                    = Float           
type PointType47BaseDensity                       = Float           
type PointType47ReynoldsNum                       = Float           
type PointType47UpStrmStaticPress                 = Float           
type PointType47MolecularWeight                   = Float           
type PointType47Fam                               = Float           
type PointType47Fwt                               = Float           
type PointType47Fwl                               = Float           
type PointType47LocalGravCrctStaticPress          = Float           
type PointType47LocalGravCrctDP                   = Float           
type PointType47Fhgm                              = Float           
type PointType47Fhgt                              = Float           
type PointType47FlowToday                         = Float           
type PointType47FlowYesterday                     = Float           
type PointType47FlowMonth                         = Float           
type PointType47FlowPrvsMonth                     = Float           
type PointType47FlowAccum                         = Float           
type PointType47MinutesToday                      = Float           
type PointType47MinutesYesterday                  = Float           
type PointType47MinutesMonth                      = Float           
type PointType47MinutesPrvsMonth                  = Float           
type PointType47MinutesAccum                      = Float           
type PointType47EnergyToday                       = Float           
type PointType47EnergyYesterday                   = Float           
type PointType47EnergyMonth                       = Float           
type PointType47EnergyPrvsMonth                   = Float           
type PointType47EnergyAccum                       = Float           
type PointType47UncrtdToday                       = Float           
type PointType47UncrtdYesterday                   = Float           
type PointType47UncrtdMonth                       = Float           
type PointType47UncrtdPrvsMonth                   = Float           
type PointType47UncrtdAccum                       = Float           
type PointType47PartialRecalcFlag                 = Word8           
type PointType47RedundantFlowRatePerDay           = Float           
type PointType47RedundantTotalCounts              = Float           
type PointType47LinMeterRawPulses                 = Word32           
type PointType47MeterFlowingStatus                = Bool           
type PointType47DailyMassFlowRate                 = Float           
type PointType47HourlyMassFlowRate                = Float           
type PointType47MassFlowToday                     = Float           
type PointType47MassFlowYesterday                 = Float           
type PointType47MassFlowCurrentMonth              = Float           
type PointType47MassFlowPrvsMonth                 = Float           
type PointType47MassFlowAccumLastReset            = Float           
type PointType47FlowCalcCFG                       = Word8           
type PointType47FlowCalcAGA7PressMult             = Float           
type PointType47FlowCalcAGA7TempMult              = Float           
type PointType47FlowCalcAGA7CompMult              = Float           
type PointType47DescActiveFlowCalc                = BS.ByteString           
type PointType47DescActivePropCalc                = BS.ByteString           
type PointType47UpStrmFlowingTemp                 = Float           
  
pointType47Parser :: Get PointType47
pointType47Parser = do 
  flowRatePerDay <- getIeeeFloat32
  energyRatePerDay <- getIeeeFloat32
  flowRatePerHour <- getIeeeFloat32
  energyRatePerHour <- getIeeeFloat32
  orPressExtLinMeterUncrtdFlow <- getIeeeFloat32  
  orExpFactorTbFpm <- getIeeeFloat32  
  orCdFT <- getIeeeFloat32  
  orFmTbFtm <- getIeeeFloat32  
  fpb <- getIeeeFloat32  
  ftb <- getIeeeFloat32  
  ftf <- getIeeeFloat32  
  fgr <- getIeeeFloat32  
  fpv <- getIeeeFloat32  
  zsCompStndCond <- getIeeeFloat32  
  zbCompBaseCond <- getIeeeFloat32  
  zf1CompFlowingCond <- getIeeeFloat32  
  orIntegMultValueTbBaseMultValue <- getIeeeFloat32  
  orPlateBoreDiamFlowingCond <- getIeeeFloat32  
  meterTubeIntDiamFlowingCond <- getIeeeFloat32  
  diamRatioBeta <- getIeeeFloat32  
  velocityOfApproachEV <- getIeeeFloat32  
  orAvghLinMeterTotalCountsLastBMP <- getIeeeFloat32  
  pfAvgFlowingPress <- getIeeeFloat32  
  tfAvgFlowingTemp <- getIeeeFloat32  
  flowingDensity <- getIeeeFloat32  
  baseDensity <- getIeeeFloat32  
  reynoldsNum <- getIeeeFloat32  
  upStrmStaticPress <- getIeeeFloat32  
  molecularWeight <- getIeeeFloat32  
  fam <- getIeeeFloat32  
  fwt <- getIeeeFloat32  
  fwl <- getIeeeFloat32  
  localGravCrctStaticPress <- getIeeeFloat32  
  localGravCrctDP <- getIeeeFloat32  
  fhgm <- getIeeeFloat32  
  fhgt <- getIeeeFloat32  
  flowToday <- getIeeeFloat32  
  flowYesterday <- getIeeeFloat32  
  flowMonth <- getIeeeFloat32  
  flowPrvsMonth <- getIeeeFloat32  
  flowAccum <- getIeeeFloat32  
  minutesToday <- getIeeeFloat32  
  minutesYesterday <- getIeeeFloat32  
  minutesMonth <- getIeeeFloat32  
  minutesPrvsMonth <- getIeeeFloat32  
  minutesAccum <- getIeeeFloat32  
  energyToday <- getIeeeFloat32  
  energyYesterday <- getIeeeFloat32  
  energyMonth <- getIeeeFloat32  
  energyPrvsMonth <- getIeeeFloat32  
  energyAccum <- getIeeeFloat32  
  uncrtdToday <- getIeeeFloat32  
  uncrtdYesterday <- getIeeeFloat32  
  uncrtdMonth <- getIeeeFloat32  
  uncrtdPrvsMonth <- getIeeeFloat32  
  uncrtdAccum <- getIeeeFloat32  
  partialRecalcFlag <- getWord8 
  redundantFlowRatePerDay <- getIeeeFloat32 
  redundantTotalCounts <- getIeeeFloat32
  linMeterRawPulses <- getWord32le
  meterFlowingStatus <- anyButNull
  dailyMassFlowRate <- getIeeeFloat32
  hourlyMassFlowRate <- getIeeeFloat32 
  massFlowToday <- getIeeeFloat32 
  massFlowYesterday <- getIeeeFloat32 
  massFlowCurrentMonth <- getIeeeFloat32 
  massFlowPrvsMonth <- getIeeeFloat32 
  massFlowAccumLastReset <- getIeeeFloat32 
  flowCalcCFG <- getWord8
  flowCalcAGA7PressMult <- getIeeeFloat32 
  flowCalcAGA7TempMult <- getIeeeFloat32
  flowCalcAGA7CompMult <- getIeeeFloat32
  descActiveFlowCalc <- getByteString 20
  descActivePropCalc <- getByteString 20 
  upStrmFlowingTemp <- getIeeeFloat32
  
  return $ PointType47 flowRatePerDay energyRatePerDay flowRatePerHour energyRatePerHour orPressExtLinMeterUncrtdFlow orExpFactorTbFpm orCdFT orFmTbFtm fpb ftb ftf fgr fpv 
                    zsCompStndCond zbCompBaseCond zf1CompFlowingCond orIntegMultValueTbBaseMultValue orPlateBoreDiamFlowingCond meterTubeIntDiamFlowingCond diamRatioBeta 
                    velocityOfApproachEV orAvghLinMeterTotalCountsLastBMP pfAvgFlowingPress tfAvgFlowingTemp flowingDensity baseDensity reynoldsNum upStrmStaticPress 
                    molecularWeight fam fwt fwl localGravCrctStaticPress localGravCrctDP fhgm fhgt flowToday flowYesterday flowMonth flowPrvsMonth flowAccum minutesToday 
                    minutesYesterday minutesMonth minutesPrvsMonth minutesAccum energyToday energyYesterday energyMonth energyPrvsMonth energyAccum uncrtdToday uncrtdYesterday 
                    uncrtdMonth uncrtdPrvsMonth uncrtdAccum partialRecalcFlag redundantFlowRatePerDay redundantTotalCounts linMeterRawPulses meterFlowingStatus dailyMassFlowRate 
                    hourlyMassFlowRate massFlowToday massFlowYesterday massFlowCurrentMonth massFlowPrvsMonth massFlowAccumLastReset flowCalcCFG flowCalcAGA7PressMult 
                    flowCalcAGA7TempMult flowCalcAGA7CompMult descActiveFlowCalc descActivePropCalc upStrmFlowingTemp


