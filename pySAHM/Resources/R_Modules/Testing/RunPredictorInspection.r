setwd("I:\\VisTrails\\DevWorkspace\\Marian\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules")
ScriptPath="I:\\VisTrails\\DevWorkspace\\Marian\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules"

source("PairsExplore.r")
source("read.dat.r")
source("chk.libs.r")
source("read.dat.r")
source("my.panel.smooth.binary.r")
source("Predictor.inspection.r")

infil="C:\\temp\\TestDataSets\\CanadaThistleNewFormat.csv"
predictor="bio_13"

## Nonspatial data should work through SAHM
i="C:\\temp\\SAHM_workspace\\NonSpatialData.csv"
predictor="PrecipitatoinRast"

## testing factors
i="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode2\\TestSuite\\PresAbsFactorCVEvaluation.csv"
predictor="romoveg_rc_categorical"
predictor="NDVI_browndownrates1_2009"
o="C:\\temp\\SAHMDebugJunk\\BRTOut1" 
rc="responseBinary"

## testing count
#i="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode2\\TestSuite\\CountFactorSplit.csv"
i="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode2\\VistrailsSessions\\MergedDataset_Split.csv"
predictor="TemperatureRastPois"
o="I:\\VisTrails\\WorkingFiles\\workspace\\NewOutput\\SyntheticCountData"
rc="responseCount"

bgd=TRUE 
i="I:\\VisTrails\\WorkingFiles\\workspace\\MarianTesting\\TestSuite\\modelSelection_split_1.csv"
o="I:\\VisTrails\\WorkingFiles\\workspace\\MarianTesting\\TestSuite\\PredictorInspections"
p="PRISM_bio_03_1971_2000_800m" 
rc="responseBinary" 
   Predictor.inspection(p,
        input.file=i,
    		output.dir=o,
    		response.col=rc,
    		pres=TRUE,
    		absn=TRUE,
    		bgd=bgd)
    		
infil="J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\ModelEvaluation_Split_2.csv"
predictor="bio_08_2000_2009_2km"
infil="I:\\VisTrails\\WorkingFiles\\secondseason\\secondseason_workfile_2012_02_28b\\ModelEvaluation_Split_10.csv"
predictor="PRISM_bio_06_1971_2000_800m"
output.dir="C:\\temp\\SAHMDebugJunk\\BRTOut1"
response.col="responseBinary"
pres=TRUE
absn=TRUE
bgd=TRUE
infil="C:\\VisTrails\\mtalbert_20110504T132851\\readMaTests\\Split.csv"
predictor="bio_13_wgs84"

infil="I:\\VisTrails\\WorkingFiles\\secondseason\\secondseason_workfile_2012_02_28b\\ModelEvaluation_Split_9.csv"
predictor="bio_17_2009_2km"
predictor="bio_14_2009_2km"
infil="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\modelSelection_split_8.csv" 
output.dir="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode"
predictor="EVI_baselevels2_2001" 
i="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\modelSelection_split_20.csv" 
o="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\PredictorInspections" 
rc="responseBinary" 
predictor="romoveg_rc_categorical"    
    Predictor.inspection(predictor,
        input.file=i,
    		output.dir=o,
    		response.col=rc,
    		pres=TRUE,
    		absn=TRUE,
    		bgd=FALSE)