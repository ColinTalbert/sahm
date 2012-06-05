source("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules\\Predictor.inspection.r")
source("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules\\my.panel.smooth.binary.r")
infil="C:\\temp\\TestDataSets\\CanadaThistleNewFormat.csv"
predictor="bio_13"

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
predictor="romoveg_rc_categorical"

Predictor.inspection(predictor,
    input.file=infil,
		output.dir=output.dir,
		response.col=response.col,
		pres=TRUE,
		absn=TRUE,
		bgd=TRUE)