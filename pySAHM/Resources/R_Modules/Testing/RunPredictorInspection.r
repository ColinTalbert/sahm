infil="C:\\temp\\TestDataSets\\CanadaThistleNewFormat.csv"
predictor="bio_13"

infil="J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\ModelEvaluation_Split_2.csv"
predictor="bio_08_2000_2009_2km"
infil="C:\\temp\\TestDataSets\\TestTrainingSplit_8.csv"
predictor="bio_01_2008_2km"
output.dir="C:\\temp\\SAHMDebugJunk\\BRTOut1"
response.col="responseBinary"
pres=TRUE
absn=TRUE
bgd=TRUE

Predictor.inspection(predictor,
    input.file=infil,
		output.dir=output.dir,
		response.col=response.col,
		pres=TRUE,
		absn=TRUE,
		bgd=FALSE)