
output.file="C:\\VisTrails\\mtalbert_20110504T132851\\readMaTests\\SpatWeights.csv"

SetWeights(input.file,output.file,response.col="ResponseBinary",method="Density",sigma.sd=500)


output.file="C:\\temp\\TestDataSets\\CanadaThistlePseudoAbsenceWeights.csv"
input.file<-"C:\\temp\\TestDataSets\\CanadaThistlePseudoAbsence.csv"
SetWeights(input.file,output.file,response.col="ResponseBinary",method="PresAbs")

input.file<-"C:\\temp\\TestDataSets\\CanadaThistleNewFormat.csv"
output.file<-"C:\\temp\\TestDataSets\\CanadaThistleWeights.csv"
SetWeights(input.file,output.file,response.col="ResponseBinary",method="PresAbs")


I:\VisTrails\Central_VisTrailsInstall\Central_R\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall_debug\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\SetWeights.r --args  i="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv" o="C:\temp\SAHMDebugJunk\BRTOut1\out.csv" rc=responseBinary met="Density"