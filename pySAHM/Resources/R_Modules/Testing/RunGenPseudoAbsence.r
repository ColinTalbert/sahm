input.file="C:\\VisTrails\\mtalbert_20110504T132851\\readMaTests\\CanadaThistleNewFormat.csv"
input.file="C:\\VisTrails\\mtalbert_20110504T132851\\readMaTests\\Split.csv"
input.file="I:\\VisTrails\\WorkingFiles\\workspace\\testingWeights\\MergedDataset_2.csv"
response.col="ResponseBinary"
output.file="C:\\VisTrails\\mtalbert_20110504T132851\\readMaTests\\SpatWeights.csv"


input.file="C:\\temp\\cheatgrass.csv"
output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\95IsoplethKDE.tif"
PseudoAbsGen(input.file,output.dir,response.col="ResponseBinary",method="KDE",bw.otim="adhoc",isopleth=95,bias=FALSE)
output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\95MCP.tif"
PseudoAbsGen(input.file,output.dir,response.col="ResponseBinary",method="MCP",bw.otim="adhoc",isopleth=95,bias=FALSE)
output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\ContinKDE.tif"
PseudoAbsGen(input.file,output.dir,response.col="ResponseBinary",method="KDE",bw.otim="adhoc",isopleth=95,bias=TRUE)
#PseudoAbsGen(input.file,output.file,response.col="ResponseBinary",method="Density",isopleth=95)

input.file="C:\\temp\\TestDataSets\\TestTrainingSplit_8.csv"

SetWeights(input.file,output.file,response.col="ResponseBinary",method="PresAbs")
I:\VisTrails\Central_VisTrailsInstall\Central_R\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall_debug\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\SetWeights.r --args  i="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv" o="C:\temp\SAHMDebugJunk\BRTOut1\out.csv" rc=responseBinary met="Density"