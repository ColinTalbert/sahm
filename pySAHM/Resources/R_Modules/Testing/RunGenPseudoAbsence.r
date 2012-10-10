output.dir="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\FDAW_2_MCP.tif"

input.file="J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\FDAW_1.csv"
input.file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\FDAW_2.csv"
output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\BackgroundPtSurfaces\\95AdHocIsoplethKDE.tif"
PseudoAbsGen(input.file,output.dir,method="KDE",bw.otim="adhoc",isopleth=95,bias=FALSE)
template<-"I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\PARC_100mTemplate\\bio_03_1971_2000_800m.tif"

output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\BackgroundPtSurfaces\\95MCP.tif"
PseudoAbsGen(input.file,output.dir,method="MCP",bw.otim="adhoc",isopleth=95,bias=FALSE,template=template)

output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\BackgroundPtSurfaces\\AdHocContinKDE.tif"
PseudoAbsGen(input.file,output.dir,method="KDE",bw.otim="adhoc",isopleth=95,bias=TRUE)

output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\BackgroundPtSurfaces\\95HpiKDE.tif"
PseudoAbsGen(input.file,output.dir,method="KDE",bw.otim="Hpi",isopleth=95,bias=FALSE)

output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\BackgroundPtSurfaces"
PseudoAbsGen(input.file,output.dir,method="KDE",bw.otim="Hpi",isopleth=95,bias=TRUE)

output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\BackgroundPtSurfaces\\TestOrig.tif"
PseudoAbsGen(input.file,output.dir,method="KDE",bw.otim="adhoc",isopleth=95,bias=TRUE)
#PseudoAbsGen(input.file,output.file,response.col="ResponseBinary",method="Density",isopleth=95)

input.file="C:\\temp\\TestDataSets\\TestTrainingSplit_8.csv"

file.list<-c("J:\\Projects\\Climate_RS_Comparison\\backgroundPoint_sensitivityAnalysis\\withoutPresAbs2\\PointFiles\\95MCP\\pts_320000\\MergedDataset_1.csv",
"J:\\Projects\\Climate_RS_Comparison\\backgroundPoint_sensitivityAnalysis\\withoutPresAbs2\\PointFiles\\95HpiKDE\\pts_320000\\MergedDataset_1.csv",
"J:\\Projects\\Climate_RS_Comparison\\backgroundPoint_sensitivityAnalysis\\withoutPresAbs2\\PointFiles\\95HpiContKDE\\pts_320000\\MergedDataset_1.csv",
"J:\\Projects\\Climate_RS_Comparison\\backgroundPoint_sensitivityAnalysis\\withoutPresAbs2\\PointFiles\\95AdHocIsoplethKDE\\pts_320000\\MergedDataset_1.csv",
"J:\\Projects\\Climate_RS_Comparison\\backgroundPoint_sensitivityAnalysis\\withoutPresAbs2\\PointFiles\\95AdHocContKDE\\pts_320000\\MergedDataset_1.csv")
num.reps=10
num.pts<-c(500,1000,2000,4000,8000,16000,32000)


file.list<-c("J:\\Projects\\Climate_RS_Comparison\\backgroundPoint_sensitivityAnalysis\\withoutPresAbs3\\95MCP\\pts_320000\\MergedDataset_1.csv",
"J:\\Projects\\Climate_RS_Comparison\\backgroundPoint_sensitivityAnalysis\\withoutPresAbs3\\95MCP\\pts_32\\MergedDataset_1.csv")
num.reps=10
num.pts<-c(500,1000,2000,4000,8000,16000,32000)


SetWeights(input.file,output.file,response.col="ResponseBinary",method="PresAbs")
I:\VisTrails\Central_VisTrailsInstall\Central_R\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall_debug\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\SetWeights.r --args  i="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv" o="C:\temp\SAHMDebugJunk\BRTOut1\out.csv" rc=responseBinary met="Density"