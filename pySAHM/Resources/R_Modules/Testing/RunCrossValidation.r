response.col="responseBinary"
input.file="I:\\VisTrails\\WorkingFiles\\workspace\\_PseudoAbs\\MergedDataset_3.csv"
output.file<-"I:\\VisTrails\\WorkingFiles\\workspace\\_PseudoAbs\\MergedDataset_CVSplit.csv"

input.file="J:Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\WUS\\FEB2013\\ParameterOptimization\\brt_4\\CovariateCorrelationOutputMDS_cv_6_KDE_cont.csv"
output.file<-"I:\\VisTrails\\WorkingFiles\\workspace\\CheatgrassApplyModel\\KudzoError\\CovariateCorrelationOutputMDS.csv"
CrossValidationSplit(input.file,output.file,stratify=TRUE,n.folds=10,seed=NULL,spatSplit=TRUE)

I:\VisTrails\Central_VisTrailsInstall\Central_R\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall_debug\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\CrossValidationSplit.r --args  i="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv" o="C:\temp\SAHMDebugJunk\BRTOut1\out.csv" rc=responseBinary stra=F nf=10