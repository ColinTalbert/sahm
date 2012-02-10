input.file="C:\\VisTrails\\mtalbert_20110504T132851\\readMaTests\\Factor.csv"
response.col="ResponseBinary"
output.file="C:\\VisTrails\\mtalbert_20110504T132851\\readMaTests\\Debugging.csv"
CrossValidationSplit(input.file,output.file,stratify=TRUE,n.folds=5,seed=NULL)

I:\VisTrails\Central_VisTrailsInstall\Central_R\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall_debug\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\CrossValidationSplit.r --args  i="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv" o="C:\temp\SAHMDebugJunk\BRTOut1\out.csv" rc=responseBinary stra=F nf=10