
#==============================
#   Input
response.col="responseBinary"
input.file='C:\\Users\\mallen\\Downloads\\CovariateCorrelationOutputMDS_TestFire_NoSplit.csv' 
output.file='C:\\Users\\mallen\\Downloads\\CovariateCorrelationOutputMDS_TestFire_StratSplit.csv' 


#==============================
#  Run it 
CrossValidationSplit(input.file,output.file,stratify=TRUE,n.folds=10,seed=NULL,spatSplit=TRUE)

a<-read.csv(output.file)
table(as.numeric(a$responseBinary[4:nrow(a)]),as.numeric(a$Split[4:nrow(a)]))

I:\VisTrails\Central_VisTrailsInstall\Central_R\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall_debug\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\CrossValidationSplit.r --args  i="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv" o="C:\temp\SAHMDebugJunk\BRTOut1\out.csv" rc=responseBinary stra=F nf=10