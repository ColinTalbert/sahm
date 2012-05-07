setwd("I:\\VisTrails\\Central_VisTrails_x32_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules")
ScriptPath="I:\\VisTrails\\Central_VisTrails_x32_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules"

source("LoadRequiredCode.r")
source("MARS.helper.fcts.r")
source("GLM.helper.fcts.r")
source("BRT.helper.fcts.r")
source("RF.helper.fcts.r")
output.dir="C:\\temp\\SAHMDebugJunk\\BRTOut1"
rc="responseBinary"
#options(warn=2)
#options(error=expression(if(interactive()) recover() else dump.calls()))
#options(error=NULL)
#trace(proc.tiff,browser)
#list.files()

#Testing Compile Output on data with no test train and with a test train

rc="responseCount"
input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Count.csv"


###########################################################################
############### Quick debug  ##############################################
input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv"
input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/NoSplit.csv"
input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitCrossVal.csv"
input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Factor.csv"
input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/CanadaThistleNewFormat.csv"
input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Spat.Weights.csv"
input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/LargeDataset.csv"
input.file<-"I:\\VisTrails\\WorkingFiles\\workspace\\_PseudoAbs\\MergedDataset_CVSplit.csv"
#######################################################################

input.file="C:\\temp\\TestDataSets\\CanadaThistlePseudoAbsenceWeights.csv"
input.file<-"C:\\temp\\TestDataSets\\CanadaThistleWeights.csv"
##MARS

FitModels(ma.name=input.file,
            output.dir=output.dir,
            response.col=rc,make.p.tif=T,make.binary.tif=T,
            mars.degree=1,mars.penalty=2,debug.mode=T,responseCurveForm="pdf",script.name="mars",opt.methods=2,MESS=F,pseudoabsence=TRUE)

EvaluateNewData(workspace=paste(output.dir,"modelWorkspace",sep="\\"),out.dir=output.dir,make.binary.tif=TRUE,make.p.tif=TRUE,MESS=TRUE)

##GLM
input.file="J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\modelSelection_cv_3.csv"
FitModels(ma.name=input.file,
          tif.dir=NULL,
          output.dir=output.dir,
          response.col=rc,make.p.tif=F,make.binary.tif=F,
          simp.method="AIC",debug.mode=T,responseCurveForm="pdf",script.name="glm",MESS=FALSE,opt.methods=2,squared.terms=FALSE)
          
#RF
input.file="I:\\VisTrails\\WorkingFiles\\workspace\\_PseudoAbs\\MergedDataset_3.csv"
set.seed(1)
proximity=NULL
FitModels(ma.name=input.file,
      tif.dir=NULL,
      output.dir=output.dir,
      response.col=rc,make.p.tif=F,make.binary.tif=F,
          debug.mode=T,opt.methods=2,script.name="rf",
responseCurveForm="pdf",xtest=NULL,ytest=NULL,n.trees=1000,mtry=NULL,
samp.replace=FALSE,sampsize=NULL,nodesize=NULL,maxnodes=NULL,importance=FALSE,
localImp=FALSE,nPerm=1,proximity=NULL,oob.prox=proximity,norm.votes=TRUE,
do.trace=FALSE,keep.forest=NULL,keep.inbag=FALSE,save.model=TRUE,MESS=FALSE,pseudoabsence=TRUE)

#BRT
set.seed(1)
#input.file="I:\\SpeciesData\\APHIS\\GypsyMoth\\modelSelection_split_2.csv"
FitModels(ma.name=input.file,
          tif.dir=NULL,output.dir=output.dir,
          response.col=rc,make.p.tif=T,make.binary.tif=T,n.folds=3,simp.method="cross-validation",tc=NULL,alpha=1,
      family = "bernoulli",max.trees = 10000,tolerance.method = "auto",
  tolerance = 0.001,seed=NULL,opt.methods=2,
          simp.method="cross-validation",debug.mode=T,responseCurveForm="pdf",script.name="brt",
          learning.rate =NULL, bag.fraction = 0.5,prev.stratify = TRUE, max.trees = NULL,opt.methods=2,save.model=TRUE,MESS=TRUE)

#Now evaluating new data
workspace="I:\\VisTrails\\WorkingFiles\\workspace\\_TutorialTesting\\brt_1"

EvaluateNewData(workspace=paste(workspace,"modelWorkspace",sep="\\"),outDir=output.dir,binary.tif=TRUE,p.tif=TRUE,mes=TRUE)
  logname<-NULL
  sink(logname)
  sink(logname, type="message")

 PredictModel(workspace=,out.dir=output.dir)


## Command line C:\temp\SAHM_workspace\mtalbert_20111014T113851\TestTrainingSplit_1.csv
C:\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall_debug\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\FIT_BRT_pluggable.r --args c=C:\VisTrails\mtalbert_20110504T132851\readMaTests\Split.csv o=C:\temp\SAHMDebugJunk\BRTOut1 rc=responseBinary
C:\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall\vistrails\packages\sahm\pySAHM\Resources\R_Modules\FIT_GLM_pluggable.r --args c==I:\VisTrails\Yellowstone_example\workspace_for_paper\CovariateCorrelationOutputMDS_Both.csv o=C:\temp\SAHMDebugJunk\BRTOut1 rc=responseBinary

I:\VisTrails\Central_VisTrailsInstall\Central_R\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall_debug\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\FIT_BRT_pluggable.r --args  mbt=TRUE mpt=TRUE c="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitCrossVal.csv" o="C:\temp\SAHMDebugJunk\BRTOut1" rc=responseBinary mes=TRUE

#evaluate new data
I:\VisTrails\Central_VisTrails_x32\Central_R\R-2.14.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrails_x32_debug\VisTrails\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\EvaluateNewData.r --args ws=I:\VisTrails\WorkingFiles\workspace\_TutorialTesting\brt_1\modelWorkspace o=I:\VisTrails\WorkingFiles\workspace\_TutorialTesting\FinalModelEvaluation_TestTrainBinom\brt_1 mpt=TRUE mbt=TRUE mes=TRUE