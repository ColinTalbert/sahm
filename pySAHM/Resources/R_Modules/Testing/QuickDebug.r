setwd("I:\\VisTrails\\Central_VisTrailsInstall_debug\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules")
ScriptPath="I:\\VisTrails\\Central_VisTrailsInstall_debug\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules"

#setwd("I:\\VisTrails\\Central_VisTrailsInstall\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules")
#source("FIT_BRT_pluggable.r")
#source("FIT_MARS_pluggable.r")
#source("FIT_RF_pluggable.r")
#source("FIT_GLM_pluggable.r")

source("LoadRequiredCode.r")
source("MARS.helper.fcts.r")
source("GLM.helper.fcts.r")
source("BRT.helper.fcts.r")
source("RF.helper.fcts.r")


options(error=expression(if(interactive()) recover() else dump.calls()))
options(error=NULL)
trace(proc.tiff,browser)

list.files()
#Testing Compile Output on data with no test train and with a test train
output.dir="C:\\temp\\SAHMDebugJunk\\BRTOut1"
rc="responseCount"
input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Count.csv"

input.file[11]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/CountSplit.csv"

#fit.model(ma.name=input.file,output.dir=output.dir,response.col=rc,make.p.tif=T,make.binary.tif=T,script.name="glm.r",
#opt.methods=2,save.model=TRUE,UnitTest=FALSE,MESS=FALSE,aic.form=TRUE,parm2=4)

###########################################################################
############### Quick debug  ##############################################
input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv"
input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/NoSplit.csv"

input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitCrossVal.csv"


rc="responseBinary"
input.file="I:\\VisTrails\\WorkingFiles\\workspace\\Test_CrossValidation\\modelSelection_split_1.csv"
input.file="I:\\VisTrails\\WorkingFiles\\workspace\\Test_CrossValidation\\modelSelection_cv_1.csv"
output.dir="C:\\temp\\SAHMDebugJunk\\BRTOut1"
#######################################################################
##MARS
FitModels(ma.name=input.file,
            tif.dir=NULL,output.dir=output.dir,
            response.col=rc,make.p.tif=T,make.binary.tif=T,
            mars.degree=1,mars.penalty=2,debug.mode=T,responseCurveForm="pdf",script.name="mars",opt.methods=2,MESS=TRUE)
            
##GLM
FitModels(ma.name=input.file,
          tif.dir=NULL,
          output.dir=output.dir,
          response.col=rc,make.p.tif=T,make.binary.tif=T,
          simp.method="AIC",debug.mode=T,responseCurveForm="pdf",script.name="glm",MESS=TRUE,opt.methods=2)
          
#RF
set.seed(1)
proximity=NULL
FitModels(ma.name=input.file,
      tif.dir=NULL,
      output.dir=output.dir,
      response.col=rc,make.p.tif=T,make.binary.tif=T,
          debug.mode=T,opt.methods=2,script.name="rf",
responseCurveForm="pdf",xtest=NULL,ytest=NULL,n.trees=1000,mtry=NULL,
samp.replace=FALSE,sampsize=NULL,nodesize=NULL,maxnodes=NULL,importance=FALSE,
localImp=FALSE,nPerm=1,proximity=NULL,oob.prox=proximity,norm.votes=TRUE,
do.trace=FALSE,keep.forest=NULL,keep.inbag=FALSE,save.model=TRUE,MESS=TRUE)

#BRT
set.seed(1)
FitModels(ma.name=input.file,
          tif.dir=NULL,output.dir=output.dir,
          response.col=rc,make.p.tif=T,make.binary.tif=T,n.folds=3,simp.method="cross-validation",tc=NULL,alpha=1,
      family = "bernoulli",max.trees = 10000,tolerance.method = "auto",
  tolerance = 0.001,seed=NULL,opt.methods=2,
          simp.method="cross-validation",debug.mode=T,responseCurveForm="pdf",script.name="brt",
          learning.rate =NULL, bag.fraction = 0.5,prev.stratify = TRUE, max.trees = NULL,opt.methods=2,save.model=TRUE,MESS=TRUE)

##RF
set.seed(1)
    fit.rf.fct(ma.name=input.file,
      tif.dir=NULL,
      output.dir=output.dir,
      response.col=rc,make.p.tif=F,make.binary.tif=F,
          debug.mode=T)

    PredictModel(workspace=paste(output.dir,"modelWorkspace",sep="\\"),out.dir=output.dir)

##BRT

    fit.brt.fct(ma.name=input.file,
          tif.dir=NULL,output.dir=output.dir,
          response.col=rc,make.p.tif=T,make.binary.tif=T,
          simp.method="cross-validation",debug.mode=F,responseCurveForm="pdf",script.name="brt.r",
          learning.rate =NULL, bag.fraction = 0.5,prev.stratify = TRUE, max.trees = NULL,opt.methods=2,seed=1,save.model=TRUE,MESS=TRUE)
          
    PredictModel(workspace=paste(output.dir,"modelWorkspace",sep="\\"),out.dir="C:\\VisTrails")

##MARS
    fit.mars.fct(ma.name=input.file,
            tif.dir=NULL,output.dir=output.dir,
            response.col=rc,make.p.tif=T,make.binary.tif=T,
            mars.degree=1,mars.penalty=2,debug.mode=T,responseCurveForm="pdf",script.name="mars.r",opt.methods=2)

    PredictModel(workspace=paste(output.dir,"modelWorkspace",sep="\\"),out.dir="C:\\VisTrails")

##GLM
    fit.glm.fct(ma.name=input.file,
          tif.dir=NULL,
          output.dir=output.dir,
          response.col=rc,make.p.tif=T,make.binary.tif=T,
          simp.method="AIC",debug.mode=T,responseCurveForm="pdf",script.name="glm.r",MESS=TRUE)

    PredictModel(workspace=paste(output.dir,"modelWorkspace",sep="\\"),out.dir="C:\\VisTrails")


## Command line C:\temp\SAHM_workspace\mtalbert_20111014T113851\TestTrainingSplit_1.csv
C:\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall_debug\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\FIT_BRT_pluggable.r --args c=C:\VisTrails\mtalbert_20110504T132851\readMaTests\Split.csv o=C:\temp\SAHMDebugJunk\BRTOut1 rc=responseBinary
C:\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall\vistrails\packages\sahm\pySAHM\Resources\R_Modules\FIT_GLM_pluggable.r --args c==I:\VisTrails\Yellowstone_example\workspace_for_paper\CovariateCorrelationOutputMDS_Both.csv o=C:\temp\SAHMDebugJunk\BRTOut1 rc=responseBinary

I:\VisTrails\Central_VisTrailsInstall\Central_R\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall_debug\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\FIT_MARS_pluggable.r --args  mbt=TRUE mpt=TRUE c="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitCrossVal.csv" o="C:\temp\SAHMDebugJunk\BRTOut1" rc=responseBinary mes=TRUE