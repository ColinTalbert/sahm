setwd("I:\\VisTrails\\Central_VisTrailsInstall_debug\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules")
setwd("I:\\VisTrails\\Central_VisTrailsInstall\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules")
ScriptPath="I:\\VisTrails\\Central_VisTrailsInstall_debug\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules"

#setwd("I:\\VisTrails\\Central_VisTrailsInstall\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules")
source("FIT_BRT_pluggable.r")
source("FIT_MARS_pluggable.r")
source("FIT_RF_pluggable.r")
source("FIT_GLM_pluggable.r")
#source("EvaluationStats.r")
source("EvalStats.r")
source("make.auc.r")
source("EvalStatsHelperFcts.r")
source("AppendOut.r")
source("ResidualImage.r")
source("TestTrainRocPlot.r")
source("read.ma.r")
source("proc.tiff.r")
source("PredictModel.r")
source("modalDialog.R")
source("check.libs.r")
source("Pred.Surface.r")

options(error=expression(if(interactive()) recover() else dump.calls()))
options(error=NULL)
trace(proc.tiff,browser)

list.files()
#Testing Compile Output on data with no test train and with a test train
output.dir="C:\\temp\\SAHMDebugJunk\\BRTOut1"
rc="responseBinary"
input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/CanadaThistleNewFormat.csv"
input.file="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv"
input.file="C:\\temp\\SAHM_workspace\\mtalbert_20111014T113851\\TestTrainingSplit_1.csv"


#fit.model(ma.name=input.file,output.dir=output.dir,response.col=rc,make.p.tif=T,make.binary.tif=T,script.name="glm.r",
#opt.methods=2,save.model=TRUE,UnitTest=FALSE,MESS=FALSE,aic.form=TRUE,parm2=4)

###########################################################################
############### Quick debug  ##############################################
input.file="I:\\VisTrails\\WorkingFiles\\workspace\\morisettej_20111027T131301\\CovariateCorrelationOutputMDS_Both.csv"
output.dir="C:\\temp\\SAHMDebugJunk\\BRTOut1"
rc="responseBinary"
#######################################################################

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
            response.col=rc,make.p.tif=F,make.binary.tif=F,
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

I:\VisTrails\Central_VisTrailsInstall\Central_R\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall\vistrails\packages\sahm\pySAHM\Resources\R_Modules\FIT_BRT_pluggable.r --args  mbt=TRUE mpt=TRUE c="I:\VisTrails\WorkingFiles\workspace\morisettej_20111027T131301\CovariateCorrelationOutputMDS_Both.csv" mes=TRUE o="C:\temp\SAHMDebugJunk\BRTOut1" rc=responseBinary