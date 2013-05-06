#setwd("I:\\VisTrails\\VisTrails_SAHM_x64_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules")
#ScriptPath="I:\\VisTrails\\VisTrails_SAHM_x64_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules"

#setwd("I:\\VisTrails\\VisTrails_SAHM_x64_debug\\VisTrails\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules")
#ScriptPath="I:\\VisTrails\\VisTrails_SAHM_x64_debug\\VisTrails\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules"

setwd("I:\\VisTrails\\DevWorkspace\\Marian\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules")
ScriptPath="I:\\VisTrails\\DevWorkspace\\Marian\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules"

setwd("I:\\VisTrails\\DevWorkspace\\Colin\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules")
ScriptPath="I:\\VisTrails\\DevWorkspace\\Colin\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules"
source("LoadRequiredCode.r")
source("MARS.helper.fcts.r")
source("GLM.helper.fcts.r")
source("BRT.helper.fcts.r")
source("RF.helper.fcts.r")
source("MAXENT.helper.fcts.r")
output.dir="C:\\temp\\SAHMDebugJunk\\BRTOut1"
rc="responseBinary"
#Testing GitHub again Another commit
#options(warn=2)
#options(error=expression(if(interactive()) recover() else dump.calls()))
#options(error=NULL)
#trace(proc.tiff,browser)
#list.files()

#Testing Compile Output on data with no test train and with a test train
I:\VisTrails\VisTrails_SAHM_x64_debug\Central_R\R-2.14.1\bin\x64\Rterm.exe --vanilla -f I:\VisTrails\DevWorkspace\Catherine\userpackages\..\userpackages\sahm\pySAHM\Resources\R_Modules\FIT_BRT_pluggable.r --args cur_processing_mode=multiple cores asynchronously c=J:\Projects\NormalsComparison\SAHM4\AHB\modelSelection_cv_2.csv seed=-789157713 o=J:\Projects\NormalsComparison\SAHM4\AHB\brt_4 rc=responseBinary mes=TRUE

input.file="N:\\Research\\nccsc\\Private\\Projects\\VisTrails\\DevelopmentWorkspace\\Marian\\Workspace\\SAHMDebugging\\break145.csv"
input.file="C:\\temp\\TestDataSets\\modelSelection_split_1.csv"

#================================================================#
#                       Maxent
#================================================================#
input.file="I:\\VisTrails\\WorkingFiles\\workspace\\_64xTesting\\CovariateCorrelationOutputMDS.csv"
lambdas.file="I:\\VisTrails\\WorkingFiles\\workspace\\_64xTesting\maxentFiles_94"
FitModels(ma.name=input.file,
            output.dir=output.dir,
            response.col=rc,make.p.tif=T,make.binary.tif=T,
            debug.mode=T,script.name="maxent",opt.methods=2,MESS=T,lambdas=lambdas.file)
#================================================================#
#                            MARS
#================================================================#
input.file="H:\\Desktop\\CheatgrassAnalysis\\BlockCV.csv"
FitModels(ma.name=input.file,
            output.dir=output.dir,
            response.col=rc,make.p.tif=F,make.binary.tif=F,
            mars.degree=1,mars.penalty=2,debug.mode=T,script.name="mars",opt.methods=2,MESS=F,ScriptPath=ScriptPath,multCore=TRUE)
total.time<-Sys.time()-start.time
total.time
#================================================================#
#                   Evaluate New Data
#================================================================#
o=I:\VisTrails\WorkingFiles\workspace\CheatgrassApplyModel\ApplyModelResults\TestingNewVariableImportance\MDS1\ApplyModel_5 mpt=FALSE ws=J:\Projects\Climate_RS_Comparison\Cheatgrass_VisTrails\WUS\FEB2013\ParameterOptimization\brt_4\modelWorkspace mes=FALSE rc=responseBinary mbt=FALSE

ws<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\WUS\\FEB2013\\ParameterOptimization\\brt_4\\modelWorkspace"
output.dir="I:\\VisTrails\\WorkingFiles\\workspace\\CheatgrassApplyModel\\ApplyModelResults\\TestingNewVariableImportance\\MDS1\\ApplyModel_5"
new.tiffs="J:/Projects/Climate_RS_Comparison/Cheatgrass_VisTrails/WUS/FEB2013/validation/MergedDataset_1.csv"

load(ws)
setwd("I:\\VisTrails\\DevWorkspace\\Marian\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules")
ScriptPath="I:\\VisTrails\\DevWorkspace\\Marian\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules"
source(file.path(ScriptPath,"LoadRequiredCode.r"))
source(paste(toupper(out$input$script.name),".helper.fcts.r",sep=""))
EvaluateNewData(workspace=ws,out.dir=output.dir,b.tif=FALSE,p.tif=FALSE,mess=FALSE,new.tifs=new.tiffs,out=out)

#================================================================#
I:\VisTrails\VisTrails_SAHM_x64_debug\Central_R\R-2.14.1\bin\x64\Rterm.exe --vanilla -f I:\VisTrails\VisTrails_SAHM_x64_debug\VisTrails\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\EvaluateNewData.r --args mbt=TRUE mpt=TRUE ws="C:\temp\SAHM_workspace\rf_1\modelWorkspace" mes=FALSE o=C:\temp\SAHM_workspace\rf_3\modelWorkspace\rf_1

#================================================================#
#                            GLM
#================================================================#
#input.file="I:\\VisTrails\\WorkingFiles\\workspace\\_condor\\modelSelection_cv_3.csv"
FitModels(ma.name=input.file,
          tif.dir=NULL,
          output.dir=output.dir,
          response.col=rc,make.p.tif=F,make.binary.tif=F,
          simp.method="AIC",debug.mode=T,responseCurveForm="pdf",script.name="glm",MESS=F,opt.methods=2,squared.terms=FALSE,ScriptPath=ScriptPath)

   
#================================================================#
#                            RF
#================================================================#
FitModels(ma.name=input.file,
      tif.dir=NULL,
      output.dir=output.dir,
      response.col=rc,make.p.tif=F,make.binary.tif=F,
          debug.mode=T,opt.methods=2,script.name="rf",
responseCurveForm="pdf",xtest=NULL,ytest=NULL,n.trees=1000,mtry=NULL,
samp.replace=FALSE,sampsize=NULL,nodesize=NULL,maxnodes=NULL,importance=FALSE,
localImp=FALSE,nPerm=1,proximity=NULL,oob.prox=NULL,norm.votes=TRUE,
do.trace=FALSE,keep.forest=NULL,keep.inbag=FALSE,MESS=F,ScriptPath=ScriptPath)
total.time<-Sys.time()-start.time
total.time

rc="responseBinary"
#================================================================#
#                            BRT
#================================================================#
input.file="J:\\Projects\\NormalsComparison\\SAHM4\\AHB\\modelSelection_cv_2.csv"
#input.file="I:\\VisTrails\\WorkingFiles\\workspace\\_FinalTest\\CovariateCorrelationOutputMDS_initial.csv"
FitModels(ma.name=input.file,
          tif.dir=NULL,output.dir=output.dir,
          response.col=rc,make.p.tif=T,make.binary.tif=T,n.folds=3,simp.method="cross-validation",tc=NULL,alpha=1,
      family = "bernoulli",max.trees = 10000,tolerance.method = "auto",
  tolerance = 0.001,seed=1,opt.methods=2,
          simp.method="cross-validation",debug.mode=T,responseCurveForm="pdf",script.name="brt",
          bag.fraction = 0.5,prev.stratify = TRUE, max.trees = NULL,opt.methods=2,MESS=T,ScriptPath=ScriptPath,multCore=FALSE)

"I:\\VisTrails\\WorkingFiles\\workspace\\_64xTesting\\RFDebug\\modelWorkspace"
EvaluateNewData(workspace="I:\\VisTrails\\WorkingFiles\\workspace\\_64xTesting\\RFDebug\\modelWorkspace",out.dir=output.dir,b.tif=TRUE,p.tif=TRUE,mess=FALSE,produce.metrics=TRUE)

#Maxlike
input.file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\CovariateCorrelationOutputMDS_initial.csv"
Formula="~bio_06_2000_2km + bio_14_2000_4km + NDVI_annualMaximumValue_2009 + NDVI_greenuprates1_2003 + NDVI_peakdates1_2003"
"I:\VisTrails\VisTrails_SAHM_x32_debug\Central_R\R-2.14.1\bin\i386\Rterm.exe" --vanilla -f "I:\VisTrails\VisTrails_SAHM_x32_debug\VisTrails\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\FIT_MaxLike_pluggable.r" --args  om=2 c="I:/VisTrails/VisTrails_SAHM_x32_debug/VisTrails/vistrails/packages/TestingRCode/CovariateCorrelationOutputMDS_initial.csv" fmla="~bio_06_2000_2km + bio_14_2000_4km + NDVI_annualMaximumValue_2009 + NDVI_greenuprates1_2003 + NDVI_peakdates1_2003" o="C:\temp\SAHM_workspace\maxlike_4" rc=responseBinary

c="I:/VisTrails/VisTrails_SAHM_x32_debug/VisTrails/vistrails/packages/TestingRCode/CovariateCorrelationOutputMDS_initial.csv" 
fmla=~bio_06_2000_2km + bio_14_2000_4km + NDVI_annualMaximumValue_2009 + NDVI_greenuprates1_2003 + NDVI_peakdates1_2003 
o="C:\temp\SAHM_workspace\maxlike_1" 
rc=responseBinary

FitModels(ma.name=input.file,
		tif.dir=NULL,
		output.dir=output.dir,
		response.col=rc,
		make.p.tif=T,make.binary.tif=T,
		debug.mode=F,responseCurveForm="pdf",script.name="maxlike",
		opt.methods=2,MESS=T,Formula=Formula,UseTiffs=FALSE)


#Now evaluating new data
workspace=output.dir

EvaluateNewData(workspace=paste(workspace,"modelWorkspace",sep="\\"),out.dir=output.dir,b.tif=TRUE,p.tif=TRUE,mes=FALSE,ScriptPath=ScriptPath)
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

I:\VisTrails\VisTrails_SAHM_x64_debug\Central_R\R-2.14.1\bin\x64\Rterm.exe --vanilla -f "I:\VisTrails\VisTrails_SAHM_x64_debug\vistrails\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\FIT_BRT_pluggable.r" --args  c="I:\VisTrails\WorkingFiles\workspace\_FinalTest\CovariateCorrelationOutputMDS_initial.csv" seed=-635517616 o="I:\VisTrails\WorkingFiles\workspace\_FinalTest\brt_2" rc=responseBinary

I:\VisTrails\VisTrails_SAHM_x64_debug\Central_R\R-2.14.1\bin\x64\Rterm.exe --vanilla -f "I:\VisTrails\VisTrails_SAHM_x64_debug\vistrails\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\FIT_MARS_pluggable.r" --args  c="I:\VisTrails\WorkingFiles\workspace\_FinalTest\CovariateCorrelationOutputMDS_initial.csv" o="I:\VisTrails\WorkingFiles\workspace\_FinalTest\mars_2" rc=responseBinary