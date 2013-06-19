#setwd("I:\\VisTrails\\VisTrails_SAHM_x64_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules")
#ScriptPath="I:\\VisTrails\\VisTrails_SAHM_x64_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules"

#setwd("I:\\VisTrails\\VisTrails_SAHM_x64_debug\\VisTrails\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules")
#ScriptPath="I:\\VisTrails\\VisTrails_SAHM_x64_debug\\VisTrails\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules"

setwd("N:\\Research\\nccsc\\Private\\Projects\\VisTrails\\DevelopmentWorkspace\\Marian\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules")
ScriptPath="N:\\Research\\nccsc\\Private\\Projects\\VisTrails\\DevelopmentWorkspace\\Marian\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules"

#setwd("I:\\VisTrails\\DevWorkspace\\Colin\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules")
#ScriptPath="I:\\VisTrails\\DevWorkspace\\Colin\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules"
source("LoadRequiredCode.r")
source("MARS.helper.fcts.r")
source("GLM.helper.fcts.r")
source("BRT.helper.fcts.r")
source("RF.helper.fcts.r")
source("MAXENT.helper.fcts.r")
output.dir="N:\\Research\\nccsc\\Private\\Projects\\VisTrails\\DevelopmentWorkspace\\Marian\\Workspace\\SAHMDebugging\\DebugOut"
rc="responseBinary"
#Testing GitHub again Another commit
#options(warn=2)
#options(error=expression(if(interactive()) recover() else dump.calls()))
#options(error=NULL)
#trace(proc.tiff,browser)
#list.files()

#Testing Compile Output on data with no test train and with a test train
I:\VisTrails\VisTrails_SAHM_x64_debug\Central_R\R-2.14.1\bin\x64\Rterm.exe --vanilla -f I:\VisTrails\DevWorkspace\Catherine\userpackages\..\userpackages\sahm\pySAHM\Resources\R_Modules\FIT_BRT_pluggable.r --args cur_processing_mode=multiple cores asynchronously c=J:\Projects\NormalsComparison\SAHM4\AHB\modelSelection_cv_2.csv seed=-789157713 o=J:\Projects\NormalsComparison\SAHM4\AHB\brt_4 rc=responseBinary mes=TRUE

input.file="N:\\Research\\nccsc\\Private\\Projects\\VisTrails\\DevelopmentWorkspace\\Marian\\Workspace\\SAHMDebugging\\ParameterOpt\\modelSelection_split_1.csv"
input.file="N:\\Research\\nccsc\\Private\\Projects\\VisTrails\\DevelopmentWorkspace\\Marian\\Workspace\\Telemetry\\SAHMData\\VistrailsOutput\\modelSelection_split_1.csv"
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
input.file="N:\\Research\\nccsc\\Private\\Projects\\VisTrails\\DevelopmentWorkspace\\Marian\\Workspace\\SAHMWorkflows\\MessDebug\\modelSelection_split_LessClasses.csv"
FitModels(ma.name=input.file,
            output.dir=output.dir,
            response.col=rc,make.p.tif=T,make.binary.tif=F,
            mars.degree=1,mars.penalty=2,debug.mode=T,script.name="mars",opt.methods=2,MESS=T,ScriptPath=ScriptPath,multCore=FALSE)
total.time<-Sys.time()-start.time
total.time
#================================================================#
#                   Evaluate New Data
#================================================================#
o=I:\VisTrails\WorkingFiles\workspace\CheatgrassApplyModel\ApplyModelResults\TestingNewVariableImportance\MDS1\ApplyModel_5 mpt=FALSE ws=J:\Projects\Climate_RS_Comparison\Cheatgrass_VisTrails\WUS\FEB2013\ParameterOptimization\brt_4\modelWorkspace mes=FALSE rc=responseBinary mbt=FALSE



ws<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\WUS\\FEB2013\\ParameterOptimization_SpatialCV2\\maxent_7\\modelWorkspace"
output.dir="J:\\Projects\\Climate_RS_Comparison\\MarianTesting2\\ApplyModel_MDS1_KDEbin_maxent_2"
new.tiffs="J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\WUS\\FEB2013\\validation\\MergedDataset_1.csv"

load(ws)
setwd("I:\\VisTrails\\DevWorkspace\\Marian\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules")
ScriptPath="I:\\VisTrails\\DevWorkspace\\Marian\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules"
source(file.path(ScriptPath,"LoadRequiredCode.r"))
source(paste(toupper(out$input$script.name),".helper.fcts.r",sep=""))
options(error=expression(if(interactive()) recover() else dump.calls()))
EvaluateNewData(workspace=ws,out.dir=output.dir,b.tif=TRUE,p.tif=TRUE,mess=FALSE,out=out,produce.metrics=FALSE)

#================================================================#
I:\VisTrails\VisTrails_SAHM_x64_debug\Central_R\R-2.14.1\bin\x64\Rterm.exe --vanilla -f I:\VisTrails\VisTrails_SAHM_x64_debug\VisTrails\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\EvaluateNewData.r --args mbt=TRUE mpt=TRUE ws="C:\temp\SAHM_workspace\rf_1\modelWorkspace" mes=FALSE o=C:\temp\SAHM_workspace\rf_3\modelWorkspace\rf_1

#================================================================#
#                            GLM
#================================================================#

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
      response.col=rc,make.p.tif=T,make.binary.tif=T,
          debug.mode=T,opt.methods=2,script.name="rf",
responseCurveForm="pdf",xtest=NULL,ytest=NULL,n.trees=1000,mtry=NULL,
samp.replace=FALSE,sampsize=NULL,nodesize=NULL,maxnodes=NULL,importance=FALSE,
localImp=FALSE,nPerm=1,proximity=NULL,oob.prox=NULL,norm.votes=TRUE,
do.trace=FALSE,keep.forest=NULL,keep.inbag=FALSE,MESS=F,ScriptPath=ScriptPath,multiCore=FALSE)
total.time<-Sys.time()-start.time
total.time

rc="responseBinary"
#================================================================#
#                            BRT
#================================================================#

FitModels(ma.name=input.file,
          tif.dir=NULL,output.dir=output.dir,
          response.col=rc,make.p.tif=T,make.binary.tif=T,n.folds=3,simp.method="cross-validation",tc=NULL,alpha=1,
      family = "bernoulli",max.trees = 10000,tolerance.method = "auto",
  tolerance = 0.001,seed=1,opt.methods=2,
          simp.method="cross-validation",debug.mode=T,responseCurveForm="pdf",script.name="brt",
          bag.fraction = 0.5,prev.stratify = TRUE, max.trees = NULL,opt.methods=2,MESS=T,ScriptPath=ScriptPath,multCore=FALSE)

