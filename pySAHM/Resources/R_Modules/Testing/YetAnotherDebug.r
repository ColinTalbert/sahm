setwd("I:\\VisTrails\\Central_VisTrailsInstall_debug\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules")
ScriptPath="I:\\VisTrails\\Central_VisTrailsInstall_debug\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules"

#The idea here is to run this code then change above directories then rerun with the new code and compare (using append output and possible comparing maps)
source("FIT_BRT_pluggable.r")
source("FIT_MARS_pluggable.r")
source("FIT_RF_pluggable.r")
source("FIT_GLM_pluggable.r")
source("LoadRequiredCode.r")

rc=c(rep("responseBinary",times=9),rep("responseCount",times=2))
input.file<-vector()
input.file[1]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/BadPath.csv"
input.file[2]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/LargeSplit.csv"
input.file[3]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv"
input.file[4]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitBadFactor.csv"
input.file[5]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitFactor.csv"
input.file[6]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitFactorHighNA.csv"
input.file[7]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitFactorNA.csv"
input.file[8]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitWeights.csv"
input.file[9]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/CanadaThistleNewFormat.csv"
input.file[10]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Count.csv"
input.file[11]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/CountSplit.csv"

output.dir<-vector()
output.dir[1]<-"C:\\temp\\AcrossModelPerformanceDetailsForTesting\\NewMasterBranch\\rf"
output.dir[2]<-"C:\\temp\\AcrossModelPerformanceDetailsForTesting\\NewMasterBranch\\brt"
output.dir[3]<-"C:\\temp\\AcrossModelPerformanceDetailsForTesting\\NewMasterBranch\\mars"
output.dir[4]<-"C:\\temp\\AcrossModelPerformanceDetailsForTesting\\NewMasterBranch\\glm"

for(i in 1:length(input.file)){
set.seed(1)
try(fit.rf.fct(ma.name=input.file[i],
      tif.dir=NULL,
      output.dir=output.dir[1],
      response.col=rc[i],make.p.tif=T,make.binary.tif=T,
          debug.mode=T))
         }
   # PredictModel(workspace=paste(output.dir,"modelWorkspace",sep="\\"),out.dir=output.dir)

##BRT
 for(i in 1:length(input.file)){
 set.seed(1)
    try(fit.brt.fct(ma.name=input.file[i],
          tif.dir=NULL,output.dir=output.dir[2],
          response.col=rc[i],make.p.tif=T,make.binary.tif=T,
          simp.method="cross-validation",debug.mode=T,responseCurveForm="pdf",tc=NULL,n.folds=6,alpha=.3,script.name="brt.r",
          learning.rate =NULL, bag.fraction = 0.5,prev.stratify = TRUE, max.trees = NULL,opt.methods=2,seed=1,save.model=TRUE))
        }
   # PredictModel(workspace=paste(output.dir,"modelWorkspace",sep="\\"),out.dir="C:\\VisTrails")

##MARS
for(i in 1:length(input.file)){
    try(fit.mars.fct(ma.name=input.file[i],
            tif.dir=NULL,output.dir=output.dir[3],
            response.col=rc[i],make.p.tif=T,make.binary.tif=T,
            mars.degree=1,mars.penalty=2,debug.mode=T,responseCurveForm="pdf",script.name="mars.r"))
        }
   # PredictModel(workspace=paste(output.dir,"modelWorkspace",sep="\\"),out.dir="C:\\VisTrails")

##GLM
for(i in 1:length(input.file)){
    try(fit.glm.fct(ma.name=input.file[i],
          tif.dir=NULL,
          output.dir=output.dir[4],
          response.col=rc[i],make.p.tif=T,make.binary.tif=T,
          simp.method="AIC",debug.mode=T,responseCurveForm="pdf",script.name="glm.r",MESS=TRUE))
          }

