setwd("I:\\VisTrails\\Central_VisTrails_x32\\VisTrails\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules")
ScriptPath="I:\\VisTrails\\Central_VisTrails_x32\\VisTrails\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules"

#The idea here is to run this code then change above directories then rerun with the new code and compare (using append output and possible comparing maps)
source("FIT_BRT_pluggable.r")
source("FIT_MARS_pluggable.r")
source("FIT_RF_pluggable.r")
source("FIT_GLM_pluggable.r")
source("LoadRequiredCode.r")
source("RF.helper.fcts.r")
source("BRT.helper.fcts.r")
source("GLM.helper.fcts.r")
source("MARS.helper.fcts.r")
rc=c(rep("responseBinary",times=11),rep("responseCount",times=2))
input.file<-vector()
input.file[1]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/BadPath.csv"
input.file[2]="I:\VisTrails\VisTrails_SAHM_x32_debug\VisTrails\vistrails\packages\TestingRCode\CrossValidationSp1test.csv"
input.file[3]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv"
input.file[4]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitBadFactor.csv"
input.file[5]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitFactor.csv"
input.file[6]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Factor.csv"
input.file[7]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitFactor2.csv"
input.file[8]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitWeights.csv"
input.file[9]="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\UsedAvailableSp1NoCV.csv"
input.file[10]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/NoSplit.csv"
input.file[11]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitCrossVal.csv"
input.file[12]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Count.csv"
input.file[13]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/CountSplit.csv"

#I'm cutting these out of the standard test suite because they take a long time to run
#and only test whether we run well on large datasets or big tiffs
"C:/VisTrails/mtalbert_20110504T132851/readMaTests/CanadaThistleNewFormat.csv"
"C:/VisTrails/mtalbert_20110504T132851/readMaTests/LargeSplit.csv"

output.dir<-vector()
output.dir[1]<-"C:\\temp\\AcrossModelPerformanceDetailsForTesting\\NewMasterBranch2.10\\rf"
output.dir[2]<-"C:\\temp\\AcrossModelPerformanceDetailsForTesting\\NewMasterBranch2.10\\brt"
output.dir[3]<-"C:\\temp\\AcrossModelPerformanceDetailsForTesting\\NewMasterBranch2.10\\mars"
output.dir[4]<-"C:\\temp\\AcrossModelPerformanceDetailsForTesting\\NewMasterBranch2.10\\glm"

##BRT
 for(i in 1:length(input.file)){
try(FitModels(ma.name=input.file[i],
          tif.dir=NULL,output.dir=output.dir[2],
          response.col=rc[i],make.p.tif=T,make.binary.tif=T,n.folds=3,simp.method="cross-validation",tc=NULL,alpha=1,
      family = "bernoulli",max.trees = 10000,tolerance.method = "auto",
  tolerance = 0.001,seed=1,opt.methods=2,
          simp.method="cross-validation",debug.mode=T,responseCurveForm="pdf",script.name="brt",
          learning.rate =NULL, bag.fraction = 0.5,prev.stratify = TRUE, max.trees = NULL,opt.methods=2,save.model=TRUE,MESS=TRUE))
        }
   # PredictModel(workspace=paste(output.dir,"modelWorkspace",sep="\\"),out.dir="C:\\VisTrails")

##MARS
for(i in 1:length(input.file)){
    try(FitModels(ma.name=input.file[i],
            tif.dir=NULL,output.dir=output.dir[3],
            response.col=rc[i],make.p.tif=T,make.binary.tif=T,
            mars.degree=1,mars.penalty=2,debug.mode=T,responseCurveForm="pdf",script.name="mars",opt.methods=2,MESS=TRUE))
        }
   # PredictModel(workspace=paste(output.dir,"modelWorkspace",sep="\\"),out.dir="C:\\VisTrails")

##GLM
for(i in 1:length(input.file)){
    try(FitModels(ma.name=input.file[i],
          tif.dir=NULL,
          output.dir=output.dir[4],
          response.col=rc[i],make.p.tif=T,make.binary.tif=T,
          simp.method="AIC",debug.mode=T,responseCurveForm="pdf",script.name="glm",MESS=TRUE,opt.methods=2))
          }

### Random Forest
for(i in 1:length(input.file)){
proximity=NULL
try(FitModels(ma.name=input.file[i],
      tif.dir=NULL,
      output.dir=output.dir[1],
      response.col=rc[i],make.p.tif=T,make.binary.tif=T,
          debug.mode=T,opt.methods=2,script.name="rf",
responseCurveForm="pdf",xtest=NULL,ytest=NULL,n.trees=1000,mtry=NULL,
samp.replace=FALSE,sampsize=NULL,nodesize=NULL,maxnodes=NULL,importance=FALSE,
localImp=FALSE,nPerm=1,proximity=NULL,oob.prox=proximity,norm.votes=TRUE,
do.trace=FALSE,keep.forest=NULL,keep.inbag=FALSE,save.model=TRUE,MESS=TRUE,seed=1))
         }

#Predictor Inspection Tests
predictor="romoveg_rc_categorical"
infil<-vector()
infil[1]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/UsedAvailable.csv"
infil[2]="C:\\VisTrails\\mtalbert_20110504T132851\\readMaTests\\Split.csv"

predictor<-vector()
predictor[1]<-"romoveg_rc_categorical"
predictor[2]="bio_13_wgs84"

for(i in 1:length(input.file)){
      try(Predictor.inspection(predictor[i],
          input.file=infil[i],
      		output.dir=output.dir,
      		response.col=response.col,
      		pres=TRUE,
      		absn=TRUE,
      		bgd=TRUE))
		}