setwd("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules")
ScriptPath="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules"

#For Model tests
source("LoadRequiredCode.r")
source("MARS.helper.fcts.r")
source("GLM.helper.fcts.r")
source("BRT.helper.fcts.r")
source("RF.helper.fcts.r")

#For Apply Model Tests
source("EvaluateNewData.r")

#For PairsExplore and parameter inspection
source("PairsExplore.r")
source("Predictor.inspector.r")
source("my.panel.smooth.binary.r")

#For Data Splitting
source("TestTrainSplit.r")
source("CrossValidationSplit.r")

rc=c(rep("responseBinary",times=12),rep("responseCount",times=2))
input.file<-vector()
input.file[1]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/BadPath.csv"
input.file[2]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/LargeSplit.csv"
input.file[3]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv"
input.file[4]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitBadFactor.csv"
input.file[5]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitFactor.csv"
input.file[6]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Factor.csv"
input.file[7]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitFactor2.csv"
input.file[8]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitWeights.csv"
input.file[9]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/CanadaThistleNewFormat.csv"
input.file[10]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/NoSplit.csv"
input.file[11]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitCrossVal.csv"
input.file[12]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/LargeDataset.csv"
input.file[13]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Count.csv"
input.file[14]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/CountSplit.csv"
#add a missing data csv and maybe a couple with pseudo absence
output.dir<-vector()
output.dir[1]<-"C:\\temp\\AcrossModelPerformanceDetailsForTesting\\OneFunction4.11\\rf"
output.dir[2]<-"C:\\temp\\AcrossModelPerformanceDetailsForTesting\\OneFunction4.11\\brt"
output.dir[3]<-"C:\\temp\\AcrossModelPerformanceDetailsForTesting\\OneFunction4.11\\mars"
output.dir[4]<-"C:\\temp\\AcrossModelPerformanceDetailsForTesting\\OneFunction4.11\\glm"



########   Model Fit Test  ###########
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
              
              
              ##MARS
              for(i in 1:length(input.file)){
                  try(FitModels(ma.name=input.file[i],
                          tif.dir=NULL,output.dir=output.dir[3],
                          response.col=rc[i],make.p.tif=T,make.binary.tif=T,
                          mars.degree=1,mars.penalty=2,debug.mode=T,responseCurveForm="pdf",script.name="mars",opt.methods=2,MESS=TRUE))
                      }
                
              
              ##GLM
              for(i in 1:length(input.file)){
                  try(FitModels(ma.name=input.file[i],
                        tif.dir=NULL,
                        output.dir=output.dir[4],
                        response.col=rc[i],make.p.tif=T,make.binary.tif=T,
                        simp.method="AIC",debug.mode=T,responseCurveForm="pdf",script.name="glm",MESS=TRUE,opt.methods=2,squared.terms=TRUE))
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
 

### Pairs Explore Tests  #####
input.file<-vector()

input.file[1]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv"
input.file[2]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitFactor.csv"
input.file[3]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitWeights.csv"
input.file[4]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/CanadaThistleNewFormat.csv"
input.file[5]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitCrossVal.csv"
input.file[6]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/CanadaThistlePseudoAbsenceWeights.csv"
input.file[7]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Count.csv"

predictor<-c("bio_13_wgs84","bio_15_wgs84_categorical","slopedeg","asp_2k_alb","bio_19","bio_8","dem")
responseCol<-c(rep("responseBinary",times=6,rep("responseCount",times=1))

for(i in 1:6){
   if(i==1) { 
       try(Pairs.Explore(num.plots=5,
                min.cor=.5,
                input.file=input.file[i],
            		output.file=paste("C:\\temp\\SAHMDebugJunk\\BRTOut1\\PairsExploreTest\\",i,"Par1","\.jpg",sep=""),
            		response.col=responseCol[i],
            		pres=TRUE,
            		absn=TRUE,
            		bgd=TRUE))
        try(Pairs.Explore(num.plots=10,
                min.cor=.5,
                input.file=input.file[i],
            		output.file=paste("C:\\temp\\SAHMDebugJunk\\BRTOut1\\PairsExploreTest\\",i,"Par2","\.jpg",sep=""),
            		response.col=responseCol[i],
            		pres=TRUE,
            		absn=FALSE,
            		bgd=FALSE
                cor.w.highest=TRUE))
       try(Predictor.inspection(predictor[i],
                input.file=input.file[i],
            		output.dir==paste("C:\\temp\\SAHMDebugJunk\\BRTOut1\\PairsExploreTest\\",i,"Par",sep="")
            		response.col=response.col[i],
            		pres=TRUE,
            		absn=TRUE,
            		bgd=TRUE))              				
    }		
 try(Pairs.Explore(num.plots=15,
    min.cor=min.cor,
    input.file=input.file[i],
		output.file=paste("C:\\temp\\SAHMDebugJunk\\BRTOut1\\PairsExploreTest\\",i,"\.jpg",sep=""),
		response.col=responseCol[i],
		pres=TRUE,
		absn=TRUE,
		bgd=TRUE))
		
	try(Predictor.inspection(predictor[i],
    input.file=input.file[i],
		output.dir==paste("C:\\temp\\SAHMDebugJunk\\BRTOut1\\PairsExploreTest",sep="")
		response.col=response.col[i],
		pres=TRUE,
		absn=TRUE,
		bgd=TRUE))
		}
