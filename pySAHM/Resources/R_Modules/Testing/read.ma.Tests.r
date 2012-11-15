setwd("I:\\VisTrails\\Central_VisTrailsInstall_debug\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules")
source("FIT_BRT_pluggable.r")
source("FIT_MARS_pluggable.r")
source("FIT_RF_pluggable.r")
source("FIT_GLM_pluggable.r")
source("EvaluationStats.r")
source("TestTrainRocPlot.r")
source("proc.tiff.r")
source("PredictModel.r")
source("PairsExplore.r")
setwd("I:\\VisTrails\\Central_VisTrailsInstall_debug\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules\\Testing")
source("unit.test.r")
source("TestFunction2.r")
#################################################################
## Current test files
input.file<-vector()
input.file[1]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/BadPath.csv"
input.file[2]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/LargeSplit.csv"
input.file[3]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv"
input.file[4]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitBadFactor.csv"
input.file[5]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitFactor.csv"
input.file[6]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitFactorHighNA.csv"
input.file[7]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitFactorNA.csv"
input.file[8]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/SplitWeights.csv"
input.file[9]="C:/VisTrails/mtalbert_20110504T132851/readMaTests/CountSplit.csv"

output.file<-sub("851/","851/TestOutput/",input.file)
output.file<-sub(".csv",".jpg",output.file)

Debug=FALSE
rc="responseBinary"
output.dir="C:/VisTrails/mtalbert_20110504T132851/TestOutput"

#BRT TEST LIST
      brt.list<-list()
      #this is the default scenario
      brt.list[[1]]<-list(make.p.tif=TRUE,
       			make.binary.tif=TRUE,
            tc=NULL,
       			n.folds=3,
       			alpha=1,
            learning.rate=NULL,
       			bag.fraction=.5,
       			model.family="\"bernoulli\"",
       			prev.stratify=TRUE,
       			max.trees=10000,
       			opt.methods=2,
       			seed=NULL,
       		  tolerance.method="\"auto\"",
       		  tolerance=.001,
             debug.mode=FALSE,
             UnitTest=1)


      #which datasets to test with the given set of parameters
      brt.use.list<-list()
      brt.use.list[[1]]<-c(1,2,3,4,5,6,7,8)

parameter.list<-list(brt.list=brt.list,brt.use.list=brt.use.list)
source("I:\\VisTrails\\Central_VisTrailsInstall_debug\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules\\read.ma.r")
set.seed(123)
Out<-TestFunction(input.file,parameter.list,Debug,rc,output.dir)
Out<-Out[[1]]

source("I:\\VisTrails\\Central_VisTrailsInstall_debug\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules\\read.maOld.r")
set.seed(123)
Out2<-TestFunction(input.file,parameter.list,Debug,rc,output.dir)
Out2<-Out2[[1]]
a<-all.equal(Out,Out2)
# Bad factor isn't removed in the old so it is longer in component 4

##################################################################
#run seperately for count data

rc="responseCount"
 brt.use.list<-list()
      brt.use.list[[1]]<-9

parameter.list<-list(brt.list=brt.list,brt.use.list=brt.use.list)
source("read.ma.r")
set.seed(123)
Out<-TestFunction(input.file,parameter.list,Debug,rc,output.dir)
Out<-Out[[1]]

source("read.maOld.r")
set.seed(123)
Out2<-TestFunction(input.file,parameter.list,Debug,rc,output.dir)
Out2<-Out2[[1]]
a<-all.equal(Out,Out2)