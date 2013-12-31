#==============================================================
#   Testing is controled through an testDatsetList path 
#        where all input mds' can be found
#   An output directory where output is to be written
#   and a code path where souce code can be read
#  as long as input file names have the word count or PseudoAbs
# the code will sort out responses and only fit appropriate models 
#==============================================================
#set this path to a directory where output is to be writtend
Outdir<-"I:\\VisTrails\\MarianTesting\\Output\\Debug12_30_13"
testDatsetList<-list.files("I:\\VisTrails\\MarianTesting\\BrewersSparrowTests",full.name=TRUE,recursive=FALSE,pattern=".csv")
CodePath<-"C:\\GoogleDrive\\Python\\DevWorkspace\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules"
#master code path until Colin changes it again...
CodePath<-"K:\\USERS\\ISS\\VisTrails_SAHM_x64_1.1.0\\VisTrails\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules"
#==============================================================
#sourcecode path
setwd(CodePath)
ScriptPath=CodePath

#Load the code
source("LoadTestCode.r")

#determine the response
rc=c(rep("responseBinary",times=length(testDatsetList)))
rc[grep("count",testDatsetList,ignore.case=TRUE)]<-"responseCount"
runMaxent<-grep("PseudoAbs",testDatsetList,ignore.case=TRUE,value=TRUE) #return the indicies here

predictor<-c("NDVI_annualMinimumValue_2005","NDVI_browndownrates1_2009","romoveg_rc_categorical","Temperature","Noise2Rast","NDVI_amplitudes1_2006","ppt_1971_2000_06_800m",
"NDVI_annualMeanValue_2006","NDVI_greenuprates1_2003")
grep("count",testDatsetList,ignore.case=TRUE)


#creating output directories
dir.create(Outdir)
output.dir<-vector()
output.dir[1]<-file.path(Outdir,"rf")
output.dir[2]<-file.path(Outdir,"brt")
output.dir[3]<-file.path(Outdir,"mars")
output.dir[4]<-file.path(Outdir,"glm")
output.dir[5]<-file.path(Outdir,"maxent")                                                                                          

for(i in 1:length(output.dir)) dir.create(output.dir[i])

########   Model Fit Test  ###########
        ##BRT
         for(i in 1:length(testDatsetList)){
              try(FitModels(ma.name=testDatsetList[i],
                        tif.dir=NULL,output.dir=output.dir[2],
                        response.col=rc[i],make.p.tif=T,make.binary.tif=F,n.folds=3,simp.method="cross-validation",tc=NULL,alpha=1,
                    family = "bernoulli",max.trees = 10000,tolerance.method = "auto",
                tolerance = 0.001,seed=1,opt.methods=2,
                        simp.method="cross-validation",debug.mode=T,responseCurveForm="pdf",script.name="brt",
                        learning.rate =NULL, bag.fraction = 0.5,prev.stratify = TRUE, max.trees = NULL,opt.methods=2,MESS=F,multCore=FALSE,predSelect=FALSE))
                        try(rm(out),silent=TRUE)
                      }
              
              
              ##MARS
              for(i in 1:length(testDatsetList)){
                  try(FitModels(ma.name=testDatsetList[i],
                          tif.dir=NULL,output.dir=output.dir[3],
                          response.col=rc[i],make.p.tif=T,make.binary.tif=T,
                          mars.degree=1,mars.penalty=2,debug.mode=T,responseCurveForm="pdf",script.name="mars",opt.methods=2,MESS=TRUE))
                      }
                
              
              ##GLM
              for(i in 1:length(testDatsetList)){
                  try(FitModels(ma.name=testDatsetList[i],
                        tif.dir=NULL,
                        output.dir=output.dir[4],
                        response.col=rc[i],make.p.tif=T,make.binary.tif=F,
                        simp.method="AIC",debug.mode=T,responseCurveForm="pdf",script.name="glm",MESS=FALSE,opt.methods=2,squared.terms=TRUE,predSelect=FALSE))
                        }
              
              ### Random Forest
              for(i in 1:length(testDatsetList)){
              proximity=NULL
              try(FitModels(ma.name=testDatsetList[i],
                    tif.dir=NULL,
                    output.dir=output.dir[1],
                    response.col=rc[i],make.p.tif=T,make.binary.tif=F,
                        debug.mode=T,opt.methods=2,script.name="rf",
              responseCurveForm="pdf",xtest=NULL,ytest=NULL,n.trees=1000,mtry=NULL,
              samp.replace=FALSE,sampsize=NULL,nodesize=NULL,maxnodes=NULL,importance=FALSE,
              localImp=FALSE,nPerm=1,proximity=NULL,oob.prox=proximity,norm.votes=TRUE,
              do.trace=FALSE,keep.forest=NULL,keep.inbag=FALSE,MESS=F,seed=1))
                 }
 
              ### Maxent
              
              		




for(i in 1:length(predictor)){
   if(i==1) { 
       try(Pairs.Explore(num.plots=5,
                min.cor=.5,
                 input.file=testDatsetList[i],
            		output.file=paste(Outdir,"\\",i,"Par1",".jpg",sep=""),
            		response.col=rc[i],
            		pres=TRUE,
            		absn=TRUE,
            		bgd=TRUE))
        try(Pairs.Explore(num.plots=10,
                min.cor=.5,
                input.file=testDatsetList[i],
            		output.file=paste(Outdir,"\\",i,"Par2",".jpg",sep=""),
            		response.col=rc[i],
            		pres=TRUE,
            		absn=FALSE,
            		bgd=FALSE,
                cors.w.highest=TRUE))
       try(Predictor.inspection(predictor[i],
                input.file=testDatsetList[i],
            		output.dir=Outdir,
            		response.col=rc[i],
            		pres=TRUE,
            		absn=TRUE,
            		bgd=TRUE))              				
    }		
 try(Pairs.Explore(num.plots=15,
    min.cor=min.cor,
    input.file=testDatsetList[i],
		output.file=paste(Outdir,"\\",i,".jpg",sep=""),
		response.col=rc[i],
		pres=TRUE,
		absn=TRUE,
		bgd=TRUE))
		
	try(Predictor.inspection(predictor[i],
    testDatsetList[i],
		output.dir=paste(Outdir,"\\",sep=""),
		response.col=rc[i],
		pres=TRUE,
		absn=TRUE,
		bgd=TRUE))
		}


testDatsetList<-"I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode2\\TestSuite\\PairsExploreManyPredictors.csv"
for (i in 1:25){ 
 try(Pairs.Explore(num.plots=i,
                min.cor=.5,
                input.file=testDatsetList,
            		output.file=paste(Outdir,"\\",i,"NumPlotsTest",".jpg",sep=""),
            		response.col=rc[4],
            		pres=TRUE,
            		absn=TRUE,
            		bgd=TRUE
                ))
            		}
### Apply Model Test

input.workspace=list(

for(i in 1:length(input.workspace){
EvaluateNewData(workspace=paste(output.dir,"modelWorkspace",sep="\\"),out.dir=output.dir,b.tif=TRUE,p.tif=TRUE,mess=TRUE,new.tifs="I:\\VisTrails\\WorkingFiles\\workspace\\_applyModel\\Error\\MergedDataset_10.csv",produce.metrics=TRUE)
}
### Data Splitting Tests