setwd("C:\\GoogleDrive\\Python\\DevWorkspace\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules")
ScriptPath="C:\\GoogleDrive\\Python\\DevWorkspace\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules"


source("LoadRequiredCode.r")
source("MARS.helper.fcts.r")
source("GLM.helper.fcts.r")
source("BRT.helper.fcts.r")
source("RF.helper.fcts.r")
source("UDC.helper.fcts.r")
source("MAXENT.helper.fcts.r")
output.dir="C:\\temp\\SAHM_workspace"
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
#           Habitat Suitability Criterion            
#================================================================#
'multicore=TRUE', 'om=2', 'maxent_path=K:\\USERS\\ISS\\VisTrails\\VisTrails2.1.2_SAHM_x64_1.2.0\\Central_Maxent', 'species_name=species', 'cur_processing_mode=single models sequentially (n - 1 cores each)', 'c=J:\\Projects\\CNHPforJorge\\Maxent_WildParsnip_4\\CovariateCorrelationOutputMDS_WildParsnip_initial.csv', 'o=J:\\Projects\\CNHPforJorge\\Maxent_WildParsnip_4'
  hsc.file="I:\\VisTrails\\WorkingFiles\\workspace\\_HabitatSuitabilityCurves\\hsc_BrewersSparrowHSC_test_4\\hsc.json"
  input.file="I:\\VisTrails\\WorkingFiles\\workspace\\_HabitatSuitabilityCurves\\hsc_BrewersSparrowHSC_test_4\\CovariateCorrelationOutputMDS_BrewersSparrowHSC_initial.csv"
  
  hsc.file="I:\\VisTrails\\WorkingFiles\\workspace\\_HabitatSuitabilityCurves2\\hsc_BrewersSparrowCV_1\\hsc.json"
  input.file="I:\\VisTrails\\WorkingFiles\\workspace\\_HabitatSuitabilityCurves2\\hsc_BrewersSparrowCV_2\\CovariateCorrelationOutputMDS_BrewersSparrowCV_initial_orig.csv"
  udc.file="I:\\VisTrails\\WorkingFiles\\workspace\\_HabitatSuitabilityCurves\\vespersSparrow_withCat\\UserDefinedCurve_2\\hsc.json"
 input.file="I:\\VisTrails\\WorkingFiles\\workspace\\_HabitatSuitabilityCurves\\vespersSparrow_withCat\\UserDefinedCurve_2\\CovariateCorrelationOutputMDS_initial.csv"
 

FitModels(ma.name=input.file,
            output.dir=output.dir,
            response.col=rc,make.p.tif=T,make.binary.tif=T,
            debug.mode=T,script.name="udc",opt.methods=2,MESS=F,udc.file=udc.file,multCore=TRUE,ScriptPath=ScriptPath)
            
            

#================================================================#
#                       Maxent
#================================================================#
'multicore=TRUE', 'om=2', 'maxent_path=K:\\USERS\\ISS\\VisTrails\\VisTrails2.1.2_SAHM_x64_1.2.0\\Central_Maxent', 'species_name=species', 'cur_processing_mode=single models sequentially (n - 1 cores each)', 'c=J:\\Projects\\CNHPforJorge\\Maxent_WildParsnip_4\\CovariateCorrelationOutputMDS_WildParsnip_initial.csv', 'o=J:\\Projects\\CNHPforJorge\\Maxent_WildParsnip_4'


  lambdas.file="J:\\Projects\\CNHPforJorge\\Maxent_WildParsnip_4"
 input.file="J:\\Projects\\CNHPforJorge\\Maxent_WildParsnip_4\\CovariateCorrelationOutputMDS_WildParsnip_initial.csv"

FitModels(ma.name=input.file,
            output.dir=output.dir,
            response.col=rc,make.p.tif=F,make.binary.tif=F,
            debug.mode=T,script.name="maxent",opt.methods=2,MESS=F,lambdas=lambdas.file,multCore=TRUE,ScriptPath=ScriptPath)
#================================================================#
#                            MARS
#================================================================#
input.file = "J:\\Projects\\MojaveInvasives\\DerivedData\\AnalysisWith2000to2010base\\BRORUB20002010prism\\brt_1\\CovariateCorrelationOutputMDS_initial.csv"

penalty=c(3,5,10)
for(i in 1:length(penalty)){
output.dir=file.path("C:\\temp\\MARS",paste("MARS",i,sep="_"))
dir.create(output.dir)
FitModels(ma.name=input.file,
        tif.dir=NULL,output.dir=output.dir,
        response.col=rc,make.p.tif=F,make.binary.tif=F,
            mars.degree=1,mars.penalty=penalty[i],debug.mode=F,
            script.name="mars",opt.methods=2,MESS=F,ScriptPath=ScriptPath)
}            
#================================================================#
#                   Evaluate New Data
#================================================================#


ws<-"C:\\temp\\SAHM_workspace\\modelWorkspace"
output.dir="C:\\temp\\SAHM_workspace"
new.tiffs="C:\\Users\\mallen\\Downloads\\ForMarian_4\\ForMarian_4\\MergedDataset_WCMPI2650casq_1.csv"

load(ws)

source(file.path(ScriptPath,"LoadRequiredCode.r"))
source(paste(toupper(out$input$script.name),".helper.fcts.r",sep=""))
#options(error=expression(if(interactive()) recover() else dump.calls()))
EvaluateNewData(workspace=ws,out.dir=output.dir,b.tif=TRUE,p.tif=TRUE,mess=TRUE,out=out,produce.metrics=FALSE,new.tifs=new.tiffs)

#================================================================#
I:\VisTrails\VisTrails_SAHM_x64_debug\Central_R\R-2.14.1\bin\x64\Rterm.exe --vanilla -f I:\VisTrails\VisTrails_SAHM_x64_debug\VisTrails\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\EvaluateNewData.r --args mbt=TRUE mpt=TRUE ws="C:\temp\SAHM_workspace\rf_1\modelWorkspace" mes=FALSE o=C:\temp\SAHM_workspace\rf_3\modelWorkspace\rf_1

#================================================================#
#                            GLM
#================================================================#
input.file='C:\\Users\\mallen\\Downloads\\CovariateCorrelationOutputMDS_TestFire_initial.csv' 
input.file="C:\\Users\\mallen\\Downloads\\ForMarian_4\\ForMarian_4\\CovariateCorrelationOutputMDS_prismcasq_initial.csv"

FitModels(ma.name=input.file,
          tif.dir=NULL,
          output.dir=output.dir,
          response.col=rc,make.p.tif=F,make.binary.tif=F,
          simp.method="AIC",debug.mode=T,responseCurveForm="pdf",script.name="glm",MESS=F,opt.methods=2,squared.terms=TRUE,ScriptPath=ScriptPath,predSelect=TRUE,seed=1234)

   
#================================================================#
#                            RF
#================================================================#
input.file = "C:\\temp\\SAHM_workspace\\ForResponseCurveTool\\rf_1\\CovariateCorrelationOutputMDS_initial.csv"
input.file="I:\\VisTrails\\WorkingFiles\\workspace\\_tutorial_2.0_testing\\brewersSparrow_justMaurerObs\\rf_asage_1\\CovariateCorrelationOutputMDS_asage_initial.csv 

FitModels(ma.name=input.file,
      tif.dir=NULL,
      output.dir=output.dir,nodesize=NULL,maxnodes=NULL,
      response.col=rc,make.p.tif=F,make.binary.tif=F,
          debug.mode=T,opt.methods=2,script.name="rf",ScriptPath=ScriptPath,multCore=FALSE)


rc="responseBinary"
#================================================================#
#                            BRT
#================================================================#
input.file="J:\Projects\NormalsComparison\SAHM4\AHB\MergedDataset_1.csv"
input.file="I:\\VisTrails\\WorkingFiles\\workspace\\_ntr\\brt_BrewersSparrowCV_1\\CovariateCorrelationOutputMDS_BrewersSparrowCV_initial.csv"
#input.file="I:\\VisTrails\\WorkingFiles\\workspace\\_FinalTest\\CovariateCorrelationOutputMDS_initial.csv"
input.file="J:\\Projects\\MojaveInvasives\\DerivedData\\AnalysisWith2000to2010base\\BRORUB20002010prism\\brt_1\\CovariateCorrelationOutputMDS_initial.csv"
maxTrees=c(25,50,75,100,150,NULL)
output.dir="C:\\temp\\BRT"
for(i in 1:length(maxTrees)){
  d<-file.path(output.dir,paste("brt",i,sep="_"))
  dir.create(d)
  FitModels(ma.name=input.file,
            tif.dir=NULL,output.dir=d,
            response.col=rc,make.p.tif=F,make.binary.tif=F,n.folds=3,simp.method="cross-validation",tc=NULL,alpha=1,
        family = "bernoulli",n.trees =maxTrees[i],tolerance.method = "auto",
    tolerance = 0.001,seed=1234,opt.methods=2,
            simp.method="cross-validation",debug.mode=T,responseCurveForm="pdf",script.name="brt",
            bag.fraction = 0.5,prev.stratify = TRUE, opt.methods=2,MESS=F,ScriptPath=ScriptPath,multCore=FALSE,predSelect=FALSE)
}
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