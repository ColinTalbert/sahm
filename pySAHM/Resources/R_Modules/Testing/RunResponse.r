
setwd("C:\\GoogleDrive\\Python\\DevWorkspace\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules")
ScriptPath="C:\\GoogleDrive\\Python\\DevWorkspace\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules"

source("LoadRequiredCode.r")
source("MARS.helper.fcts.r")
source("GLM.helper.fcts.r")
source("BRT.helper.fcts.r")
source("RF.helper.fcts.r")
source("MAXENT.helper.fcts.r")
#setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\MyCode")
ShinyCode<-file.path(ScriptPath,"ResponseCurves\\External")
sourceList<-list.files(ShinyCode,full.names=TRUE)
unlist(lapply(as.list(sourceList),source))
ChkLibs(list("gbm","randomForest","maptools","rgdal","shiny","leaflet","maptools","rgdal","raster","ncdf4","fields","maps",
            "ggplot2","zoo","XML","RColorBrewer","chron","wesanderson","sm"))


wsLst<-list()
wsLst[[1]]<-"C:\\temp\\SAHM_workspace\\ForResponseCurveTool\\brewersSparrow\\brt_1\\modelWorkspace"
wsLst[[2]]<-"C:\\temp\\SAHM_workspace\\ForResponseCurveTool\\brewersSparrow\\glm_1\\modelWorkspace"
wsLst[[3]]<-"C:\\temp\\SAHM_workspace\\ForResponseCurveTool\\brewersSparrow\\mars_1\\modelWorkspace"
wsLst[[4]]<-"C:\\temp\\SAHM_workspace\\ForResponseCurveTool\\brewersSparrow\\rf_1\\modelWorkspace"

wsLst[[1]]<-"J:\\Projects\\cnhp_swap\\derived_data\\workspace_03072014\\brt_SpruceFir_1\\modelWorkspace"
wsLst[[2]]<-"J:\\Projects\\cnhp_swap\\derived_data\\workspace_03072014\\glm_SpruceFir_1\\modelWorkspace"
wsLst[[3]]<-"J:\\Projects\\cnhp_swap\\derived_data\\workspace_03072014\\mars_SpruceFir_1\\modelWorkspace"
wsLst[[4]]<-"J:\\Projects\\cnhp_swap\\derived_data\\workspace_03072014\\rf_SpruceFir_1\\modelWorkspace"

wsLst[[1]]<-"J:\\Projects\\cnhp_swap\\derived_data\\workspace_03072014\\brt_sagebrush_1\\modelWorkspace"
wsLst[[2]]<-"J:\\Projects\\cnhp_swap\\derived_data\\workspace_03072014\\glm_sagebrush_1\\modelWorkspace"
wsLst[[3]]<-"J:\\Projects\\cnhp_swap\\derived_data\\workspace_03072014\\mars_sagebrush_1\\modelWorkspace"
wsLst[[4]]<-"J:\\Projects\\cnhp_swap\\derived_data\\workspace_03072014\\rf_sagebrush_1\\modelWorkspace"


fitLst<-list()
modelLst<-list()
varImpLst<-list()
mapLst<-vector()
rastLst<-vector()
for(w in 1:length(wsLst)){
 load(wsLst[[w]])
  modelLst[[w]]<-out$input$script.name
  rastLst<-out$dat$tif.ind
  if(w>1 & any(rastLst!=out$dat$tif.ind)) stop("Rasters Don't match for all workspaces")
  fitLst[[w]]<-out
  rm(out)
  mapLst[[w]]<-file.path(dirname(wsLst[[w]]),paste(modelLst[[w]],"prob_map.tif",sep="_"))
  varImpLst[[w]]<-getVarImp(dirname(wsLst[[w]]))
}
mapStk<<-stack(mapLst)
stk<-stack(rastLst)
maxImp<-max(unlist(lapply(varImpLst,max)))


Cols<<-c(wes_palette("Darjeeling"),wes_palette("GrandBudapest2"),wes_palette("Cavalcanti"),wes_palette("Moonrise3"))
max_plots<-5

Variables<<-unique(unlist(lapply(fitLst,FUN=function(fit){fit$mods$vnames})))

dat<<-fitLst[[1]]$dat$ma$train$dat[,-1]
resp<<-fitLst[[1]]$dat$ma$train$dat[,1]
 d=data.frame(Name=names(dat),min=apply(dat,2,min,na.rm=TRUE),
   max=apply(dat,2,max,na.rm=TRUE),mean=apply(dat,2,mean,na.rm=TRUE))
dataLst<<-split(d,f=seq(1:nrow(d)))
IntractVals<-vector()
rspHgt<-c("150px","300px","550px","750px")[length(fitLst)]

#=========================================
#    This is where the magic happens
runApp("C:\\GoogleDrive\\Python\\DevWorkspace\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules\\ResponseCurves")


#=============================================
#From the command line
C:\R-3.2.0\bin\x64\Rterm.exe --vanilla -f C:\GoogleDrive\Python\DevWorkspace\userpackages\sahm\pySAHM\Resources\R_Modules\ResponseCurveShinyApp.r --args port=5678 wsList=C:/temp/SAHM_workspace/ForResponseCurveTool/brewersSparrow/brt_1/modelWorkspace,C:/temp/SAHM_workspace/ForResponseCurveTool/brewersSparrow/glm_1/modelWorkspace,C:/temp/SAHM_workspace/ForResponseCurveTool/brewersSparrow/mars_1/modelWorkspace,C:/temp/SAHM_workspace/ForResponseCurveTool/brewersSparrow/rf_1/modelWorkspace


#=========================================
# scratch pad 
#switching to contour
r2<-sampleRegular(r,size=20000,xy=TRUE)
a<- matrix(r2[,3],nrow=length(unique(r2[,1])))
a<-a[,ncol(a):1]
rastTm<-Sys.time()
plot(r,maxpixels=100000,col=Colors)
Sys.time()-rastTm

rastTm<-Sys.time()
my.filled.contour(a, plot.axes = {},col=Colors,nlevels = 26)
Sys.time()-rastTm

vals<-rbind(c(.2,.1,50,-10,.2,.06,.5,2),
c(.9,-.6,50,-10,.2,.06,.5,2),
c(.2,.1,50,-10,.2,0,.17,2),
c(.2,.1,50,-10,.2,.06,.5,0))

responseCurves(f=list(fitLst[[1]]),m=list(modelLst[[1]]),varImp=list(varImpLst[[1]]),addImp=TRUE,vals)
interactionPlot(fitLst[[1]],modelLst[[1]],vals=NULL,theta=30,phi=25,x="Average_FirstFrost_sd",y="ppt_season_sd_3")
densityPlot(fitLst[[3]])

