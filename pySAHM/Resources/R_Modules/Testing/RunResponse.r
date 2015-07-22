setwd("C:\\GoogleDrive\\Python\\DevWorkspace\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules")
ScriptPath="C:\\GoogleDrive\\Python\\DevWorkspace\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules"


source("LoadRequiredCode.r")
source("MARS.helper.fcts.r")
source("GLM.helper.fcts.r")
source("BRT.helper.fcts.r")
source("RF.helper.fcts.r")
source("MAXENT.helper.fcts.r")
#setwd("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\MyCode")
sourceList<-list("ResponseCurves\\external\\ChkLibs.r","ResponseCurves\\external\\Colors.r","ResponseCurves\\external\\response.curvesOneModel.r","ResponseCurves\\external\\interactionPlot.r")
unlist(lapply(sourceList,source))
ChkLibs(list("gbm","randomForest","maptools","rgdal","shiny","leaflet","maptools","rgdal","raster","ncdf4","fields","maps",
            "ggplot2","zoo","XML","RColorBrewer","chron","wesanderson"))


wsLst<-list()
wsLst[[1]]<-"C:\\temp\\SAHM_workspace\\ForResponseCurveTool\\brewersSparrow\\brt_1\\modelWorkspace"
wsLst[[2]]<-"C:\\temp\\SAHM_workspace\\ForResponseCurveTool\\brewersSparrow\\glm_1\\modelWorkspace"
wsLst[[3]]<-"C:\\temp\\SAHM_workspace\\ForResponseCurveTool\\brewersSparrow\\mars_1\\modelWorkspace"
wsLst[[4]]<-"C:\\temp\\SAHM_workspace\\ForResponseCurveTool\\brewersSparrow\\rf_1\\modelWorkspace"

fitLst<-list()
modelLst<-list()
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
}
mapStk<<-stack(mapLst)
stk<-stack(rastLst)


Cols<<-c(wes_palette("Darjeeling"),wes_palette("Moonrise3"))
max_plots<-5
nModels<<-4
Variables<<-unique(unlist(lapply(fitLst,FUN=function(fit){fit$mods$vnames})))

dat<-fitLst[[1]]$dat$ma$train$dat[,-1]
resp<-fitLst[[1]]$dat$ma$train$dat[,1]
 d=data.frame(Name=names(dat),min=apply(dat,2,min,na.rm=TRUE),
   max=apply(dat,2,max,na.rm=TRUE),mean=apply(dat,2,mean,na.rm=TRUE))
dataLst<-split(d,f=seq(1:nrow(d)))
       
#=========================================
#    This is where the ma
runApp("C:\\GoogleDrive\\Python\\DevWorkspace\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules\\ResponseCurves")


#=========================================
# scratch pad 
par(mfrow=c(2,2),mar=c(0,0,2,0),oma=c(0,0,0,0))
 response.curvesInteraction(fitLst[[1]],modelLst[[1]],vals,phi=phi,theta=theta)
  response.curvesInteraction(fitLst[[2]],modelLst[[2]],vals,phi=phi,theta=theta)
  response.curvesInteraction(fitLst[[3]],modelLst[[3]],vals,phi=phi,theta=theta)
  response.curvesInteraction(fitLst[[4]],modelLst[[4]],vals,phi=phi,theta=theta)
vals<-rbind(c(.2,.1,50,-10,.2,.06,.5,2),
c(.9,-.6,50,-10,.2,.06,.5,2),
c(.2,.1,50,-10,.2,0,.17,2),
c(.2,.1,50,-10,.2,.06,.5,0))
response.curvesInteraction(fitLst[[3]],modelLst[[3]],vals)
response.curves(fitLst,modelLst,vals)
response.curvesOneModel(fitLst[[3]],modelLst[[3]],vals) 