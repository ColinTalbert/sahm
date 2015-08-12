###############################################################################
##
## Copyright (C) 2010-2012, USGS Fort Collins Science Center. 
## All rights reserved.
## Contact: talbertc@usgs.gov
##
## This file is part of the Software for Assisted Habitat Modeling package
## for VisTrails.
##
## "Redistribution and use in source and binary forms, with or without 
## modification, are permitted provided that the following conditions are met:
##
##  - Redistributions of source code must retain the above copyright notice, 
##    this list of conditions and the following disclaimer.
##  - Redistributions in binary form must reproduce the above copyright 
##    notice, this list of conditions and the following disclaimer in the 
##    documentation and/or other materials provided with the distribution.
##  - Neither the name of the University of Utah nor the names of its 
##    contributors may be used to endorse or promote products derived from 
##    this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
## THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
## PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
## CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
## EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
## PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
## OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
## OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
## ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
##
## Although this program has been used by the U.S. Geological Survey (USGS), 
## no warranty, expressed or implied, is made by the USGS or the 
## U.S. Government as to the accuracy and functioning of the program and 
## related program material nor shall the fact of distribution constitute 
## any such warranty, and no responsibility is assumed by the USGS 
## in connection therewith.
##
## Any use of trade, firm, or product names is for descriptive purposes only 
## and does not imply endorsement by the U.S. Government.
###############################################################################

# Interpret command line argurments #
# Make Function Call #
 # Interpret command line argurments #
# Make Function Call #

Args <- commandArgs(trailingOnly=FALSE)

    for (i in 1:length(Args)){
     if(Args[i]=="-f") ScriptPath<-Args[i+1]
     argSplit <- strsplit(Args[i], "=")
     if(argSplit[[1]][1]=="--file") ScriptPath <- argSplit[[1]][2]
     }

    print(Args)
    for (arg in Args) {
    	argSplit <- strsplit(arg, "=")
    	argSplit[[1]][1]
    	argSplit[[1]][2]
    	if(argSplit[[1]][1]=="port") Port <- as.numeric(argSplit[[1]][2])
    	if(argSplit[[1]][1]=="wsList") wsList <- as.character(argSplit[[1]][2])
    }
    
wsLst<-as.list(strsplit(wsList,",")[[1]])
 #scriptPath gets renamed when we load workspaces so we use ScrptPath
 ScrptPath<-ScriptPath<-dirname(ScriptPath)

#load the SAHM code

source(file.path(ScrptPath,"LoadRequiredCode.r"))
source(file.path(ScrptPath,"MARS.helper.fcts.r"))
source(file.path(ScrptPath,"GLM.helper.fcts.r"))
source(file.path(ScrptPath,"BRT.helper.fcts.r"))
source(file.path(ScrptPath,"RF.helper.fcts.r"))
source(file.path(ScrptPath,"MAXENT.helper.fcts.r"))

ShinyCode<-file.path(ScrptPath,"ResponseCurves\\External")
sourceList<-list.files(ShinyCode,full.names=TRUE)
unlist(lapply(as.list(sourceList),source))

ChkLibs(list("gbm","randomForest","maptools","rgdal","shiny","leaflet","maptools","rgdal","raster","ncdf4","fields","maps",
            "ggplot2","zoo","XML","RColorBrewer","chron","wesanderson","sm"))


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


Cols<<-c(wes_palette("Darjeeling"),wes_palette("GrandBudapest2"),wes_palette("Cavalcanti"),wes_palette("Moonrise3"))
max_plots<-5
Variables<<-unique(unlist(lapply(fitLst,FUN=function(fit){fit$mods$vnames})))

dat<-fitLst[[1]]$dat$ma$train$dat[,-1]
resp<-fitLst[[1]]$dat$ma$train$dat[,1]

 d=data.frame(Name=names(dat),min=apply(dat,2,min,na.rm=TRUE),
   max=apply(dat,2,max,na.rm=TRUE),mean=apply(dat,2,mean,na.rm=TRUE))
dataLst<<-split(d,f=seq(1:nrow(d)))
rspHgt<-c("150px","300px","550px","750px")[length(fitLst)]   
#=========================================
#    This is where the ma


runApp(file.path(ScrptPath,"ResponseCurves"),port=Port)
 

