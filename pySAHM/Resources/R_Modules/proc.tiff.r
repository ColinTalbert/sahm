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

proc.tiff<- function(model,vnames,tif.dir=NULL,filenames=NULL,factor.levels=NA,make.binary.tif=F,make.p.tif=T,
   thresh=0.5,outfile.p="brt.prob.map.tif",outfile.bin="brt.bin.map.tif",tsize=2.0,NAval=-3000,fnames=NULL,out,Model){

    # vnames,fpath,myfun,make.binary.tif=F,outfile=NA,outfile.bin=NA,output.dir=NA,tsize=10.0,NAval=NA,fnames=NA
    # Written by Alan Swanson, YERC, 6-11-08
    # Revised and Edited by Marian Talbert 2010-2011
    # Description:
    # This function is used to make predictions using a number of .tiff image inputs
    # in cases where memory limitations don't allow the full images to be read in.
    #
    # Arguments:
    # vname: names of variables used for prediction.  must be same as filenames and variables
    #   in model used for prediction. do not include a .tif extension as this is added in code.
    # fpath: path to .tif files for predictors. use forward slashes and end with a forward slash ('/').
    # myfun: prediction function.  must generate a vector of predictions using only a
    #   dataframe as input.
    # outfile:  name of output file.  placed in same directory as input .tif files.  should
    #   have .tif extension in name.
    # tsize: size of dataframe used for prediction in MB.  this controls the size of tiles
    #   extracted from the input files, and the memory usage of this function.
    # NAval: this is the NAvalue used in the input files.
    # fnames: if the filenames of input files are different from the variable names used in the
    #   prediction model.
   #
    # Modification history:
    # Fixed problem with NA values causing crash 10/2010
    # Included code to produce MESS map and Mod map  8/2011
    # Removed Tiff Directory option as well as some other unused code 10/2011
    #
    # Description:
    # This function reads in a limited number of lines of each image (specified in terms of the
    # size of the temporary predictor dataframe), applies a user-specified
    # prediction function, and stores the results as matrix.  Alternatively, if an
    # output file is specified, a file is written directly to that file in .tif format to
    # the same directory as the input files.  Geographic information from the input images
    # is retained.
    #

    # Start of function #
    
    if(is.null(factor.levels)) factor.levels<-NA
    MESS=out$input$MESS
    if(is.null(thresh)) thresh<-.5
    nvars<-length(vnames)
    vnames.final.mod<-out$mods$vnames
    if(any(out$mods$vnames%in%names(factor.levels))) vnames.final.mod<-vnames.final.mod[!out$mods$vnames%in%names(factor.levels)]
    nvars.final<-length(vnames.final.mod)
# setup up output raster to match input raster
names(filenames)<-sub("_categorical","",names(filenames))
          fullnames <- as.character(filenames[match(vnames,names(filenames))])
          goodfiles <- file.access(fullnames)==0
          if(!all(goodfiles)) stop(paste("ERROR: the following image files are missing:",paste(fullnames[!goodfiles],collapse=", ")))

if(nvars.final<=1) MESS=FALSE
 
 ######################################
 # get spatial reference info from existing image file
 options(warn=-1)
    gi <- GDALinfo(fullnames[1])
options(warn=1)
    dims <- as.vector(gi)[1:2]
    ps <- as.vector(gi)[6:7]
    ll <- as.vector(gi)[4:5]
    pref<-attr(gi,"projection")

RasterInfo=raster(fullnames[1])
RasterInfo@file@datanotation<-"FLT4S"
NAval<- -3.399999999999999961272e+38

#To remove use of the Raster package I need to see if rgdal handles area or point correctly
if(!is.na(match("AREA_OR_POINT=Point",attr(gi,"mdata")))){
   xx<-RasterInfo  #this shifts by a half pixel
nrow(xx) <- nrow(xx) - 1
ncol(xx) <- ncol(xx) - 1
rs <- res(xx)
xmin(RasterInfo) <- xmin(RasterInfo) - 0.5 * rs[1]
xmax(RasterInfo) <- xmax(RasterInfo) - 0.5 * rs[1]
ymin(RasterInfo) <- ymin(RasterInfo) + 0.5 * rs[2]
ymax(RasterInfo) <- ymax(RasterInfo) + 0.5 * rs[2]
 }

    # setting tile size
    MB.per.row<-dims[2]*nvars*32/8/1000/1024
    if(MESS) MB.per.row<-MB.per.row*3 #use more blocks for mess
    nrows<-min(round(tsize/MB.per.row),dims[1])
    bs<-c(nrows,dims[2])
    chunksize<-bs[1]*bs[2]
    tr<-blockSize(RasterInfo,chunksize=chunksize)


FactorInd<-which(!is.na(match(names(temp),names(factor.levels))),arr.ind=TRUE)
  if((nvars-length(FactorInd))==0) MESS<-FALSE #turn this off if only one factor column was selected
  if(MESS) {
     MessRaster<-raster(RasterInfo)
     ModRaster<-raster(RasterInfo)
      MessRaster <- writeStart(MessRaster, filename=sub("bin","mess",outfile.bin), overwrite=TRUE)
      ModRaster <- writeStart(ModRaster, filename=sub("bin","MoD",outfile.bin), overwrite=TRUE)
      pred.rng<-temp[,names(temp)%in%vnames.final.mod]
      
    }

  if((out$input$ResidMaps)) Pred.Surface(object=RasterInfo,model=out$mods$auc.output$residual.smooth.fct,filename=sub("prob_map.tif","resid_map.tif",outfile.p),NAval=NAval)
   if(Model=="maxlike"){
   #this is a bit ugly to copy these sections of code but I'm hoping maxlike will eventually be able to predict to a vector instead of a raster in which case
   #this will all be deleted anyway
         model$call$formula<-eval(model$call$formula)
        y <- predict(model,rasters=stack(model$rast.lst))
         writeRaster(y,outfile.p)
         if(make.binary.tif) {
           y<-y>thresh
           writeRaster(y,outfile.bin)
           }
         if(MESS) warning("Maxlike mess option currently nonfuctional") 
         return(0) 
    }
if(out$input$make.p.tif)
  dir.create(paste(out$input$output.dir,"\\ProbTiff",sep=""))
  outfile.p=paste(paste(out$input$output.dir,"\\ProbTiff\\",sep=""),"_prob_map.tif",sep="")
if(out$input$make.binary.tif)
  dir.create(paste(out$input$output.dir,"\\BinTiff",sep=""))
if(MESS)
  dir.create(paste(out$input$output.dir,"\\MESSTiff",sep=""))    
  

cl <- makeCluster(detectCores())
parLapply(cl,X=1:tr$n,fun=parRaster,nrows=nrows,dims=dims,
   tr=tr,MESS=MESS,nvars=nvars,fullnames=fullnames,nvars.final=nvars.final,vnames=vnames,NAval=NAval,factor.levels=factor.levels,
   model=model,Model=Model,pred.fct=pred.fct,make.binary.tif=make.binary.tif,RasterInfo=RasterInfo,outfile.p=outfile.p,
   outfile.bin=outfile.bin,thresh=thresh,nToDo=1,ScriptPath=out$input$ScriptPath)
stopCluster(cl)

   return(0)
   }
CalcMESS<-function(tiff.entry,pred.vect){
              f<-sum(pred.vect<tiff.entry)/length(pred.vect)*100
              if(is.na(f)) return(NA)
              if(f==0) return((tiff.entry-min(pred.vect))/(max(pred.vect)-min(pred.vect))*100)
              if(0<f & f<=50) return(2*f)
              if(50<=f & f<100) return(2*(100-f))
              if(f==100) return((max(pred.vect)-tiff.entry)/(max(pred.vect)-min(pred.vect))*100)
              else return(NA)
}