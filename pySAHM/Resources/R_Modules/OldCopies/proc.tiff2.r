proc.tiff<- function(model,vnames,tif.dir=NULL,filenames=NULL,pred.fct,factor.levels=NA,make.binary.tif=F,make.p.tif=T,
    thresh=0.5,outfile.p="brt.prob.map.tif",outfile.bin="brt.bin.map.tif",tsize=2.0,NAval=-3000,fnames=NULL,logname=NULL,out){

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
    # NA
    #
    # Description:
    # This function reads in a limited number of lines of each image (specified in terms of the
    # size of the temporary predictor dataframe), applies a user-specified
    # prediction function, and stores the results as matrix.  Alternatively, if an
    # output file is specified, a file is written directly to that file in .tif format to
    # the same directory as the input files.  Geographic information from the input images
    # is retained.
    #
    # Example:
    # tdata <- read.csv("D:/yerc/LISN biodiversity/resource selection/split/05_GYA_Leafyspurge_reduced_250m_train.csv")
    # tdata$gya_250m_evi_16landcovermap_4ag05<-factor(tdata$gya_250m_evi_16landcovermap_4ag05)
    # f.levels <- levels(tdata$gya_250m_evi_16landcovermap_4ag05)
    # m0 <- glm(pres_abs~gya_250m_evi_01greenup_4ag05+gya_250m_evi_02browndown_4ag05+gya_250m_evi_03seasonlength_4ag05+
    #          gya_250m_evi_04baselevel_4ag05+gya_250m_evi_05peakdate_4ag05+gya_250m_evi_16landcovermap_4ag05,data=tdata,family=binomial())#
    # glm.predict <- function(x) {
    #   x$gya_250m_evi_16landcovermap_4ag05<-factor(x$gya_250m_evi_16landcovermap_4ag05,levels=f.levels)
    #   y <- as.vector(predict(m0,x,type="response"))
    #   y[is.na(y)]<- -1
    #   return(y)
    # }
    # x<-glm.predict(temp)
    # fnames <- names(tdata)[c(10:14,25)]
    # vnames <- fnames
    # fpath <- 'D:/yerc/LISN biodiversity/GYA data/gya_250m_tif_feb08_2/'
    # x <- proc.tiff(vnames,fpath,glm.predict)
    # proc.tiff(vnames,fpath,glm.predict,"test11.tif")

    # Start of function #
    library(rgdal)
    library(raster)
MESS=out$input$MESS
    if(is.null(thresh)) thresh<-.5
    if(is.na(NAval)) NAval<- -3000
    if(is.null(fnames)) fnames <- paste(vnames,"tif",sep=".")
    nvars<-length(vnames)

    # check availability of image files #
   if(!is.null(tif.dir)){
      fnames <- fnames[match(vnames,basename(sub(".tif","",fnames)))]
      fullnames <- paste(tif.dir,fnames,sep="/")
      goodfiles <- file.access(fullnames)==0
      if(!all(goodfiles)){
          cat('\n',paste("ERROR: the following image files are missing:",paste(fullnames[!goodfiles],collapse=", ")),'\n','\n')
         flush.console()
          return(paste("ERROR: the following image files are missing:",paste(fullnames[!goodfiles],collapse=", ")))
          }}
# settup up output raster to match input raster
       if(!is.null(filenames)){
          fullnames <- as.character(filenames[match(vnames,basename(sub(".tif","",filenames)))])
          goodfiles <- file.access(fullnames)==0
        if(!all(goodfiles)){
          cat('\n',paste("ERROR: the following image files are missing:",paste(fullnames[!goodfiles],collapse=", ")),'\n','\n')
         flush.console()
          return(paste("ERROR: the following image files are missing:",paste(fullnames[!goodfiles],collapse=", ")))
          }}

 ######################################
 # get spatial reference info from existing image file
options(warn=-1)
    gi <- GDALinfo(fullnames[1])
options(warn=0)
    dims <- as.vector(gi)[1:2]
    ps <- as.vector(gi)[6:7]
    ll <- as.vector(gi)[4:5]
    pref<-attr(gi,"projection")

RasterInfo=raster(fullnames[1])
RasterInfo@file@datanotation<-"FLT4S"
NAval<- -3.399999999999999961272e+38


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
    # calculate position of upper left corner and get geotransform ala http://www.gdal.org/gdal_datamodel.html
    #ul <- c(ll[1]-ps[1]/2,ll[2]+(dims[1]+.5)*ps[2])
    ul <- c(ll[1],ll[2]+(dims[1])*ps[2])
    gt<-c(ul[1],ps[1],0,ul[2],0,ps[2])

    # setting tile size
    MB.per.row<-dims[2]*nvars*32/8/1000/1024

    nrows<-min(round(tsize/MB.per.row),dims[1])
    bs<-c(nrows,dims[2])
    nbs <- ceiling(dims[1]/nrows)
    inc<-round(10/nbs,1)

    chunksize<-bs[1]*bs[2]
    tr<-blockSize(RasterInfo,chunksize=chunksize)

  continuousRaster<-raster(RasterInfo)
  continuousRaster <- writeStart(continuousRaster, filename=outfile.p, overwrite=TRUE)
    if(make.binary.tif) {
     binaryRaster<-raster(RasterInfo)
      binaryRaster <- writeStart(binaryRaster, filename=outfile.bin, overwrite=TRUE)}
    if(MESS) {
     MessRaster<-raster(RasterInfo)
     ModRaster<-raster(RasterInfo)
      MessRaster <- writeStart(MessRaster, filename=sub("bin","mess",outfile.bin), overwrite=TRUE)
      ModRaster <- writeStart(ModRaster, filename=sub("bin","MoD",outfile.bin), overwrite=TRUE)
      }
      
temp <- data.frame(matrix(ncol=nvars,nrow=tr$size*ncol(RasterInfo))) # temp data.frame.
names(temp) <- vnames
    if(MESS) {
      pred.rng<-temp
        CalcMESS<-function(tiff.entry,pred.vect){
              f<-sum(pred.vect<tiff.entry)/length(pred.vect)*100
              if(is.na(f)) return(NA)
              if(f==0) return((tiff.entry-min(pred.vect))/(max(pred.vect)-min(pred.vect))*100)
              if(0<f & f<=50) return(2*f)
              if(50<=f & f<100) return(2*(100-f))
              if(f==100) return((max(pred.vect)-tiff.entry)/(max(pred.vect)-min(pred.vect))*100)
              else return(NA)
        }
    }

 Pred.Surface(object=RasterInfo,model=out$mods$auc.output$residual.smooth.fct,filename=sub("prob_map.tif","resid_map.tif",outfile.p),NAval=NAval)

  min.pred<-1
  max.pred<-0

  for (i in 1:tr$n) {
   if(i==2) browser()
    strt <- c((i-1)*nrows,0)
     region.dims <- c(min(dims[1]-strt[1],nrows),dims[2])
        if (i==tr$n) if(ncol(temp)==1) { temp <- temp[1:(tr$nrows[i]*dims[2]),]
                                              if(MESS) pred.rng<-pred.rng[1:(tr$nrows[i]*dims[2])]
        } else {temp <- temp[1:(tr$nrows[i]*dims[2]),]
                      if(MESS) pred.rng<-as.matrix(pred.rng[1:(tr$nrows[i]*dims[2]),])
                }
         # for the last tile...
      for(k in 1:nvars) { # fill temp data frame
            if(is.null(dim(temp))){
              temp<- getValuesBlock(raster(fullnames[k]), row=tr$row[i], nrows=tr$size)
            } else {temp[,k]<- getValuesBlock(raster(fullnames[k]), row=tr$row[i], nrows=tr$size)
                    }
                  if(MESS){
                        pred.range<-as.matrix(out$dat$ma$ma[,c(match(sub(".tif","",basename(fullnames[k])),names(out$dat$ma$ma)))])
                        if(is.null(names(pred.range))) colnames(pred.range)=basename(fullnames[k])
                        if(nvars>1) pred.rng[,k]<-mapply(CalcMESS,tiff.entry=temp[,k],MoreArgs=list(pred.vect=pred.range))
                        else pred.rng<-apply(temp,1,CalcMESS,pred.vect=pred.range)
                         }
            }
   # temp[temp==NAval] <- NA # replace missing values #
  #  temp[is.na(temp)]<-NA #this seemingly worthless line switches NaNs to NA so they aren't predicted
        if(sum(!is.na(factor.levels))>0){
            factor.cols <- match(names(factor.levels),names(temp))
            if(sum(!is.na(factor.cols))>0){
            for(j in 1:length(factor.cols)){
                if(!is.na(factor.cols[j])){
                    temp[,factor.cols[j]] <- factor(temp[,factor.cols[j]],levels=factor.levels[[j]]$number,labels=factor.levels[[j]]$class)
                }
            }
            if(nvars>1) pred.rng<-pred.rng[,-c(factor.cols)] #not currently considering factor columns because it's not obvious how to
                   else pred.rng<-NA
                   }}
    ifelse(sum(!is.na(temp))==0,  # does not calculate predictions if all predictors in the region are na
        preds<-matrix(data=NaN,nrow=region.dims[1],ncol=region.dims[2]),
        preds <- t(matrix(pred.fct(model,temp),ncol=dims[2],byrow=T)))
        min.pred<-min(na.omit(preds),min.pred)
        max.pred<-max(na.omit(preds),max.pred)
        preds[is.na(preds)]<-NAval
    ## Writing to the rasters u
     f<-function(x){
     a<-which(x==min(x),arr.ind=TRUE)
     if(length(a>1)) a<-sample(a,size=1)
     return(a)
     }
    if(MESS) {
    MessRaster<-writeValues(MessRaster,apply(as.matrix(pred.rng),1,min), tr$row[i])
    if(!is.null(dim(pred.rng)[2])) a<-apply(as.matrix(pred.rng),1,f)
    else a<-rep(1,times=length(pred.rng))
    if(is.list(a)) a<-unlist(a)
      ModRaster<-writeValues(ModRaster,a, tr$row[i])
    }
      if(make.binary.tif) binaryRaster<-writeValues(binaryRaster,(preds>thresh),tr$row[i])
   continuousRaster <- writeValues(continuousRaster,preds, tr$row[i])
  #NAvalue(continuousRaster) <-NAval
        rm(preds);gc() #why is gc not working on the last call
}

  continuousRaster <- writeStop(continuousRaster)
    a<-readGDAL(continuousRaster@file@name)
    writeGDAL(a,continuousRaster@file@name, drivername = "GTiff",setStatistics=TRUE,mvFlag=NAval)



  if(make.binary.tif) {
    writeStop(binaryRaster)
     a<-readGDAL(binaryRaster@file@name)
    writeGDAL(a,binaryRaster@file@name, drivername = "GTiff",setStatistics=TRUE,mvFlag=NAval)
  }
   if(MESS) {
    writeStop(MessRaster)
      a<-readGDAL(MessRaster@file@name)
      writeGDAL(a,MessRaster@file@name, drivername = "GTiff",setStatistics=TRUE,mvFlag=NAval)

    writeStop(ModRaster)
      a<-readGDAL(ModRaster@file@name)
      d<-data.frame(as.integer(seq(1:ncol(pred.rng))),names(pred.rng))
      names(d)=c("Value","Class")
      ModRaster@file@datanotation<-"INT1U"
      write.dbf(d, sub(".tif",".tif.vat.dbf",ModRaster@file@name), factor2char = TRUE, max_nchar = 254)
      writeGDAL(a,ModRaster@file@name, drivername = "GTiff",setStatistics=TRUE,mvFlag=255,type="UInt16")
  }

   return(0)
   }