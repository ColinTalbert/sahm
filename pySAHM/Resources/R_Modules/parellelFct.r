parRaster<-function(i,nrows,dims,tr,MESS,nvars,fullnames,nvars.final,vnames,NAval,factor.levels,model,Model,pred.fct,make.binary.tif,RasterInfo,outfile.p,outfile.bin,thresh) {
    library(raster)
      setwd(file.path("I:\\VisTrails\\VisTrails_SAHM_x64_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules"))
    ScriptPath=file.path("I:\\VisTrails\\VisTrails_SAHM_x64_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules")
    
    #Fitting Models and making predictions
    source("generic.model.fit.r")
    source("FitModels.r")
    source("model.fit.r")
    source("pred.fct.r")
    source("chk.libs.r")
    chk.libs(Model)
        
   continuousRaster<-raster(RasterInfo)
   outfile.p<-file.path(paste(substr(outfile.p,1,(nchar(outfile.p)-4)),i,".tiff",sep=""))
   outtext<-paste(substr(outfile.p,1,(nchar(outfile.p)-4)),i,".txt",sep="")
   capture.output(cat(i),file=outtext)
    capture.output(cat(ScriptPath),file=outtext,append=TRUE)
       
  continuousRaster <- writeStart(continuousRaster, filename=outfile.p, overwrite=TRUE)
  if(make.binary.tif) {
     binaryRaster<-raster(RasterInfo)
      binaryRaster <- writeStart(binaryRaster, filename=outfile.bin, overwrite=TRUE)}
      
   min.pred<-1
   max.pred<-0
 
   temp <- data.frame(matrix(ncol=nvars,nrow=tr$nrows[1]*ncol(RasterInfo))) # temp data.frame.
names(temp) <- vnames
    strt <- c((i-1)*nrows,0)
     region.dims <- c(min(dims[1]-strt[1],nrows),dims[2])

        if (i==tr$n) if(is.null(dim(temp))) { temp <- temp[1:(tr$nrows[i]*dims[2])]
                                              if(MESS) pred.rng<-pred.rng[1:(tr$nrows[i]*dims[2])]
        } else {temp <- as.data.frame(temp[1:(tr$nrows[i]*dims[2]),])
                      if(MESS) pred.rng<-pred.rng[1:(tr$nrows[i]*dims[2]),]
                }

         # for the last tile...
      for(k in 1:nvars) { # fill temp data frame
            if(is.null(dim(temp))){
              temp<- getValuesBlock(raster(fullnames[k]), row=tr$row[i], nrows=tr$nrows[i])
               temp<-as.data.frame(as.matrix(x=temp,nrow=(tr$nrows[i]*dims[2]),ncol=1))
            } else {temp[,k]<- getValuesBlock(raster(fullnames[k]), row=tr$row[i], nrows=tr$nrows[i])
                    }

            }

             if(MESS){
             for(k in 1:nvars.final){
                        pred.range<-out$dat$ma$train$dat[,match(vnames.final.mod[k],names(out$dat$ma$train$dat))]
                        if(nvars.final>1) pred.rng[,k]<-mapply(CalcMESS,tiff.entry=temp[,match(vnames.final.mod[k],names(temp))],MoreArgs=list(pred.vect=pred.range))
                        else pred.rng<-mapply(CalcMESS,tiff.entry=temp,MoreArgs=list(pred.vect=pred.range))
                         }
                      }
                if(length(vnames)==1) names(temp)=vnames

    temp[temp==NAval] <- NA # replace missing values #
        if(sum(!is.na(factor.levels))){
            factor.cols <- match(names(factor.levels),names(temp))
            if(sum(!is.na(factor.cols))>0){
            for(j in 1:length(factor.cols)){
                if(!is.na(factor.cols[j])){
                    temp[,factor.cols[j]] <- factor(temp[,factor.cols[j]],levels=factor.levels[[j]]$number,labels=factor.levels[[j]]$class)
                }
            }
                   }}
            capture.output(cat("\npred.fct"),file=outtext,append=TRUE)
    ifelse(sum(complete.cases(temp))==0,  # does not calculate predictions if all predictors in the region are na
        preds<-matrix(data=NA,nrow=region.dims[1],ncol=region.dims[2]),
      preds <- t(matrix(pred.fct(model,temp,Model),ncol=dims[2],byrow=T)))
        
        capture.output(cat("\npred.fct2"),file=outtext,append=TRUE)
       
        min.pred<-min(na.omit(preds),min.pred)
        max.pred<-max(na.omit(preds),max.pred)
        preds[is.na(preds)]<-NAval
         capture.output(cat("\nAfter pred.fct"),file=outtext,append=TRUE)
    ## Writing to the rasters u
     f<-function(x){
     if(any(is.na(x))) return(NA)
     a<-which(x==min(x),arr.ind=TRUE)
     if(length(a>1)) a<-sample(a,size=1)
     return(a)
    }
    if(MESS) {
    MessRaster<-writeValues(MessRaster,apply(pred.rng,1,min), tr$row[i])
    if(!is.null(dim(pred.rng)[2])) a<-apply(as.matrix(pred.rng),1,f)
    else a<-rep(1,times=length(pred.rng))
    #if(is.list(a)) a<-unlist(a)
      ModRaster<-writeValues(ModRaster,a, tr$row[i])
    }
      if(make.binary.tif) binaryRaster<-writeValues(binaryRaster,(preds>thresh),tr$row[i])
   continuousRaster <- writeValues(continuousRaster,preds, tr$row[i])
   continuousRaster <- writeStop(continuousRaster)

  if(make.binary.tif) {
    writeStop(binaryRaster)
  }

  #NAvalue(continuousRaster) <-NAval
        rm(preds);gc() #why is gc not working on the last call
}
