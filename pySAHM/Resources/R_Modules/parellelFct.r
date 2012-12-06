parRaster<-function(start.tile,nrows,dims,tr,MESS,nvars,fullnames,nvars.final,vnames,NAval,
factor.levels,model,Model,pred.fct,make.binary.tif,RasterInfo,outfile.p,outfile.bin,thresh,nToDo,ScriptPath) {
    #loading code and libraries that are needed
    setwd(file.path(ScriptPath))
    source("generic.model.fit.r")
    source("FitModels.r")
    source("model.fit.r")
    source("pred.fct.r")
    source("chk.libs.r")
    source(paste(toupper(Model),".helper.fcts.r",sep=""))
    chk.libs(Model)
        
   continuousRaster<-raster(RasterInfo)
   outfile.p<-file.path(paste(substr(outfile.p,1,(nchar(outfile.p)-4)),start.tile,".tiff",sep=""))
   outtext<-paste(substr(outfile.p,1,(nchar(outfile.p)-4)),start.tile,".txt",sep="")
   capture.output(cat(start.tile),file=outtext)
    capture.output(cat(ScriptPath),file=outtext,append=TRUE)
    
    #start up any rasters we need   
    continuousRaster <- writeStart(continuousRaster, filename=outfile.p, overwrite=TRUE)
    if(make.binary.tif) {
      binaryRaster<-raster(RasterInfo)
      binaryRaster <- writeStart(binaryRaster, filename=outfile.bin, overwrite=TRUE)}
    if(MESS) {
      MessRaster<-raster(RasterInfo)
      ModRaster<-raster(RasterInfo)
      MessRaster <- writeStart(MessRaster, filename=sub("bin","mess",outfile.bin), overwrite=TRUE)
      ModRaster <- writeStart(ModRaster, filename=sub("bin","MoD",outfile.bin), overwrite=TRUE)
      pred.rng<-temp[,names(temp)%in%vnames.final.mod]  
    }
    
 for (i in start.tile:min(start.tile+nToDo-1,length(tr$row))){
       temp <- data.frame(matrix(ncol=nvars,nrow=tr$nrows[i]*dims[2]))
       names(temp) <- vnames
       if(MESS) pred.rng<-pred.rng[1:(tr$nrows[i]*dims[2]),]
       
       # fill temp data frame         
      for(k in 1:nvars) 
           temp[,k]<- getValuesBlock(raster(fullnames[k]), row=tr$row[i], nrows=tr$nrows[i])
          

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
        preds<-matrix(data=NA,nrow=dims[2],ncol=tr$nrows[i]),
        preds <- t(matrix(pred.fct(model,temp,Model),ncol=dims[2],byrow=T)))
       # preds<-matrix(data=runif(dims[2]*tr$nrows[i]),nrow=dims[2],ncol=tr$nrows[i])
        capture.output(cat("\npred.fct2"),file=outtext,append=TRUE)
       
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
          ModRaster<-writeValues(ModRaster,a, tr$row[i])
        }
          if(make.binary.tif) binaryRaster<-writeValues(binaryRaster,(preds>thresh),tr$row[i])
       continuousRaster <- writeValues(continuousRaster,preds, tr$row[i])
   } #end of the big for loop
   
   #closing and cropping the files
   end.seq<-c(tr$row,dims[1]+1)
   e<-extent(continuousRaster, tr$row[start.tile],(end.seq[start.tile+nToDo]-1),1,dims[2])
   continuousRaster <- writeStop(continuousRaster)
   crop(continuousRaster,e, filename=outfile.p,overwrite=TRUE, snap='near')
  if(make.binary.tif) {
    writeStop(binaryRaster)
    crop(binaryRaster, e, filename=outfile.p,overwrite=TRUE, snap='near')
  }
  if(MESS) {
    writeStop(MessRaster)
    writeStop(ModRaster)
    crop(MessRaster,e, filename=outfile.p,overwrite=TRUE, snap='near')
    crop(ModRaster,e, filename=outfile.p,overwrite=TRUE, snap='near')
      d<-data.frame(as.integer(seq(1:ncol(pred.rng))),names(pred.rng))
      names(d)=c("Value","Class")
      ModRaster@file@datanotation<-"INT1U"
      write.dbf(d, sub(".tif",".tif.vat.dbf",ModRaster@file@name), factor2char = TRUE, max_nchar = 254)

  }
}
