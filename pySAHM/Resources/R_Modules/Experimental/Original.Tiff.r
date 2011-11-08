proc.tiff <- function(model,vnames,tif.dir,pred.fct,factor.levels=NA,make.binary.tif=F,make.p.tif=T,binary.thresh=NA,
    thresh=0.5,outfile.p="brt.prob.map.tif",outfile.bin="brt.bin.map.tif",tsize=2.0,NAval=-3000,fnames=NULL,logname=NULL){
    # vnames,fpath,myfun,make.binary.tif=F,outfile=NA,outfile.bin=NA,output.dir=NA,tsize=10.0,NAval=NA,fnames=NA
    # Written by Alan Swanson, YERC, 6-11-08
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
    require(rgdal)

    if(is.na(NAval)) NAval<- -3000
    if(is.null(fnames)) fnames <- paste(vnames,"tif",sep=".")
    nvars<-length(vnames)

    # check availability of image files #
    fnames <- fnames[match(vnames,basename(sub(".tif","",fnames)))]
    fullnames <- paste(tif.dir,fnames,sep="/")
    goodfiles <- file.access(fullnames)==0
    if(!all(goodfiles)){
        cat('\n',paste("ERROR: the following image files are missing:",paste(fullnames[!goodfiles],collapse=", ")),'\n','\n')
        flush.console()
        return(paste("ERROR: the following image files are missing:",paste(fullnames[!goodfiles],collapse=", ")))
        }

    # get spatial reference info from existing image file
    gi <- GDALinfo(fullnames[1])
    dims <- as.vector(gi)[1:2]
    ps <- as.vector(gi)[6:7]
    ll <- as.vector(gi)[4:5]
    pref<-attr(gi,"projection")

    # calculate position of upper left corner and get geotransform ala http://www.gdal.org/gdal_datamodel.html
    #ul <- c(ll[1]-ps[1]/2,ll[2]+(dims[1]+.5)*ps[2])
    ul <- c(ll[1],ll[2]+(dims[1])*ps[2])
    gt<-c(ul[1],ps[1],0,ul[2],0,ps[2])

    # create transient dataset to hold output maps, and add spatial reference info
    tds <- new("GDALTransientDataset",new('GDALDriver','GTiff'),rows=dims[1],cols=dims[2],type="Float32")
    .Call("RGDAL_SetProject", tds, pref, PACKAGE = "rgdal")
    .Call("RGDAL_SetGeoTransform", tds, gt, PACKAGE = "rgdal")

    MB.per.row<-dims[2]*nvars*32/8/1000/1024

    nrows<-min(round(tsize/MB.per.row),dims[1])
    bs<-c(nrows,dims[2])
    nbs <- ceiling(dims[1]/nrows)
    inc<-round(10/nbs,1)

    # create binary tif if necessary #
    if(make.binary.tif){
        btds <- new("GDALTransientDataset",new('GDALDriver','GTiff'),rows=dims[1],cols=dims[2],type="Byte")
        .Call("RGDAL_SetProject", btds, pref, PACKAGE = "rgdal")
        .Call("RGDAL_SetGeoTransform", btds, gt, PACKAGE = "rgdal")
        }

    temp <- data.frame(matrix(ncol=nvars,nrow=nrows*dims[2])) # temp data.frame.
    names(temp) <- vnames

    # loop through tiles and apply prediction function #
    for(i in 1:nbs){
        strt <- c((i-1)*nrows,0)
        region.dims <- c(min(dims[1]-strt[1],nrows),dims[2])
        if (i==nbs) temp <- temp[1:(region.dims[1]*region.dims[2]),] # for the last tile...
        for(k in 1:nvars) { # fill temp data frame
            handle <-suppressMessages(GDAL.open(fullnames[k],read.only=T))
            temp[,k]<-as.vector(suppressMessages(getRasterData(handle,offset=strt,region.dim=region.dims)))
            suppressMessages(GDAL.close(handle))
            }
        temp[temp==NAval] <- NA # replace missing values #
        if(!is.na(factor.levels)){
            factor.cols <- match(names(factor.levels),names(temp))
            for(j in 1:length(factor.cols)){
                if(!is.na(factor.cols[j])){
                    temp[,factor.cols[j]] <- factor(temp[,factor.cols[j]],levels=factor.levels[[j]]$number,labels=factor.levels[[j]]$class)
                }
            }
        }
        preds <- matrix(pred.fct(model,temp),ncol=dims[2],byrow=T)
        putRasterData(tds,t(preds), offset= c(strt[1], 0))
        if(make.binary.tif) putRasterData(btds,t(preds>thresh), offset= c(strt[1], 0))

        rm(preds);gc()
        if(!is.null(logname) & i!=nbs) {sink();cat("Progress:",paste(round(80+i*inc,1),"%\n",sep=""));sink(logname,append=T);flush.console()}
        if(is.null(logname) & i!=nbs) {cat(paste(round(80+i*inc,1),"%\n",sep=""));flush.console()}
        }
    saveDataset(tds,outfile.p)
    GDAL.close(tds)
    if(make.binary.tif){
        saveDataset(btds,outfile.bin)
        GDAL.close(btds)
        }
   return(0)
   }


get.image.info <- function(image.names){
    # this function creates a data.frame with summary image info for a set of images #
    require(rgdal)
    n.images <- length(image.names)

    full.names <- image.names
    out <- data.frame(image=full.names,available=rep(F,n.images),size=rep(NA,n.images),
        type=factor(rep("unk",n.images),levels=c("asc","envi","tif","unk")))
    out$type[grep(".tif",image.names)]<-"tif"
    out$type[grep(".asc",image.names)]<-"asc"
    for(i in 1:n.images){
        if(out$type[i]=="tif"){
            x <-try(GDAL.open(full.names[1],read.only=T),silent=T)
            suppressMessages(try(GDAL.close(x),silent=T))
            if(class(x)!="try-error") out$available[i]<-T
            x<-try(file.info(full.names[i]))
        } else {
            x<-try(file.info(full.names[i]))
            if(!is.na(x$size)) out$available[i]<-T
        }
        if(out$available[i]==T){
            out$size[i]<-x$size
            if(out$type[i]=="unk"){
                # if extension not known, look for envi .hdr file in same directory #
                if(file.access(paste(file_path_sans_ext(full.names[i]),".hdr",sep=""))==0)
                    out$type[i]<-"envi"
                }
        }
    }
    return(out)
}
