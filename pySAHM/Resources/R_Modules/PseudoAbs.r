PseudoAbsGen<-function(input.file,outfile,method="KDE",bw.otim="adhoc",isopleth=95,bias=FALSE,template){
    
    #Written by Marian Talbert 4/5/2012
    #This function takes a field data file and based on the options specified creates a bias or binary mask for generation of background points
    #The mask can be based on a KDE function or a minimum convex polygon (method=KDE or MCP) bias specifies that a continuous surface is to be created
    #This is ignored by method=MCP isopleth specifies the isopleth to be used (a number, generally 95).  It is assumed that the 8th name in the input csv is the name of a template
    #that can be used.  currently 4 methods are available for optimization of the kde bandwith (bw.otim=adhoc, Hpi,Hscv,Hbcv,Hlscv.
    #A tiff is generated using the header from the template csv which can be used by the MDS builder to generate background points.
    
          if(method=="KDE" & isopleth==100) stop("Isopleth must be set to less than 100 when the KDE method is used")
    #make sure all libraries are available and loaded
        chk.libs("GenPsdAbs")
              
    #Read input data and remove any columns to be excluded
              dat<-read.csv(input.file,header=TRUE,as.is=TRUE)
              #this should happen before the extra rows get added
             #  names(dat)<-dat.in[1,]
                                                                                                                  
                xy<-data.frame(cbind(as.numeric(as.character(dat$X)),as.numeric(as.character(dat$Y))))
    
    #################################################################
    ### library adehabitatHR methods gives 95% isopleth for kde but LSCV is flaky
     
        if(bw.otim=="adhoc" | method=="MCP"){    
             
              names(xy)<-c("X","Y")
              xy<-SpatialPoints(xy)
             
              if(method=="KDE"){
                  ud=kernelUD(xy,extent=.5,grid=150)
                   #take the 95% home range contour
                  ver=getverticeshr(ud, isopleth)
                  
                  if(bias){
                      #why they can't put the data in a logical order I don't know but it makes me angry
                      mm<-ud@coords[order(ud@coords[,1]),]
                      mm<-order(ud@coords[,2])                                                      
                      m<-ud@data$ud[mm]
                      kde.mat<-matrix(m,nrow=length(unique(ud@coords[,1])))
                      x.cut<-sort(unique(ud@coords[,1]))
                      y.cut<-sort(unique(ud@coords[,2]))
                      #image(x.cut,y.cut,kde.mat)
                      #contour(sort(unique(ud@coords[,1])),sort(unique(ud@coords[,2])),kde.mat,add=TRUE)
                  }
               
                 
              }
             
              if(method=="MCP"){
                  ver<-mcp(xy, percent=isopleth)
              }
                
                if(!bias){  
                      WindowList<-list()    
                    for(j in 1:length(ver@polygons[[1]]@Polygons)){
                          x<-y<-vector()
                          x<-ver@polygons[[1]]@Polygons[[j]]@coords[,1]
                          y<-ver@polygons[[1]]@Polygons[[j]]@coords[,2]
                          xy<-paste(x,y,sep="")
                          x<-x[!(duplicated(xy))]
                          y<-y[!(duplicated(xy))]
                           MyWindow<-try(owin(poly=list(x=x[length(x):1],y=y[length(x):1])),silent=TRUE)
                           if(class(MyWindow)=="try-error") MyWindow<-owin(poly=list(x=x[1:length(x)],y=y[1:length(x)]))
                           ifelse(j==1,WindowList<-MyWindow,{
                            ifelse(is.subset.owin(MyWindow,WindowList),
                                WindowList<-setminus.owin(WindowList,MyWindow),
                                WindowList<-union.owin(MyWindow,WindowList))
                           })
                      }
                }  
        } else{
          bw.to.use<-switch(bw.otim,
               Hpi = Hpi(xy,binned=TRUE,bgridsize=rep(500,times=2)),
               Hscv = Hscv(xy,binned=TRUE,bgridsize=rep(500,times=2)),
               Hbcv=Hbcv(xy,which=1),
               Hlscv=Hlscv(xy,binned=TRUE,bgridsize=rep(5000,times=2)))
         a<-kde(xy,H=bw.to.use) 
          #get the observed values for the points
         if(!bias){
             dobs <- kde(x = a$x, H = a$H, eval.points = a$x,
                      w = rep(1,times=nrow(xy)))$estimate
              #calculate a cuttoff for the isopleth based on what includes isopleth% of the observatiosn
              lev<-quantile(dobs,prob=1-isopleth/100)
              WindowList<-owin(mask=a$estimate>lev,range(a$eval.points[[1]]),range(a$eval.points[[2]]))
              } else{
               kde.mat<-a$estimate
               x.cut<-a$eval.points[[1]]
               y.cut<-a$eval.points[[2]]
          }
        }
       
        #if(bias==TRUE) {
            #I extend the range a little bit on each side of the kde.matrix and insert a boundary of zero values so anything 
            #outside the range of the input data gets a kde estmate of 0
         #   x.cut<-c(x.cut[1]-diff(x.cut[1:2]),x.cut,x.cut[length(x.cut)]+diff(x.cut[1:2]))
          #  y.cut<-c(y.cut[1]-diff(y.cut[1:2]),y.cut,y.cut[length(y.cut)]+diff(y.cut[1:2]))
           #  kde.mat<-cbind(rep(0,times=nrow(kde.mat)),kde.mat,rep(0,times=nrow(kde.mat)))
           #  kde.mat<-rbind(rep(0,times=ncol(kde.mat)),kde.mat,rep(0,times=ncol(kde.mat)))
        #}
        
    ####################################################################
    ##### now write the info to a raster eventually this should be an additional function
    ##### since all but the selection of values is already written as a separate function
     
    
     options(warn=-1)
        gi <- GDALinfo(template)
    options(warn=1)
        dims <- as.vector(gi)[1:2]
        ps <- as.vector(gi)[6:7]
        ll <- as.vector(gi)[4:5]
        pref<-attr(gi,"projection")
        
    RasterInfo=raster(template)
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
     attr(gi,which="projection")
       # calculate position of upper left corner and get geotransform ala http://www.gdal.org/gdal_datamodel.html
        ul <- c(ll[1],ll[2]+(dims[1])*ps[2])
        gt<-c(ul[1],ps[1],0,ul[2],0,ps[2])
        # setting tile size
        MB.per.row<-dims[2]*32/8/1000/1024
        nrows<-min(round(2/MB.per.row),dims[1])
        bs<-c(nrows,dims[2])
        nbs <- ceiling(dims[1]/nrows)
        inc<-round(10/nbs,1)
        chunksize<-bs[1]*bs[2]
        tr<-blockSize(RasterInfo,chunksize=chunksize)
     
      predrast <- raster(RasterInfo)
    		filename <-outfile
    			firstrow <- 1
    			firstcol <- 1
    		ncols <- ncol(predrast)
    		lyrnames <- names(RasterInfo)
    		xylyrnames <- c('x', 'y', lyrnames)
    		v <- matrix(NA, ncol=nrow(predrast), nrow=ncol(predrast))
          na.rm <- FALSE
    
        tr <- blockSize(predrast, n=nlayers(RasterInfo)+5)
    		ablock <- 1:(ncol(RasterInfo) * tr$nrows[1])
    		napred <- rep(NA, ncol(predrast)*tr$nrows[1])
      	predrast <- writeStart(predrast, filename=filename,overwrite=TRUE)
      ############################################################
      	for (i in 1:tr$n) {
    			if (i==tr$n) { 
    				ablock <- 1:(ncol(RasterInfo) * tr$nrows[i])
    				napred <- rep(NA, ncol(predrast) * tr$nrows[i])
    			}
    			rr <- firstrow + tr$row[i] - 1
    				p <- xyFromCell(predrast, ablock + (tr$row[i]-1) * ncol(predrast)) 
    				p <- na.omit(p)
    				blockvals <- data.frame(x=p[,1], y=p[,2])
            if (na.rm) {
    					blockvals <- na.omit(blockvals)		
    				}
        if (nrow(blockvals) == 0 ) {
    					predv <- napred
    				} else {
    				if(bias){
        				a<-apply(matrix(blockvals[,1]),1,FUN=get.nearest.index,cuts=x.cut)
                b<-apply(matrix(blockvals[,2]),1,FUN=get.nearest.index,cuts=y.cut)
                predv <-  100*kde.mat[cbind(a,b)]/max(kde.mat)
                } else{
        				predv <-  100*inside.owin(blockvals[,1],blockvals[,2],WindowList)
    				}
    			predv[is.na(predv)]<-NAval	
       	}
    				if (na.rm) {  
    					naind <- as.vector(attr(blockvals, "na.action"))
    					if (!is.null(naind)) {
    						p <- napred
    						p[-naind] <- predv
    						predv <- p
    						rm(p)
    					}
    				}
    
    				# to change factor to numeric; should keep track of this to return a factor type RasterLayer
    				predv = as.numeric(predv)
    				predrast <- writeValues(predrast, predv, tr$row[i])
    				NAvalue(predrast)<-NAval
    				print(i)
    			}
    
    	predrast <- writeStop(predrast)
}

get.nearest.index<-function(a,cuts){
  temp<-which.min(abs(cuts-a))
  }    
    

# Interpret command line argurments #
# Make Function Call #
Args <- commandArgs(trailingOnly=FALSE)

for (i in 1:length(Args)){
	if(Args[i]=="-f") ScriptPath<-Args[i+1]
	argSplit <- strsplit(Args[i], "=")
	if(argSplit[[1]][1]=="--file") ScriptPath <- argSplit[[1]][2]
}
   
    #assign default values
    method <- "KDE"
    bw.opt="adhoc"
    ispt=95
    continuous=FALSE
    
    #replace the defaults with passed values
    for (arg in Args) {
    	argSplit <- strsplit(arg, "=")
    	argSplit[[1]][1]
    	argSplit[[1]][2]
    	if(argSplit[[1]][1]=="o") output <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="i") infile <- argSplit[[1]][2]
   	  if(argSplit[[1]][1]=="mth") method <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="bwopt") bw.opt <- argSplit[[1]][2]
      if(argSplit[[1]][1]=="ispt") ispt <- as.numeric(argSplit[[1]][2])
      if(argSplit[[1]][1]=="continuous") continuous <- as.logical(argSplit[[1]][2])
      if(argSplit[[1]][1]=="tmplt") template<-argSplit[[1]][2]
     
    }

if(method=="MCP") continuous=FALSE
ScriptPath<-dirname(ScriptPath)
source(file.path(ScriptPath,"chk.libs.r"))

PseudoAbsGen(input.file=infile,outfile=output,method=method,bw.otim=bw.opt,isopleth=ispt,bias=continuous,template=template)