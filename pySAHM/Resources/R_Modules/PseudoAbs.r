PseudoAbsGen<-function(input.file,output.dir,response.col="ResponseBinary",method="KDE",bw.otim="adhoc",isopleth=95,bias=FALSE){

#Written by Marian Talbert 4/5/2012
#make sure all libraries are available and loaded
    libs<-list("adehabitatHR","ks","raster","rgdal","sp","spatstat")
      lib.mssg <- unlist(suppressMessages(suppressWarnings(lapply(libs,require,quietly = T, warn.conflicts=F,character.only=T))))
          if(any(!lib.mssg)){
                install.packages(unlist(libs[!lib.mssg]), repos = "http://cran.r-project.org")
                lib.mssg <- unlist(suppressMessages(suppressWarnings(lapply(libs,require,quietly = T, warn.conflicts=F,character.only=T))))
                }
            if(any(!lib.mssg)) stop(paste("\nthe following package(s) could not be loaded: ",paste(unlist(libs[!lib.mssg]),sep="")))
          
#Read input data and remove any columns to be excluded
          dat.in<-read.csv(input.file,header=FALSE,as.is=TRUE)
          dat<-as.data.frame(dat.in[4:dim(dat.in)[1],])
           names(dat)<-dat.in[1,]
        
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
              image(ver)
              
              if(bias){
                  #why they can't put the data in a logical order I don't know but it makes me angry
                  mm<-ud@coords[order(ud@coords[,1]),]
                  mm<-order(ud@coords[,2])
                  m<-ud@data$ud[mm]
                  kde.mat<-matrix(m,nrow=length(unique(ud@coords[,1])))
                  x.cut<-sort(unique(ud@coords[,1]))
                  y.cut<-sort(unique(ud@coords[,2]))
                  image(x.cut,y.cut,kde.mat)
                  contour(sort(unique(ud@coords[,1])),sort(unique(ud@coords[,2])),kde.mat,add=TRUE)
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
                       MyWindow<-owin(poly=list(x=x[(length(x)-2):1],y=y[(length(x)-2):1]))
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

####################################################################
##### now write the info to a raster eventually this should be an additional function
##### since all but the selection of values is already written as a separate function
 
fullnames<-names(dat)[8] # I'm not sure which column I'll be able to match at this point in the workflow 
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
		filename <- trim(output.dir)
			firstrow <- 1
			firstcol <- 1
		ncols <- ncol(predrast)
		lyrnames <- layerNames(RasterInfo)
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
    
    
