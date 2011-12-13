#the following works but it seems the whole idea might not be worth pursuing see bivands posts
  resid.dists <- as.matrix(dist(cbind(x,y)))
     resid.dists.inv <- 1/resid.dists
     diag(resid.dists.inv) <- 0
     m<-Moran.I(z, resid.dists.inv)

resid.image<-function(dev.contrib,pred,raw.dat,x,y,model.type,file.name,out){
   z<-sign(pred-raw.dat)*dev.contrib
              browser()
              ####################################################
              ####  New experimental section
              ## Currently not sure where to go with this. Moran's I is produced below
              ## but the spatial weights matrix is only calculated for distances less than 2000
              ## the issue is that Moran's I is quite sensitive to the maximum distance used if set too great
              ## then the test is insensitive to autocorrelation at a close scale if set too small there are no non-zero weights I hate to set an arbitrary threshold
              ## another option might be to resample Moran's I (calculated at a series of cutoffs) and plot actual over resampled values
              ## so one can at least see what it might look like under the null hyp.  Also a variance estimate based on normal assumptions is available in the test
              ## might plot envalopes based on these or use spline correlograms which can have plotted confidence regions as well.  Is the normal approximation acceptible
              ## for deviance residuals?
              library(spdep)
              library(ncf)
               z<-dev.contrib
              correlog1.1 <- correlog(z, y, z,na.rm=T, increment=40, resamp=100)
              par(mar=c(5,5,0.1, 0.1))
              plot(correlog1.1)
              plot(correlog1.1$correlation, type="l", pch=16, cex=1.5, lwd=1.5,
              xlab="distance", ylab="Moran's I", cex.lab=2, cex.axis=1.5); abline(h=0)


              correlog1.1 <- correlog(z, y, z,na.rm=T, increment=1, resamp=20)
              # make a map of the residuals:
              plot(x, y, col=c("blue",
              "red")[sign(z)/2+1.5], pch=19,
              cex=abs(z)/max(z)*2.5, xlab="geographical xcoordinates",
              ylab="geographical y-coordinates")
              
              dist.nb <- dnearneigh(as.matrix(cbind(x,y)), 0, 5000) #give lower and
              dist.listw <- nb2listw(dist.nb) #turns neighbourhood object into a
              GlobMT<- moran.test(z, listw=dist.listw)
              GlobMT<-moran.mc(z, listw=dist.listw,nsim=1000)
                            #####################################################
              a<-loess(z~x*y)
               x.lim<-rep(seq(from=min(out$dat$ma$train.xy[,1]),to=max(out$dat$ma$train.xy[,1]),length=100),each=100)
               y.lim<-rep(seq(from=min(out$dat$ma$train.xy[,2]),to=max(out$dat$ma$train.xy[,2]),length=100),times=100)
              z<-predict(a,newdata=cbind("x"=x.lim,"y"=y.lim))
              x.lim<-seq(from=min(out$dat$ma$train.xy[,1]),to=max(out$dat$ma$train.xy[,1]),length=100)
              y.lim<-seq(from=min(out$dat$ma$train.xy[,2]),to=max(out$dat$ma$train.xy[,2]),length=100)
                 z<-matrix(data=z,ncol=100,nrow=100,byrow=TRUE)
                # browser()
                 ########################################### experiment
                # if(out$input$make.binary.tif==TRUE | out$input$make.p.tif==TRUE){
                # out$dat$tif.ind[1]
                # RasterInfo=raster(out$dat$tif.ind[1])
                #  gi <- GDALinfo(out$dat$tif.ind[1])
                #    dims <- as.vector(gi)[1:2]
                #    ps <- as.vector(gi)[6:7]
                #    ll <- as.vector(gi)[4:5]
                 #   pref<-attr(gi,"projection")


                 # }
                  
                  
                 ##########################################################
              jpeg(file=paste(file.name,"resid.plot.jpg",sep="/"))
                 par(oma=c(3,3,3,3))
                 layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
                  image(z,x=x.lim,y=y.lim,col=beachcolours(heightrange=c(min(z),max(z)),sealevel=0,ncolours=length(table(z))),
                  main="Spatial pattern of deviance residuals\n(magnitude and sign)",xlab="X coordinate",ylab="Y coordinate")
                  points(x,y,cex=.5)
                  #image(x=c(1,2),y=sort(unique(z)),z=matrix(data=cbind(rep(sort(unique(z)),times=2)),ncol=2),col=beachcolours(heightrange=c(min(z),max(z)),sealevel=0,ncolours=length(table(z))))
                  par(mar = c(3,2.5,2.5,2))
              colrange<-seq(from=min(z),to=max(z),length=100)
               image(1,colrange,
               matrix(data=colrange, ncol=length(colrange),nrow=1),
              col=beachcolours(heightrange=c(min(z),max(z)),sealevel=0,ncolours=length(colrange)),
              xlab="",ylab="",
              xaxt="n")
              graphics.off()
              return(a)
              }
