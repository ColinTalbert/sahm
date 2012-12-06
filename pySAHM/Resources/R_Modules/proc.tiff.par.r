
makePSOCKcluster()

detectCores()
cl <- makeCluster(detectCores())
parLapply(cl,x,FUN,...)
parApply
parCapply
parLapply(cl,x=1:tr$n,FUN=f,...)
g<-function(x,raster,nvars,pred,...){
   
}
start.time<-Sys.time()
cl <- makeCluster(detectCores())
parLapply(cl,X=1:tr$n,fun=parRaster,nrows=nrows,dims=dims,
   tr=tr,MESS=MESS,nvars=nvars,fullnames=fullnames,nvars.final=nvars.final,vnames=vnames,NAval=NAval,factor.levels=factor.levels,
   model=model,Model=Model,pred.fct=pred.fct,make.binary.tif=make.binary.tif,RasterInfo=RasterInfo,outfile.p=outfile.p,outfile.bin=outfile.bin,thresh=thresh)
stopCluster(cl)
total.time<-Sys.time()-start.time
total.time

start.time<-Sys.time()

 a<-raster("C:\\temp\\SAHMDebugJunk\\BRTOut1\\mars_prob_map.tif") 
 extent(a, 1, 176, 1, 748)
 #this works quickly but I think I might want to clip before projecting
 crop(a, extent(a, 1, 176, 1, 748), filename="C:\\temp\\SAHMDebugJunk\\BRTOut1\\mars_prob_map.tif",overwrite=TRUE, snap='near')

 extent(a)<-extent(a, 1, 176, 1, 748)