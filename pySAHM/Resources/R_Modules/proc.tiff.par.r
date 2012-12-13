
makePSOCKcluster()

detectCores()
cl <- makeCluster(detectCores())
parLapply(cl,x,FUN,...)
parApply
parCapply
parLapply(cl,x=1:tr$n,FUN=f,...)
g<-function(x,raster,nvars,pred,...){
   
}
if(out$input$make.p.tif)
  dir.create(paste(out$input$output.dir,"\\ProbTiff",sep=""))
  outfile.p=paste(paste(out$input$output.dir,"\\ProbTiff\\",sep=""),"_prob_map.tif",sep="")
if(out$input$make.binary.tif)
  dir.create(paste(out$input$output.dir,"\\BinTiff",sep=""))
if(MESS)
  dir.create(paste(out$input$output.dir,"\\MESSTiff",sep=""))    
  
start.time<-Sys.time()
cl <- makeCluster(detectCores())
parLapply(cl,X=1:tr$n,fun=parRaster,nrows=nrows,dims=dims,
   tr=tr,MESS=MESS,nvars=nvars,fullnames=fullnames,nvars.final=nvars.final,vnames=vnames,NAval=NAval,factor.levels=factor.levels,
   model=model,Model=Model,pred.fct=pred.fct,make.binary.tif=make.binary.tif,RasterInfo=RasterInfo,outfile.p=outfile.p,
   outfile.bin=outfile.bin,thresh=thresh,nToDo=1,ScriptPath=out$input$ScriptPath)
stopCluster(cl)
total.time<-Sys.time()-start.time
total.time

start.time<-Sys.time()
#merge the rasters back together
m <- merge(raster("C:\\temp\\SAHMDebugJunk\\BRTOut1\\mars_prob_map1.tif"),
raster("C:\\temp\\SAHMDebugJunk\\BRTOut1\\mars_prob_map2.tif"),
raster("C:\\temp\\SAHMDebugJunk\\BRTOut1\\mars_prob_map3.tif"),
raster("C:\\temp\\SAHMDebugJunk\\BRTOut1\\mars_prob_map4.tif"),
raster("C:\\temp\\SAHMDebugJunk\\BRTOut1\\mars_prob_map5.tif"), filename="C:\\temp\\SAHMDebugJunk\\BRTOut1/mars_prob_mapMerge.tif", overwrite=TRUE)
total.time<-Sys.time()-start.time
total.time
#clean up by deleting the tiles
unlink("C:\\temp\\SAHMDebugJunk\\BRTOut1\\mars_resid_map.tif")
#to delete a folder we need to set recursive equal to true
unlink("C:\\temp\\SAHMDebugJunk\\BRTOut1\\ProbTiff",recursive=TRUE)

#without doing things in parallel
parRaster(start.tile=5,nrows=nrows,dims=dims,
   tr=tr,MESS=MESS,nvars=nvars,fullnames=fullnames,nvars.final=nvars.final,vnames=vnames,NAval=NAval,factor.levels=factor.levels,
   model=model,Model=Model,pred.fct=pred.fct,make.binary.tif=make.binary.tif,RasterInfo=RasterInfo,outfile.p=outfile.p,outfile.bin=outfile.bin,thresh=thresh,nToDo=1)

start.time<-Sys.time()

 a<-raster("C:\\temp\\SAHMDebugJunk\\BRTOut1\\mars_prob_map.tif") 
 extent(a, 1, 176, 1, 748)
 #this works quickly but I think I might want to clip before projecting
 crop(a, extent(a, 1, 176, 1, 748), filename="C:\\temp\\SAHMDebugJunk\\BRTOut1\\mars_prob_map.tif",overwrite=TRUE, snap='near')
 
  
  
  
 extent(a)<-extent(a, 1, 176, 1, 748)