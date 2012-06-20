setwd("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules")
ScriptPath="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\sahm_MarianDev\\pySAHM\\Resources\\R_Modules"

source("Pred.Surface.r")

library(raster)
#start with two uncorrelated rasters
bio_6<-raster("C:\\temp\\SAHM_workspace\\SimulationTiffs\\bio_06_2000_2km.tif")
NDVI<-raster("C:\\temp\\SAHM_workspace\\SimulationTiffs\\NDVI_annualMaximumValue_2009.tif")


NDVIMean<-mean(sampleRegular(NDVI,10000))
NDVIsd<-sd(sampleRegular(NDVI,10000))
bio_6Mean<-mean(sampleRegular(bio_6,10000))
bio_6sd<-sd(sampleRegular(bio_6,10000))

## Sample the NDVI raster and calculate probabililities assuming a gaussian response curve
SampLocs<-sampleRandom(NDVI,10000,cells=TRUE)
Sp1NDVI<-dnorm(SampLocs[,2], mean = 6500, sd = 100, log = FALSE)
Sp2NDVI<-dnorm(SampLocs[,2], mean = 4500, sd = 300, log = FALSE)




bio_6samp<-extract(bio_6,SampLocs[,1])
Sp1bio6<-dnorm(bio_6samp, mean = -150, sd = 15, log = FALSE)
Sp2bio6<-dnorm(bio_6samp, mean = -100, sd = 15, log = FALSE)

xys<-xyFromCell(NDVI,SampLocs[,1])

plot(NDVI)
points(xys)
points(xys[Sp1NDVI>.001,],col="red",cex=2,pch=19)
points(xys[Sp2NDVI>.0001,],col="blue",cex=2,pch=19)

plot(bio_6)
points(xys)
points(xys[Sp1bio6>.0061,],col="red",cex=2,pch=19)
points(xys[Sp2bio6>.0065,],col="blue",cex=1,pch=19)
points(xys)

Sp1Prob<-Sp1NDVI*Sp1bio6/max(Sp1NDVI*Sp1bio6)
Sp2Prob<-Sp2NDVI*Sp2bio6/max(Sp2NDVI*Sp2bio6)

plot(SampLocs[,2],Sp1NDVI)

#showing about the number of sucesses to expect at a given threshold
thresh<-.03
sum(Sp1Prob>thresh)
sum(Sp2Prob>thresh)
##For a presence\absence sample set a threshold and then use bernoulli trials to assign
Sp1PresAbs<-rbinom(length(Sp1Prob),1,prob=Sp1Prob)
Sp2PresAbs<-rbinom(length(Sp2Prob),1,prob=Sp2Prob)

plot(NDVI)
points(xys)
points(xys[Sp1PresAbs==1,],col="red",cex=2,pch=19)
points(xys[Sp2PresAbs==1,],col="purple",cex=2,pch=19)

plot(bio_6)
points(xys)
points(xys[Sp1PresAbs==1,],col="red",cex=2,pch=19)
points(xys[Sp2PresAbs==1,],col="purple",cex=2,pch=19)

Sp1Samp<-c(which(Sp1PresAbs==1,arr.ind=TRUE),sample(which(Sp1PresAbs==0,arr.ind=TRUE),size=500))
Sp2Samp<-c(which(Sp2PresAbs==1,arr.ind=TRUE),sample(which(Sp2PresAbs==0,arr.ind=TRUE),size=500))

Sp1FDQ<-cbind(xys[Sp1Samp,],Sp1PresAbs[Sp1Samp])
Sp2FDQ<-cbind(xys[Sp1Samp,],Sp2PresAbs[Sp1Samp])
colnames(Sp1FDQ)<-colnames(Sp2FDQ)<-c("X","Y","responseBinary")

plot(Sp1bio6,Sp1PresAbs)
plot(Sp1NDVI,Sp1PresAbs)

plot(bio_6)
points(Sp1FDQ[Sp1FDQ[,3]==0,1:2],cex=1,col="blue",pch=21,bg="blue")
points(Sp1FDQ[Sp1FDQ[,3]==1,1:2],cex=1,col="red",pch=21,bg="red")

plot(NDVI)
points(Sp1FDQ[Sp1FDQ[,3]==0,1:2],cex=1,col="blue",pch=21,bg="blue")
points(Sp1FDQ[Sp1FDQ[,3]==1,1:2],cex=1,col="red",pch=21,bg="red")

write.table(Sp1FDQ,file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\SyntheticSp1.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
write.table(Sp2FDQ,file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\SyntheticSp2.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)

write.table(Sp1FDQ[Sp1FDQ[,3]==1,],file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\SyntheticSp1PresOnly.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
write.table(Sp2FDQ[Sp2FDQ[,3]==1,],file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\SyntheticSp2PresOnly.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
##For used\available set the threshold use bernoulli trials to calculate pres and randomly sample the background

Sp1PredSurface<-overlay(bio_6,NDVI,fun=function(x,y){(dnorm(x, mean = -150, sd = 15, log = FALSE)*dnorm(y, mean = 6500, sd = 100, log = FALSE))})*(1/max(Sp1NDVI*Sp1bio6))
glm.out<-raster("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\glm_11\\glm_prob_map.tif")
rf.out<-raster("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\rf_4\\rf_prob_map.tif")
mars.out<-("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\mars_5\\glm_prob_map.tif")

plot(Sp1PredSurface)
plot(glm.out)
plot(rf.out)
plot(Sp1PredSurface)
points(xys[Sp1PresAbs==1,],col="red",cex=.5,pch=19)

































library(rgdal)
project(Sp1FDQ[1:10,1:2],bio_6@crs@projargs)
 RasterInfo=raster("C:\\temp\\SAHM_workspace\\SimulationTiffs\\bio_06_2000_2km.tif")





#putting bio_4 on a 0 to 1 range
temp<-as.matrix(log(bio_4))
b4<-(log(bio_4)-min(temp))/max(temp-min(temp))

#putting bio_7 on a 0 to 1 range
temp<-as.matrix(2^bio_7)
b7<-(2^bio_7-min(temp))/max(temp-min(temp))

#putting slopedeg on a 0 to 1 range
temp<-as.matrix(slopedeg)
sld<-(slopedeg-min(temp))/max(temp-min(temp))

#making bio_12 discrete
temp<-as.matrix(bio_12)
qan<-quantile(temp,probs=c(.25,.5,.75))
b12<-.25+.25*(bio_12>=qan[1])+.25*(bio_12>qan[2])+.25*(bio_12>qan[3])
writeRaster(b12, filename="C:\\VisTrails\\mtalbert_20110504T132851\\PARC_1\\Cat12_wgs84.tif")


#for binomal data
myFct<-.5*b4*b7+.25*sld+.25*b12+.18
m<-as.matrix(myFct)
xcoord<-sample(dim(m)[1],size=200)
ycoord<-sample(dim(m)[2],size=200)
cells<-cbind(xcoord,ycoord)
cellNumbs<-cellFromRowCol(myFct,xcoord,ycoord)

writeRaster(myFct, filename="C:\\VisTrails\\mtalbert_20110504T132851\\PARC_1\\BinomialSurface.tif")

value<-extract(myFct,cellNumbs)
responseBinary<-rbinom(length(value),1,value)
xy = xyFromCell(myFct, cellNumbs)

Binom.out<-as.data.frame(cbind(xy,responseBinary))
write.table(Binom.out,file="C:\\VisTrails\\mtalbert_20110504T132851\\FieldDatBinom.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)

#now for Poisson data generate a lambda surface we want a lot of zeros

PoisFct<-myFct*4
m<-as.matrix(PoisFct)
xcoord<-sample(dim(m)[1],size=200)
ycoord<-sample(dim(m)[2],size=200)
cells<-cbind(xcoord,ycoord)
cellNumbs<-cellFromRowCol(PoisFct,xcoord,ycoord)

value<-extract(PoisFct,cellNumbs)
responseCount<-rpois(length(value),value)
xy = xyFromCell(myFct, cellNumbs)

par(mfrow=c(2,2))
plot(as.vector(as.matrix(b4)),as.vector(as.matrix(PoisFct)))
plot(as.vector(as.matrix(b7)),as.vector(as.matrix(PoisFct)))
plot(as.vector(as.matrix(sld)),as.vector(as.matrix(PoisFct)))
plot(as.vector(as.matrix(b12)),as.vector(as.matrix(PoisFct)))

writeRaster(PoisFct, filename="C:\\VisTrails\\mtalbert_20110504T132851\\PARC_1\\PoissonSurface.tif")

Pois.out<-as.data.frame(cbind(xy,responseCount))
write.table(Pois.out,file="C:\\VisTrails\\mtalbert_20110504T132851\\FieldDatPois.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)

BRT.Out<-raster("C:\\VisTrails\\prob_map.tif")
BRT.Bin<-raster("C:\\VisTrails\\prob_map.tif")
resids<-BRT.Out-PoisFct

par(mfrow=c(2,2))
plot(resids)
plot(PoisFct)
plot(BRT.Out)

par(mfrow=c(2,1))
hist(as.matrix(PoisFct),breaks=40)
hist(as.matrix(BRT.Out),breaks=40)