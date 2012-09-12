library(raster)
a<-raster("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\PARC_100mTemplate\\bio_17_1971_2000_800m.tif")
#start with creating two rasters for predictoin and some noise over the same extent
Temp<-Precip<-Rand1<-Rand2<-raster(a)
values(Temp)<-rep(seq(from=0,to=1,length=435),times=472)
values(Rand1)<-runif(205320)
values(Rand2)<-runif(205320)
values(Precip)<-rep(seq(from=0,to=1,length=435),each=472)

#Temp and Precip are independent
Surf1<-(.5*(Temp+Precip))^3
Surf2<-.5*(Temp+Precip)
par(mfrow=c(2,2))
plot(Temp)
plot(Precip)
plot(Surf1)
plot(Surf2)


################################################################
############ Presence Absence Surface #########################
#creating a binary sample for surface 1 elith actually used only surface 2
#but I've created the cubed surface as well. 

SampLocs<-sampleRandom(Surf1,10000,cells=TRUE)
xys<-xyFromCell(Surf1,SampLocs[,1])
response1<-rbinom(nrow(SampLocs),1,prob=SampLocs[,2])
plot(x=xys[,1],y=xys[,2],col=response1+1,pch=19,cex=.4)
CubeSurf<-cbind(xys,response1)

#creating a binary sample for surface 2 
response2<-rbinom(nrow(SampLocs),1,prob=extract(Surf2,SampLocs[,1]))
plot(x=xys[,1],y=xys[,2],col=response2+1,pch=19,cex=.4)
ElithSurf<-cbind(xys,response2)

colnames(CubeSurf)<-colnames(ElithSurf)<-c("X","Y","responseBinary")
write.table(CubeSurf,file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\ElithCube.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
write.table(ElithSurf,file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\ElithSurf.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
writeRaster(Temp,filename="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\TemperatureRast.tif",overwrite=TRUE)
writeRaster(Precip,filename="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\PrecipitatoinRast.tif",overwrite=TRUE)
writeRaster(Rand1,filename="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\Noise1Rast.tif",overwrite=TRUE)
writeRaster(Rand2,filename="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\Noise2Rast.tif",overwrite=TRUE)

################################################################
########## Now creating presence only surfaces #################
PresSamp<-sampleRandom(Surf2,10000,cells=TRUE)
xys<-xyFromCell(Surf2,PresSamp[,1])
response1<-rbinom(nrow(PresSamp),1,prob=PresSamp[,2])
samp<-sample(which(response1==1,arr.ind=TRUE),size=1000,replace=TRUE)
xys<-xys[samp,]
response1<-response1[samp]
dat<-cbind(xys,response1)
plot(dat[,1],dat[,2],col=factor(dat[,3]))

bgd<-sampleRandom(Surf2,20000,cells=TRUE)
xys<-xyFromCell(Surf2,bgd[,1])
response2<-rbinom(nrow(bgd),1,prob=bgd[,2])
samp<-sample(1:20000,size=10000)
xys<-xys[samp,]

dat<-rbind(dat,cbind(xys,rep(-9998,times=length(samp))))
colnames(dat)<-c("X","Y","responseBinary")
Temperature<-extract(Temp,dat[,1:2])
Precipitation<-extract(Precip,dat[,1:2])
Noise1<-extract(Rand1,dat[,1:2])
Noise2<-extract(Rand2,dat[,1:2])
dat<-cbind(dat,Temperature,Precipitation,Noise1,Noise2)
write.table(dat,file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\ElithPsdoAbs.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)


##################### The end of Elith's synthetic data

mean(sampleRegular(Pred2,10000))
sd(sampleRegular(Pred2,10000))
mean(sampleRegular(Pred1,10000))
sd(sampleRegular(Pred1,10000))
SampLocs<-sampleRandom(Pred2,10000,cells=TRUE)
hist(Pred1)
hist(Pred2)
## Sample the Pred2 raster and calculate probabililities assuming a gaussian response curve
SampLocs<-sampleRandom(Pred2,10000,cells=TRUE)
Sp1NDVI<-dnorm(SampLocs[,2], mean = 6500, sd = 100, log = FALSE)
Sp2NDVI<-dnorm(SampLocs[,2], mean = 4500, sd = 300, log = FALSE)


Pred1samp<-extract(Pred1,SampLocs[,1])
Sp1bio6<-dnorm(Pred1samp, mean = -150, sd = 15, log = FALSE)
Sp2bio6<-dnorm(Pred1samp, mean = -100, sd = 15, log = FALSE)

xys<-xyFromCell(Pred2,SampLocs[,1])

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

plot(Pred2)
points(xys)
points(xys[Sp1PresAbs==1,],col="red",cex=2,pch=19)
points(xys[Sp2PresAbs==1,],col="purple",cex=2,pch=19)

plot(Pred1)
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

plot(Pred1)
points(Sp1FDQ[Sp1FDQ[,3]==0,1:2],cex=1,col="blue",pch=21,bg="blue")
points(Sp1FDQ[Sp1FDQ[,3]==1,1:2],cex=1,col="red",pch=21,bg="red")

plot(Pred2)
points(Sp1FDQ[Sp1FDQ[,3]==0,1:2],cex=1,col="blue",pch=21,bg="blue")
points(Sp1FDQ[Sp1FDQ[,3]==1,1:2],cex=1,col="red",pch=21,bg="red")

write.table(Sp1FDQ,file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\SyntheticSp1.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
write.table(Sp2FDQ,file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\SyntheticSp2.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)

write.table(Sp1FDQ[Sp1FDQ[,3]==1,],file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\SyntheticSp1PresOnly.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
write.table(Sp2FDQ[Sp2FDQ[,3]==1,],file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\SyntheticSp2PresOnly.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
##For used\available set the threshold use bernoulli trials to calculate pres and randomly sample the background

Sp1PredSurface<-overlay(Pred1,Pred2,fun=function(x,y){(dnorm(x, mean = -150, sd = 15, log = FALSE)*dnorm(y, mean = 6500, sd = 100, log = FALSE))})*(1/max(Sp1NDVI*Sp1bio6))
glm.out<-raster("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\glm_13\\glm_prob_map.tif")
rf.out<-raster("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\rf_5\\rf_prob_map.tif")
mars.out<-raster("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\mars_7\\mars_prob_map.tif")

brt.out<-raster("C:\\temp\\AcrossModelPerformanceDetailsForTesting\\Debug7.13\\brt\\brt_1_prob_map.tif")
glm.out<-raster("C:\\temp\\AcrossModelPerformanceDetailsForTesting\\Debug7.13\\glm\\glm_1_prob_map.tif")
rf.out<-raster("C:\\temp\\AcrossModelPerformanceDetailsForTesting\\Debug7.13\\rf\\rf_1_prob_map.tif")
mars.out<-raster("C:\\temp\\AcrossModelPerformanceDetailsForTesting\\Debug7.13\\mars\\mars_1_prob_map.tif")

par(mfrow=c(2,3))
plot(Sp1PredSurface,main="TRUE")
points(xys[Sp1PresAbs==1,],col="red",cex=.5,pch=19)
plot(glm.out,main="GLM")
plot(rf.out,main="RF")
plot(mars.out,main="MARS")
plot(brt.out,main="BRT")

Sp2PredSurface<-overlay(Pred1,Pred2,fun=function(x,y){(dnorm(x, mean = -100, sd = 15, log = FALSE)*dnorm(y, mean = 4500, sd = 300, log = FALSE))})*(1/max(Sp2NDVI*Sp2bio6))
glm.out<-raster("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\glm_14\\glm_prob_map.tif")
brt.out<-raster("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\brt_18\\brt_prob_map.tif")
mars.out<-raster("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\mars_8\\mars_prob_map.tif")

par(mfrow=c(2,2))
plot(Sp2PredSurface,main="TRUE")
points(xys[Sp2PresAbs==1,],col="red",cex=.5,pch=19)
plot(glm.out,main="GLM")
plot(brt.out,main="BRT")
plot(mars.out,main="MARS")

Sp1PredSurface<-overlay(Pred1,Pred2,fun=function(x,y){(dnorm(x, mean = -150, sd = 15, log = FALSE)*dnorm(y, mean = 6500, sd = 100, log = FALSE))})*(1/max(Sp1NDVI*Sp1bio6))
glm.out<-raster("C:\\temp\\SAHMDebugJunk\\BRTOut1\\glm_28_prob_map.tif")
rf.out<-raster("C:\\temp\\SAHMDebugJunk\\BRTOut1\\rf_24_prob_map.tif")
rf2.out<-raster("C:\\temp\\SAHMDebugJunk\\BRTOut1\\rf_26_prob_map.tif")
par(mfrow=c(2,2))
plot(Sp1PredSurface,main="TRUE")
points(xys[Sp1PresAbs==1,],col="red",cex=.5,pch=19)
plot(glm.out,main="GLM")
plot(rf.out,main="RF")
plot(rf2.out,main="RF2")

































library(rgdal)
project(Sp1FDQ[1:10,1:2],Pred1@crs@projargs)
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