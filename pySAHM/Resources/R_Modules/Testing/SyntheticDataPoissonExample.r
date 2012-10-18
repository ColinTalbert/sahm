library(raster)
a<-raster("I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\PARC_100mTemplate\\bio_17_1971_2000_800m.tif")
#start with creating two rasters for predictoin and some noise over the same extent
Temp<-Precip<-Rand1<-Rand2<-raster(a)
values(Temp)<-rep(seq(from=0,to=3,length=435),times=472)
values(Rand1)<-runif(205320)
values(Rand2)<-runif(205320)
values(Precip)<-rep(seq(from=0,to=3,length=435),each=472)

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
response1<-rpois(nrow(SampLocs),lambda=SampLocs[,2])
plot(x=xys[,1],y=xys[,2],col=heat.colors(length(unique(response1)))[factor(response1)],pch=19,cex=.4)
CubeSurf<-cbind(xys,response1)

#creating a binary sample for surface 2 
response2<-rpois(nrow(SampLocs),lambda=extract(Surf2,SampLocs[,1]))
plot(x=xys[,1],y=xys[,2],col=heat.colors(length(unique(response2)))[factor(response2)],pch=19,cex=.4)
ElithSurf<-cbind(xys,response2)

colnames(CubeSurf)<-colnames(ElithSurf)<-c("X","Y","responseBinary")
write.table(CubeSurf,file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode2\\TestSuite\\PoissonCube.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
write.table(ElithSurf,file="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode2\\TestSuite\\PoissonSurf.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
writeRaster(Temp,filename="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\TemperatureRastPois.tif",overwrite=TRUE)
writeRaster(Precip,filename="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\PrecipitatoinRastPois.tif",overwrite=TRUE)
writeRaster(Rand1,filename="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\Noise1RastPois.tif",overwrite=TRUE)
writeRaster(Rand2,filename="I:\\VisTrails\\VisTrails_SAHM_x32_debug\\VisTrails\\vistrails\\packages\\TestingRCode\\Noise2RastPois.tif",overwrite=TRUE)



##################### The end of Elith's synthetic data

par(mfrow=c(3,2))
plot(Surf2)
plot(x=xys[,1],y=xys[,2],col=rainbow(length(unique(response2)))[factor(response2)],pch=19,cex=.4)
#now plotting model fit results
glm.out<-raster("I:\\VisTrails\\WorkingFiles\\workspace\\NewOutput\\SyntheticCountData\\glm\\glm_1_prob_map.tif")
mars.out<-raster("I:\\VisTrails\\WorkingFiles\\workspace\\NewOutput\\SyntheticCountData\\mars\\mars_1_prob_map.tif")
brt.out<-raster("I:\\VisTrails\\WorkingFiles\\workspace\\NewOutput\\SyntheticCountData\\brt\\brt_1_prob_map.tif")
rf.out<-raster("I:\\VisTrails\\WorkingFiles\\workspace\\NewOutput\\SyntheticCountData\\rf\\rf_prob_map.tif")

plot(glm.out,main="GLM")
plot(mars.out,main="MARS")
plot(brt.out,main="BRT")
plot(rf.out,main="RF")

































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