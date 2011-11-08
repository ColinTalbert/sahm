data(mafragh)
tab0 <- (as.data.frame(scalewt(mafragh$mil)))
bilis0 <- neig2mat(mafragh$neig)
gm0 <- gearymoran(bilis0, tab0, 999)
gm0
plot(gm0, nclass = 20)

d<-as.matrix(dist(cbind(x,y)))
d<-1/d
diag(d)<-0
gearymoran(d,z)

plot(continuousRaster)
points(
points(out$dat$ma$train.xy[out$dat$ma$ma$response==0,],col="grey",pch=19,cex=.1)
points(out$dat$ma$train.xy[out$dat$ma$ma$response==1,],col="black",pch=19,cex=.1)