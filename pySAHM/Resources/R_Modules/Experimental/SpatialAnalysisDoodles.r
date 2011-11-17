
 x<-runif(500,min=1,max=100)
 y<-runif(500,min=1,max=100)
 z<-rnorm(500,mean=10*x)
 z<-rnorm(500,mean=10,sd=1)
 par(mfrow=c(2,2))
 plot(x,y,pch=19,cex=z/10)
 correlog1.1 <- correlog(z, y, z,na.rm=T, increment=5, resamp=50) # I think this one uses actual distances rather than resample
 plot(correlog1.1)
 correlog1.1 <- correlog(z, y, z,na.rm=T, increment=10, resamp=50)
 plot(correlog1.1)

 
 plot(correlog1.1)
 sp.corr<-spline.correlog(x, y, z)
dist.nb <- dnearneigh(as.matrix(cbind(x,y)), 0, 5) #this gives an indicator matrix either everything is a neighbor or not, not based on distance
dist.listw <- nb2listw(dist.nb,zero.policy=TRUE) #calculates weights based on a number  of possible criteria


 dist.nb <- dnearneigh(as.matrix(cbind(x,y)), 0, 100) #give lower and
              dist.listw <- nb2listw(dist.nb) #turns neighbourhood object into a
              GlobMT<- moran.test(z, listw=dist.listw)
              GlobMT<-moran.mc(z, listw=dist.listw,nsim=1000)
data(oldcol)
colw <- nb2listw(COL.nb, style="W")
nsim <- 99
set.seed(1234)
sim1 <- moran.mc(COL.OLD$CRIME, listw=colw, nsim=nsim)
sim1
mean(sim1$res[1:nsim])
var(sim1$res[1:nsim])
summary(sim1$res[1:nsim])
MoranI.boot <- function(var, i, ...) {
var <- var[i]
return(moran(x=var, ...)$I)
}
set.seed(1234)
library(boot)
boot1 <- boot(COL.OLD$CRIME, statistic=MoranI.boot, R=nsim,
sim="permutation", listw=colw, n=nrow(COL.OLD), S0=Szero(colw))
boot1
plot(boot1)
mean(boot1$t)