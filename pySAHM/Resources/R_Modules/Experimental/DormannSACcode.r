#set your path here!


snouter.df <- read.table("H:\\Desktop\\SAHM\\Data\\SACPaperDat.txt", header=T, sep="\t")

summary(ols1 <- lm(snouter1.1 ~ rain + djungle, data=snouter.df))
summary(binom1 <- glm(snouter2.1 ~rain+djungle, family=binomial,
data=snouter.df))
summary(pois1 <- glm(snouter3.1 ~rain+djungle, family=poisson,
data=snouter.df))

library(mgcv)
summary(gam.norm <- gam(snouter1.1 ~ rain + djungle + s(X, Y),
data=snouter.df, family=gaussian))
summary(gam.bino <- gam(snouter2.1 ~ rain + djungle + s(X, Y),
data=snouter.df, family=binomial))
summary(gam.pois <- gam(snouter3.1 ~ rain + djungle + s(X, Y),
data=snouter.df, family=poisson))

# First, install the library ncf (http://onb.ent.psu.edu/onb1/);
# for windows:
install.packages("ncf",contriburl="http://asi23.ent.psu.edu/onb1/R/windows")
# for linux:
#install.packages("ncf",contriburl="http://asi23.ent.psu.edu/onb1/R/src")
require(ncf)
?correlog
model <- out$mods$final.mod # or binom1 or pois1
X<-out$dat$ma$train$XY$X
Y<-out$dat$ma$train$XY$Y
d<-dist(cbind(X,Y))
incr<-quantile(d, probs = .025)
correlog1.1 <- correlog(X, Y, residuals(model),
na.rm=T, increment=incr, resamp=0)
 plot(correlog1.1$correlation, type="b", pch=16, cex=1.5, lwd=1.5,
xlab="distance", ylab="Moran's I", cex.lab=2, cex.axis=1.5); abline(h=0)
glob.cutoff<-quantile(d, probs = .3)
my.nb <- dnearneigh(as.matrix(cbind(X,Y)), 0, glob.cutoff) #give lower and
weight.list <- nb2listw(my.nb) #turns neighbourhood object into a
GlobMT1.1<- moran.test(residuals(model), listw=weight.list)
#weighted list

#this next step takes often several minutes to run:
GlobMT1.1<- moran.test(residuals(model), listw=snouter.listw)
correlog1.1 <- correlog(snouter.df$X, snouter.df$Y, residuals(model),
na.rm=T, increment=1, resamp=0)
# now plot only the first 20 distance classes:
par(mar=c(5,5,0.1, 0.1))
plot(correlog1.1$correlation[1:20], type="b", pch=16, cex=1.5, lwd=1.5,
xlab="distance", ylab="Moran's I", cex.lab=2, cex.axis=1.5); abline(h=0)
# make a map of the residuals:
plot(snouter.df$X, snouter.df$Y, col=c("blue",
"red")[sign(resid(model))/2+1.5], pch=19,
cex=abs(resid(model))/max(resid(model))*2, xlab="geographical xcoordinates",
ylab="geographical y-coordinates")
# calculate Moran's I values explicitly for a certain distance,
# and to test for its significance:
require(spdep)
snouter.nb <- dnearneigh(as.matrix(snouter.df[1:2]), 0, 20) #give lower and
upper distance class here!
snouter.listw <- nb2listw(snouter.nb) #turns neighbourhood object into a
weighted list
#this next step takes often several minutes to run:
GlobMT1.1<- moran.test(residuals(model), listw=snouter.listw)

require(spdep)
# Data preparation
snouter.df <- read.table("snouterdata.txt", header=T, sep="\t")
# Define coordinates, neighbours, and spatial weights
coords<-as.matrix(cbind(snouter.df$X,snouter.df$Y))
#Define neighbours up to a certain distance
nb1.5<-dnearneigh(coords,0,1.5) #you can use e.g. distance 1 or 2 instead
#Spatial weights
nb1.5.w<-nb2listw(nb1.5, glist=NULL, style="W", zero.policy=FALSE)
#1. Spatial SAR error model
sem.nb1.5.w <- errorsarlm(ols1, listw=nb1.5.w)
summary(sem.nb1.5.w) #Gives a summary of the SAR error model
# 2. Spatial SAR lag model
slm.nb1.5.w <- lagsarlm(ols1, listw=nb1.5.w, type="lag")
summary(slm.nb1.5.w) #Gives a summary of the SAR lag model
# 3. Spatial SAR mixed model
smm.nb1.5.w <- lagsarlm(ols1, listw=nb1.5.w, type="mixed")
summary(smm.nb1.5.w) #Gives a summary of the SAR mixed model

# Lagrange multiplier diagnostics for model comparison
#To test which model is most appropriate:
lm.LMtests(lm1, nb1.5.w, test="all")

require(spdep)
#Make a matrix of coordinates
coords<-as.matrix(cbind(data$X,data$Y))

#Define neighbourhood
nb<-dnearneigh(coords, 0, 2) # 2nd order neighbourhood
# define spatial weights matrix
w<-nb2listw(nb, style="W", zero.policy=FALSE)
# specify and run CAR model
cem<-spautolm(snouter1.1 ~ rain + djungle, data=snouter.df, listw=w,
family="CAR") # CAR error model
summary(cem)

require(nlme)
?gls #for syntax help
?corClasses #for help with correlation functions available
summary(gls.exp <- gls(snouter1.1 ~ rain + djungle, data=snouter.df,
correlation=corExp(form=~X+Y)))
summary(gls.gauss <- gls(snouter1.1 ~ rain + djungle, data=snouter.df,
correlation=corGaus(form=~X+Y)))
summary(gls.spher <- gls(snouter1.1 ~ rain + djungle, data=snouter.df,
correlation=corSpher(form=~X+Y)))

#%%%%%%%%%%
# here the real analysis with GEE starts:
# define dependent variable "snouter" (response)
response <- "snouter1.1"
# load necessary libraries and codes
require(gee)
require(geepack)
source("geefunctions.R") # attached as text file
# GEE ---------------------------------------------------
# gee model with fixed correlation structure
# first prepare a dataframe

data <-
data.frame(snouter.df[,c("snouter1.1",names(snouter.df[c(3:4,1:2)]))])
attach(data)
nn <- nrow(data)
coord <- cbind(X, Y)
# next compute glm model:
mglm <- glm(snouter1.1 ~ rain + djungle, family=gaussian, data=data)
resglm <- resid(mglm)
corglm <- correlog(X, Y, resglm, na.rm=T, increment=1,
resamp=1,legacy=FALSE)
# fit of autocorrelation for gee fixed model
alpha <- corglm$correlation[1]
idn <- rep(1,nn)
D <- as.matrix(dist(coord))
R <- alpha^D
# then gee model
mgee <- gee(snouter1.1 ~ rain + djungle, family=gaussian, data=data,
id=idn, R=R, corstr="fixed")
summary(mgee)[1:7]
resgee <-res.gee(snouter1.1 ~ rain + djungle, data=data, nn, b=mgee$coeff,
R=R, fam="g", type="resid")
fitgee <-res.gee(snouter1.1 ~ rain + djungle, data=data, nn, b=mgee$coeff,
R=R, fam="g", type="fitted")
detach(data)
# GEESE --------------------------------------------------
# geese model with user defined correlation structure
# cluster size, nr=3 --> 3*3 cells in a cluster
nr <- 3
# first prepare a new dataframe
data.cluster <- dat.nn(snouter.df[,c("snouter1.1",
names(snouter.df[c(3:4,1:2)]))], n=nr)
attach(data.cluster)
# prepare cluster sizes, waves within clusters and quadratic correlation
structure
# by functions from geepack and homemade functions
clusz <- clus.sz(id)
zcor <- genZcor(clusz=clusz, waves=waves, corstrv="unstructured")
zcorq <- zcor.quad(zcor, n=nr, quad=T)
# then geese model
mgeese <- geese(snouter1.1 ~ rain + djungle, family=gaussian,
data=data.cluster, id=id, corstr="userdefined", zcor=zcorq)
summary(mgeese)
# residuals and fitted values extracted by homemade functions
ageese <- a.gee(mgeese$a,n=nr,type="geese",corstr="userdefined",quad=T)
resgeese.cluster <- res.gee(snouter1.1 ~ rain + djungle,
data=data.cluster,n=nr,clusz,zcor,ageese,mgeese$b,fam="g",type="resid")
fitgeese.cluster <- res.gee(snouter1.1 ~ rain + djungle,
data=data.cluster,n=nr,clusz,zcor,ageese,mgeese$b,fam="g",type="fitted")
resgeese <- resgeese.cluster[order(o)]
fitgeese <- fitgeese.cluster[order(o)]
detach(data.cluster)

#Spatial Generalized LInear Mixed Models
require(MASS)
?glmmPQL
?corClasses
snouter.df <- read.table("snouterdata.txt", head=T)
#define a grouping factor that assigns all observations to the same group
group <- factor(rep("a",nrow(snouter.df)))
snouter.df <- cbind(snouter.df, group)
attach(snouter.df) #For some reason, the data have to be attached AND
specified in the formula!
# GLMM fits ----------------------
#exponential correlation structure
model.e <- glmmPQL(snouter2.1 ~ rain + djungle, random=~1|group,
data=snouter.df,
correlation=corExp(form=~X+Y), family=binomial))
#Gaussian correlation structure
model.g <- glmmPQL(snouter2.1 ~ rain + djungle, random=~1|group,
data=snouter.df, correlation=corGaus(form=~X+Y), family=binomial))

#spherical correlation structure
model.s <- glmmPQL(snouter2.1 ~ rain + djungle, random=~1|group,
data=snouter.df, correlation=corSpher(form=~X+Y), family=binomial))
detach(snouter.df)

#Spatial Eigenvector Mapping
require(spdep)
?ME
snouter_sp <- SpatialPixelsDataFrame(as.matrix(snouter.df[,2:1]),
snouter.df)
nb1.0 <- dnearneigh(coordinates(snouter_sp), 0, 1.0)
nb1.0_dists <- nbdists(nb1.0, coordinates(snouter_sp))
nb1.0_sims <- lapply(nb1.0_dists, function(x) (1-((x/4)^2)) )
ME.listw <- nb2listw(nb1.0, glist=nb1.0_sims, style="B")

sevm1 <- ME(snouter1.1 ~ rain + djungle, data=snouter.df, family=gaussian,
listw=ME.listw)
# modify the arguments "family" according to your error distribution