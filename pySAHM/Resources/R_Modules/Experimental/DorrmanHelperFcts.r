#%%%%%%%%%%
# Helper functions for the spatial usage of gee and geepack
# Code written by Gudrun Carl, 2005-2007

# for more details on GEE see also:
# Carl, G. & Kühn, I. 2007. Analyzing spatial autocorrelation in species distributions using Gaussian and logit models. Ecological Modelling, in press.
##############################################################

#########################################################################
dat.nn<-function(data,n){
#########################################################################
# A function to generate clusters and order variables and
# to produce a data frame with response, predictors, coordinates, and
# 3 new parameters:
# o for order
# id for identifying clusters and
# waves for identifying members of clusters
#
# Arguments
# data      a data frame with response and predictors and
#           in the last columns with ordered cartesian coordinates
# n         for maximal cluster size  n*n
#########################################################################

l<-dim(data)[2]
OST<-data[,l-1]
NORD<-data[,l]
ko<-OST-min(OST)
idx<-(ko-(ko%%(n)))/n+1
ks<-NORD-min(NORD)
idy<-(ks-(ks%%(n)))/n+1
ie<-(idy-1)*max(idx)+idx
idwx<-ko%%(n)+1
idwy<-ks%%(n)+1
wav<-(idwy-1)*n+idwx
data<-as.matrix(data)
o<-order(ie,wav)
id<-ie[o]
waves<-wav[o]
dat.new1<-data[o,]
dat.new2<-cbind(dat.new1,o,id,waves)
dat.new<-as.data.frame(dat.new2)
}


#########################################################################
a.gee<-function(mgee,n,type="glm",corstr="independence",quad=T) {
#########################################################################
# A function to order correlation parameters of Generalized Estimating
# Equation Models
# Arguments
# mgee       matrix or vector of correlation parameters according to model
# n          for maximal cluster size n*n
# type       type of model
#            "glm", "gee", "geese" are allowed
# corstr     correlation structure
#            "independence", "exchangeable", "userdefined" are allowed
# quad       by default quadratic correlation structure
#            for model "geese" and "userdefined" correlation only
#########################################################################

if(n==2)n3<-6
if(n==3)n3<-36
if(n==4)n3<-120
a<-rep(0,n3)
if(type=="glm") a<-a
if(type=="gee"){
  if(corstr=="exchangeable") a[c(1:n3)]<-mgee[1,2]
  if(corstr=="independence") a<-a
}
a<-as.vector(a)

if(type=="geese") {
 if(corstr=="userdefined"){
if(quad) {
if(n==2)  {
 a<-rep(0,6)
 a[c(1,2,5,6)]<-mgee[1]
 a[c(3,4)]<-mgee[2]
}
if(n==3)  {
 a<-rep(0,36)
 a[c(1,3,9,11,18,22,24,27,29,33,34,36)]<-mgee[1]
 a[c(2,6,14,21,23,35)]<-mgee[2]
 a[c(4,10,12,17,25,28,30,32)]<-mgee[3]
 a[c(5,7,13,15,16,20,26,31)]<-mgee[4]
 a[c(8,19)]<-mgee[5]
}
if(n==4)  {
 a<-rep(0,120)
 a[c(1,4,16,19,30,33,46,55,58,66,69,76,79,88,93,96,100,103,106,109,
114,115,118,120)]<-mgee[1]
 a[c(2,8,17,23,37,50,56,62,67,73,83,92,94,101,116,119)]<-mgee[2]
 a[c(3,12,27,41,54,57,95,117)]<-mgee[3]
 a[c(5,18,20,32,34,45,59,68,70,78,80,87,97,102,104,108,110,113)]<-mgee[4]
 a[c(6,9,21,22,24,31,36,38,44,49,60,63,71,72,74,77,82,84,86,91,98,
105,107,112)]<-mgee[5]
 a[c(7,13,26,28,40,42,43,53,61,85,99,111)]<-mgee[6]
 a[c(10,25,35,48,64,75,81,90)]<-mgee[7]
 a[c(11,14,29,39,47,52,65,89)]<-mgee[8]
 a[c(15,51)]<-mgee[9]
}}
if(!quad) a<-mgee
}
if(corstr=="exchangeable") a[c(1:n3)]<-mgee
if(corstr=="independence") a<-a
 }
a<-as.vector(a)
}

#########################################################################
clus.sz<-function(id){
#########################################################################
# A function to calculate sizes of clusters
# Argument
# id     vector which identifies the clusters
#########################################################################

clus<-rep(0,length(id))
k0<-0
k1<-1
for(i in 2:length(id)) { i1<-i-1
if(id[i]==id[i1]) {k1<-k1+1
if(i==length(id)) {k0<-k0+1
                  clus[k0]<-k1}}
if(id[i]!=id[i1]) {k0<-k0+1
                  clus[k0]<-k1
                  k1<-1
if(i==length(id)) {k0<-k0+1
                  clus[k0]<-k1 }}}
clusz<-clus[clus>0]
}

#########################################################################
zcor.quad<-function(zcor,n,quad=TRUE) {
#########################################################################
# A function to create a quadratic correlation structure
# zcor    an object of class "genZcor" (see: geepack)
# n       for maximal cluster size n*n
# quad    by default quadratic correlation structure
#########################################################################

if(quad) {
if(n==2)  {
 zcorn<-matrix(0,dim(zcor)[1],2)
 zcorn[,1]<-zcor[,1]+zcor[,2]+zcor[,5]+zcor[,6]
 zcorn[,2]<-zcor[,3]+zcor[,4]
}
if(n==3)  {
 zcorn<-matrix(0,dim(zcor)[1],5)
 zcorn[,1]<-zcor[,1]+zcor[,3]+zcor[,9]+zcor[,11]+zcor[,18]+zcor[,22]+
 zcor[,24]+zcor[,27]+zcor[,29]+zcor[,33]+zcor[,34]+zcor[,36]
 zcorn[,2]<-zcor[,2]+zcor[,6]+zcor[,14]+zcor[,21]+zcor[,23]+zcor[,35]
 zcorn[,3]<-zcor[,4]+zcor[,10]+zcor[,12]+zcor[,17]+zcor[,25]+zcor[,28]+
 zcor[,30]+zcor[,32]
 zcorn[,4]<-zcor[,5]+zcor[,7]+zcor[,13]+zcor[,15]+zcor[,16]+zcor[,20]+
 zcor[,26]+zcor[,31]
 zcorn[,5]<-zcor[,8]+zcor[,19]
}
if(n==4)  {
 zcorn<-matrix(0,dim(zcor)[1],9)
  zcorn[,1]<-zcor[,1]+zcor[,4]+zcor[,16]+zcor[,19]+zcor[,30]+zcor[,33]+
zcor[,46]+zcor[,55]+zcor[,58]+zcor[,66]+zcor[,69]+zcor[,76]+
zcor[,79]+zcor[,88]+zcor[,93]+zcor[,96]+zcor[,100]+zcor[,103]+
zcor[,106]+zcor[,109]+zcor[,114]+zcor[,115]+zcor[,118]+zcor[,120]
  zcorn[,2]<-zcor[,2]+zcor[,8]+zcor[,17]+zcor[,23]+zcor[,37]+zcor[,50]+
zcor[,56]+zcor[,62]+zcor[,67]+zcor[,73]+zcor[,83]+zcor[,92]+
zcor[,94]+zcor[,101]+zcor[,116]+zcor[,119]
  zcorn[,3]<-zcor[,3]+zcor[,12]+zcor[,27]+zcor[,41]+zcor[,54]+zcor[,57]+
zcor[,95]+zcor[,117]
  zcorn[,4]<-zcor[,5]+zcor[,18]+zcor[,20]+zcor[,32]+zcor[,34]+zcor[,45]+
zcor[,59]+zcor[,68]+zcor[,70]+zcor[,78]+zcor[,80]+zcor[,87]+
zcor[,97]+zcor[,102]+zcor[,104]+zcor[,108]+zcor[,110]+zcor[,113]
  zcorn[,5]<-zcor[,6]+zcor[,9]+zcor[,21]+zcor[,22]+zcor[,24]+zcor[,31]+
zcor[,36]+zcor[,38]+zcor[,44]+zcor[,49]+zcor[,60]+zcor[,63]+
zcor[,71]+zcor[,72]+zcor[,74]+zcor[,77]+zcor[,82]+zcor[,84]+
zcor[,86]+zcor[,91]+zcor[,98]+zcor[,105]+zcor[,107]+zcor[,112]
  zcorn[,6]<-zcor[,7]+zcor[,13]+zcor[,26]+zcor[,28]+zcor[,40]+zcor[,42]+
zcor[,43]+zcor[,53]+zcor[,61]+zcor[,85]+zcor[,99]+zcor[,111]
  zcorn[,7]<-zcor[,10]+zcor[,25]+zcor[,35]+zcor[,48]+zcor[,64]+zcor[,75]+
zcor[,81]+zcor[,90]
  zcorn[,8]<-zcor[,11]+zcor[,14]+zcor[,29]+zcor[,39]+zcor[,47]+zcor[,52]+
zcor[,65]+zcor[,89]
  zcorn[,9]<-zcor[,15]+zcor[,51]
}
}
if(!quad) zcorn<-zcor
zcorn<-as.matrix(zcorn)
}

#########################################################################
res.gee<-function(formula,data,n,clusz=NA,zcor=NA,a=NA,b,R=NA,
fam="b",type="resid")  {
#########################################################################
# A function to calculate fitted values and residuals
# for Generalized Estimating Equation Models
# for gaussian or binary data (with logit link) or Poisson data (log link)
# Arguments
# formula     a formula expression
# data        a data frame
# n           for maximal cluster size n*n
# clusz       an object of class "clus.sz"
# zcor        an object of class "genZcor"
# a           a vector of correlation parameters
#             for clusters only
#             as an object of class "a.gee"
# b           a vector of regression parameters beta
# R           a square matrix of correlation parameters
#             for full dimension (=number of observations)  only
# fam         family
#             "g", "b", "p" (gaussian, binary, Poisson) are allowed
# type        "fitted" calculates fitted values
#             "resid" calculates residuals
#
#########################################################################

l<-dim(data)[2]
ieo<-data[,l-1]
if(n!=dim(data)[1]) {
n2<-n*n
n3<-n2*(n2-1)/2
n4<-n2-1
n5<-n2-2
for(i in 1:dim(zcor)[1]){
for(k in 1:n3){
if(zcor[i,k]==1) zcor[i,k]<-a[k]  }}
lc<-length(clusz)
z2<-matrix(0,lc,n3)
for( j in 1:n3) {
k3<-0
k2<-0
for(i in 1:lc) {
if(clusz[i]!=1) {
k2<-k3+1
k3<-clusz[i]*(clusz[i]-1)/2+k3
for(k in k2:k3)
z2[i,j]<-zcor[k,j]+z2[i,j] }}}
if(n==2)
iod<-c(1,1,2)
if(n==3)
iod<-c(1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,4,4,4,4,5,5,5,6,6,7)
if(n==4)
iod<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,
3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,
6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,10,10,
11,11,11,11,12,12,12,13,13,14)
cs<-0
v<-matrix(0,length(ieo),length(ieo))
vgl<-rep(0,n2)
for(i in 1:lc) {clu<-clusz[i]
if(clu!=1) {
v1<-matrix(0,n2,n2)
if(n==2)
{  v1[1,2:4]<-z2[i,1:3]
   v1[2,3:4]<-z2[i,4:5]
   v1[3,4]<-z2[i,6]  }
if(n==3)
{  v1[1,2:9]<-z2[i,1:8]
   v1[2,3:9]<-z2[i,9:15]
   v1[3,4:9]<-z2[i,16:21]
   v1[4,5:9]<-z2[i,22:26]
   v1[5,6:9]<-z2[i,27:30]
   v1[6,7:9]<-z2[i,31:33]
   v1[7,8:9]<-z2[i,34:35]
   v1[8,9]<-z2[i,36]  }
if(n==4)
{  v1[1,2:16]<-z2[i,1:15]
   v1[2,3:16]<-z2[i,16:29]
   v1[3,4:16]<-z2[i,30:42]
   v1[4,5:16]<-z2[i,43:54]
   v1[5,6:16]<-z2[i,55:65]
   v1[6,7:16]<-z2[i,66:75]
   v1[7,8:16]<-z2[i,76:84]
   v1[8,9:16]<-z2[i,85:92]
   v1[9,10:16]<-z2[i,93:99]
   v1[10,11:16]<-z2[i,100:105]
   v1[11,12:16]<-z2[i,106:110]
   v1[12,13:16]<-z2[i,111:114]
   v1[13,14:16]<-z2[i,115:117]
   v1[14,15:16]<-z2[i,118:119]
   v1[15,16]<-z2[i,120]   }
for(i1 in 1:length(iod)) {
i2<-iod[i1]
if(var(v1[i2,1:n2])==0) {for(k in i2:n5) {k1<-k+1
                                        v1[k,]<-v1[k1,]
                                        v1[k1,]<-vgl[]}}}
for(i1 in 1:length(iod)){
i3<-iod[i1]+1
if(var(v1[1:n2,i3])==0) {for(k in i3:n4) {k1<-k+1
                                        v1[,k]<-v1[,k1]
                                        v1[,k1]<-vgl[]}}}

clu1<-clu-1
for(k in 1:clu1) {csk<-cs+k
f1<-2
for(k1 in f1:clu) {k2<-cs+f1
v[csk,k2]<-v1[k,k1]
f1<-f1+1 }}
for(k in 1:clu) {csk<-cs+k
v[csk,csk]<- 0.5 } }
if(clu==1) {cs1<-cs+1
v[cs1,cs1]<-0.5 }
cs<- cumsum(clusz)[i]  }
v<-v+t(v)
}
if(n==dim(data)[1]) v<-R
ww<-solve(v)

s.geese<-svd(ww,LINPACK=T)
d.geese<-diag(sqrt(s.geese$d))
w<-s.geese$u%*%d.geese%*%t(s.geese$u)

x.matrix<-model.matrix(formula)
fitted<-x.matrix%*%b
fitted<-fitted[1:length(ieo)]
if(fam=="p") fitted<-exp(fitted)
if(fam=="b") fitted<-exp(fitted)/(1+exp(fitted))

if(type=="fitted")resgeeseo<-fitted
if(type=="resid"){
if(fam=="g") rgeese<-data[,1]-fitted
if(fam=="p") rgeese<-(data[,1]-fitted)/sqrt(fitted)
if(fam=="b") rgeese<-(data[,1]-fitted)/sqrt(fitted*(1-fitted))
rsgeese<-w%*%rgeese
resgeeseo<-rsgeese[1:length(ieo)]
}

resgeeseo<-as.vector(resgeeseo)
}


#########################################################

#%%%%%%%%%%

