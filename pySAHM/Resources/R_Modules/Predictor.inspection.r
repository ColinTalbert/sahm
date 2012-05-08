Predictor.inspection<-function(predictor,input.file,output.dir,response.col="ResponseBinary",pres=TRUE,absn=TRUE,bgd=TRUE){
  chk.libs("Pred.inpect")
cex.mult<-1.5
      absn<-as.logical(absn)
      pres<-as.logical(pres)
      bgd<-as.logical(bgd)

   #Read input data and remove any columns to be excluded
    dat<-read.csv(input.file,skip=3,header=FALSE)

          hl<-readLines(input.file,1)
          hl=strsplit(hl,',')
          colnames(dat) = hl[[1]]

          tif.info<-readLines(input.file,3)
          tif.info<-strsplit(tif.info,',')
          options(warn=-1)
          include<-(as.numeric(tif.info[[2]]))
          options(warn=1)

 rm.cols <- as.vector(na.omit(c(match("x",tolower(names(dat))),match("y",tolower(names(dat))),
    match("site.weights",tolower(names(dat))),match(tolower(response.col),tolower(names(dat))),match("Split",names(dat)),match("EvalSplit",names(dat)))))

     #remove testing split
     if(!is.na(match("EvalSplit",names(dat)))) dat<-dat[-c(which(dat$EvalSplit=="test"),arr.ind=TRUE),]
    if(!is.na(match("Split",names(dat)))) dat<-dat[-c(which(dat$Split=="test"),arr.ind=TRUE),]
    include[is.na(include)]<-0
    rm.cols<-unique(c(rm.cols,which(include==0,arr.ind=TRUE)))
    response<-dat[,match(tolower(response.col),tolower(names(dat)))]
          if(any(response==-9998)) {
           response[response==-9998]<-0
           }
       dat<-dat[order(response),]
       response<-response[order(response)]

 temp<-c(0,1,-9999)
     temp<-temp[c(absn,pres,bgd)]
     dat<-dat[response%in%temp,]
     response<-response[response%in%temp]
        xy<-dat[,c(match("x",tolower(names(dat))),match("y",tolower(names(dat))))]
        dat<-dat[-rm.cols]
     dat[dat==-9999]<-NA
     pred.indx<-match(predictor,names(dat))
     pred<-dat[,pred.indx]
     output.file<-paste(output.dir,paste(names(dat)[pred.indx],".jpg",sep=""),sep="\\")
    # browser()
 jpeg(output.file,pointsize=15,height=1500,width=1500,quality=100)
     par(mfrow=c(3,2),mar=c(3,4,3,2))
     hst<-hist(pred,col="red",main="blue=abs, red=pres",xlab=names(dat)[pred.indx],cex.lab=cex.mult,cex=cex.mult,cex.main=cex.mult)
     hist(pred[response==0],breaks=hst$breaks,add=TRUE,col="blue")
     #
         a<-raster(tif.info[[3]][pred.indx])
     plot(a,maxpixels=10000,main="",cex.lab=cex.mult,cex=cex.mult,cex.axis=cex.mult)
      #
     plot(a,maxpixels=5000,main="Spatial distribution of missing data",xlab="Latitude",ylab="Longitude",cex.lab=cex.mult,cex=cex.mult)
         points(xy$x,xy$y,col=c("red","black")[complete.cases(pred)+1],pch=16,cex=cex.mult)
         #xlim=c(extent(a)@xmin,extent(a)@xmax),ylim=c(extent(a)@ymin,extent(a)@ymax)
           xlimit<-c(range(xy$x)[1],range(xy$x)[1]+diff(range(xy$x)*.5))
           rect(xlimit,rep(range(xy$y)[2],times=2),xlimit+abs(diff(range(xy$x)))*.05,extendrange(xy$y,f=.075)[2],
               col=c("black","red"))
           text(labels=c("observations","missing"),x=xlimit+abs(diff(range(xy$x)))*.05,y=extendrange(xy$y,f=.075)[2],pos=4,cex=cex.mult)
          #
      plot(a,maxpixels=5000,main="Spatial Distribution of Response",xlab="Latitude",ylab="Longitude",cex.lab=cex.mult,cex=cex.mult)
           points(x=xy$x,y=xy$y,col=c("blue","red")[(response==0)+1],bg=c("steelblue","tomato")[(response==0)+1],pch=21,cex=.5*cex.mult)
              #
               x<-pred[complete.cases(pred)]
               y<-response[complete.cases(pred)]
               o<-order(x)
                x<-x[o]
                y<-y[o]
                wgt<-c(table(y)[2]/table(y)[1],1)[factor(y,levels=c(0,1))]
                g<-try(gam(y~s(x,2),weights=wgt,family=binomial),silent=TRUE)
      plot(x,y,bg=c("blue","red","yellow")[factor(y,levels=c(0,1,-9999))],pch=21,
               ylab=ifelse(class(g)=="try-error","",paste("% dev exp ",round(100*(1-g$dev/g$null.deviance),digits=3),sep="")),
               main="GAM showing predictor response relationship",cex.lab=cex.mult,cex=cex.mult)
        y.fit<-predict.gam(g,type="response")
        segments(x0=x[1:(length(x)-1)],y0=y.fit[1:(length(x)-1)],x1=x[2:length(x)],y1=y.fit[2:length(x)],col="red",lwd=cex.mult)

dev.off()
}

make.p.tif=T
make.binary.tif=T

tc=NULL
n.folds=3
alpha=1

learning.rate = NULL
bag.fraction = 0.5
prev.stratify = TRUE
max.trees = 10000
tolerance.method = "auto"
tolerance = 0.001
seed=NULL
opt.methods=2
save.model=TRUE
MESS=FALSE

# Interpret command line argurments #
# Make Function Call #
Args <- commandArgs(trailingOnly=FALSE)

    for (i in 1:length(Args)){
     if(Args[i]=="-f") ScriptPath<-Args[i+1]
     }

    for (arg in Args) {
    	argSplit <- strsplit(arg, "=")
    	argSplit[[1]][1]
    	argSplit[[1]][2]
    	if(argSplit[[1]][1]=="c") csv <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="o") output <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="rc") responseCol <- argSplit[[1]][2]
   		if(argSplit[[1]][1]=="mpt") make.p.tif <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="mbt")  make.binary.tif <- argSplit[[1]][2]
      if(argSplit[[1]][1]=="tc")  tc <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="nf")  n.folds <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="alp")  alpha <- argSplit[[1]][2]
      if(argSplit[[1]][1]=="lr")  learning.rate <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="bf")  bag.fraction <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="ps")  prev.stratify <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="mt")  max.trees <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="om")  opt.methods <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="seed")  seed <- argSplit[[1]][2]
 		  if(argSplit[[1]][1]=="savm")  save.model <- argSplit[[1]][2]
 		  if(argSplit[[1]][1]=="tolm")  tolerance.method <- argSplit[[1]][2]
 		  if(argSplit[[1]][1]=="tol")  tolerance <- argSplit[[1]][2]
 		  if(argSplit[[1]][1]=="mes")  MESS <- argSplit[[1]][2]

    }
	print(csv)
	print(output)
	print(responseCol)

ScriptPath<-dirname(ScriptPath)
source(paste(ScriptPath,"chk.libs.r",sep="\\"))
