Predictor.inspection<-function(predictor,input.file,output.dir,response.col="ResponseBinary",pres=TRUE,absn=TRUE,bgd=TRUE){
  chk.libs("Pred.inpect")
cex.mult<-3.2
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

    rm.cols<-unique(rm.cols)
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
        names(xy)<-c("x","y")
        dat<-dat[-rm.cols]
     dat[dat==-9999]<-NA
     pred.indx<-match(predictor,names(dat))
     pred<-dat[,pred.indx]
     output.file<-paste(output.dir,paste(names(dat)[pred.indx],".jpg",sep=""),sep="\\")
     ### Producing some plots
    jpeg(output.file,pointsize=13,height=2000,width=2000,quality=100)
         par(mfrow=c(2,2),mar=c(5,7,9,6))
             hst<-hist(pred,plot=FALSE)
         ####PLOT 1.
         hist(pred,col="red",xlab="",main="",cex.lab=cex.mult,cex=cex.mult,cex.main=cex.mult,cex.axis=.7*cex.mult,ylim=c(0,1.5*max(hst$counts)))
             hist(pred[response==0],breaks=hst$breaks,add=TRUE,col="blue")
             legend("topright",xjust=1,yjust=1,legend=c("pres","abs"),fill=c("red","blue"),cex=cex.mult)
              mtext(paste(names(dat[pred.indx]),"  (",round(100*(1-sum(complete.cases(pred))/length(pred)),digits=1),"% missing)",sep=""), side = 3,outer=TRUE,line=-4,cex=1.2*cex.mult)
             if(any(is.na(pred))){ #a table of missing values but only if we have missing data
                  rect(1.1*min(hst$breaks),1.1*max(hst$counts),quantile(hst$breaks,prob=.55),ytop=1.45*max(hst$counts),lwd=cex.mult)
                   xloc<-rep(as.vector(quantile(hst$breaks,probs=c(0,.2,.4))),c(2,3,3))
                   yloc<-rep(c(1.4,1.3,1.2)*max(hst$counts),times=3)
                   yloc<-yloc[-1]
                   my.table<-table(complete.cases(pred),response)
                   text(labels=c("Missing","Not Mssg","Abs",paste(round(100*table(complete.cases(pred[response==0]))/sum(response==0),digits=1),"%",sep=""),
                   "Pres",paste(round(100*table(complete.cases(pred[response==1]))/sum(response==1),digits=1),"%",sep="")),x=xloc,
                        y=yloc,pos=4,cex=.7*cex.mult)
                   text("Missing By Response",x=xloc[1],y=1.5*max(hst$counts),pos=4,cex=.9*cex.mult)
               }
         ####PLOT 2.
              a<-raster(tif.info[[3]][pred.indx])
              text("Spatial Distribution of Predictor")
         plot(a,maxpixels=10000,main="",cex.lab=cex.mult,cex=cex.mult,
              cex.axis=.7*cex.mult,cex.main=cex.mult,legend.shrink = cex.mult*.3, legend.width = cex.mult*.5)
          ####PLOT 3.
          par(mar=c(5,7,5,5))
         plot(a,alpha=.5,maxpixels=5000,main="Spatial distribution of missing data\n presence and absence",xlab="",ylab="",cex.lab=cex.mult,
                   cex.axis=.7*cex.mult,cex=cex.mult,cex.main=cex.mult*.8,legend=FALSE)
             #points(xy$x,xy$y,col=c("black","red")[complete.cases(pred)+1],pch=16,cex=.5*cex.mult)
             points(x=xy$x[complete.cases(pred)],y=xy$y[complete.cases(pred)],col=c("red4","blue4")[(response[complete.cases(pred)]==0)+1],
                   bg=c("tomato","steelblue")[(response[complete.cases(pred)]==0)+1],pch=21,cex=.5*cex.mult)
              points(x=xy$x[(!complete.cases(pred))],y=xy$y[(!complete.cases(pred))],col=c("black","gold")[(response[!complete.cases(pred)]==1)+1],
                   bg=c("grey18","gold")[(response[!complete.cases(pred)]==1)+1],pch=21,cex=.5*cex.mult)
              legend("topleft",legend=c(if(any(is.na(pred))) "Pres Missing", if(any(is.na(pred))) "Abs Missing","Pres (not missing)","Abs (not missing)"),
                   fill=c(if(any(is.na(pred))) "yellow",if(any(is.na(pred))) "black","red","blue"),xjust=0,yjust=1,
                   ncol = 2,cex=cex.mult*.9,bty="n")
                  #
                   x<-pred[complete.cases(pred)]
                   y<-response[complete.cases(pred)]
                   o<-order(x)
                    x<-x[o]
                    y<-y[o]
                    wgt<-c(table(y)[2]/table(y)[1],1)[factor(y,levels=c(0,1))]
                    g<-try(gam(y~s(x,2),weights=wgt,family=binomial),silent=TRUE)
          ####PLOT 4.
          plot(x,y,bg=c("blue","red","yellow")[factor(y,levels=c(0,1,-9999))],pch=21,col=c("blue4","red4","yellow3")[factor(y,levels=c(0,1,-9999))],
                   ylab=ifelse("gam"%in%class(g),paste("% dev exp ",round(100*(1-g$dev/g$null.deviance),digits=1),sep=""),""),
                   main="GAM showing predictor response relationship",cex.lab=cex.mult,cex=cex.mult,cex.main=.8*cex.mult,cex.axis=.7*cex.mult)
              y.fit<-predict.gam(g,type="response")
              segments(x0=x[1:(length(x)-1)],y0=y.fit[1:(length(x)-1)],x1=x[2:length(x)],y1=y.fit[2:length(x)],col="red",lwd=cex.mult)

    dev.off()
     #where would this look nice in the above plots???
     table(complete.cases(pred),response)
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
