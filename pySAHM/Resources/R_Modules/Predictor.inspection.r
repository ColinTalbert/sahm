Predictor.inspection<-function(predictor,input.file,output.dir,response.col="ResponseBinary",pres=TRUE,absn=TRUE,bgd=TRUE){
  chk.libs("Pred.inpect")
cex.mult<-3.2

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

    response<-dat[,match(tolower(response.col),tolower(names(dat)))]
          if(any(response==-9998)) {
           response[response==-9998]<-0
           abs.lab<-"Avail"
           pres.lab<-"Used"
           } else {
            abs.lab<-"Abs"
            pres.lab<-"Pres"
           }
       dat<-dat[order(response),]
       response<-response[order(response)]
     temp<-c(0,1,-9999)
     temp<-temp[c(absn,pres,bgd)]
     dat<-dat[response%in%temp,]
     response<-response[response%in%temp]
    
     if(any(unique(response)==-9999) & !any(unique(response)==0)){
    abs.lab<-"PsedoAbs"
    pres.lab<-"Pres" 
     }
     if(any(response==-9999)) response[response==-9999]<-0
        xy<-dat[,c(match("x",tolower(names(dat))),match("y",tolower(names(dat))))]
        names(xy)<-c("x","y")
     dat[dat==-9999]<-NA
     pred.indx<-match(predictor,tif.info[[1]])
     pred<-dat[,pred.indx]
     output.file<-paste(output.dir,paste(names(dat)[pred.indx],".jpg",sep=""),sep="\\")
     ### Producing some plots
    jpeg(output.file,pointsize=13,height=2000,width=2000,quality=100)
         par(mfrow=c(2,2),mar=c(5,7,9,6))
             hst<-hist(pred,plot=FALSE)
      ####PLOT 1.
         hist(pred,col="red",xlab="",main="",cex.lab=cex.mult,cex=cex.mult,cex.main=cex.mult,cex.axis=.7*cex.mult,ylim=c(0,1.5*max(hst$counts)))
             hist(pred[response==0],breaks=hst$breaks,add=TRUE,col="blue")
             legend("topright",xjust=1,yjust=1,legend=c(pres.lab,abs.lab),fill=c("red","blue"),cex=cex.mult)
              mtext(paste(names(dat[pred.indx]),"  (",round(100*(1-sum(complete.cases(pred))/length(pred)),digits=1),"% missing)",sep=""), side = 3,outer=TRUE,line=-4,cex=1.2*cex.mult)
             if(any(is.na(pred))){ #a table of missing values but only if we have missing data
                  rect(1.1*min(hst$breaks),1.1*max(hst$counts),quantile(hst$breaks,prob=.55),ytop=1.45*max(hst$counts),lwd=cex.mult)
                   xloc<-rep(as.vector(quantile(hst$breaks,probs=c(0,.2,.4))),c(2,3,3))
                   yloc<-rep(c(1.4,1.3,1.2)*max(hst$counts),times=3)
                   yloc<-yloc[-1]
                   my.table<-table(complete.cases(pred),response)
                   text(labels=c("Missing","Not Mssg",abs.lab,paste(round(100*table(complete.cases(pred[response==0]))/sum(response==0),digits=1),"%",sep=""),
                   pres.lab,paste(round(100*table(complete.cases(pred[response==1]))/sum(response==1),digits=1),"%",sep="")),x=xloc,
                        y=yloc,pos=4,cex=.7*cex.mult)
                   text("Missing By Response",x=xloc[1],y=1.5*max(hst$counts),pos=4,cex=.9*cex.mult)
               }
       ####PLOT 2.
              a<-raster(tif.info[[3]][pred.indx])
              text("Spatial Distribution of Predictor")
         plot(a,maxpixels=5000,main="",cex.lab=cex.mult,cex=cex.mult,
              cex.axis=.7*cex.mult,cex.main=cex.mult,legend.shrink = cex.mult*.3, legend.width = cex.mult*.5)
       ####PLOT 3.
          par(mar=c(5,7,5,5))
          plot(a,alpha=.5,maxpixels=5000,main=paste("Spatial distribution of missing data\n",ifelse(pres.lab=="Pres","presence and absence","used and available"),sep=""),xlab="",ylab="",cex.lab=cex.mult,
                   cex.axis=.7*cex.mult,cex=cex.mult,cex.main=cex.mult*.8,legend=FALSE)
             #points(xy$x,xy$y,col=c("black","red")[complete.cases(pred)+1],pch=16,cex=.5*cex.mult)
             points(x=xy$x[complete.cases(pred)],y=xy$y[complete.cases(pred)],col=c("red4","blue4")[(response[complete.cases(pred)]==0)+1],
                   bg=c("red","steelblue")[(response[complete.cases(pred)]==0)+1],pch=21,cex=c(.6*cex.mult,.4*cex.mult)[(response[complete.cases(pred)]==0)+1])       
              points(x=xy$x[(!complete.cases(pred))],y=xy$y[(!complete.cases(pred))],col=c("black","gold")[(response[!complete.cases(pred)]==1)+1],
                   bg=c("grey18","gold")[(response[!complete.cases(pred)]==1)+1],pch=21,cex=.5*cex.mult)
              legend("topleft",legend=c(if(any(is.na(pred))) paste(pres.lab,"Missing",sep=" "), if(any(is.na(pred))) paste(abs.lab," Missing",sep=""), paste(pres.lab,"(not missing)",sep=" "),paste(abs.lab," (not missing)",sep="")),
                   fill=c(if(any(is.na(pred))) "yellow",if(any(is.na(pred))) "black","red","blue"),xjust=0,yjust=1,
                   ncol = 2,cex=cex.mult*.9,bg="white")
                  #
       ####PLOT 4.  
                 max.points<-1000
                 response.table<-table(response)
                 if(any(response.table> max.points)){
                   for(i in names(response.table[response.table> max.points])){
                         s<-sample(which(response==i,arr.ind=TRUE),size=(sum(response==i)- max.points))
                         pred<-pred[-c(s)]
                         response<-response[-c(s)]
                   }
                 }  
                   x<-pred[complete.cases(pred)]
                   y<-response[complete.cases(pred)]
                   o<-order(x)
                    x<-x[o]
                    y<-y[o]
                    options(warn=2) #promote warnings to errors so we don't plot terrible fits
                    g<-try(gam(y~s(x,2),family=binomial),silent=TRUE)
                    options(warn=0)
                 #   g<-gam(as.vector(response[!is.na(pred)])~s(as.vector(na.omit(pred)),2),family=binomial)
      #  browser()
          plot(x,y,bg=c("blue","red","yellow")[factor(y,levels=c(0,1,-9999))],pch=21,col=c("blue4","red4","yellow3")[factor(y,levels=c(0,1,-9999))],
                   ylab=ifelse("gam"%in%class(g),paste("% dev exp ",round(100*(1-g$dev/g$null.deviance),digits=1),sep=""),""),
                   main="GAM showing predictor response relationship",cex.lab=cex.mult,cex=cex.mult,cex.main=.8*cex.mult,cex.axis=.7*cex.mult)
             if(!("try-error"%in%class(g))){
              y.fit<-try(predict.gam(g,type="response"),silent=TRUE)
               segments(x0=x[1:(length(x)-1)],y0=y.fit[1:(length(x)-1)],x1=x[2:length(x)],y1=y.fit[2:length(x)],col="red",lwd=cex.mult)
               }
            #           xnew<-as.data.frame(unique(x))
            #           colnames(xnew)="x"
            #  y.fit<-predict.gam(g,newdata=xnew,type="response")
            #  segments(x0=xnew[1:(nrow(xnew)-1),],y0=y.fit[1:(nrow(xnew)-1)],x1=xnew[2:nrow(xnew),],y1=y.fit[2:nrow(xnew)],col="red",lwd=cex.mult)
    dev.off()    
}

# Interpret command line argurments #
# Make Function Call #
Args <- commandArgs(T)
   
    #assign default values
   
    responseCol <- "ResponseBinary"
    pres=TRUE
    absn=TRUE
    bgd=TRUE
    #replace the defaults with passed values
    for (arg in Args) {
    	argSplit <- strsplit(arg, "=")
    	argSplit[[1]][1]
    	argSplit[[1]][2]
    	if(argSplit[[1]][1]=="p") predictor <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="o") output <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="i") infile <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="rc") responseCol <- argSplit[[1]][2]
      if(argSplit[[1]][1]=="pres") pres <- as.logical(argSplit[[1]][2])
      if(argSplit[[1]][1]=="absn") absn <- as.logical(argSplit[[1]][2])
      if(argSplit[[1]][1]=="bgd") bgd <- as.logical(argSplit[[1]][2])
    }

ScriptPath<-dirname(ScriptPath)
source(paste(ScriptPath,"chk.libs.r",sep="\\"))

Predictor.inspection(predictor=predictor,input.file=infile,output.dir=output,response.col=responseCol,pres=TRUE,absn=TRUE,bgd=TRUE)