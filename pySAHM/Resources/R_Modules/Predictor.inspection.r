Predictor.inspection<-function(predictor,input.file,output.dir,response.col="ResponseBinary",pres=TRUE,absn=TRUE,bgd=TRUE){


#This function produces several plots to inspect the relationship between the predictor and response
#and the spatial relationship between response and the predictor. 
#It fits a gam to the response/predictor but this seems to be unstable if there is an overwhelming 
#amount of absence data so I weight the absence so that the sum of absences weights is equal to sum of presence weights
#if the initial presence\abs ratio is >1.2 The gam can still break so if a negaive percent deviance explained is reported
#I replace the gam with a glm quadratic in the predictor 

#Written by Marian Talbert 5/23/2012
    chk.libs("Pred.inspect")
    cex.mult<-3.2

   #Read input data and remove any columns to be excluded
    read.dat(input.file,response.col=response.col,is.inspect=TRUE,,pres=pres,absn=absn,bgd=bgd)

     if(any(unique(response)==-9999) & !any(unique(response)==0)){
    abs.lab<-"PsedoAbs"
    pres.lab<-"Pres" 
     }
     if(any(response==-9999)) response[response==-9999]<-0
        xy<-dat[,c(match("x",tolower(names(dat))),match("y",tolower(names(dat))))]
        names(xy)<-c("x","y")
    
     pred.indx<-match(predictor,tif.info[[1]])
     pred<-dat[,pred.indx]
     
     output.file<-paste(output.dir,paste(names(dat)[pred.indx],".jpg",sep=""),sep="\\")
     ### Producing some plots
    jpeg(output.file,pointsize=13,height=2000,width=2000,quality=100)
         par(mfrow=c(2,2),mar=c(5,7,9,6),oma=c(6,2,2,2))
             hst<-hist(pred,plot=FALSE)
      ####PLOT 1. new
         hist(pred,col="red",xlab="",main="",cex.lab=cex.mult,cex=cex.mult,cex.main=cex.mult,cex.axis=.7*cex.mult,ylim=c(0,1.5*max(hst$counts)))
             hist(pred[response==0],breaks=hst$breaks,add=TRUE,col="blue")
             legend("topright",xjust=1,yjust=1,legend=c(pres.lab,abs.lab),fill=c("red","blue"),cex=cex.mult)
              mtext(paste(names(dat[pred.indx]),"  (",round(100*(1-sum(complete.cases(pred))/length(pred)),digits=1),"% missing)",sep=""), side = 3,outer=TRUE,line=-4,cex=1.2*cex.mult)
             if(sum(is.na(pred))/length(pred)>.003){ #a table of missing values but only if we have missing data
                  rect(1*min(hst$breaks),1.1*max(hst$counts),quantile(hst$breaks,prob=.55),ytop=1.45*max(hst$counts),lwd=cex.mult)
                   xloc<-rep(as.vector(quantile(hst$breaks,probs=c(0,.2,.4))),c(2,3,3))
                   yloc<-rep(c(1.4,1.3,1.2)*max(hst$counts),times=3)
                   yloc<-yloc[-1]
                   my.table<-table(complete.cases(pred),response)
                   text(labels=c("Missing","Not Mssg",abs.lab,paste(round(100*my.table[,1]/sum(my.table[,1]),digits=1),"%",sep=""),
                   pres.lab,paste(round(100*my.table[,2]/sum(my.table[,2]),digits=1),"%",sep="")),x=xloc,
                        y=yloc,pos=4,cex=.7*cex.mult)
                   text("Missing By Response",x=xloc[1],y=1.5*max(hst$counts),pos=4,cex=.9*cex.mult)
               }
       ####PLOT 2.
              a<-raster(tif.info[[3]][pred.indx])
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
                y<-as.numeric(TrueResponse[complete.cases(pred)])
                pred<-as.numeric(pred[complete.cases(pred)])
                 x<-pred[complete.cases(pred)]
                 y<-y[complete.cases(pred)] 
                 response.table<-table(y)
             
         if(length(response.table)>1){
         par(mgp=c(4, 1, 0),mar=c(7,7,5,5))
            plot(x,y,ylab="",xlab="",type="n",cex.axis=.7*cex.mult)
            gam.failed<-my.panel.smooth(x=x, y=y,cex.mult=cex.mult,pch=21,cex.lab=cex.mult,cex.axis=.9*cex.mult,cex.lab=cex.mult,famly=famly,lin=4)
            title(main=paste(ifelse(gam.failed,"GLM","GAM")," showing predictor response relationship",sep=""),cex.main=.8*cex.mult)
            title(xlab=predictor,line=5,cex.lab=1.2*cex.mult)
        }
    dev.off()    
}

# Interpret command line argurments #
# Make Function Call #
Args <- commandArgs(trailingOnly=FALSE)

    for (i in 1:length(Args)){
     if(Args[i]=="-f") ScriptPath<-Args[i+1]
     }
   
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
source(paste(ScriptPath,"my.panel.smooth.binary.r",sep="\\"))
source(paste(ScriptPath,"read.dat.r",sep="\\"))
Predictor.inspection(predictor=predictor,input.file=infile,output.dir=output,response.col=responseCol,pres=pres,absn=absn,bgd=bgd)