###############################################################################
##
## Copyright (C) 2010-2012, USGS Fort Collins Science Center. 
## All rights reserved.
## Contact: talbertc@usgs.gov
##
## This file is part of the Software for Assisted Habitat Modeling package
## for VisTrails.
##
## "Redistribution and use in source and binary forms, with or without 
## modification, are permitted provided that the following conditions are met:
##
##  - Redistributions of source code must retain the above copyright notice, 
##    this list of conditions and the following disclaimer.
##  - Redistributions in binary form must reproduce the above copyright 
##    notice, this list of conditions and the following disclaimer in the 
##    documentation and/or other materials provided with the distribution.
##  - Neither the name of the University of Utah nor the names of its 
##    contributors may be used to endorse or promote products derived from 
##    this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
## THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
## PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
## CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
## EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
## PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
## OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
## OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
## ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
##
## Although this program has been used by the U.S. Geological Survey (USGS), 
## no warranty, expressed or implied, is made by the USGS or the 
## U.S. Government as to the accuracy and functioning of the program and 
## related program material nor shall the fact of distribution constitute 
## any such warranty, and no responsibility is assumed by the USGS 
## in connection therewith.
##
## Any use of trade, firm, or product names is for descriptive purposes only 
## and does not imply endorsement by the U.S. Government.
###############################################################################

Pairs.Explore<-function(num.plots=5,min.cor=.7,input.file,output.file,response.col="ResponseBinary",cors.w.highest=FALSE,pres=TRUE,absn=TRUE,bgd=TRUE,Debug=FALSE){


      #num.plots=plots per page of display
      #min.cor=the minimum correlation to be included in determining which set of predictors to display
      #input.file=a csv assumed to have the new vistrails form with the first several rows specifiying where to find tiffs
      #   and which columns to include in analysis
      #output.file=...
      #response.col=name of response column to be removed and used elsewhere
    #modifications
      #5/10/2011 altered to handle count data as well presence absence.
      #any counts higher than or equal to 1 are set to be presence though I might consider
      #adding the option to have a threshold set instead of using just presence/absence
      #this subsamples data to improve running speed this along with weights set in glm/gam to improve output inspection
      #makes the gam/glm only really appropriate for looking at the relationship between the predictor and response
      #modified to output a csv with % deviance explained so Colin can display in his widget
      #modified to remove incomplete cases on a pair by pair basis (when calculating correlations) and a pair by pair basis otherwise
      #no longer removes all incomplete rows
      #Written by Marian Talbert 2011
     
       if(is.na(match("gam",installed.packages()[,1]))) {
             install.packages("gam",repos="http://lib.stat.cmu.edu/R/CRAN")
            }
        library(gam)    
      
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
  #Remove coordinates, response column, site.weights
  #before exploring predictor relationship COLUMNS 
    rm.cols <- as.vector(na.omit(c(match("x",tolower(names(dat))),match("y",tolower(names(dat))),
    match("site.weights",tolower(names(dat))),match(tolower(response.col),tolower(names(dat))),match("Split",names(dat)),match("EvalSplit",names(dat)))))
   
     #remove testing split ROWS
     if(!is.na(match("EvalSplit",names(dat)))) dat<-dat[-c(which(dat$EvalSplit=="test"),arr.ind=TRUE),]
    if(!is.na(match("Split",names(dat)))) dat<-dat[-c(which(dat$Split=="test"),arr.ind=TRUE),]
    include[is.na(include)]<-0
    
    response<-dat[,match(tolower(response.col),tolower(names(dat)))]
          if(any(response==-9998)) {
           response[response==-9998]<-0
           }
       dat<-dat[order(response),]
       response<-response[order(response)]
       #the deviance calculation requires even columns which will be removed for the pairs explore
       #but to get the same answer for the plot I need the same subsample
       for.dev<-list(dat=dat[-c(rm.cols)],response=response)
 
       rm.cols<-unique(c(rm.cols,which(include==0,arr.ind=TRUE)))
        
       #for the purpose of the pairs plot, taking all counts greater than 1 and setting them equal to presence
       #this is never exported
      if(response.col=="responseCount") {response[response>=1]<-1
      }
   
        dat<-dat[,-rm.cols]
        dat[dat==-9999]<-NA
   

     response.table<-table(response)
     max.points<-1500
     if(any(response.table> max.points)){
       for(i in names(response.table[response.table> max.points])){
             s<-sample(which(response==i,arr.ind=TRUE),size=(sum(response==i)- max.points))
             dat<-dat[-c(s),]
             response<-response[-c(s)]
            for.dev[[1]]<-for.dev[[1]][-c(s),] 
            for.dev[[2]]<-for.dev[[2]][-c(s)] 
       }
     }
      
      devExp<-vector()
      if(any(for.dev$response==-9999)) for.dev$response[for.dev$response==-9999]<-0
       for(i in (1:ncol(for.dev$dat))){
            devExp[i]<-my.panel.smooth(for.dev$dat[,i], for.dev$response,plot.it=FALSE)
           }
          write.csv(as.data.frame(devExp,row.names=names(for.dev[[1]])), file = paste(dirname(output.file),"devInfo.csv",sep="/"))

   if (response.col=="responseCount") {
    TrueResponse<-dat[,match(tolower(response.col),tolower(names(dat)))]
    } else TrueResponse<-response

    #remove any of pres absn or bgd that aren't desired
     temp<-c(0,1,-9999)
     temp<-temp[c(absn,pres,bgd)]
     dat<-dat[response%in%temp,]
     response<-response[response%in%temp]

  missing.summary<-1-apply(apply(dat,2,complete.cases),2,sum)/nrow(dat)

  #Remove columns with only one unique value
    dat<-try(dat[,as.vector(apply(dat,2,var,na.rm=TRUE)==0)!=1],silent=TRUE)
    if(class(dat)=="try-error") stop("mds file contains nonnumeric columns please remove and continue")
  
  #record correlations for later plots

    cmat<-cor(dat,use="pairwise.complete.obs")
    smat<-cor(dat,method="spearman",use="pairwise.complete.obs")
    if(dim(dat)[1]<2000){
    kmat<-cor(dat,method="kendall",use="pairwise.complete.obs")}
    else {s<-sample(seq(1:dim(dat)[1]),size=2000,replace=FALSE)
     kmat<-cor(dat[s,],method="kendall",use="pairwise.complete.obs")
    }
    cmat=pmax(abs(cmat),abs(smat),abs(kmat),na.rm=TRUE)
    High.cor<-sort(apply(abs(cmat)>min.cor,2,sum)-1,decreasing=TRUE)
         
  #take the top num.plots to put in the pairs plot or if the looking at a single
  #predictor and other predictors it's correlated with, take the top num.plots-1
  #of those with which it is correlated
    {if(cors.w.highest==FALSE){
    HighToPlot<-dat[,match(names(High.cor),names(dat))[1:min(num.plots,length(High.cor))]]
      }else{
          #take the column of the correlation matrix corresponding to the
          #predictor with the higest number of total correlations record the names
          #of the predictors that are correlated with this one predictor
          temp<-cmat[rownames(cmat)==names(High.cor[1]),]
          CorWHigh<-temp[abs(cmat[,colnames(cmat)==names(High.cor[1])])>min.cor]

          #record counts of total number of correlations with all predictors for those
          #predictors that are highly correlated with the Highest predictor
          High.cor<-sort(High.cor[names(CorWHigh)],decreasing=TRUE)
          HighToPlot<-dat[,match(names(High.cor),names(dat))[1:min(num.plots,length(High.cor))]]
          }}
              cor.hightoplot<-abs(cor(HighToPlot,use="pairwise.complete.obs"))
              diag(cor.hightoplot)<-0
    cor.range<-c(quantile(as.vector(cor.hightoplot),probs=c(0,.5,.7,.85)),1)
     
     missing.summary<-missing.summary[match(names(High.cor),names(missing.summary))[1:min(num.plots,length(High.cor))]]
  ## put histograms on the diagonal
    panel.hist <- function(x, ...)
    {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks; nB <- length(breaks)
        y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col="steelblue", ...)

    }


  ## put (absolute) correlations on the upper panels,
  ## with size proportional to the correlations.
      panel.cor <- function(x, y, digits=2, prefix="", cor.range,cor.mult, ...)
      {
      a<-colors()
          usr <- par("usr"); on.exit(par(usr))
          par(usr = c(0, 1, 0, 1))
          r <- abs(cor(x, y,use="pairwise.complete.obs"))
          spear<-abs(cor(x,y,method="spearman",use="pairwise.complete.obs"))
          ken<- abs(cor(x,y,method="kendall",use="pairwise.complete.obs"))
          all.cor<-max(r,spear,ken)
               ramp<-heat.colors(20, alpha = .7)[20:1]
          if(all.cor>=.6){        
            rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
            ramp[which.min(abs(all.cor-seq(from=.65,to=1,length=20)))])} 
          r<-max(all.cor)
               cex.cor=3*cor.mult
         txt <- format(c(r, 0.123456789), digits=digits)[1]
          txt <- paste(prefix, txt, sep="")
           #if(missing(cex.cor)) cex.cor <- 1.2/strwidth(txt)
         
              txt2=""
            if(max(all.cor)>cor.range[2]){
            if(spear==max(all.cor) && spear!=cor(x,y,use="pairwise.complete.obs")) {txt2 <- " s"
              } else if(ken==max(all.cor) && ken!=cor(x,y,use="pairwise.complete.obs")){
              txt2 <-" k"
              }

         }
          text(0.5, 0.5, txt, cex = .7+cex.cor * (r-min(cor.range))/(max(cor.range)-min(cor.range)))
          text(.9,.1,txt2,cex=cor.mult)   
         }
  
  #Find a new unique file name (one in the desired directory that hasn't yet been used)

 options(warn=-1)
 num.plots<-min(ncol(HighToPlot),num.plots)
 if(num.plots<8) wdth=1500
 else if(num.plots<15) wdth=3000
      else wdth=4500
 if(Debug==FALSE) jpeg(output.file,width=wdth,height=wdth,pointsize=13)
    MyPairs(cbind(TrueResponse,HighToPlot),cor.range=cor.range,missing.summary=missing.summary,my.labels=(as.vector(High.cor)[1:num.plots]),
    lower.panel=panel.smooth,diag.panel=panel.hist, upper.panel=panel.cor,pch=21,
    bg = c("blue","red","yellow")[factor(response,levels=c(0,1,-9999))],col.smooth = "red")

 if(Debug==FALSE) graphics.off()
 options(warn=0)
 
  }

MyPairs<-function (x,missing.summary,my.labels,labels, panel = points, ..., lower.panel = panel,
    upper.panel = panel,diag.panel = NULL, text.panel = textPanel,
    label.pos = 0.5 + has.diag/3, cex.labels = NULL, font.labels = 1,
    row1attop = TRUE, gap = 1,Toplabs=NULL)
{
    response<-x[,1]
    response[response==-9999]<-0
    x<-x[,2:dim(x)[2]]
    cex.mult<-3
    textPanel <- function(x = 0.5, y = 0.5, txt, cex, font) text(x,
        y, txt, cex = cex, font = font)
    localAxis <- function(side, x, y, xpd, bg, col = NULL, main,
        oma, ...) {
        if (side%%2 == 1)
            Axis(x, side = side, xpd = NA, ...)
        else Axis(y, side = side, xpd = NA, ...)
    }
    localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
    localLowerPanel <- function(..., main, oma, font.main, cex.main) lower.panel(...)
    localUpperPanel <- function(..., main, oma, font.main, cex.main) upper.panel(...)
    localDiagPanel <- function(..., main, oma, font.main, cex.main) diag.panel(...)
    dots <- list(...)
    nmdots <- names(dots)
    if (!is.matrix(x)) {
        x <- as.data.frame(x)
        for (i in seq_along(names(x))) {
            if (is.factor(x[[i]]) || is.logical(x[[i]]))
                x[[i]] <- as.numeric(x[[i]])
            if (!is.numeric(unclass(x[[i]])))
                stop("non-numeric argument to 'pairs'")
        }
    } else if (!is.numeric(x))
        stop("non-numeric argument to 'pairs'")
    panel <- match.fun(panel)
    if ((has.lower <- !is.null(lower.panel)) && !missing(lower.panel))
        lower.panel <- match.fun(lower.panel)
    if ((has.upper <- !is.null(upper.panel)) && !missing(upper.panel))
        upper.panel <- match.fun(upper.panel)
    if ((has.diag <- !is.null(diag.panel)) && !missing(diag.panel))
        diag.panel <- match.fun(diag.panel)
    if (row1attop) {
        tmp <- lower.panel
        lower.panel <- upper.panel
        upper.panel <- tmp
        tmp <- has.lower
        has.lower <- has.upper
        has.upper <- tmp
    }
    nc <- ncol(x)
    if (nc < 2)
        stop("only one column in the argument to 'pairs'")
    has.labs <- TRUE
    if (missing(labels)) {
        labels <- colnames(x)
        if (is.null(labels))
            labels <- paste("var", 1L:nc)
    }
    else if (is.null(labels))
        has.labs <- FALSE
    oma <- if ("oma" %in% nmdots)
        dots$oma
    else NULL
    main <- if ("main" %in% nmdots)
        dots$main
    else NULL
    if (is.null(oma)) {
        oma <- c(4, 4, 4, 4)
        if (!is.null(main))
            oma[3L] <- 6
    }
    nCol<-ifelse(length(unique(response))>1,nc+1,nc)
    j.start<-ifelse(length(unique(response))>1,0,1)
    opar <- par(mfrow = c(nc, nCol), mar = rep.int(gap/2, 4))
    on.exit(par(opar))
    for (i in if (row1attop)
        1L:(nc)
    else nc:1L) for (j in j.start:(nc)) {
       top.gap<-c(6*gap,rep(gap/2,times=nc-1))
       bottom.gap<-c(rep(gap/2,times=nc-1),3*gap)
       left.gap<-c(3*gap,3*gap,rep(gap/2,times=nc-1))
       par(mar = c(bottom.gap[i],left.gap[j+1],top.gap[i],gap/2))
       
       
       
         if(j==0){
         localPlot(x[, i],response, xlab = "", ylab = "", axes = FALSE,
                type="n",...)
          if(i==1) title(main="Response",line=.04,cex.main=1.4*cex.mult)

                  box()
                     my.lab<-paste("cor=",round(max(abs(cor(x[,(i)],response,use="pairwise.complete.obs")),abs(cor(x[,(i)],response,method="spearman",use="pairwise.complete.obs")),
                     abs(cor(x[,(i)],response,method="kendall",use="pairwise.complete.obs"))),digits=2),sep="")

                   if(length(unique(response))>2) {panel.smooth(as.vector(x[, (i)]), as.vector(response),...)
                      title(ylab=paste("cor=",round(max(abs(cor(x[,(i)],response,use="pairwise.complete.obs")),
                          abs(cor(x[,(i)],response,method="spearman",use="pairwise.complete.obs")),abs(cor(x[,(i)],response,method="kendall",use="pairwise.complete.obs"))),digits=2),
                          sep=""),line=.02,cex.lab=1.5)
                   }
                  pct.dev<-try(my.panel.smooth(as.vector(x[, (i)]), as.vector(response),cex.mult=cex.mult,cex.lab=cex.mult,line=.02,...),silent=TRUE)
                         

                 } else{
            
             localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE,
           type="n",...)   
        if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
            box() 
            if(i==1) {
            title(main=paste("Total Cor=",my.labels[j],sep=""),line=ifelse(missing.summary[j]>.03,2.2,.04),cex.main=1.1*cex.mult)
            if(missing.summary[j]>.03) mtext(paste(round(missing.summary[j]*100), "% missing",sep=""),side=3,line=.04,cex=cex.mult*.6)
            }
            if (i == nc)
                localAxis(3 - 2 * row1attop, x[, j], x[, i],cex.axis=cex.mult*.5,
                  ...)
            if (j == 1 && (i!=1 || !has.upper || !has.lower))
                localAxis(2, x[, j], x[, i],cex.axis=cex.mult*.5, ...)
            
            mfg <- par("mfg")
            if (i == j) {
                if (has.diag)
                  localDiagPanel(as.vector(x[, i]),...)
                if (has.labs) {
                  par(usr = c(0, 1, 0, 1))
                  if(i==1){
                     for(k in 1:length(labels)){
                         if((lng<-nchar(labels[k]))>=10) labels[k]<-paste(substr(labels[k],1,10),"\n",substr(labels[k],11,lng),sep="")
                     }
                       if (is.null(cex.labels)) {
                          l.wid <- strwidth(labels, "user")
                          cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
                      }
                  }
                  text.panel(0.5, label.pos, labels[i], cex = .65*cex.labels*cex.mult,
                    font = font.labels)
               }
            }
            else if (i < j)
                  if(length(unique(x[,i])>2)){
                  localLowerPanel(as.vector(x[, j]), as.vector(x[,
                    i]),cex=cex.mult*3,cor.mult=cex.mult,...) } else {
                      my.panel.smooth(as.vector(x[, j]),as.vector(x[,i]),cex.mult=cex.mult*2,Ylab="")
                    }    
            else {
            localUpperPanel(as.vector(x[, j]), as.vector(x[,
                i]),cex=cex.mult,...)    
                }
            if (any(par("mfg") != mfg))
                stop("the 'panel' function made a new plot")
        }
        else par(new = FALSE)
    }}
    if (!is.null(main)) {
        font.main <- if ("font.main" %in% nmdots)
            dots$font.main
        else par("font.main")
        cex.main <- if ("cex.main" %in% nmdots)
            dots$cex.main
        else par("cex.main")
        mtext(main, 3, 3, TRUE, 0.5, cex = cex.main, font = font.main)
    }
    invisible(NULL)
}





Args <- commandArgs(trailingOnly=FALSE)

    for (i in 1:length(Args)){
     if(Args[i]=="-f") ScriptPath<-Args[i+1]
     }
     
    #assign default values
    num.plots <- 10
    min.cor <- .7
    responseCol <- "ResponseBinary"
    cors.w.highest <- FALSE
    pres=TRUE
    absn=TRUE
    bgd=TRUE
    #replace the defaults with passed values
    for (arg in Args) {
    	argSplit <- strsplit(arg, "=")
    	argSplit[[1]][1]
    	argSplit[[1]][2]
    	if(argSplit[[1]][1]=="p") num.plots <- as.numeric(argSplit[[1]][2])
    	if(argSplit[[1]][1]=="m") min.cor <- as.numeric(argSplit[[1]][2])
    	if(argSplit[[1]][1]=="o") output.file <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="i") infile <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="rc") responseCol <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="core") cors.w.highest <- as.logical(argSplit[[1]][2])
      if(argSplit[[1]][1]=="pres") pres <- as.logical(argSplit[[1]][2])
      if(argSplit[[1]][1]=="absn") absn <- as.logical(argSplit[[1]][2])
      if(argSplit[[1]][1]=="bgd") bgd <- as.logical(argSplit[[1]][2])
    }
 
 ScriptPath<-dirname(ScriptPath)
source(paste(ScriptPath,"my.panel.smooth.binary.r",sep="\\"))
	#Run the Pairs Explore function with these parameters
    Pairs.Explore(num.plots=num.plots,
    min.cor=min.cor,
    input.file=infile,
		output.file=output.file,
		response.col=responseCol,
		cors.w.highest=cors.w.highest,
		pres,
		absn,
		bgd)

