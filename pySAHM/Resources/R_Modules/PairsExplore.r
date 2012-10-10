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
     
   chk.libs("PairsExplore")   
            
   #Read input data 
   read.dat(input.file=input.file,response.col=response.col,is.inspect=TRUE,pres=pres,absn=absn,bgd=bgd)
                
  #Remove coordinates, response column, site.weights
  #before exploring predictor relationship COLUMNS 
  rm.cols <- as.vector(na.omit(c(match("x",tolower(names(dat))),match("y",tolower(names(dat))),
  match("site.weights",tolower(names(dat))),match(tolower(response.col),tolower(names(dat))),match("Split",names(dat)),match("EvalSplit",names(dat)))))
                
  missing.summary<-1-apply(apply(dat,2,complete.cases),2,sum)/nrow(dat)
     
  #the deviance calculation requires even columns which will be removed for the pairs explore
  #but to get the same answer for the plot I need the same subsample
       for.dev<-list(dat=dat[-c(rm.cols)],response=TrueResponse)  
       
       rm.cols<-unique(c(rm.cols,which(include==0,arr.ind=TRUE)))
        dat<-dat[,-rm.cols]
        
      devExp<-vector()
      if(any(for.dev$response==-9999)) for.dev$response[for.dev$response==-9999]<-0
       for(i in (1:ncol(for.dev$dat))){
            devExp[i]<-try(my.panel.smooth(for.dev$dat[,i], for.dev$response,plot.it=FALSE,famly=famly),silent=TRUE)
           }
          write.csv(as.data.frame(devExp,row.names=names(for.dev[[1]])), file = paste(dirname(output.file),"devInfo.csv",sep="/"))

          #after calculating the deviance for all predictors we have to remove the excluded predictors for the following plots
      for.dev$dat=dat 
     #subsample the data so we can calculate correlations quickly
     response.table<-table(response)
     max.points<-1500
     if(any(response.table> max.points)){
       for(i in names(response.table[response.table> max.points])){
             s<-sample(which(response==i,arr.ind=TRUE),size=(sum(response==i)- max.points))
             dat<-dat[-c(s),]
             response<-response[-c(s)]
             TrueResponse<-TrueResponse[-c(s)]
       }
     }
     
    
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
    cmat[is.na(cmat)]<-0
    High.cor<-sort(apply(abs(cmat)>min.cor,2,sum)-1,decreasing=TRUE)

  #take the top num.plots to put in the pairs plot or if the looking at a single
  #predictor and other predictors it's correlated with, take the top num.plots-1
  #of those with which it is correlated
    if(cors.w.highest==TRUE){
          #take the column of the correlation matrix corresponding to the
          #predictor with the higest number of total correlations record the names
          #of the predictors that are correlated with this one predictor
          temp<-cmat[rownames(cmat)==names(High.cor[1]),]
          CorWHigh<-temp[abs(cmat[,colnames(cmat)==names(High.cor[1])])>min.cor]

          #record counts of total number of correlations with all predictors for those
          #predictors that are highly correlated with the Highest predictor
          High.cor<-sort(High.cor[names(CorWHigh)],decreasing=TRUE)    
     }
     HighToPlot<-dat[,match(names(High.cor),names(dat))[1:min(num.plots,length(High.cor))]]
     for.dev$dat<-for.dev$dat[,match(names(High.cor),names(dat))[1:min(num.plots,length(High.cor))]]
     
    cor.hightoplot<-abs(cor(HighToPlot,use="pairwise.complete.obs"))
    diag(cor.hightoplot)<-0
    cor.hightoplot[is.na(cor.hightoplot)]<-0 
    cor.range<-c(quantile(as.vector(cor.hightoplot),probs=c(0,.5,.7,.85)),1)
     
     missing.summary<-missing.summary[match(names(High.cor),names(missing.summary))[1:min(num.plots,length(High.cor))]]
     
  #Find a new unique file name (one in the desired directory that hasn't yet been used)

 options(warn=-1)
 num.plots<-min(ncol(HighToPlot),num.plots)
 if(num.plots<8) {wdth=1500
                 cex.mult=3}
 else if(num.plots<15) {wdth=3000
                        if(num.plots<12) cex.mult=4
                        else cex.mult=3
                          }
      else {wdth=4500
      if(num.plots<17) cex.mult=4
                        else cex.mult=3
            }
           
 if(Debug==FALSE) jpeg(output.file,width=wdth,height=wdth,pointsize=13)
    MyPairs(cbind(TrueResponse,HighToPlot),cor.range=cor.range,missing.summary=missing.summary,my.labels=(as.vector(High.cor)[1:num.plots]),
    lower.panel=panel.smooth,diag.panel=panel.hist, upper.panel=panel.cor,pch=21,
    bg = c("blue","red","yellow")[factor(response,levels=c(0,1,-9999))],col.smooth = "red",cex.mult=cex.mult,oma=c(0,2,6,0),famly=famly,for.dev=for.dev)

 if(Debug==FALSE) graphics.off()
 options(warn=0)
 
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
source(paste(ScriptPath,"read.dat.r",sep="\\"))
source(paste(ScriptPath,"chk.libs.r",sep="\\"))
source(paste(ScriptPath,"PairsExploreHelperFcts.r",sep="\\"))
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

