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

resid.image<-function(dev.contrib,pred,raw.dat,x,y,wgt,model.type,file.name,out,label){
#produces a map of deviance residuals unless we're using independent evaluation data in which case
#it produces a map of predicted-observed data with a lowess smooth so the relationship is easier to see
#if there are more than 2000 points a random sample is drawn to speed up the caluclations
#if weights are available these are used for the lowess surface
#eventually this should include an option to produce moran's i corellogram but this must be an option
#as it can be computaitonally intensive.   Written by Marian Talbert 2011.
  
       if(length(pred)>2000){
           samp<-seq(1:length(pred))[order(runif(length(pred)))][1:2000]
           dev.contrib<-dev.contrib[samp]
           pred<-pred[samp]
           raw.dat<-raw.dat[samp]
           x<-x[samp]
           y<-y[samp]
           wgt<-wgt[samp]
           }
          #for some reason dev.contrib is negative for binomial and bernoulli and positive for poisson
   if(label!="eval") z<-sign(raw.dat-pred)*abs(dev.contrib)
      else z<-raw.dat-pred
     MinCol<-min(z)
     MaxCol<-max(z)
         col.i<- beachcolours(heightrange=c(min(z),max(z)),sealevel=0,s=1,ncolours=(length(table(z))+1))
         f<-function(a,b) sqrt((a-b)^2)
           s1<-seq(from=MinCol,to=MaxCol,length=length(table(z)))
         col.ind<-apply((outer(s1,z,f)),2,which.min)

              #####################################################
              a<-loess(z~x*y,weights=wgt)
               x.lim<-rep(seq(from=min(x),to=max(x),length=100),each=100)
               y.lim<-rep(seq(from=min(y),to=max(y),length=100),times=100)
              z<-predict(a,newdata=cbind("x"=x.lim,"y"=y.lim))
              x.lim<-seq(from=min(x),to=max(x),length=100)
              y.lim<-seq(from=min(y),to=max(y),length=100)
                 z<-matrix(data=z,ncol=100,nrow=100,byrow=TRUE)
             
                  
                 ########### Plot residual smooth with signed and sized residuals on top
              png(file=paste(out$dat$bname,"resid.plot.png",sep="."),width=1000,height=1000,pointsize=13)
                 par(oma=c(3,3,3,3))
                 layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
                  image(z,x=x.lim,y=y.lim,col=beachcolours(heightrange=c(min(z),max(z)),sealevel=0,s=.5,ncolours=length(table(z))),
                  main=paste("Spatial pattern of", ifelse(label!="eval"," deviance residuals\n(magnitude and sign)"," prediction error")),
                      xlab="X coordinate",ylab="Y coordinate",,cex.main=2.2,cex.axis=1.6,cex.lab=1.8)

                  points(x,y,bg=col.i[col.ind], pch=21,cex=abs(dev.contrib)*2.5)
                  par(mar = c(3,2.5,2.5,2))

             colrange<-seq(from=MinCol,to=MaxCol,length=100)
               image(1,colrange,
               matrix(data=colrange, ncol=length(colrange),nrow=1),
              col=beachcolours(heightrange=c(MinCol,MaxCol),sealevel=0,ncolours=length(colrange)),
              xlab="",ylab="",
              xaxt="n",cex.main=2,cex.axis=2,cex.lab=2)
              graphics.off()
              return(a)
              }
