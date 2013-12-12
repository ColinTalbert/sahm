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

SetWeights<-function(input.file,output.file,response.col="ResponseBinary",method="Density",sigma.sd=NULL){

#Description:
#This function sets weights as a potential remedial measure when autocorrelation is found in the residuals of
#the model fit based on the number of points in an area using a leave one out algorithm wiht a gaussian isotrophic kernel and optional argument
#sigma (standard deviation of the kernel) or weights can be set so that the total weight of absence points
#is equal to the weight of presence.  The Density options should never be used on presence only data with randomly selected
#background points.  The problem with this functions is that there is no way that I can think of to optimize weights based on the Density
#I don't know how much near by points should be downweighted or how close constitutes near as this would seem to depend on the species being
#modeled and it's environment.  If the density method is selected, a map of the spatial weights is produced

#Written by Marian Talbert 12/7/2011

         #strip the directory out of the output file and replace with the weights.map (name of the map produced)
       # DAK: this looks problematic for cross-platform operation
       last.dir<-strsplit(output.file,split="\\\\")
       plot.name<-file.path(sub(paste("\\\\",last.dir[[1]][length(last.dir[[1]])],sep=""),"",output.file),"weights.map.jpg")
   #Read input data and remove any columns to be excluded
          dat.in<-read.csv(input.file,header=FALSE,as.is=TRUE)
          dat<-as.data.frame(dat.in[4:dim(dat.in)[1],])
           names(dat)<-dat.in[1,]

        response<-dat[,match(tolower(response.col),tolower(names(dat)))]
        
        bg.dat<-dat[response==-9999,]

          if(dim(bg.dat)[1]!=0){
            dat<-dat[-c(which(response==-9999,arr.ind=TRUE)),]
            dat.in<-dat.in[-c(which(response==-9999,arr.ind=TRUE)+3),]
            response<-response[-c(which(response==-9999,arr.ind=TRUE))]
            bg.dat$Weights=""
            }

        if(method=="Density") {
            if(is.na(match("spatstat",installed.packages()[,1]))) {
             install.packages("spatstat",repos="http://lib.stat.cmu.edu/R/CRAN")
            }

            library(spatstat)
               win<-ripras(x=as.numeric(dat$X),y=as.numeric(dat$Y))
               study.area<-ppp(x=as.numeric(dat$X),y=as.numeric(dat$Y),window=win)

              # using just density causes problems for points in low density areas
              # I've taken the completely arbitrary step of setting weights equal to 1/sqrt(den+1)
              # adjusting for the density of the normal distribution (1/sqrt(2*pi*sigma^2))
              # by sqrt(2*pi*sigma.sd^2) makes sence but doesn't downweight cluster enough so I've used
              # 2*pi*sigma.sd^2
              den<-density.ppp(study.area,at="points",leaveoneout=TRUE,sigma=sigma.sd)*2*pi*sigma.sd^2
                    im.dens<-density.ppp(study.area,leaveoneout=TRUE,sigma=sigma.sd)
                    im.dens<-eval.im(im.dens*2*pi*sigma.sd^2)
                    im.dens<-eval.im(1/sqrt(im.dens+1))

               jpeg(file=plot.name,width=1500,height=1500,pointsize=20)
                     colfun <- spatstat.options("image.colfun")
                    color.list<-list(col = colfun(255))
                    plot(im.dens,main=paste("Spatial weights with sigma = ",ifelse(is.null(sigma.sd),round(attr(den,"sigma")),sigma.sd)))

               dat$Weights<-1/sqrt(den+1)
                  #  points(study.area,cex=1.5,col="gray17",pch=19)
                    s1<-seq(from=min(im.dens),to=max(im.dens),length=255)
                    points(study.area$x,study.area$y,bg=color.list[[1]][apply(outer(dat$Weights,s1,">"),1,sum)],pch=21,cex=1.2)

               dev.off()

                     }

            if(method=="PresAbs") {
            response<-as.numeric(response)
                    PresAbsTab<-table(response)
                    if(length(PresAbsTab)!=2) stop("PresAbs option is only available for binary data")
                    if(PresAbsTab[1]<PresAbsTab[2]) stop("PresAbs option is only available for oversampled absences")
                     #sum of presence weights equals sum of absence weights and total sum of weights is the same
                     #as if weights were equal to 1

                     weight<-rep(1,times=length(response))
                     weight[response==0]<-PresAbsTab[2]/PresAbsTab[1]
                      dat$Weights<-weight
            }


         #inserting data must be done in 3 steps because dat.in isn't a proper dataframe in that
         #not all elements in a column are of the same type

           dat.in[,(dim(dat.in)[2]+1)]<-NA
          dat.in[4:(dim(dat.in)[1]),(dim(dat.in)[2])]<-dat$Weights
          dat.in[c(1,3),(dim(dat.in)[2])]<-c("Weights","")
          dat.in[2,(dim(dat.in)[2])]<-1

              if(dim(bg.dat)[1]!=0) {
                names(bg.dat)<-names(dat.in)
                dat.in<-rbind(dat.in,bg.dat)}

              #write output files for R modules
             write.table(dat.in,file=output.file,row.names=FALSE,col.names=FALSE,sep=",",quote=FALSE)


    }

  #assign default values
  responseCol="ResponseBinary"
  method="Density"
  sigma.sd=NULL

 #Reading in command line arguments
 Args <- commandArgs(T)
    print(Args)

    #replace the defaults with passed values
    for (arg in Args) {
    	argSplit <- strsplit(arg, "=")
    	argSplit[[1]][1]
    	argSplit[[1]][2]
    	if(argSplit[[1]][1]=="met") method <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="o") output.file <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="i") infil <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="rc") responseCol <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="sig") sigma.sd <- argSplit[[1]][2]
    }
    if(!is.null(sigma.sd)) sigma.sd<-as.numeric(sigma.sd)

  #Run SetWeights with these parameters
	SetWeights(input.file=infil,output.file,response.col=responseCol,method=method,sigma.sd=sigma.sd)

