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

AppendOut<-function(compile.out,Header,x,out,Parm.Len,parent,split.type){
    Header.Length<-nrow(Header)

################ Writing to the csv  ############################

 input<-read.table(compile.out,fill=TRUE,sep=",")
  if(ncol(input)<=2){ #if very first time through little

          write.table(Header,file =compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",")
          write.table(x,file=compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE,sep=",")
          output<-matrix(0,0,0)
  } else { #this else (not the first time through) read current csv first
          if(nrow(input)+nrow(x)==length(c(Header[,2],as.character(x[,2])))){
            #if the first model run threw an error no subsequent output will be written without some special help
             input<-rbind(input,matrix(c(as.character(x[,1]),rep("",times=length(x[,1])*(ncol(input)-1))),nrow=length(x[,1])))
                }
          output<-cbind(input[,(1:(ncol(input)-1))],c(Header[,2],as.character(x[,2])))
              temp=try(write.table(output,file =compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=","),silent=TRUE)
           while(class(temp)=="try-error"){
                      modalDialog("","Please Close the AppendedOutput.exe\ so that R can write to it then press ok to continue ","")
                      temp<-try(write.table(output,file =compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=","),silent=TRUE)
                      }
          }

  ###################### Making the jpg image ###################
  if(ncol(output)>2){

                 jpeg(file=gsub("csv","jpg",compile.out),width=(1000+30*ncol(output)),height=1000,pointsize=13,quality=100)
                  par(mfrow=c(Parm.Len,1),mar=c(.2, 5, .6, 2),cex=1.1,oma=c(5, 0, 3, 0))
               #Getting rid of the header
                            row.nms<-as.character(output[(nrow(Header)+3):((nrow(Header)+2)+Parm.Len),1])
                      Hdr<-unlist(strsplit(readLines(compile.out,1),split=","))
                      output<-output[(nrow(Header)+1):nrow(output),2:ncol(output)]
               #Setting up train as numeric
                      train<-output[3:(Parm.Len+2),]
                      train<-matrix(data=as.numeric(as.character(as.matrix(train))),nrow=nrow(train),ncol=ncol(train))
                       row.names(train)<-row.nms
                       train[grep("Percent",rownames(train),ignore.case=TRUE),]<-train[grep("Percent",rownames(train),ignore.case=TRUE),]/100
                #Setting up test as numeric
                 if(split.type!="none"){
                      test<-output[(Parm.Len+3):nrow(output),]
                      test<-test[-c(seq(from=1,to=nrow(test),by=Parm.Len+2),seq(from=2,to=nrow(test),by=Parm.Len+2)),]
                      test<-as.data.frame(matrix(data=as.numeric(as.character(as.matrix(test))),nrow=nrow(test),ncol=ncol(test)))
                          test[is.na(test)]<-0 #Switching NAs to 0 so that the plot will come up
                      test$split.inx<-rep(seq(from=1,to=nrow(test)/Parm.Len),each=Parm.Len)
                      test.lst<-split(test[,-c(ncol(test))],f=test$split.inx)
                         for(i in 1:length(test.lst)) {row.names(test.lst[[i]])<-row.nms
                          test.lst[[i]][grep("Percent",rownames(test.lst[[i]]),ignore.case=TRUE),]<-test.lst[[i]][grep("Percent",rownames(test.lst[[i]]),ignore.case=TRUE),]/100
                                }
                          } else test.lst<-list()
                     ss<-seq(from=1,to=ncol(train),by=1)
                     x.labs<-sub(" ","\n",rownames(train))
                     x.labs<-sub("Percent","Proportion",x.labs)
                    colors.test=c("chocolate3","gold1","darkolivegreen2","steelblue1","brown3")
                  if(split.type!="crossValidation") colors.train=c("chocolate4","gold3","darkolivegreen4","steelblue4","brown4")
                   else{colors.train<-c("darkred","darkorange2","darkgreen","darkslateblue","red4")
                  #setting colors for the boxplot by adding some transparency to the test colors and darkening train colors
                  color.box<-col2rgb(colors.test,alpha=TRUE)
                       color.box[4,]<-150
                       temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
                       colors.test<-apply(color.box/255,2,temp.fct)
                        }
        #producing plots
                   for(i in 1:Parm.Len){
                            plot(c(.6,(ncol(train)+2)),c(0,max(1.25,max(train[i,],na.rm=TRUE)+.2)),type="n",xaxt="n",yaxp=c(0,max(1,max(train[i,],na.rm=TRUE)),4),
                                xlab=paste("Corresponding Column in the corresponding .csv",sep=""),
                                ylab=x.labs[i])
                                   #have to label the maximum for everything except prediction error for which we label the min
                                   if(x.labs[i]!="Prediction\nError") temp.fct<-function(a){max(a,na.rm=TRUE)}
                                    else temp.fct<-function(a){min(a,na.rm=TRUE)}
                                grid(nx=10)
                                if(split.type=="test") legend(ncol(test),y=.75,legend=c("Test","Train"),fill=c(colors.test[i],colors.train[i]))
                                if(split.type=="crossValidation") legend(ncol(test),y=.75,legend=c("CV","Train"),pch=c(22,8),pt.cex=c(2,1.5),pt.lwd=c(1,2),pt.bg=c(colors.test[i],colors.train[i]),col=c("black",colors.train[i]))
                              if(split.type!="crossValidation"){
                               rect(xleft=ss-.4,ybottom=0,xright=ss,ytop=train[i,],col=colors.train[i],lwd=2)
                               }
                             options(warn=-1)
                             if(length(test.lst)==1) rect(xleft=ss,ybottom=0,xright=(ss+.4),ytop=as.vector(pmax(0,test.lst[[1]][i,])),col=c(colors.test[i],"white")[(test.lst[[1]][i,]==0)+1],
                                                     border=c("black","white")[(test.lst[[1]][i,]==0)+1],lwd=2)
                              if(length(test.lst)>1){ mean.cv<-vector()
                                                      for(k in ss){
                                                      a<-vector()
                                                        for(j in 1:length(test.lst)) a<-c(a,as.numeric(test.lst[[j]][i,k]))
                                                        if(sum(a!=0)!=0) boxplot(a,add=TRUE,at=k,width=2,col=colors.test[i],yaxt="n")
                                                        mean.cv<-c(mean.cv,median(a))
                                                      }
                              }
                               if(split.type=="crossValidation") {
                               points(ss,train[i,],bg=colors.train[i],col=colors.train[i],cex=1.5,pch=8,lwd=3)
                               text((which(mean.cv==temp.fct(mean.cv),arr.ind=TRUE)),
                                 max(1,max(train[i,],na.rm=TRUE))+.15,
                                 labels=as.character(paste(round(temp.fct(mean.cv),digits=2),ifelse(split.type=="crossValidation"," test",""),sep="")),cex=.8)
                               }
                             options(warn=0)


                                 Offset<-ifelse(split.type=="crossValidation",0,.25)
                                 
                            text((which(train[i,]==temp.fct(train[i,]),arr.ind=TRUE)-Offset),
                                 ifelse(split.type=="crossValidation",max(1,max(train[i,],na.rm=TRUE))+.25,temp.fct(train[i,])+.07),
                                 labels=as.character(paste(round(temp.fct(train[i,]),digits=2),ifelse(split.type=="crossValidation"," train",""),sep="")),cex=.8)
                            if(length(test.lst)==1) text((which(test.lst[[1]][i,]==temp.fct(test.lst[[1]][i,]),arr.ind=TRUE)[2]+Offset),
                                  temp.fct(test.lst[[1]][i,])+.07,labels=as.character(round(temp.fct(test.lst[[1]][i,]),digits=2)),cex=.8)

                            if (i==1) par(mar=c(.2, 5, .6, 2))
                            if(i!=1 & i!=(Parm.Len-1)) par(mar=c(.3, 5, .4, 2))
                            if(i==(Parm.Len-1)) par(mar=c(2, 5, .4, 2))
                        }
                        #Outer margin labels
                             Line<-ifelse(Parm.Len==5,-13,-19)
                            for(i in 1:length(Hdr)) mtext(Hdr[i],line=Line,at=(i-1),las=2)
                          mtext("Evaluation Metrics Performance Across Model Runs",outer=TRUE,side=3,cex=1.3)
                          mtext(paste("sub-folder name where model is found in the folder ",parent
                            ,sep=""),side=1,outer=TRUE,line=4)
                       dev.off()
                    }
               }