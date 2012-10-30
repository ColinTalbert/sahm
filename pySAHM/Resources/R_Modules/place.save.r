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

place.save<-function(out,Final.Model){
                        parent<-dirname(out$input$output.dir)
                         compile.out<-file.path(parent,
                                    paste(ifelse(missing(Final.Model),"AcrossModel","FinalEvaluation"),
                                       switch(out$dat$split.type,"crossValidation"="CrossVal","test"="TestTrain","none"="NoSplit"),
                                       switch(out$input$model.family,"binomial"="Binom","bernoulli"="Binom","poisson"="Count"),if(out$input$PsdoAbs)"PsdoAbs",
                                       ".csv",
                                      sep="")
                                )
                              
 Header<-cbind(c("","Original Field Data","Field Data Template","PARC Output Folder","PARC Template","Covariate Selection Name",""),
                            c(basename(out$input$output.dir),
                            out$dat$input$OrigFieldData,out$dat$input$FieldDataTemp,out$dat$input$ParcOutputFolder,
                            out$dat$input$ParcTemplate,ifelse(length(out$dat$input$CovSelectName)==0,"NONE",out$dat$input$CovSelectName),""))

if(file.access(compile.out,mode=0)==-1){ #if very first time through little
          write.table(Header,file =compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",")

  } else { #this else (not the first time through) read current csv first
          input<-read.table(compile.out,fill=TRUE,sep=",")
          output<-cbind(input,c(Header[,2],rep("",times=(nrow(input)-nrow(Header)))))
              temp=try(write.table(output,file =compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=","),silent=TRUE)
           while(class(temp)=="try-error"){
                      modalDialog("","Please Close the AppendedOutput.exe\ so that R can write to it then press ok to continue ","")
                      temp<-try(write.table(output,file =compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=","),silent=TRUE)
                      }
          }

 out$input$Append.Dir<-compile.out
 return(out)
 }