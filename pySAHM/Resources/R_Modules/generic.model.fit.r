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

generic.model.fit<-function(out,Model,t0){

#This code recognizes the specified model signature as well as several tags that change the analysis such as
#differnt responses (pres/abs, used/available, count) and user specified options and fits the appropriate model 
#returning results in a 
#format common to all models so further output functions can work the same for all models.
#Written by Marian Talbert 11/2011

attach(out$input)
attach(out$dat$ma$train)
on.exit(detach(out$dat$ma$train))
on.exit(detach(out$input))
   
     out<-model.fit(dat,out,Model,weight=weight,full.fit=TRUE)
       write.txt(out,t0)         
  if(Model=="glm") {
     
      #post processing to get a common output for all model fits
            txt0<-paste("\n\n","Settings:\n","\n\t model family          : ",model.family,
                                             "\n\t simplification method : ",simp.method,
            "\n\n\n","Results:\n\t ","number covariates in final model   : ",length(attr(terms(formula(out$mods$final.mod[[1]])),"term.labels")),"\n",sep="")
            print(out$mods$final.mod[[1]]$summary <- summary(out$mods$final.mod[[1]]))
             
            capture.output(cat(txt0),out$mods$final.mod[[1]]$summary,file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)
            cat("\n","Finished with stepwise GLM","\n")
            cat("Summary of Model:","\n")

                if(length(coef(out$mods$final.mod[[1]]))==1) stop("Null model was selected.  \nEvaluation metrics and plots will not be produced")

              #storing number of variables in final model
              out$mods$n.vars.final<-length(attr(terms(formula(out$mods$final.mod[[1]])),"term.labels"))
              out$mods$vnames<-attr(terms(formula(out$mods$final.mod[[1]])),"term.labels")
              #have to remove all the junk with powsers and interactions for mess map production to work
              out$mods$vnames<-unique(unlist(strsplit(gsub("I\\(","",gsub("\\^2)","",out$mods$vnames)),":")))
               }
               
 if(Model=="mars"){        
           
       #post processing steps to aggregate and summarize ouput 
            if(out$input$PsdoAbs){ 
                 varIp <- do.call("rbind",fit_contribs)
                 TimesIncl <- table(rownames(varIp))
                 varIp <- aggregate(varIp,list(rownames(varIp)),FUN=mean)
                 varIp$TimesIncl <- TimesIncl
                 varIp <- varIp[order(varIp$nsubsets,decreasing =TRUE),c(1,4,5,7,9)] #ordering by most important and removing columns I don't want
                 varIp <- format(varIp,digits=3) #formatting
                 rownames(varIp)<-varIp[,1]; varIp<-varIp[,-1]
                 if(length(grep("-unused",rownames(varIp)))!=0) varIp<-varIp[-c(grep("-unused",rownames(varIp))),]
                 x<-varIp
                 
          } else{
                x<-varIp<-fit_contribs
                x <- x[x[,2]!=0,]
                x <- x[order(x[,4]),]
                row.names(x) <- x[,1]
                x$df <- -1*x$df
                x <- x[,-1]
          }
          #if there are factors the new mars gives them names based on pred.name and level so we have go remove these names and
                 #record this as just one predictor I'm not really sure how this will work if there are multiple factors or multiple levels of a factor
                 if(any(badName<-is.na(match(rownames(varIp),names(out$dat$ma$train$dat))))){
                     indx<-which(badName==TRUE,arr.ind=TRUE)
                     origNames<-vector()
                     for(i in 1:length(indx)){
                            origNames<-which(!is.na(pmatch(names(out$dat$ma$train$dat),rownames(varIp)[indx[i]])),arr.ind=TRUE)
                     }
                     origNames<-names(out$dat$ma$train$dat)[unique(origNames)]
                     varIp<-varIp[-c(indx),]#I have to remove these because with multiple factor levels they will have multiple rows
                     NewNames<-c(rownames(varIp),origNames)
                     varIp<-rbind(as.matrix(varIp),matrix(data=NA,nrow=length(origNames),ncol=4))
                     varIp<- apply(varIp,2,as.numeric)  
                     rownames(varIp)<-NewNames
                         
                 }
                 
      #caputuring output
          cat("Summary of Model:","\n")
          print(out$mods$summary <- x)
           
           out$mods$contributions$var<-names(dat)[-1]
           out$mods$n.vars.final<-nrow(varIp)
           out$mods$vnames<-rownames(varIp)

          txt0 <- paste("\n\n","Settings:\n",
                      "\n\trandom seed used             : ",out$input$seed,
                      "\n\tmars degree                  : ",out$input$mars.degree,
                      "\n\tmars penalty                 : ",out$input$mars.penalty,"\n\n",sep="")
          cat("\n","Storing output...","\n","\n")
          capture.output(cat(txt0),file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)
          capture.output(cat("\n\nSummary of Model:\n"),file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)  
          capture.output(print(out$mods$summary),file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)

      }
              
 if(Model=="brt"){
               
           txt0 <- paste("\n\n","Settings:\n",
                      if(out$input$PsdoAbs) "(Averaged across available splits)\n", 
                      "\n\trandom seed used             : ",out$input$seed,
                      "\n\ttree complexity              : ",out$mods$parms$tc.full,
                      "\n\tlearning rate                : ",round(out$mods$lr.mod$lr,4),
                      "\n\tn(trees)                     : ",mean(unlist(lapply(out$mods$final.mod,function(lst){lst$n.trees}))),
                      "\n\tmodel simplification         : ",simp.method,
                      "\n\tn folds                      : ",n.folds,
                      "\n\tn covariates in final model  : ",out$mods$n.vars.final,
             sep="")
          txt1 <- "\nRelative influence of predictors in final model:\n\n"
          txt2 <- if(!out$input$PsdoAbs) "\nImportant interactions in final model:\n\n"
                     else "\nImportant interactions in at least one split of available points:\n\n"
                    
          capture.output(cat(txt0),cat(txt1),print(out$mods$summary,row.names=FALSE),cat(txt2),print(out$mods$interactions,row.names=F),file=paste(out$dat$bname,"_output.txt",sep=""),
             append=TRUE)
          cat(txt0);cat(txt1);print(out$mods$summary);cat(txt2);print(out$mods$interactions,row.names=F)
      }
 
   if(Model=="rf"){
              
              txt0 <- paste("\n\n","Settings:",
              "\n\trandom seed used                       : ",out$input$seed,
              "\n\tn covariates considered at each split  : ", mean(unlist(lapply(out$mods$final.mod,"[",14))),
                if(out$input$PsdoAbs==TRUE) "\n\t   (averaged over each used available split)\n",
              "\n\tn trees                                : ",n.trees,
                 if(out$input$PsdoAbs==TRUE) "\n\t   (for each used available split)\n",
              sep="")
          txt1 <- "\n\nRelative performance of predictors in final model:\n\n"
          
          capture.output(cat(txt0),cat(txt1),print(round(out$mods$summary,4)),file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE) 
        
         #storing number of variables in final model
        out$mods$n.vars.final<-length(out$dat$used.covs) #random forest doesn't drop variables
        out$mods$vnames<-out$dat$used.covs
   }
   
     if(Model=="maxent"){
      #maybe parse the parameters file to write out settings at some point
     
         #storing number of variables in final model
        out$mods$n.vars.final<- out$mods$n.vars.final<-ncol(out$dat$ma$train$dat)-1
        out$mods$vnames<-names(out$dat$ma$train$dat)[-1]
   }
  
  return(out)
}