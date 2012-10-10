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
 
  if(Model=="glm") {
  penalty <- if(simp.method=="AIC") 2 else 
             log(nrow(out$dat$ma$ma))
             
          if(!squared.terms){   
              scope.glm <- list(lower=as.formula(paste("response","~1")),
              upper=as.formula(paste("response","~",paste(out$dat$used.covs,collapse='+'))))
          }else{
              factor.mask<-na.omit(match(names(out$dat$factor.levels),out$dat$used.covs))
              cont.mask<-seq(1:length(out$dat$used.covs))
              if(length(factor.mask)!=0) cont.mask<-cont.mask[-c(factor.mask)]

               scope.glm <- list(lower=as.formula(paste("response","~1")),
                 upper=as.formula(paste("response","~",paste(c(if(length(factor.mask)>0) paste(out$dat$used.covs[factor.mask],collapse=" + "),
                 paste("(",paste(out$dat$used.covs[cont.mask],collapse=" + "),")^2",sep=""),
                 paste("I(",out$dat$used.covs[cont.mask],"^2)",sep="")),collapse=" + "),sep="")))
           }
          mymodel.glm.step <- step(glm(as.formula(paste("response","~1")),family=model.family,data=dat,weights=weight,na.action="na.exclude"),
          direction='both',scope=scope.glm,k=penalty,trace=1)
         
          out$mods$final.mod<-mymodel.glm.step
                  txt0<-paste("\n\n","Settings:\n","\n\t model family=",model.family,
                  "\n\n","Results:\n\t ","number covariates in final model=",length(attr(terms(formula(out$mods$final.mod)),"term.labels")),sep="")
                  print(out$mods$final.mod$summary <- summary(mymodel.glm.step))
            write.txt(out,t0)  
          capture.output(txt0,out$mods$final.mod$summary,file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)
            cat("\n","Finished with stepwise GLM","\n")
            cat("Summary of Model:","\n")

                if(length(coef(out$mods$final.mod))==1) stop("Null model was selected.  \nEvaluation metrics and plots will not be produced")

              #storing number of variables in final model
              out$mods$n.vars.final<-length(attr(terms(formula(out$mods$final.mod)),"term.labels"))
              out$mods$vnames<-attr(terms(formula(out$mods$final.mod)),"term.labels")
              #have to remove all the junk with powsers and interactions for mess map production to work
              out$mods$vnames<-unique(unlist(strsplit(gsub("I\\(","",gsub("\\^2)","",out$mods$vnames)),":")))
               }
               
 if(Model=="mars"){
          SplitBackground(out)
          fit_contribs<-list()
         
          for(i in 1:num.splits){
              out$mods$final.mod[[i]]<-mars.glm(data=dat[c(Split,rep(i,times=sum(resp>0)))==i,], mars.x=c(2:ncol(dat)), mars.y=1, mars.degree=mars.degree, family=model.family,
                                       penalty=mars.penalty)
                out$mods$final.mod[[i]]$fit.dat<-dat[c(Split,rep(i,times=sum(resp>0)))==i,]                        
               fit_contribs[[i]] <- mars.contribs(out$mods$final.mod[[i]])                           
           }
            
            if(out$input$PsdoAbs){ 
                 fc<-function(x){x[2][[1]]}
                 dev.data<-do.call("rbind",lapply(fit_contribs,fc))
                  dev.data<-dev.data[dev.data$df!=0,]
                 delta_dev<-aggregate(dev.data[,2],list(Var=dev.data$variable),FUN=mean)
                 mod.df<-aggregate(dev.data[,3],list(Var=dev.data$variable),FUN=mean)
                 mod.pval<-aggregate(dev.data[,4],list(Var=dev.data$variable),FUN=mean)
                 var.count<-table(dev.data[,1])
                 fit_contribs$deviance.table<-data.frame(cbind("mean delta dev"=round(delta_dev[,2],digits=4),
                            "mean df"=round(mod.df[,2],digits=2),"mean p-value"=round(mod.pval[,2],digits=6),"times in model"=var.count[match(delta_dev$Var,names(var.count))]))
                 x<-fit_contribs$deviance.table
          } else{
                x <- x[x[,2]!=0,]
                x <- x[order(x[,4]),]
                row.names(x) <- x[,1]
                x$df <- -1*x$df
                x <- x[,-1]
          }
          cat("Summary of Model:","\n")
          print(out$mods$summary <- x)

           out$mods$contributions$var<-names(dat)[-1]
           out$mods$n.vars.final<-nrow(out$mods$summary)
           out$mods$vnames<-rownames(out$mods$summary)

          
          cat("\n","Storing output...","\n","\n")
          write.txt(out,t0)
          capture.output(cat("\n\nSummary of Model:\n"),file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)
          capture.output(print(out$mods$summary),file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)

      }
              
 if(Model=="brt"){
          SplitBackground(out)

          brt.full<-list()
          lr.list<-list()
          mod.simp<-list() 
          
          if(model.family=="binomial")  out$input$model.family<-model.family<-"bernoulli"
            if(!is.null(tc)) out$mods$parms$tc.full<-out$mods$parms$tc.sub<-tc
           
           #going to try to estimate learning rate and predictors to use in final model not just on the subset but by calculating for 
           #several of the splits (if the used was split)
           lr.samp<-sample(1:num.splits,size=min(num.splits,5),replace=FALSE)
           for(i in 1:length(lr.samp)){     
               if(length(lr.samp)>1) {out$dat$Subset$dat<-dat[c(Split,rep(lr.samp[i],times=sum(resp>0)))==lr.samp[i],]
                                      out$dat$Subset$weight<-weight[c(Split,rep(lr.samp[i],times=sum(resp>0)))==lr.samp[i]]
                                      out$dat$Subset$ratio=.5
                                      }
                lr.list[[i]]<-est.lr(out)
            }
            #now reassembling everything from lr estimation before continuing
                 out$mods$lr.mod$good.cols<-unique(unlist(lapply(lr.list,function(lst){lst$lr.mod$good.cols})))
                 out$mods$parms$tc.sub<-round(mean(unlist(lapply(lr.list,function(lst){lst$parms$tc.sub}))))
                 out$mods$parms$tc.full<-round(mean(unlist(lapply(lr.list,function(lst){lst$parms$tc.full}))))
                 out$mods$lr.mod$lr0<-mean(unlist(lapply(lr.list,function(lst){lst$lr.mod$lr0})))
                 out$mods$lr.mod$lr<-mean(unlist(lapply(lr.list,function(lst){lst$lr.mod$lr})))
                 
                cat("\nfinished with learning rate estimation, lr=",out$mods$lr.mod$lr0)
                cat("\nfor final fit, lr=",out$mods$lr.mod$lr,"and tc=",out$mods$parms$tc.full,"\n")
    
                if(simp.method=="cross-validation"){
                    for(i in 1:length(lr.samp)){     
                       if(length(lr.samp)>1) {out$dat$Subset$dat<-dat[c(Split,rep(lr.samp[i],times=sum(resp>0)))==lr.samp[i],]
                                          out$dat$Subset$weight<-weight[c(Split,rep(lr.samp[i],times=sum(resp>0)))==lr.samp[i]]
                                          out$dat$Subset$ratio=.5
                                          }
                        # remove variables with <1% relative influence and re-fit model
                            if(length(out$mods$lr.mod$good.cols)<=1) stop("BRT must have at least two independent variables")
                            max.trees<-NULL
                        m0 <- gbm.step.fast(dat=out$dat$Subset$dat,gbm.x=out$mods$lr.mod$good.cols,gbm.y=1,family=model.family,
                              n.trees = c(300,600,800,1000,1200,1500,1800),step.size=step.size,max.trees=max.trees,
                              tolerance.method=tolerance.method,tolerance=tolerance, n.folds=n.folds,prev.stratify=prev.stratify,
                              tree.complexity=out$mods$parms$tc.sub,learning.rate=out$mods$lr.mod$lr0,bag.fraction=bag.fraction,site.weights=out$dat$Subset$weight,
                              autostop=T,debug.mode=F,silent=!debug.mode,
                              plot.main=F,superfast=F)
                        mod.simp[[i]] <- gbm.simplify(m0,n.folds=n.folds,plot=F,verbose=F,alpha=alpha) # this step is very slow #
                     }
                              out$mods$simp.mod$good.cols <- unique(unlist(lapply(mod.simp,function(lst){lst$pred.list[[length(lst$pred.list)]]}))) 
                              out$mods$simp.mod$good.vars <- names(dat)[out$mods$simp.mod$good.cols]
                             {cat("\n");cat("50%\n")}
                }
            # fit final model #
            final.mod<-list()
            for(i in 1:num.splits){
                 if(out$mods$lr.mod$lr==0) out$mods$lr.mod$lr<-out$mods$lr.mod$lr0
                 final.mod[[i]] <- gbm.step.fast(dat=dat[c(Split,rep(i,times=sum(resp>0)))==i,],gbm.x=out$mods$simp.mod$good.cols,gbm.y = 1,family=model.family,
                                n.trees = c(300,600,700,800,900,1000,1200,1500,1800,2200,2600,3000,3500,4000,4500,5000),n.folds=n.folds,max.trees,
                                tree.complexity=out$mods$parms$tc.full,learning.rate=out$mods$lr.mod$lr,bag.fraction=bag.fraction,site.weights=weight[c(Split,rep(i,times=sum(resp>0)))==i],
                                autostop=T,debug.mode=F,silent=!debug.mode,plot.main=F,superfast=F)
                  y <- gbm.interactions(final.mod[[i]])
                  int <- y$rank.list;
                  int<-int[int$p<.05,]
                  int <- int[order(int$p),]
                  int$p <- round(int$p,4)
                  names(int) <- c("v1","name1","v2","name2","int.size","p-value")
                  row.names(int)<-NULL
                  if(nrow(int)>0) out$mods$interactions[[i]] <- int else out$mods$interactions <- NULL
          }
        
          out$mods$final.mod<-final.mod
          var.name<-unlist(lapply(final.mod,function(lst){as.character(lst$contributions[,1])}))
          var.contrib<-unlist(lapply(final.mod,function(lst){lst$contributions[,2]}))
          var.final<-unique(var.name)
          
          #can't take mean here because we need to account for when the variable didn't show up in the model
          out$mods$summary<-aggregate(var.contrib,list(Var=var.name),FUN=sum)
          out$mods$summary[,2]<-out$mods$summary[,2]/num.splits
          
          out$mods$n.vars.final<-length(var.final)
         
          interaction.lst<-out$mods$interactions[!unlist(lapply(out$mods$interactions,is.null))]
          #taking out the names of predictors from interactions and then ordering them so we can aggregate
           interaction.list<-apply(cbind(do.call("rbind",lapply(interaction.lst,"[",2)),do.call("rbind",lapply(interaction.lst,"[",4))),1,sort)
           out$mods$interactions<-interaction.lst[!duplicated(interaction.list,MARGIN=2)]
            
          write.txt(out,t0)
           txt0 <- paste("\n\n","Settings:\n",
                      if(out$input$PsdoAbs) "(Averaged across available splits)\n", 
                      "\n\trandom seed used =            ",seed,
                      "\n\ttree complexity =             ",out$mods$parms$tc.full,
                      "\n\tlearning rate =               ",round(out$mods$lr.mod$lr,4),
                      "\n\tn(trees) =                    ",mean(unlist(lapply(out$mods$final.mod,"[",39))),
                      "\n\tmodel simplification =        ",simp.method,
                      "\n\tn folds =                     ",n.folds,
                      "\n\tn covariates in final model = ",length(var.final),
             sep="")
          txt1 <- "\nRelative influence of predictors in final model:\n\n"
          txt2 <- if(num.splits==1) "\nImportant interactions in final model:\n\n"
                     else "\nImportant interactions in at least one split of available points:\n\n"

          capture.output(cat(txt0),cat(txt1),print(out$mods$summary),cat(txt2),print(out$mods$interactions,row.names=F),file=paste(out$dat$bname,"_output.txt",sep=""),
             append=TRUE)
          cat(txt0);cat(txt1);print(out$mods$summary);cat(txt2);print(out$mods$interactions,row.names=F)

          #storing number of variables in final model
              out$mods$vnames<- unique(var.name)

   }
 
   if(Model=="rf"){
          SplitBackground(out)
          psd.abs<-dat[dat$response==0,]
          rf.full<-list() 
               for(i in 1:length(table(Split))){
                    # tune the mtry parameter - this controls the number of covariates randomly subset for each split #
                  cat("\ntuning mtry parameter\n")  
                  x=rbind(dat[dat$response==1,-1],psd.abs[i==Split,-1])
                  y=factor(c(dat[dat$response==1,1],psd.abs[i==Split,1]))
                  if(is.null(mtry)){
                     mtry <- tuneRF(x=x,y=y,mtryStart=3,importance=TRUE,ntreeTry=100,
                     replace=FALSE, doBest=F, plot=F)
                     mtry <- mtry[mtry[,2]==min(mtry[,2]),1][1]
                     t2 <- unclass(Sys.time())
                  }
                    cat("\nnow fitting full random forest model using mtry=",mtry,"\n")
                       #
                     rf.full[[i]] <- randomForest(x=x,y=y,xtest=xtest,ytest=ytest,importance=TRUE, ntree=n.trees,
                        mtry=mtry,replace=samp.replace,sampsize=ifelse(is.null(sampsize),(ifelse(samp.replace,nrow(x),ceiling(.632*nrow(x)))),sampsize),
                        nodesize=ifelse(is.null(nodesize),(if (!is.null(y) && !is.factor(y)) 5 else 1),nodesize),maxnodes=maxnodes,
                        localImp=localImp, nPerm=nPerm, keep.forest=ifelse(is.null(keep.forest),!is.null(y) && is.null(xtest),keep.forest),
                        corr.bias=corr.bias, keep.inbag=keep.inbag)
                  if(i==1)model.summary<-importance(rf.full[[i]])
                      else model.summary<-model.summary+importance(rf.full[[i]])
             }
               n.pres<-sum(dat$response==1)
               out$mods$parms$mtry=mean(unlist(lapply(rf.full,FUN=function(lst){lst$mtry})))            
                        #Reduce("combine",rf.full)
               out$mods$final.mod <- rf.full
               if(PsdoAbs){
                  votes<-rep(NA,times=nrow(dat))
                  
                  #getting pres. votes in the right place
                  votes[dat$response==1]<-apply(do.call("rbind",lapply(lapply(rf.full,predict,type="vote"),"[",1:n.pres,2)),2,mean)         
                 
                  #these should be oob votes for the absence in a fairly random order 
                  for(i in 1:num.splits){
                       votes[which(i==Split,arr.ind=TRUE)]<-as.vector(apply(do.call("rbind",lapply(lapply(rf.full[-c(i)],predict,newdata=psd.abs[i==Split,-1],type="vote"),"[",,2)),2,mean)) 
                   }
               
                 	votes[votes==1]<-max(votes[votes<1])
                  votes[votes==0]<-min(votes[votes>0]) #from the original SAHM these can't be equal to 0 or 1 otherwise deviance can't be caluclated
                  #though I'm not sure deviance makes sense for RF anyway
                  response<-c(0,1)[factor(votes>.5)]
                  confusion.mat<-table(dat$response,response)
                  oob.error<-100*(1-sum(diag(confusion.mat))/sum(confusion.mat))
                  class.error<-c(confusion.mat[1,2],confusion.mat[2,1])/(apply(confusion.mat,1,sum))   
                                            
                  out$mods$final.mod$predictions<-votes
              }
            
          model.summary<-1/num.splits*model.summary[order(model.summary[,3],decreasing=T),]
          out$mods$summary <- model.summary                    
              write.txt(out,t0)
              txt0 <- paste("\n\n","Settings:",
              "\n\trandom seed used =                      ",seed,
              "\n\tn covariates considered at each split = ",mtry,
              "\n\tn trees =                               ",n.trees*length(unique(Split)),sep="")
          txt1 <- "\n\nRelative performance of predictors in final model:\n\n"
          
          capture.output(cat(txt0),cat(txt1),print(round(model.summary,4)),file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE) 
        
         #storing number of variables in final model
        out$mods$n.vars.final<-length(out$dat$used.covs) #random forest doesn't drop variables
        out$mods$vnames<-out$dat$used.covs
   }
  return(out)
}