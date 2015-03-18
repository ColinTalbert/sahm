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

make.auc.plot.jpg<-function(out=out){
 
  plotname<-paste(out$dat$bname,"_modelEvalPlot.png",sep="")
  calib.plot<-paste(out$dat$bname,"_CalibrationPlot.png",sep="")
  modelname<-toupper(out$input$model)
  inlst<-out$dat$ma
 
########################################################################
######################### Calc threshold on train split #################
         
 if(out$input$model.family!="poisson"){
            inlst$train$thresh<-out$dat$ma$train$thresh<- as.numeric(optimal.thresholds(data.frame(ID=1:length(inlst$train$resp$response),pres.abs=inlst$train$resp,
                pred=inlst$train$pred),opt.methods=out$input$opt.methods))[2]
              if(out$dat$split.type%in%c("test","eval"))  
              inlst$test$thresh<-out$dat$ma$test$thresh <- as.numeric(optimal.thresholds(data.frame(ID=1:length(inlst$test$resp$response),pres.abs=inlst$test$resp,
                pred=inlst$test$pred),opt.methods=out$input$opt.methods))[2]
            }
            else inlst$train$thresh=NULL
  
##################################################################
### Standard residual analysis plots for glm
    if(out$input$script.name%in%c("glm","mars") & out$dat$split.type!="eval" & !(out$input$script.name=="mars" & out$input$PsdoAbs==TRUE)){
          png(paste(out$dat$bname,"_stand.resid.plots.png",sep=""),height=1000,width=1000)
          par(mfrow=c(2,2))
          if(out$input$script.name=="glm") plot(out$mods$final.mod[[1]],cex=1.5,lwd=1.5,cex.main=1.5,cex.lab=1.5)
          if(out$input$script.name=="mars") plot(out$mods$final.mod[[1]]$glm.list[[1]],cex=1.5,lwd=1.5,cex.main=1.5,cex.lab=1.5)
          par(mfrow=c(1,1))
          graphics.off()
    }
################# Calculate all statistics on test\train or train\cv splits
  
   out$input$has.split<-(out$input$PsdoAbs & !out$input$script.name%in%c("glm","maxent"))
  Stats<-lapply(inlst,calcStat,family=out$input$model.family,has.split=out$input$has.split)

#################### Variable importance plots #####################

    if(length(out$mods$vnames)>1 & out$input$model.family!="poisson"){
      png(paste(out$dat$bname,"_variable.importance.png",sep=""),height=1000,width=1000)  
        VariableImportance(out$input$script.name,out=out,auc=lapply(Stats,"[",9)) 
      graphics.off()
    }    

 ##### lst doesn't contain the training portion of the data
   train.mask<-seq(1:length(Stats))[names(Stats)=="train"]

      ## breaking of the non-train split must be done separately because list structure is different for the test only and cv
    lst<-list()
    if(out$dat$split.type%in%c("test","eval"))
      lst$Test<-Stats[[-c(train.mask)]]
    if(out$dat$split.type=="crossValidation") lst<-Stats[-c(train.mask)]
    if(out$dat$split.type%in%c("none")) lst<-Stats

 #################################################
 ############### Confusion Matrix Plot ###########

  if(out$input$model.family!="poisson"){
     
   png(file=paste(out$dat$bname,"confusion.matrix.png",sep="."),width=1000,height=1000,pointsize=13)
    confusion.matrix(Stats,out$dat$split.type)
    graphics.off()
   }
########################## PLOTS ################################
#########  Residual surface of input data  ##########

  if(out$input$ResidMaps){
        if(out$dat$split.label!="eval"){
        residual.smooth.fct<-resid.image(calc.dev(inlst$train$dat$response, inlst$train$pred, inlst$train$weight, family=out$input$model.family)$dev.cont,inlst$train$pred,
                inlst$train$dat$response,inlst$train$XY$X,inlst$train$XY$Y,inlst$train$weight,out$input$model.family,out$input$output.dir,label=out$dat$split.label,out)
            }
        else{
             residual.smooth.fct<-resid.image(calc.dev(inlst$test$dat$response, inlst$test$pred, inlst$test$weight, family=out$input$model.family)$dev.cont,inlst$test$pred,
                inlst$test$dat$response,inlst$test$XY$X,inlst$test$XY$Y,inlst$test$weight,out$input$model.family,out$input$output.dir,label=out$dat$split.label,out)
             }
      } else residual.smooth.fct=NULL
########## AUC and Calibration plot for binomial data #######################
 
    if(out$input$model.family%in%c("binomial","bernoulli")){
          
            png(file=plotname,height=1000,width=1000,pointsize=20)
    ## ROC AUC plots
            TestTrainRocPlot(DATA=Stats$train$auc.data,opt.thresholds=inlst$train$thresh,add.legend=FALSE,lwd=2)
                 if(out$dat$split.type=="none") legend(x=.8,y=.15,paste("AUC=",round(Stats$train$auc.fit,digits=3),sep=""))
            if(out$dat$split.type!="none") {
                #so here we have to extract a sublist and apply a function to the sublist but if it has length 2 the structure of the list changes when the sublist is extracted
                 if(out$dat$split.type%in%c("test","eval")){ TestTrainRocPlot(do.call("rbind",lapply(lst,function(lst){lst$auc.data})),add.roc=TRUE,line.type=2,color="red",add.legend=FALSE)
                    legend(x=.46,y=.24,c(paste("Training Split (AUC=",round(Stats$train$auc.fit,digits=3), ")",sep=""),paste("Testing Split  (AUC=",round(Stats$test$auc.fit,digits=3), ")",sep="")),
                       lty=2,col=c("black","red"),lwd=2,cex=1.3)
                 }
                 if(out$dat$split.type=="crossValidation"){
                
                      ROC.list<-list(predictions=lapply(lst,function(lst){lst$auc.data$pred}),labels=lapply(lst,function(lst){lst$auc.data$pres.abs}))
                      pred <- prediction(ROC.list$predictions, ROC.list$labels)
                      perf <- performance(pred,"tpr","fpr")
                      plot(perf,col="grey82",lty=3,xlab="1-Specificity (False Positive)",ylab="Sensitivity (True Positive)",main="ROC Plot for Cross-Validation",cex.main=2,cex.axis=1.4,cex.lab=1.5)
                      plot(perf,lwd=1,avg="vertical",spread.estimate="boxplot",add=TRUE)
                      TestTrainRocPlot(DATA=Stats$train$auc.data,opt.thresholds=inlst$train$thresh,add.legend=FALSE,lwd=1.5,add.roc=TRUE,line.type=1,col="red",legend.cex=2)
                      points(1-Stats$train$Specf,Stats$train$Sens,pch=21,cex=2.5,bg="red")
                       segments(x0=0,y0=0,x1=1,y1=1,col="blue")
                      text(x=(.96-Stats$train$Specf),y=Stats$train$Sens+.03,label=round(Stats$train$thresh,digits=2))
                        legend(x=.5,y=.24,c(paste("Training Split (AUC=",round(Stats$train$auc.fit,digits=3), ")",sep=""),
                             paste("Cross Validation Mean \n (AUC=",round(mean(unlist(lapply(lst,function(lst){lst$auc.fit}))),digits=3), ")",sep="")),lwd=c(4,1),lty=c(1,1),
                             col=c("red","black"),cex=1.3)
                 }
                }
                graphics.off()

            #I'm pretty sure calibration plots should work for count data as well but I'm not quite ready to make a plot
           png(file=calib.plot,height=1000,width=1000,pointsize=20)
                cal.results<-switch(out$dat$split.type,
                            none = Stats$train$calibration.stats,
                             test = Stats$test$calibration.stats,
                             eval = Stats$test$calibration.stats,
                                crossValidation =  apply(do.call("rbind",lapply(lst,function(lst){lst$calibration.stats})),2,mean))
     ## Calibration plot
     options(warn=-1) #this often gives warnings about probabilities numerically 0 or 1
            a<-do.call("rbind",lapply(lst,function(lst){lst$auc.data}))
            if(out$input$PsdoAbs==TRUE) {
              
                #I'm not at all sure this is right but it would seem you can't change the ratio of pres to background between the test
                #and train split so we can downsample abs or weight the predictions for the absence I'm not sure either of these are 
                #appropriate
                if(!(out$input$script.name%in%c("glm"))){
                    absn<-which(a$pres.abs==0,arr.ind=TRUE)
                    samp<-sample(absn,size=min(table(a$pres.abs)),replace=FALSE) 
                }
                p.plt<-try(pocplot(a$pred[a$pres.abs==1], a$pred[a$pres.abs==0], 
                title=paste("Presence Only Calibration Plot for \n",switch(out$dat$split.type,none="Training Data",test="Test Split",
                eval="Test Split",crossValidation="Cross Validation Split"),sep="")),silent=TRUE)
                if(class(p.plt)=="try-error"){
                             par(mfrow=c(2,1))
                             hist(a$pred[a$pres.abs==1],freq=TRUE,col="red",xlim=range(a$pred),xlab="Predicted Probability",main="Presence")
                             hist(a$pred[a$pres.abs==0],freq=TRUE,col="blue",xlim=range(a$pred),xlab="Predicted Probability",main="Available")
                }
            } else{ 
            pacplot(a$pred,a$pres.abs,title=paste("Calibration Plot for ",
                   switch(out$dat$split.type,none="Training Data",test="Test Split",eval="Test Split",crossValidation="Cross Validation Split"),sep=""))
             }
            dev.off()
      }
     options(warn=0)     
   #Some residual plots for poisson data
    if(out$input$model.family%in%c("poisson")){
            png(file=plotname)
            par(mfrow=c(2,2))
             plot(log(Stats$train$auc.data$pred[Stats$train$auc.data$pred!=0]),
                  (Stats$train$auc.data$pres.abs[Stats$train$auc.data$pred!=0]-Stats$train$auc.data$pred[Stats$train$auc.data$pred!=0]),
                  xlab="Predicted Values (log scale)",ylab="Residuals",main="Residuals vs Fitted",ylim=c(-3,3))
              abline(h=0,lty=2)
              panel.smooth(log(Stats$train$auc.data$pred[Stats$train$auc.data$pred!=0]),
              (Stats$train$auc.data$pres.abs[Stats$train$auc.data$pred!=0]-Stats$train$auc.data$pred[Stats$train$auc.data$pred!=0]))
               if(out$input$script.name!="rf"){
                    #this is the residual plot from glm but I don't think it will work for anything else
                    qqnorm(residuals(out$mods$final.mod[[1]]),ylab="Std. deviance residuals")
                    qqline(residuals(out$mods$final.mod[[1]]))
                     yl <- as.expression(substitute(sqrt(abs(YL)), list(YL = as.name("Std. Deviance Resid"))))
                    plot(log(Stats$train$auc.data$pred[Stats$train$auc.data$pred!=0]),
                       sqrt((abs(residuals(out$mods$final.mod[[1]],type="deviance")[Stats$train$auc.data$pred!=0]))),
                       xlab="Predicted Values (log Scale)",ylab=yl)
              }
            graphics.off()}

 ##################### CAPTURING TEXT OUTPUT #######################
    capture.output(cat("\n\n============================================================",
                        "\n\nEvaluation Statistics"),file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)
      #this is kind of a pain but I have to keep everything in the same list format
      train.stats=list()
     if(out$dat$split.type=="none") train.stats<-Stats
      else train.stats$train=Stats[[train.mask]]

    capture.stats(train.stats,file.name=paste(out$dat$bname,"_output.txt",sep=""),label="train",family=out$input$model.family,opt.methods=out$input$opt.methods,out)
    if(out$dat$split.type!="none"){
    capture.output(cat("\n\n============================================================",
                        "\n\nEvaluation Statistics"),file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)
        capture.stats(lst,file.name=paste(out$dat$bname,"_output.txt",sep=""),label=out$dat$split.label,family=out$input$model.family,opt.methods=out$input$opt.methods,out)
    }

        ############ getting statistics along with appropriate names into a data frame for creating the appended output
                        parent<-dirname(out$input$output.dir) 
                        
                       if(out$input$model.family%in%c("binomial","bernoulli")){
                           csv.stats<-lapply(Stats,function(lst){
                               return(c("","",lst$correlation,lst$pct.dev.exp,lst$Pcc,lst$auc.fit,lst$Tss))})
                            stat.names<-c("Correlation Coefficient","Percent Deviance Explained","Percent Correctly Classified","AUC","True Skill Stat")
                        } else{
                        csv.stats<-lapply(Stats,function(lst){
                            return(c("","",lst$correlation,lst$pct.dev.exp,lst$prediction.error/100))})
                                stat.names<-c("Correlation Coefficient","Percent Deviance Explained","Prediction Error")
                               }
                            csv.vect<-c(t(t(as.vector(unlist(csv.stats[train.mask])))),if(out$dat$split.type!="none") unlist(csv.stats[-c(train.mask)]))
                            csv.vect[seq(from=2,by=length(csv.vect)/length(Stats),length=length(Stats))]<-if(out$dat$split.type=="none"){"Train"}else{c("Train",names(lst))}
                           x=data.frame(cbind(rep(c("","",stat.names),times=length(Stats)),
                             csv.vect),row.names=NULL)

                        Header<-cbind(c("","Original Field Data","Field Data Template","PARC Output Folder","PARC Template","Covariate Selection Name",""),
                            c(basename(out$input$output.dir),
                            out$dat$input$OrigFieldData,out$dat$input$FieldDataTemp,out$dat$input$ParcOutputFolder,
                            basename(out$dat$input$ParcTemplate),ifelse(length(out$dat$input$CovSelectName)==0,"NONE",out$dat$input$CovSelectName),""))
                      
                        AppendOut(compile.out=out$input$Append.Dir,Header,x,out,Parm.Len=length(stat.names),parent=parent,split.type=out$dat$split.type)

    return(list(thresh=train.stats$train$thresh,residual.smooth.fct=residual.smooth.fct))
}

