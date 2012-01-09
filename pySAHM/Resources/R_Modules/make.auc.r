make.auc.plot.jpg<-function(out=out){

  plotname<-paste(out$dat$bname,"_modelEvalPlot.jpg",sep="")
  calib.plot<-paste(out$dat$bname,"_CalibrationPlot.jpg",sep="")
  modelname<-toupper(out$input$model)
  input.list<-out$dat$ma

######################### Calc threshold on train split #################
 if(out$input$model.family!="poisson"){
            input.list$train$thresh<-out$dat$ma$train$thresh<- as.numeric(optimal.thresholds(data.frame(ID=1:nrow(input.list$train$dat),pres.abs=input.list$train$dat[,1],
                pred=input.list$train$pred),opt.methods=out$input$opt.methods))[2]
              if(out$dat$split.type%in%c("test","eval"))  input.list$test$thresh<-out$dat$ma$test$thresh<-input.list$train$thresh
            }
            else input.list$train$thresh=NULL

################# Calculate all statistics on test\train or train\cv splits
  Stats<-lapply(input.list,calcStat,family=out$input$model.family)

 #################################################
 ############### Confusion Matrix ##############
  if(out$input$model.family!="poisson"){
     jpeg(file=paste(out$dat$bname,"confusion.matrix.jpg",sep="."),width=1000,height=1000,pointsize=13)
     barplot3d(c(Stats$train$Cmx[2,1],Stats$train$Cmx[1,1],Stats$train$Cmx[2,2],Stats$train$Cmx[1,2]),transp="f9", rows=2, theta = 40, phi = 25, expand=.5,
       bar.size=15*max(Stats$train$Cmx)/100,bar.space=6*max(Stats$train$Cmx)/100,
    col.lab=c("Absence","Presence"), row.lab=c("Presence","Absence"), z.lab="Confusion Matrix")
    graphics.off()
   }
########################## PLOTS ################################
  #Residual surface of input data
  if(out$dat$split.type!="eval"){
  residual.smooth.fct<-resid.image(calc.dev(input.list$train$dat$response, input.list$train$pred, input.list$train$weight, family=out$input$model.family)$dev.cont,input.list$train$pred,
          input.list$train$dat$response,input.list$train$XY$X,input.list$train$XY$Y,out$input$model.family,out$input$output.dir,label=out$dat$split.type,out)
      }
  else{
       residual.smooth.fct<-resid.image(calc.dev(input.list$test$dat$response, input.list$test$pred, input.list$test$weight, family=out$input$model.family)$dev.cont,input.list$test$pred,
          input.list$test$dat$response,input.list$test$XY$X,input.list$test$XY$Y,out$input$model.family,out$input$output.dir,label=out$dat$split.type,out)
       }
  train.mask<-seq(1:length(Stats))[names(Stats)=="train"]

## breaking of the non-train split must be done separately because list structure is different for the test only and cv
    lst<-list()
    if(out$dat$split.type%in%c("test","eval"))
      lst$Test<-Stats[[-c(train.mask)]]
    if(out$dat$split.type=="crossValidation") lst<-Stats[-c(train.mask)]
    if(out$dat$split.type%in%c("none")) lst<-Stats
 #AUC and Calibration plot for binomial data
    if(out$input$model.family%in%c("binomial","bernoulli")){
            jpeg(file=plotname,height=1000,width=1000,pointsize=20,quality=100)
            TestTrainRocPlot(DATA=Stats$train$auc.data,opt.thresholds=input.list$train$thresh,add.legend=(length(Stats)==1),lwd=2)
            if(out$dat$split.type!="none") {
            #so here we have to extract a sublist and apply a function to the sublist but if it has length 2 the structure of the list changes when the sublist is extracted
           if(out$dat$split.type=="test"){ TestTrainRocPlot(do.call("rbind",lapply(lst,function(lst){lst$auc.data})),add.roc=TRUE,line.type=2,color="red",add.legend=FALSE)
                legend(x=.5,y=.2,c("Training Split","Testing Split","Cross Validation Sets")[c(1,2*(length(Stats)==2),3*(length(Stats)>2))],lty=2,col=c("black","red"),lwd=2)
                }
             if(out$dat$split.type=="crossValidation"){
                temp<-lapply(lst,function(lst){roc.plot.calculate(lst$auc.data)})
                sens<-unlist(lapply(temp,function(temp){temp$sensitivity}))
                specif<-1-unlist(lapply(temp,function(temp){temp$specificity}))
                unique.spec<-sort(unique(specif))
                for(i in 2:length(unique.spec)){
                 segments(seq(from=unique.spec[i-1],to=unique.spec[i],length=100), rep(min(sens[specif>=unique.spec[i]]),times=100),
                      x1 = seq(from=unique.spec[i-1],to=unique.spec[i],length=100), y1 = rep(max(sens[specif<=unique.spec[i]]),times=100),col="blue")
                  TestTrainRocPlot(DATA=Stats$train$auc.data,opt.thresholds=input.list$train$thresh,add.legend=(length(Stats)==1),lwd=2,add.roc=TRUE,line.type=1,col="red")
                }
              TestTrainRocPlot(DATA=Stats$train$auc.data,opt.thresholds=input.list$train$thresh,add.legend=(length(Stats)==1),lwd=2,add.roc=TRUE,line.type=1,col="red")
              points(1-Stats$train$Specf,Stats$train$Sens,pch=19,cex=2.5)
              points(1-Stats$train$Specf,Stats$train$Sens,pch=19,cex=2,col="red")
              text(x=(1.05-Stats$train$Specf),y=Stats$train$Sens-.03,label=round(Stats$train$thresh,digits=2),col="red")
                legend(x=.5,y=.25,c("Training Split","Cross Validation Range"),lty=c(2,1),col=c("red","blue"),lwd=2)
                }}
                graphics.off()
            #I'm pretty sure calibration plots should work for count data as well but I'm not quite ready to make a plot
            jpeg(file=calib.plot,height=1000,width=1000,pointsize=20,quality=100)

            a<-do.call("rbind",lapply(lst,function(lst){lst$auc.data}))
            calibration.plot(a,main="Calibration Plot")
            preds<-a$pred
            obs<-a$pres.abs
            pred <- preds + 1e-005
            pred[pred >= 1] <- 0.99999
            mod <- glm(obs ~ log((pred)/(1 - (pred))), family = binomial)
             predseq<-data.frame("pred"=seq(from=0,to=1,length=100))
             lines(predseq$pred,sort(predict(mod,newdata=predseq,type="response")))
            rug(pred)
            dev.off()
            }
    #Some residual plots for poisson data
    if(out$input$model.family%in%c("poisson")){
            jpeg(file=plotname)
            par(mfrow=c(2,2))
             plot(log(Stats$train$auc.data$pred[Stats$train$auc.data$pred!=0]),
                  (Stats$train$auc.data$pres.abs[Stats$train$auc.data$pred!=0]-Stats$train$auc.data$pred[Stats$train$auc.data$pred!=0]),
                  xlab="Predicted Values (log scale)",ylab="Residuals",main="Residuals vs Fitted",ylim=c(-3,3))
              abline(h=0,lty=2)
              panel.smooth(log(Stats$train$auc.data$pred[Stats$train$auc.data$pred!=0]),
              (Stats$train$auc.data$pres.abs[Stats$train$auc.data$pred!=0]-Stats$train$auc.data$pred[Stats$train$auc.data$pred!=0]))
               if(out$input$script.name!="rf"){
                    #this is the residual plot from glm but I don't think it will work for anything else
                    qqnorm(residuals(out$mods$final.mod),ylab="Std. deviance residuals")
                    qqline(residuals(out$mods$final.mod))
                     yl <- as.expression(substitute(sqrt(abs(YL)), list(YL = as.name("Std. Deviance Resid"))))
                    plot(log(Stats$train$auc.data$pred[Stats$train$auc.data$pred!=0]),
                       sqrt((abs(residuals(out$mods$final.mod,type="deviance")[Stats$train$auc.data$pred!=0]))),
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
        capture.stats(lst,file.name=paste(out$dat$bname,"_output.txt",sep=""),label=out$dat$split.type,family=out$input$model.family,opt.methods=out$input$opt.methods,out)
    }

   if(out$dat$split.type!="eval"){
        ############ getting statistics along with appropriate names into a data frame for creating the appended output
                        last.dir<-strsplit(out$input$output.dir,split="\\\\")
                        parent<-sub(paste("\\\\",last.dir[[1]][length(last.dir[[1]])],sep=""),"",out$input$output.dir)
                        
                       if(out$input$model.family%in%c("binomial","bernoulli")){
                           csv.stats<-lapply(Stats,function(lst){
                               return(c("","",lst$correlation,lst$pct.dev.exp,lst$Pcc,lst$Sens,lst$Specf))})
                            stat.names<-c("Correlation Coefficient","Percent Deviance Explained","Percent Correctly Classified","Sensitivity","Specificity")
                        } else{
                        csv.stats<-lapply(Stats,function(lst){
                            return(c("","",lst$correlation,lst$pct.dev.exp,lst$prediction.error/100))})
                                stat.names<-c("Correlation Coefficient","Percent Deviance Explained","Prediction Error")
                               }
                            csv.vect<-c(t(t(as.vector(unlist(csv.stats[train.mask])))),if(out$dat$split.type!="none") unlist(csv.stats[-c(train.mask)]))
                            csv.vect[seq(from=2,by=length(csv.vect)/length(Stats),length=length(Stats))]<-ifelse(out$dat$split.type=="none","Train",c("Train",names(lst)))
                           x=data.frame(cbind(rep(c("","",stat.names),times=length(Stats)),
                             csv.vect))

                        Header<-cbind(c("","Original Field Data","Field Data Template","PARC Output Folder","PARC Template","Covariate Selection Name",""),
                            c(last.dir[[1]][length(last.dir[[1]])],
                            out$dat$input$OrigFieldData,out$dat$input$FieldDataTemp,out$dat$input$ParcOutputFolder,
                            out$dat$input$ParcTemplate,ifelse(length(out$dat$input$CovSelectName)==0,"NONE",out$dat$input$CovSelectName),""))

                      AppendOut(compile.out=out$input$Append.Dir,Header,x,out,Parm.Len=length(stat.names),parent=parent,split.type=out$dat$split.type)
               }
    return(list(thresh=train.stats$train$thresh,residual.smooth.fct=residual.smooth.fct))
}

