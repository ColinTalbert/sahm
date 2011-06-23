make.auc.plot.jpg<-function(ma.reduced,pred,plotname,modelname,train.split=FALSE,thresh=NULL,train=NULL,train.pred=NULL,opt.methods=2,weight){
      if(is.null(weight)) weight=rep(1,times=dim(ma.reduced)[1])
    auc.data <- data.frame(ID=1:nrow(ma.reduced),pres.abs=ma.reduced[,1],pred=pred)
    p.bar <- sum(auc.data$pres.abs * weight) / sum(weight)
    n.pres <- sum(auc.data$pres.abs)
    n.abs <- nrow(auc.data)-n.pres

    null.dev<-calc.dev(auc.data$pres.abs, rep(p.bar,times=length(auc.data$pres.abs)), weight, family="binomial")$deviance*nrow(ma.reduced)
    dev.fit<-calc.dev(auc.data$pres.abs, pred, weight, family="binomial")$deviance*nrow(ma.reduced)
    dev.exp <- null.dev - dev.fit
    pct.dev.exp <- dev.exp/null.dev*100
   correlation<-cor(auc.data$pres.abs,pred)

    if(is.null(thresh)){
    thresh <- as.numeric(optimal.thresholds(auc.data,opt.methods=opt.methods))[2]}
    auc.fit <- auc(auc.data,st.dev=T)
    if(train.split==TRUE){

      jpeg(file=plotname)
      d<-data.frame(ID=1:nrow(train),pres.abs=train[,1],pred=train.pred)
      thresh<- as.numeric(optimal.thresholds(d,opt.methods=opt.methods))[2]
      TestTrainRocPlot(DATA=d,opt.thresholds=thresh,add.legend=FALSE,lwd=2)
      TestTrainRocPlot(auc.data,model.names=modelname,opt.thresholds=thresh,add.roc=TRUE,line.type=2,color="red",add.legend=FALSE)
      legend(x=.66,y=.2,c("Training Split","Testing Split"),lty=2,col=c("black","red"),lwd=2)
      graphics.off()} else {
      jpeg(file=plotname)
        TestTrainRocPlot(auc.data,model.names=modelname,opt.thresholds=thresh)
        graphics.off()

      }

    cmx <- cmx(auc.data,threshold=thresh)
    PCC <- pcc(cmx,st.dev=F)
    SENS <- sensitivity(cmx,st.dev=F)
    SPEC <- specificity(cmx,st.dev=F)
    KAPPA <- Kappa(cmx,st.dev=F)
    TSS <- SENS+SPEC-1
    return(list(thresh=thresh,cmx=cmx,null.dev=null.dev,dev.fit=dev.fit,dev.exp=dev.exp,pct.dev.exp=pct.dev.exp,auc=auc.fit[1,1],auc.sd=auc.fit[1,2],
        plotname=plotname,pcc=PCC,sens=SENS,spec=SPEC,kappa=KAPPA,tss=TSS,correlation=correlation))
}


make.poisson.jpg<-function(ma.reduced,pred,plotname,modelname,train.split=FALSE,thresh=NULL,train=NULL,train.pred=NULL,weight,train.weight=NULL){

    pois.data <- data.frame(ID=1:nrow(ma.reduced),pres.abs=ma.reduced[,1],pred=pred)
    p.bar <- sum(pois.data$pres.abs * weight) / sum(weight)
    n.pres <- sum(pois.data$pres.abs>=1)
    n.abs <- nrow(pois.data)-n.pres

    #takes into account potential weights but not offset

    null.dev<-calc.dev(pois.data$pres.abs, rep(p.bar,times=length(pois.data$pres.abs)), weight, family="poisson")$deviance*nrow(ma.reduced)
    dev.fit<-calc.dev(pois.data$pres.abs, pred, weight, family="poisson")$deviance*nrow(ma.reduced)
    dev.exp <- null.dev - dev.fit
    pct.dev.exp <- dev.exp/null.dev*100 #this is the pseudo R^2 using definition in the course notes
    correlation<-cor(pois.data$pres.abs,pred)
    #calibration(pois.data$pres.abs,pred,family="poisson") gbm hasn't implimented this for poisson though elith and leathwich use it, I can't find much
    prediction.error<-sum((pois.data$pres.abs-pred)^2)

    ## ADD Weights and offset?
    #function (y, mu, wt)
    #2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) - (y - mu))

   if(train.split==TRUE){
      if(is.null(train.weight)) train.weight=rep(1,times=nrow(train))
        null.dev.train<-calc.dev(train$response, rep(mean(train$response),times=nrow(train)), train.weight, family="poisson")$deviance*nrow(train)
        dev.fit.train<-calc.dev(train$response, train.pred, train.weight, family="poisson")$deviance*nrow(train)
        dev.exp.train <- null.dev.train - dev.fit.train
        pct.dev.exp.train <- dev.exp.train/null.dev.train*100
        correlation.train<-cor(train$response,train.pred)
        browser()
              jpeg(file=plotname)
           dev.contrib<-calc.dev(pois.data$pres.abs, pred, weight, family="poisson")$dev.cont
              z<-sign(pred-pois.data$pres.abs)*dev.contrib
              breaks<-quantile(z, probs = seq(0, .95,length=25))
              a<-outer(z,breaks,"<")
              res.mag<-apply(a,1,sum)
              plot(out$dat$ma$train.xy,col=heat.colors(24)[res.mag],cex=4,pch=19)
              x<-out$dat$ma$train.xy[,1]
              y<-out$dat$ma$train.xy[,2]
              a<-loess(z~x*y)
              x.lim<-rep(seq(from=min(out$dat$ma$train.xy[,1]),to=max(out$dat$ma$train.xy[,1]),length=100),each=100)
              y.lim<-rep(seq(from=min(out$dat$ma$train.xy[,2]),to=max(out$dat$ma$train.xy[,2]),length=100),times=100)
              z<-predict(a,newdata=cbind("x"=x.lim,"y"=y.lim))
              breaks<-quantile(z, probs = seq(0, .95,length=25))
              a<-outer(z,breaks,"<")
              res.mag<-apply(a,1,sum)
              z<-matrix(data=z,ncol=100,nrow=100,byrow=TRUE)
              res.mag<-matrix(data=res.mag,ncol=100,nrow=100,byrow=TRUE)

              image(x=seq(from=min(x.lim),to=max(x.lim),length=100),y=seq(from=min(y.lim),to=max(y.lim),length=100),z=matrix(data=z,ncol=100,nrow=100,byrow=TRUE))
              points(out$dat$ma$train.xy,pch=19,cex=.5)
              #plot(unique(x.lim),apply(z,2,sum))
              #plot(unique(y.lim),apply(z,1,sum))

              mod.resids<-residuals(out$mods$final.mod,type="deviance")
              plot(pred,(pois.data$pres.abs-pred),xlab="Predicted Values",ylab="Residuals",main="Residuals vs Fitted")
              panel.smooth(pred,(pois.data$pres.abs-pred))
              plot(pred,sqrt(2*dev.contrib),ylab="sqrt(Std.deviance residuals)",xlab="Predicted Values",main="Scale Location")
              panel.smooth(pred,sqrt(2*dev.contrib))
              qqnorm((sqrt(2*dev.contrib)-mean(sqrt(2*dev.contrib))),ylab="Std.Deviance residuals")
              abline(0,1)

        graphics.off()
        return(list(null.dev=null.dev,dev.fit=dev.fit,dev.exp=dev.exp,pct.dev.exp=pct.dev.exp,correlation=correlation,
          null.dev.train=null.dev.train,dev.fit.train=dev.fit.train,dev.exp.train=dev.exp.train,pct.dev.exp.train=pct.dev.exp.train,correlation.train=correlation.train))

     }else{
     jpeg(file=plotname)

        dev.contrib<-calc.dev(pois.data$pres.abs, pred, weight, family="poisson")$dev.cont
        par(mfrow=c(3,2))

              dev.contrib<-calc.dev(pois.data$pres.abs, pred, weight, family="poisson")$dev.cont
              z<-sign(pred-pois.data$pres.abs)*dev.contrib
              breaks<-quantile(z, probs = seq(0, .95,length=25))
              a<-outer(z,breaks,"<")
              res.mag<-apply(a,1,sum)
              plot(out$dat$ma$train.xy,col=heat.colors(24)[res.mag],cex=4,pch=19)
              x<-out$dat$ma$train.xy[,1]
              y<-out$dat$ma$train.xy[,2]
              a<-loess(z~x*y)
              x.lim<-rep(seq(from=min(out$dat$ma$train.xy[,1]),to=max(out$dat$ma$train.xy[,1]),length=100),each=100)
              y.lim<-rep(seq(from=min(out$dat$ma$train.xy[,2]),to=max(out$dat$ma$train.xy[,2]),length=100),times=100)
              z<-predict(a,newdata=cbind("x"=x.lim,"y"=y.lim))
              breaks<-quantile(z, probs = seq(0, .95,length=25))
              a<-outer(z,breaks,"<")
              res.mag<-apply(a,1,sum)
              z<-matrix(data=z,ncol=100,nrow=100,byrow=TRUE)
              res.mag<-matrix(data=res.mag,ncol=100,nrow=100,byrow=TRUE)

              image(x=seq(from=min(x.lim),to=max(x.lim),length=100),y=seq(from=min(y.lim),to=max(y.lim),length=100),z=matrix(data=z,ncol=100,nrow=100,byrow=TRUE))
              points(out$dat$ma$train.xy,pch=19,cex=.5)
              #plot(unique(x.lim),apply(z,2,sum))
              #plot(unique(y.lim),apply(z,1,sum))

              mod.resids<-residuals(out$mods$final.mod,type="deviance")
              plot(pred,(pois.data$pres.abs-pred),xlab="Predicted Values",ylab="Residuals",main="Residuals vs Fitted")
              panel.smooth(pred,(pois.data$pres.abs-pred))
              plot(pred,sqrt(2*dev.contrib),ylab="sqrt(Std.deviance residuals)",xlab="Predicted Values",main="Scale Location")
              panel.smooth(pred,sqrt(2*dev.contrib))
              qqnorm((sqrt(2*dev.contrib)-mean(sqrt(2*dev.contrib))),ylab="Std.Deviance residuals")
              abline(0,1)

        graphics.off()
        return(list(null.dev=null.dev,dev.fit=dev.fit,dev.exp=dev.exp,pct.dev.exp=pct.dev.exp,correlation=correlation))
        }
}

##############################################################################
EvaluationStats<-function(out,thresh,train,train.pred,opt.methods=opt.methods){
    response<-out$dat$ma$ma.test[,1]

     if(out$input$model.source.file=="rf.r") {pred<-tweak.p(as.vector(predict(out$mods$final.mod,newdata=out$dat$ma$ma.test[,-1],type="prob")[,2]))
      modelname="Random Forest"
     }

    if(out$input$model.source.file=="mars.r") {pred<-mars.predict(out$mods$final.mod,out$dat$ma$ma.test)$prediction[,1]
         modelname="MARS"
    }

    if(out$input$model.source.file=="glm.r")  {pred=glm.predict(out$mods$final.mod,out$dat$ma$ma.test)
        modelname="GLM"
    }

    if(out$input$model.source.file=="brt.r") {pred=predict.gbm(out$mods$final.mod,out$dat$ma$ma.test,out$mods$final.mod$target.trees,type="response")
       modelname="BRT"
      }

  ifelse(out$input$model.family!="poisson",
                  auc.output<-make.auc.plot.jpg(out$dat$ma$ma.test,pred=pred,plotname=paste(out$dat$bname,"_auc_plot.jpg",sep=""),
                      modelname=modelname,train.split=TRUE,thresh=thresh,train=train,train.pred,opt.methods=opt.methods,weight=out$dat$ma$test.weights),
                  auc.output<-make.poisson.jpg(out$dat$ma$ma.test,pred=pred,plotname=paste(out$dat$bname,"_auc_plot.jpg",sep=""),
                      modelname=modelname,train.split=TRUE,thresh=thresh,train=train,train.pred,weight=out$dat$ma$test.weights))




                 if(class(auc.output)=="try-error"){
              out$ec<-out$ec+1
              out$error.mssg[[out$ec]] <- paste("Error making ROC plot:",auc.output)
              } else { out$mods$auc.output<-auc.output}
    #sink(file=paste(out$dat$bname,"Evaluation.metrics.txt",sep=""))

    #deviance calcuation from Elith Leathwich code

    capture.output(cat("\n\nEvaluation Statistics applied to test split:\n",
                       "\n\t Correlation Coefficient      : ",cor.test(pred,response)$estimate,
                       "\n\t NULL Deviance                : ",auc.output$null.dev,
                       "\n\t Fit Deviance                 : ",auc.output$dev.fit,
                       "\n\t Explained Deviance           : ",auc.output$dev.exp,
                       "\n\t Percent Deviance Explained   : ",auc.output$pct.dev.exp,
                       file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE))
                       
                      if(out$input$model.family!="poisson"){
                           capture.output(cat(
                             "\n\n  Threshold Methods based on", switch(opt.methods,
                            "1"=".5 threshold",
                            "2"="Sens=Spec",
                            "3"="maximize (sensitivity+specificity)/2",
                            "4"="maximize Kappa",
                            "5"="maximize percent correctly classified",
                            "6"="predicted prevalence=observed prevalence",
                            "7"="threshold=observed prevalence",
                            "8"="mean predicted probability",
                            "9"="minimize distance between ROC plot and (0,1)",
                            ),
                            "\n\t Threshold                    : ",
                            auc.output$thresh,
                            "\n\n\t Confusion Matrix: \n\n"),
                            print.table(auc.output$cmx),
                       cat("\n\t AUC                          : ",auc.output$auc,
                       "\n\t Percent Correctly Classified : ",auc.output$pcc,
                       "\n\t Sensitivity                  : ",auc.output$sens,
                       "\n\t Specificity                  : ",auc.output$spec,
                       "\n\t Kappa                        : ",auc.output$kappa,
                       "\n\t True Skill Statistic         : ",auc.output$tss),
                       file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)}

    #possibly add confusion matrix or calibration as well

}

"calibration" <-
      function(obs, preds, family = "binomial")
      {
      #
      # j elith/j leathwick 17th March 2005
      # calculates calibration statistics for either binomial or count data
      # but the family argument must be specified for the latter
      # a conditional test for the latter will catch most failures to specify
      # the family
      #

      if (family == "bernoulli") family <- "binomial"
      pred.range <- max(preds) - min(preds)
      if(pred.range > 1.2 & family == "binomial") {
      print(paste("range of response variable is ", round(pred.range, 2)), sep = "", quote = F)
      print("check family specification", quote = F)
      return()
      }
      if(family == "binomial") {
      pred <- preds + 1e-005
      pred[pred >= 1] <- 0.99999
      mod <- glm(obs ~ log((pred)/(1 - (pred))), family = binomial)
      lp <- log((pred)/(1 - (pred)))
      a0b1 <- glm(obs ~ offset(lp) - 1, family = binomial)
      miller1 <- 1 - pchisq(a0b1$deviance - mod$deviance, 2)
      ab1 <- glm(obs ~ offset(lp), family = binomial)
      miller2 <- 1 - pchisq(a0b1$deviance - ab1$deviance, 1)
      miller3 <- 1 - pchisq(ab1$deviance - mod$deviance, 1)
      }
      if(family == "poisson") {
      mod <- glm(obs ~ log(preds), family = poisson)
      lp <- log(preds)
      a0b1 <- glm(obs ~ offset(lp) - 1, family = poisson)
      miller1 <- 1 - pchisq(a0b1$deviance - mod$deviance, 2)
      ab1 <- glm(obs ~ offset(lp), family = poisson)
      miller2 <- 1 - pchisq(a0b1$deviance - ab1$deviance, 1)
      miller3 <- 1 - pchisq(ab1$deviance - mod$deviance, 1)
      }
      calibration.result <- c(mod$coef, miller1, miller2, miller3)
      names(calibration.result) <- c("intercept", "slope", "testa0b1", "testa0|b1", "testb1|a")
      return(calibration.result)
}

"calc.dev" <-
        function(obs.values, fitted.values, weights = rep(1,length(obs.values)), family="binomial", calc.mean = TRUE)
        {
        # j. leathwick/j. elith
        #
        # version 2.1 - 5th Sept 2005
        #
        # function to calculate deviance given two vectors of raw and fitted values
        # requires a family argument which is set to binomial by default
        #
        #

        if (length(obs.values) != length(fitted.values))
           stop("observations and predictions must be of equal length")

        y_i <- obs.values

        u_i <- fitted.values

        if (family == "binomial" | family == "bernoulli") {

           deviance.contribs <- (y_i * log(u_i)) + ((1-y_i) * log(1 - u_i))
           deviance <- -2 * sum(deviance.contribs * weights)

        }

        if (family == "poisson" | family == "Poisson") {

            deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - (y_i - u_i)
            deviance <- 2 * sum(deviance.contribs * weights)

        }

        if (family == "laplace") {
            deviance <- sum(abs(y_i - u_i))
            }

        if (family == "gaussian") {
            deviance <- sum((y_i - u_i) * (y_i - u_i))
            }



        if (calc.mean) deviance <- deviance/length(obs.values)
        dev=list(deviance=deviance,dev.cont=deviance.contribs)
        return(dev)

}