make.auc.plot.jpg<-function(ma.reduced,pred,plotname,modelname,train.split=FALSE,thresh=NULL,train=NULL,train.pred=NULL,opt.methods=2){
    auc.data <- data.frame(ID=1:nrow(ma.reduced),pres_abs=ma.reduced[,1],pred=pred)
    p_bar <- mean(auc.data$pres_abs); n_pres <- sum(auc.data$pres_abs); n_abs <- nrow(auc.data)-n_pres
    null_dev <- -2*(n_pres*log(p_bar)+n_abs*log(1-p_bar))
    dev_fit <- -2*(sum(log(auc.data$pred[auc.data$pres_abs==1]))+sum(log(1-auc.data$pred[auc.data$pres_abs==0])))
    dev_exp <- null_dev - dev_fit
    pct_dev_exp <- dev_exp/null_dev*100

    if(is.null(thresh)){
    thresh <- as.numeric(optimal.thresholds(auc.data,opt.methods=opt.methods))[2]}
    auc.fit <- auc(auc.data,st.dev=T)
    if(train.split==TRUE){

      jpeg(file=plotname)
      d<-data.frame(ID=1:nrow(train),pres_abs=train[,1],pred=train.pred)
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
    return(list(thresh=thresh,cmx=cmx,null_dev=null_dev,dev_fit=dev_fit,dev_exp=dev_exp,pct_dev_exp=pct_dev_exp,auc=auc.fit[1,1],auc.sd=auc.fit[1,2],
        plotname=plotname,pcc=PCC,sens=SENS,spec=SPEC,kappa=KAPPA,tss=TSS))
}

##############################################################################
EvaluationStats<-function(out,thresh,train,train.pred,opt.methods=opt.methods){
    response<-out$dat$ma$ma.test[,1]

     if(out$input$model.source.file=="rf.r") {pred<-tweak.p(as.vector(predict(out$mods$final.mod,out$dat$ma$ma.test,type="prob")[,2]))
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

  auc.output <- try(make.auc.plot.jpg(out$dat$ma$ma.test,pred=pred,plotname=paste(out$dat$bname,"_auc_plot.jpg",sep=""),
            modelname=modelname,train.split=TRUE,thresh=thresh,train=train,train.pred,opt.methods=opt.methods))




                 if(class(auc.output)=="try-error"){
              out$ec<-out$ec+1
              out$error.mssg[[out$ec]] <- paste("Error making ROC plot:",auc.output)
              } else { out$mods$auc.output<-auc.output}
    #sink(file=paste(out$dat$bname,"Evaluation.metrics.txt",sep=""))

    #deviance calcuation from Elith Leathwich code

    cor.test(pred,response)$estimate

    capture.output(cat("\n\nEvaluation Statistics applied to test split\n",
                       "\n\t Correlation Coefficient      : ",cor.test(pred,response)$estimate,
                       "\n\t NULL Deviance                : ",auc.output$null_dev,
                       "\n\t Fit Deviance                 : ",auc.output$dev_fit,
                       "\n\t Explained Deviance           : ",auc.output$dev_exp,
                       "\n\t Percent Deviance Explained   : ",auc.output$pct_dev_exp,
                       
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
                       file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)

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
