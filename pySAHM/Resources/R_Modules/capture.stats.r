capture.stats<-function(Stats.lst,file.name,train=FALSE,label,family,opt.methods,thresh){

capture.output(cat(" applied to",label, "split:\n",sep=" "),
                        file=file.name,append=TRUE)
    capture.output(cat( "\n",
                       "\n\t Correlation Coefficient      : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$correlation}),mean)),
                       "\n\t NULL Deviance                : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$null.dev}),mean)),
                       "\n\t Fit Deviance                 : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$dev.fit}),mean)),
                       "\n\t Explained Deviance           : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$dev.exp}),mean)),
                       "\n\t Percent Deviance Explained   : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$pct.dev.exp}),mean)),
                       file=file.name,append=TRUE))

                        if(family%in%c("binomial","bernoulli")){
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
                                thresh,
                                "\n\n\t Confusion Matrix: \n\n"),
                                if(label%in%c("train","test.train")) print.table(Stats.lst[[1]]$Cmx),
                                if(label=="cross.validation") print.table(lapply(lapply(Stats.lst,function(lst){lst$Cmx}),sum)),
                           cat("\n\t AUC                          : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$auc.fit[1,1]}),mean)),
                           "\n\t Percent Correctly Classified : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$Pcc}),mean)),
                           "\n\t Sensitivity                  : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$Sens}),mean)),
                           "\n\t Specificity                  : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$Specf}),mean)),
                           "\n\t Kappa                        : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$Kappa}),mean)),
                           "\n\t True Skill Statistic         : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$Tss}),mean)),"\n"),
                           file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)
                       }

    capture.output(cat( "\n\n   Calibration Statistics",
                          "\n\t Intercept (general calibration)                            : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$calibration.stats[1]}),mean)),
                          "\n\t Slope   (direction and variation in fit)                   : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$calibration.stats[2]}),mean)),
                          "\n\t Testa0b1 (overall reliability of predictors)               : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$calibration.stats[3]}),mean)),
                          "\n\t Testa0|b1(incorrect calibration given correct refinement)  : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$calibration.stats[4]}),mean)),
                          "\n\t Testb1|a (refinement given correct calibration)            : ",unlist(lapply(lapply(Stats.lst,function(lst){lst$calibration.stats[5]}),mean)),"\n",
                       file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE))

}