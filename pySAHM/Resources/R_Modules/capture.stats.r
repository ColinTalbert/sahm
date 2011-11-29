capture.stats<-function(Stats.lst,file.name,label,family,opt.methods,out){

capture.output(cat(" applied to",label, "split:\n",sep=" "),
                        file=file.name,append=TRUE)
    capture.output(cat( "\n",
                       "\n\t Correlation Coefficient      : ",mean(unlist(lapply(Stats.lst,function(lst){lst$correlation}))),
                             if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$correlation}))),digits=5),
                              ")",sep="")},
                       "\n\t NULL Deviance                : ",mean(unlist(lapply(Stats.lst,function(lst){lst$null.dev}))),
                              if(label=="crossValidation"){paste(" (sd ",
                                 signif(sd(unlist(lapply(Stats.lst,function(lst){lst$null.dev}))),digits=5),
                                ")",sep="")},
                       "\n\t Fit Deviance                 : ",mean(unlist(lapply(Stats.lst,function(lst){lst$dev.fit}))),
                             if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$dev.fit}))),digits=5),
                              ")",sep="")},
                       "\n\t Explained Deviance           : ",mean(unlist(lapply(Stats.lst,function(lst){lst$dev.exp}))),
                              if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$dev.exp}))),digits=5),
                              ")",sep="")},
                       "\n\t Percent Deviance Explained   : ",mean(unlist(lapply(Stats.lst,function(lst){lst$pct.dev.exp}))),
                             if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$pct.dev.exp}))),5),
                              ")",sep="")},
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
                                if(label!="crossValidation"){paste("\n\t Threshold                    : ",
                                Stats.lst[[1]]$thresh)}
                                else{paste(
                                                             "\n\t Mean Threshold               : ",
                               mean(unlist(lapply(Stats.lst,function(lst){lst$thresh}))),
                                                             " (sd ",
                                signif(sd(unlist(lapply(Stats.lst,function(lst){lst$thresh}))),digits=5),")",sep="")
                                },
                                "\n\n\t Confusion Matrix: \n\n"),
                                if(label%in%c("train","test")) print.table(Stats.lst[[1]]$Cmx)
                                else{
                                  a<-lapply(Stats.lst,function(lst){lst$Cmx})
                                  cmx<-a[[1]]
                                  for(i in 2:length(a)) cmx<-cmx+a[[i]] #it's amazing I can't think of a better way to sum a list of tables
                                   print.table(cmx)
                                   },
                           cat("\n\t AUC                          : ",mean(unlist(lapply(Stats.lst,function(lst){lst$auc.fit}))),
                           if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$auc.fit}))),digits=5),
                              ")",sep="")},
                           "\n\t Percent Correctly Classified : ",mean(unlist(lapply(Stats.lst,function(lst){lst$Pcc}))),
                           if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$Pcc}))),digits=5),
                              ")",sep="")},
                           "\n\t Sensitivity                  : ",mean(unlist(lapply(Stats.lst,function(lst){lst$Sens}))),
                           if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$Sens}))),digits=5),
                              ")",sep="")},
                           "\n\t Specificity                  : ",mean(unlist(lapply(Stats.lst,function(lst){lst$Specf}))),
                           if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$Specf}))),digits=5),
                              ")",sep="")},
                           "\n\t Kappa                        : ",mean(unlist(lapply(Stats.lst,function(lst){lst$Kappa}))),
                           if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$Kappa}))),digits=5),
                              ")",sep="")},
                           "\n\t True Skill Statistic         : ",mean(unlist(lapply(Stats.lst,function(lst){lst$Tss}))),
                           if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$Tss}))),digits=5),
                              ")",sep="")},"\n"),
                           file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)
                       }

    capture.output(cat( "\n\n   Calibration Statistics",
                          "\n\t Intercept (general calibration)                            : ",mean(unlist(lapply(Stats.lst,function(lst){lst$calibration.stats[1]}))),
                          if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$calibration.stats[1]}))),digits=5),
                              ")",sep="")},
                          "\n\t Slope   (direction and variation in fit)                   : ",mean(unlist(lapply(Stats.lst,function(lst){lst$calibration.stats[2]}))),
                          if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$calibration.stats[2]}))),digits=5),
                              ")",sep="")},
                          "\n\t Testa0b1 (overall reliability of predictors)               : ",mean(unlist(lapply(Stats.lst,function(lst){lst$calibration.stats[3]}))),
                          if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$calibration.stats[3]}))),digits=5),
                              ")",sep="")},
                          "\n\t Testa0|b1(incorrect calibration given correct refinement)  : ",mean(unlist(lapply(Stats.lst,function(lst){lst$calibration.stats[4]}))),
                          if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$calibration.stats[4]}))),digits=5),
                              ")",sep="")},
                          "\n\t Testb1|a (refinement given correct calibration)            : ",mean(unlist(lapply(Stats.lst,function(lst){lst$calibration.stats[5]}))),
                          if(label=="crossValidation"){paste(" (sd ",
                              signif(sd(unlist(lapply(Stats.lst,function(lst){lst$calibration.stats[5]}))),digits=5),
                              ")",sep="")},

                              "\n\n",
                       file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE))
#if(label=="crossValidation"){cat("\n\n   Pooled Calibration Statistics\n",print.table(cbind(names(out$cv$pooled.calib),out$cv$pooled.calib)))}
#something I should include later
}