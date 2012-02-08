#source(paste(ScriptPath,"EvaluationStats.r",sep="\\"))
source(paste(ScriptPath,"TestTrainRocPlot.r",sep="\\"))
source(paste(ScriptPath,"read.ma.r",sep="\\"))
source(paste(ScriptPath,"proc.tiff.r",sep="\\"))
source(paste(ScriptPath,"modalDialog.r",sep="\\"))
#replace EvaluationStats with
source(paste(ScriptPath,"make.auc.r",sep="\\"))
source(paste(ScriptPath,"EvalStats.r",sep="\\"))

#only used in new code
source(paste(ScriptPath,"capture.stats.r",sep="\\"))
source(paste(ScriptPath,"CalcStats.r",sep="\\"))
source(paste(ScriptPath,"EvalStatsHelperFcts.r",sep="\\"))
source(paste(ScriptPath,"AppendOut.r",sep="\\"))
source(paste(ScriptPath,"ResidualImage.r",sep="\\"))
source(paste(ScriptPath,"Pred.Surface.r",sep="\\"))
source(paste(ScriptPath,"calc.deviance.r",sep="\\"))
source(paste(ScriptPath,"calibration.r",sep="\\"))
source(paste(ScriptPath,"roc.r",sep="\\"))
#New generic function source code
#source(paste(ScriptPath,"cv.fct.r",sep="\\"))
#an experimental cross-validation function that needs further testing
#this doesn't break when factor predictors are used but I'm not positive
#it always gives the same output as the original.
source(paste(ScriptPath,"cv.fctExper.r",sep="\\"))
source(paste(ScriptPath,"generic.model.fit.r",sep="\\"))
source(paste(ScriptPath,"pred.fct.r",sep="\\"))
source(paste(ScriptPath,"response.curves.r",sep="\\"))
source(paste(ScriptPath,"chk.libs.r",sep="\\"))
source(paste(ScriptPath,"FitModels.r",sep="\\"))
source(paste(ScriptPath,"place.save.r",sep="\\"))
source(paste(ScriptPath,"ConfusionMatrix.r",sep="\\"))