make.poisson.jpg<-function(ma.reduced,pred,plotname,modelname,test.split=FALSE,thresh=NULL,train=NULL,train.pred=NULL,weight,train.weight=NULL,out){
      if(is.null(weight)) weight=rep(1,times=dim(ma.reduced)[1])
    auc.data <- data.frame(ID=1:nrow(ma.reduced),pres.abs=ma.reduced[,1],pred=pred)
    p.bar <- sum(auc.data$pres.abs * weight) / sum(weight)
    n.pres <- sum(auc.data$pres.abs>=1)
    n.abs <- nrow(auc.data)-n.pres

    null.dev<-calc.dev(auc.data$pres.abs, rep(p.bar,times=length(auc.data$pres.abs)), weight, family="poisson")$deviance*nrow(ma.reduced)
    dev.fit<-calc.dev(auc.data$pres.abs, pred, weight, family="poisson")$deviance*nrow(ma.reduced)
    dev.exp <- null.dev - dev.fit
    pct.dev.exp <- dev.exp/null.dev*100 #this is the pseudo R^2 using definition in the course notes
    correlation<-cor(auc.data$pres.abs,pred)
    #calibration(auc.data$pres.abs,pred,family="poisson") gbm hasn't implimented this for poisson though elith and leathwich use it, I can't find much
    prediction.error<-sum((auc.data$pres.abs-pred)^2)
      browser()
     mod.resids<-residuals(out$mods$final.mod,type="deviance")
              plot(pred,(auc.data$pres.abs-pred),xlab="Predicted Values",ylab="Residuals",main="Residuals vs Fitted")
              panel.smooth(pred,(auc.data$pres.abs-pred))
              dev.contrib<-calc.dev(auc.data$pres.abs, pred, weight, family="poisson")$dev.cont
              plot(pred,sqrt(2*dev.contrib),ylab="sqrt(Std.deviance residuals)",xlab="Predicted Values",main="Scale Location")
              panel.smooth(pred,sqrt(2*dev.contrib))
              qqnorm(residuals(out$mods$final.mod))
              qqline(residuals(out$mods$final.mod))

      
   if(test.split==TRUE){

      if(is.null(train.weight)) train.weight=rep(1,times=nrow(train))
        null.dev.train<-calc.dev(train$response, rep(mean(train$response),times=nrow(train)), train.weight, family="poisson")$deviance*nrow(train)
        dev.fit.train<-calc.dev(train$response, train.pred, train.weight, family="poisson")$deviance*nrow(train)
        dev.exp.train <- null.dev.train - dev.fit.train
        pct.dev.exp.train <- dev.exp.train/null.dev.train*100
        correlation.train<-cor(train$response,train.pred)

              jpeg(file=plotname)
           dev.contrib<-calc.dev(auc.data$pres.abs, pred, weight, family="poisson")$dev.cont
              par(mfrow=c(3,2))
              z<-sign(pred-auc.data$pres.abs)*dev.contrib
              z.range<-max(z)-min(z)
              z.lim<-c((min(z)-.1*z.range),(max(z)+.1*z.range))
              breaks<-quantile(z, probs = seq(0, .95,length=25))
              a<-outer(z,breaks,"<")
              res.mag<-apply(a,1,sum)
              plot(out$dat$ma$test.xy,col=beachcolours(heightrange=c(min(res.mag),max(res.mag)),sealevel=mean(res.mag),ncolours=length(table(res.mag)))[res.mag],cex=4,pch=19)
              x<-out$dat$ma$test.xy[,1]
              y<-out$dat$ma$test.xy[,2]
              a<-loess(z~x*y)
              x.lim<-rep(seq(from=min(out$dat$ma$test.xy[,1]),to=max(out$dat$ma$test.xy[,1]),length=100),each=100)
              y.lim<-rep(seq(from=min(out$dat$ma$test.xy[,2]),to=max(out$dat$ma$test.xy[,2]),length=100),times=100)
              z<-predict(a,newdata=cbind("x"=x.lim,"y"=y.lim))
              z[z>z.lim[2]]<-NA
              z[z<z.lim[1]]<-NA
              breaks<-quantile(na.omit(z), probs = seq(0, .95,length=25))
              a<-outer(z,breaks,"<")
              res.mag<-apply(a,1,sum)
              z<-matrix(data=z,ncol=100,nrow=100,byrow=TRUE)
              res.mag<-matrix(data=res.mag,ncol=100,nrow=100,byrow=TRUE)
                 image(x=seq(from=min(x.lim),to=max(x.lim),length=100),y=seq(from=min(y.lim),to=max(y.lim),length=100),
                  z=res.mag,
                  col=beachcolours(heightrange=c(min(na.omit(res.mag)),max(na.omit(res.mag))),sealevel=mean(na.omit(res.mag)),
                  ncolours=length(table(na.omit(res.mag)))),xlab="Latitude",ylab="Longitude",
                  main="Smoothed deviance residuals over space")

                  points(out$dat$ma$train.xy,pch=19,cex=1)
              #plot(unique(x.lim),apply(z,2,sum))
              #plot(unique(y.lim),apply(z,1,sum))

              mod.resids<-residuals(out$mods$final.mod,type="deviance")
              plot(pred,(auc.data$pres.abs-pred),xlab="Predicted Values",ylab="Residuals",main="Residuals vs Fitted")
              panel.smooth(pred,(auc.data$pres.abs-pred))
              plot(pred,sqrt(2*dev.contrib),ylab="sqrt(Std.deviance residuals)",xlab="Predicted Values",main="Scale Location")
              panel.smooth(pred,sqrt(2*dev.contrib))
              qqnorm((sqrt(2*dev.contrib)-mean(sqrt(2*dev.contrib))),ylab="Std.Deviance residuals")
              abline(0,1)

        graphics.off()
        return(list(null.dev=null.dev,dev.fit=dev.fit,dev.exp=dev.exp,pct.dev.exp=pct.dev.exp,correlation=correlation,
          null.dev.train=null.dev.train,dev.fit.train=dev.fit.train,dev.exp.train=dev.exp.train,pct.dev.exp.train=pct.dev.exp.train,correlation.train=correlation.train))

     }else{
     resid.image(calc.dev(auc.data$pres.abs, pred, weight, family="poisson")$dev.cont,pred,
                auc.data$pres.abs,out$dat$ma$train.xy[,1],out$dat$ma$train.xy[,2],"poisson",out$input$output.dir)
     jpeg(file=plotname)
        dev.contrib<-calc.dev(auc.data$pres.abs, pred, weight, family="poisson")$dev.cont
   par(mfrow=c(2,2))
              mod.resids<-residuals(out$mods$final.mod,type="deviance")
              plot(pred,(auc.data$pres.abs-pred),xlab="Predicted Values",ylab="Residuals",main="Residuals vs Fitted")
              panel.smooth(pred,(auc.data$pres.abs-pred))
              plot(pred,sqrt(2*dev.contrib),ylab="sqrt(Std.deviance residuals)",xlab="Predicted Values",main="Scale Location")
              panel.smooth(pred,sqrt(2*dev.contrib))
              qqnorm((sqrt(2*dev.contrib)-mean(sqrt(2*dev.contrib))),ylab="Std.Deviance residuals")
              abline(0,1)

        graphics.off()

         capture.output(cat("\n\nEvaluation Statistics applied to test split:\n",
                       "\n\t Correlation Coefficient      : ",cor.test(pred,auc.data$pres.abs)$estimate,
                       "\n\t NULL Deviance                : ",null.dev,
                       "\n\t Fit Deviance                 : ",dev.fit,
                       "\n\t Explained Deviance           : ",dev.exp,
                       "\n\t Percent Deviance Explained   : ",pct.dev.exp,
                       file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE))

        return(list(null.dev=null.dev,dev.fit=dev.fit,dev.exp=dev.exp,pct.dev.exp=pct.dev.exp,correlation=correlation))
        }
}
