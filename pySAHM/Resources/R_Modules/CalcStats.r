calcStat<-function(x,family){

    auc.data<-data.frame(ID=1:nrow(x$dat),pres.abs=x$dat[,1],pred=x$pred)
    p.bar <- sum(auc.data$pres.abs * x$weight) / sum(x$weight)
       n.pres=sum(auc.data$pres.abs>=1)
        n.abs=nrow(auc.data)-n.pres
        null.dev=calc.dev(auc.data$pres.abs, rep(p.bar,times=length(auc.data$pres.abs)), x$weight, family=out$input$model.family)$deviance #*nrow(x$dat)
        dev.fit=calc.dev(auc.data$pres.abs, x$pred, x$weight, family=family)$deviance #*nrow(x$dat) Elith does not include this it might cause a weighting issue when averaging I'm not sure if I should include it
        dev.exp=null.dev - dev.fit
        pct.dev.exp=dev.exp/null.dev*100
        correlation=cor(auc.data$pres.abs,x$pred)
        #have to use roc here because auc in the PresenceAbsence package incorretly assumes that the value must be greater than .5
        #this isn't necessarily true for an independent evaluation set
        auc.fit<-roc(auc.data$pres.abs,auc.data$pred)
        calibration.stats<-calibration(auc.data$pres.abs, x$pred, family =family)
        
        if(family%in%c("binomial","bernoulli")){
            cmx <- cmx(auc.data,threshold=x$thresh)
            PCC <- pcc(cmx,st.dev=F)*100
            SENS <- sensitivity(cmx,st.dev=F)
            SPEC <- specificity(cmx,st.dev=F)
            KAPPA <- Kappa(cmx,st.dev=F)
            TSS <- SENS+SPEC-1
            return(list(n.pres=n.pres,n.abs=n.abs,null.dev=null.dev,dev.fit=dev.fit,dev.exp=dev.exp,pct.dev.exp=pct.dev.exp,correlation=correlation,auc.data=auc.data,
            auc.fit=auc.fit,Cmx=cmx,Pcc=PCC,Sens=SENS,Specf=SPEC,Kappa=KAPPA,Tss=TSS,calibration.stats=calibration.stats,thresh=x$thresh))
          }

        if(family=="poisson"){
           prediction.error<-sum((auc.data$pres.abs-auc.data$pred)^2)
            return(list(n.pres=n.pres,n.abs=n.abs,null.dev=null.dev,dev.fit=dev.fit,dev.exp=dev.exp,pct.dev.exp=pct.dev.exp,correlation=correlation,auc.data=auc.data,
            auc.fit=auc.fit,prediction.error=prediction.error,calibration.stats=calibration.stats))
            }

    }