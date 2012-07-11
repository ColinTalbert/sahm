VariableImportance<-function(Model,out){

    attach(out$dat$ma$train)
    on.exit(detach(out$dat$ma$train))
    auc.vect<-vector()
    dat<-dat[,-1] #remove the response col

    for (i in 1:length(out$mods$vnames)){
       indx<-match(out$mods$vnames[i],names(dat))
       Dat<-dat
       Dat[,indx]<-Dat[sample(1:dim(dat)[1]),indx]
       new.pred<-as.vector(pred.fct(model=out$mods$final,x=Dat,Model=out$input$script.name))
       auc.vect[i]<-auc(data.frame(cbind(seq(1:nrow(dat)),resp,new.pred)))[1]
    }
    auc.vect<-unlist(auc.vect)
    full.auc<-auc(data.frame(cbind(seq(1:nrow(dat)),resp,pred)))[1]
    names(auc.vect)<-out$mods$vnames
    xright<-c(sort(pmax(as.numeric(full.auc)-auc.vect,0),decreasing=FALSE),full.auc)
    names(xright)[length(xright)]<-"Full AUC"
    par(mar=c(5,9,4,2))
    plot(c(0,full.auc),y=c(0,length(out$mods$vnames)+1),type="n",xlab="Importance",main="AUC Drop when Predictor is Permuted",ylab="",yaxt="n",cex.lab=1.4)
    grid()
    rect(xleft=rep(0,times=(length(out$mods$vnames)+1)),ybottom=(seq(from=0,to=(length(out$mods$vnames)))),
         xright=xright,ytop=seq(1:(length(out$mods$vnames)+1)),col="blue")
    axis(2,at=seq(from=.5,to=(length(out$mods$vnames)+.5),by=1),labels=names(xright),las=2,cex=.7)
    title(ylab="Variables",line=7,cex.lab=1.4)
}