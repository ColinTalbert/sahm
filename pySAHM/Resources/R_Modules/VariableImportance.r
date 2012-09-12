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
       auc.vect[i]<-auc(data.frame(cbind(seq(1:nrow(dat)),resp,new.pred)))[1,1]
    }
   
    full.auc<-auc(data.frame(cbind(seq(1:nrow(dat)),resp,pred)))[1,1]
    auc.vect<-full.auc-auc.vect
    #now normalize the relative influence vector
    auc.vect<-auc.vect/sum(auc.vect)
    names(auc.vect)<-out$mods$vnames
     for(k in 1:length(auc.vect)){
                         if((lng<-nchar(names(auc.vect))[k])>=20) names(auc.vect)[k]<-paste(substr(names(auc.vect)[k],1,17),"\n",substr(names(auc.vect)[k],18,lng),sep="")
                     }
   
    xright<-c(sort(pmax(auc.vect,0),decreasing=FALSE))
    par(mar=c(5,17,4,2))
    plot(c(0,1),y=c(0,length(out$mods$vnames)),type="n",xlab="Importance",main="Relative Importance \n based on AUC drop when permuted",ylab="",yaxt="n",cex.lab=1.4)
    grid()
    rect(xleft=rep(0,times=(length(out$mods$vnames))),ybottom=(seq(from=.25,to=(length(out$mods$vnames))-.75,by=1)),
         xright=xright,ytop=seq(from=.75,to=(length(out$mods$vnames)-.25),by=1),col="blue")
    axis(2,at=seq(from=.5,to=(length(out$mods$vnames)-.5),by=1),labels=names(xright),las=2,cex=.7)
    title(ylab="Variables",line=15,cex.lab=1.4,font.lab=2)
}