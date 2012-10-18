VariableImportance<-function(Model,out,auc){
    #this relatively complicated structure is used for cross validation variable importance plots
 
    auc.mat<-matrix(nrow=length(out$mods$vnames),ncol=length(out$dat$ma))
    #remove the response colum
    f<-function(d.frame)d.frame$dat[,-1]
    dat<-lapply(out$dat$ma,f)
    resp<-lapply(out$dat$ma,"[",1)
    auc<-unlist(auc,recursive=TRUE)
     
    for(j in 1:length(dat)){
      for (i in 1:length(out$mods$vnames)){
       indx<-match(out$mods$vnames[i],names(dat[[j]]))
       Dat<-dat[[j]]
       Dat[,indx]<-Dat[sample(1:dim(dat[[j]])[1]),indx]
       new.pred<-as.vector(pred.fct(model=out$mods$final,x=Dat,Model=out$input$script.name))
       auc.mat[i,j]<-auc[j]-auc(data.frame(cbind(seq(1:nrow(dat[[j]])),resp[[j]][1]$resp,new.pred)))[1,1]
    }
   #auc.mat[,j]<-auc.mat[,j]/sum(auc.mat[,j])
   }
    
    rownames(auc.mat)<-out$mods$vnames
     for(k in 1:length(out$mods$vnames)){
                         if((lng<-nchar(rownames(auc.mat))[k])>=20) rownames(auc.mat)[k]<-paste(substr(rownames(auc.mat)[k],1,17),"\n",substr(rownames(auc.mat)[k],18,lng),sep="")
                     }
   #if cross validation we need to avg across folds otherwise plot for each
   #order by the best in the train split
  
   xright<-as.matrix(auc.mat[order(apply(auc.mat,1,mean),decreasing=FALSE),])
   ymiddle=seq(from=0,to=length(out$mods$vnames),length=nrow(xright))
  offSet<-.5 

######################## copied from append out
  par(mar=c(5,17,4,2))
    plot(c(min(0,min(auc.mat)),(max(auc.mat)+.1)),y=c(-.5,(length(out$mods$vnames)+.5)),type="n",xlab="Importance",main="Relative Importance \n based on AUC drop when permuted",ylab="",yaxt="n",cex.lab=1.4)
    grid()
      if(out$dat$split.type!="crossValidation"){
         rect(xleft=0,ybottom=ymiddle,xright=xright[,ncol(xright)],ytop=ymiddle+offSet,col="blue",lwd=2)
      }                         
     if(out$dat$split.type=="test"){
        rect(xleft=0,ybottom=ymiddle-offSet,xright=xright[,1],ytop=ymiddle,col="lightblue",lwd=2)
        legend("bottomright" ,legend=c("train","test"),fill=c("blue","lightblue"),bg="white",cex=2)
      }                         
      if(out$dat$split.type=="crossValidation"){ 
        auc.mat<-auc.mat[order(auc.mat[,ncol(auc.mat)],decreasing=FALSE),]
        boxplot(t(auc.mat[,1:(ncol(auc.mat)-1)]),horizontal =TRUE,add=TRUE,at=ymiddle,yaxt="n",col="lightblue")
        points(y=ymiddle,x=auc.mat[,ncol(auc.mat)],cex=3,pch=8,lwd=3,col="darkslateblue")
        legend(x="bottomright",legend=c("CV","Train"),pch=c(22,8),pt.cex=c(3,3.5),pt.lwd=c(2,3),pt.bg=c("lightblue","darkslateblue"),col=c("black","darkslateblue"),cex=1.5)
      }
 ############################### copied from appendOut 
    Offset=ifelse(out$dat$split.type=="none",.25,0)
    axis(2,at=seq(from=0,to=length(out$mods$vnames),length=length(out$mods$vnames))+Offset,labels=rownames(xright),las=2,cex=.7)
    title(ylab="Variables",line=15,cex.lab=1.4,font.lab=2)
}