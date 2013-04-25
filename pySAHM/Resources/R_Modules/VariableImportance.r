VariableImportance<-function(Model,out,auc){
    #this relatively complicated structure is used for cross validation variable importance plots
    
    cor.mat<-matrix(nrow=length(out$mods$vnames),ncol=length(out$dat$ma))
    #remove the response colum
     if(out$input$script.name=="rf") trainPred<-pred.fct(model=out$mods$final,x=out$dat$ma$train$dat,Model=out$input$script.name)
     else trainPred=out$dat$ma$train$pred
    
    cor.mat[,ncol(cor.mat)]<-PermutePredict(out$mods$vnames,dat=out$dat$ma$train$dat[,-1],pred=trainPred,out$mods$final.mod,Model)
    if(out$dat$split.type%in%c("eval","test"))
         cor.mat[,1]<-PermutePredict(out$mods$vnames,dat=out$dat$ma$test$dat[,-1],pred=out$dat$ma$test$pred,out$mods$final.mod,Model)
    if(out$dat$split.type=="crossValidation")
         cor.mat[,1:(ncol(cor.mat)-1)]<-out$cv$cor.mat
   
    colnames(cor.mat)<-names(out$dat$ma)
    rownames(cor.mat)<-out$mods$vnames
    
    #writing output to a csv for Cathrine, formalize this a bit more if we decide to keep it.
    variable.importance.csv<-file.path(dirname(dirname(out$dat$bname)),paste("VariableImportance_",out$dat$split.type,".csv",sep=""))  
    write.table(rbind(c(paste(basename(dirname(out$dat$bname)),out$input$ma.name,sep="."),colnames(cor.mat)),
    cbind(rownames(cor.mat),cor.mat)),file =variable.importance.csv,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",",append=TRUE)
    write.table("",file =variable.importance.csv,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",",append=TRUE)
    
     for(k in 1:length(out$mods$vnames)){
                         if((lng<-nchar(rownames(cor.mat))[k])>=20) rownames(cor.mat)[k]<-paste(substr(rownames(cor.mat)[k],1,17),"\n",substr(rownames(cor.mat)[k],18,lng),sep="")
                     }
   #if cross validation we need to avg across folds otherwise plot for each
   #order by the best in the train split
   
   xright<-as.matrix(cor.mat[order(cor.mat[,ncol(cor.mat)],decreasing=FALSE),])
   ymiddle=seq(from=0,to=length(out$mods$vnames),length=nrow(xright))
  offSet<-.5 

######################## copied from append out
  par(mar=c(5,17,4,2))
  
    plot(c(min(0,min(cor.mat)),(max(cor.mat)+.1)),y=c(-.5,(length(out$mods$vnames)+.5)),type="n",xlab="Importance",main="Importance using one minus the correlation \nof predictions with and without permutation of the given independent variable",ylab="",yaxt="n",cex.lab=1.4)
    grid()
      if(out$dat$split.type!="crossValidation"){
         rect(xleft=0,ybottom=ymiddle,xright=xright[,ncol(xright)],ytop=ymiddle+offSet,col="blue",lwd=2)
      }                         
     if(out$dat$split.type=="test" | out$dat$split.type=="eval"){
        rect(xleft=0,ybottom=ymiddle-offSet,xright=xright[,1],ytop=ymiddle,col="lightblue",lwd=2)
        legend("bottomright" ,legend=c("train",ifelse(out$dat$split.type=="eval","eval. split","test")),fill=c("blue","lightblue"),bg="white",cex=2)
      }                         
      if(out$dat$split.type=="crossValidation"){ 
        cor.mat<-cor.mat[order(cor.mat[,ncol(cor.mat)],decreasing=FALSE),]
        boxplot(t(cor.mat[,1:(ncol(cor.mat)-1)]),horizontal =TRUE,add=TRUE,at=ymiddle,yaxt="n",col="lightblue")
        points(y=ymiddle,x=cor.mat[,ncol(cor.mat)],cex=3,pch=8,lwd=3,col="darkslateblue")
        legend(x="bottomright",legend=c("CV","Train"),pch=c(22,8),pt.cex=c(3,3.5),pt.lwd=c(2,3),pt.bg=c("lightblue","darkslateblue"),col=c("black","darkslateblue"),cex=1.5)
      }
 ############################### copied from appendOut 
    Offset=ifelse(out$dat$split.type=="none",.25,0)
    axis(2,at=seq(from=0,to=length(out$mods$vnames),length=length(out$mods$vnames))+Offset,labels=rownames(xright),las=2,cex=.7)
    title(ylab="Variables",line=15,cex.lab=1.4,font.lab=2)
} 

PermutePredict<-function(pred.names,dat,pred,modelFit,Model){
    cor.vect<-rep(NA,times=length(pred.names))
   
     for (i in 1:length(pred.names)){
           indx<-match(pred.names[i],names(dat))
           Dat<-dat
           Dat[,indx]<-Dat[sample(1:dim(dat)[1]),indx]
           options(warn=-1)
           new.pred<-as.vector(pred.fct(model=modelFit,x=Dat,Model=Model))
           options(warn=0)
           cor.vect[i]<-1-cor(pred,new.pred)  
        }
      return(cor.vect)
}   