VariableImportance<-function(Model,out,auc){
    #this relatively complicated structure is used for cross validation variable importance plots
     
    cor.mat<-matrix(nrow=length(out$mods$vnames),ncol=length(out$dat$ma))
    #remove the response colum
    dat<-lapply(out$dat$ma,FUN=function(d.frame)d.frame$dat[,-1])
    resp<-lapply(out$dat$ma,"[",1)
    pred<-lapply(out$dat$ma,FUN=function(lst) lst[which(names(lst)=="pred",arr.ind=TRUE)]) 
    #for random forest I switch to in bag predictions here since I can't calculate oob predictions after permuting a predictor
    if(out$input$script.name=="rf") pred$train$pred<-pred.fct(model=out$mods$final,x=out$dat$ma$train$dat,Model=out$input$script.name)
    for(j in 1:length(dat)){
      for (i in 1:length(out$mods$vnames)){
       indx<-match(out$mods$vnames[i],names(dat[[j]]))
       Dat<-dat[[j]]
       Dat[,indx]<-Dat[sample(1:dim(dat[[j]])[1]),indx]
       new.pred<-as.vector(pred.fct(model=out$mods$final,x=Dat,Model=out$input$script.name))
       cor.mat[i,j]<-1-cor(unlist(pred[[j]]),new.pred)
    }
   }
    
    rownames(cor.mat)<-out$mods$vnames
     for(k in 1:length(out$mods$vnames)){
                         if((lng<-nchar(rownames(cor.mat))[k])>=20) rownames(cor.mat)[k]<-paste(substr(rownames(cor.mat)[k],1,17),"\n",substr(rownames(cor.mat)[k],18,lng),sep="")
                     }
   #if cross validation we need to avg across folds otherwise plot for each
   #order by the best in the train split
  
   xright<-as.matrix(cor.mat[order(apply(cor.mat,1,mean),decreasing=FALSE),])
   ymiddle=seq(from=0,to=length(out$mods$vnames),length=nrow(xright))
  offSet<-.5 

######################## copied from append out
  par(mar=c(5,17,4,2))
    plot(c(min(0,min(cor.mat)),(max(cor.mat)+.1)),y=c(-.5,(length(out$mods$vnames)+.5)),type="n",xlab="Importance",main="Importance using one minus the correlation \nof predictions with and without permutation of the given independent variable",ylab="",yaxt="n",cex.lab=1.4)
    grid()
      if(out$dat$split.type!="crossValidation"){
         rect(xleft=0,ybottom=ymiddle,xright=xright[,ncol(xright)],ytop=ymiddle+offSet,col="blue",lwd=2)
      }                         
     if(out$dat$split.type=="test"){
        rect(xleft=0,ybottom=ymiddle-offSet,xright=xright[,1],ytop=ymiddle,col="lightblue",lwd=2)
        legend("bottomright" ,legend=c("train","test"),fill=c("blue","lightblue"),bg="white",cex=2)
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