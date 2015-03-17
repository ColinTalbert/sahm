VariableImportance<-function(Model,out,auc){
    #this relatively complicated structure is used for cross validation variable importance plots
    #This function can produce some strange results for random Forest if it is seriously overparameterized
    #in which case permutation in any single value might not lead to a drop in auc so all variables have
    #approximately zero importance all other models drop variables and haven't shown this
    cor.mat<-matrix(nrow=length(out$mods$vnames),ncol=length(out$dat$ma))
    #remove the response colum
     if(out$input$script.name=="rf") {
        trainPred<-pred.fct(model=out$mods$final,x=out$dat$ma$train$dat,Model=out$input$script.name)
        auc$train<-roc(out$dat$ma$train$dat[,1],trainPred)
     }
     else trainPred=out$dat$ma$train$pred
       #I have to add 1 to the drop in AUC to ensure it's greater than zero then I can normalize
       
    cor.mat[,ncol(cor.mat)]<-unlist(auc$train)-PermutePredict(out$mods$vnames,dat=out$dat$ma$train$dat[,-1],pred=trainPred,out$mods$final.mod,Model,resp=out$dat$ma$train$dat[,1])
    #cor.mat[,ncol(cor.mat)]<-cor.mat[,ncol(cor.mat)]/sum(cor.mat[,ncol(cor.mat)])
    if(out$dat$split.type%in%c("eval","test"))
         cor.mat[,1]<-unlist(auc$test)-PermutePredict(out$mods$vnames,dat=out$dat$ma$test$dat[,-1],pred=out$dat$ma$test$pred,out$mods$final.mod,Model,resp=out$dat$ma$test$dat[,1])
           #I've decided not to normalize the variable importance but to give the values so people can do what they want
          # cor.mat[,1]<-cor.mat[,1]/sum(cor.mat[,1])
   
    if(out$dat$split.type=="crossValidation")
         cor.mat[,1:(ncol(cor.mat)-1)]<- -t(apply(out$cv$cor.mat,1,"-",as.vector(unlist(auc)[1:(ncol(cor.mat)-1)])))

    colnames(cor.mat)<-names(out$dat$ma)
    rownames(cor.mat)<-out$mods$vnames

    #writing output to a csv for Cathrine, formalize this a bit more if we decide to keep it.
    variable.importance.csv<-file.path(out$dat$bnameExpanded,"VariableImportance.csv")  
    write.table(cbind(predictor=out$mods$vnames,cor.mat),file = variable.importance.csv,row.names=FALSE,col.names=TRUE,quote=FALSE,sep=",")
    
    
     for(k in 1:length(out$mods$vnames)){
                         if((lng<-nchar(rownames(cor.mat))[k])>=20) rownames(cor.mat)[k]<-paste(substr(rownames(cor.mat)[k],1,17),"\n",substr(rownames(cor.mat)[k],18,lng),sep="")
                     }
   #if cross validation we need to avg across folds otherwise plot for each
   #order by the best in the train split
   
   xright<-as.matrix(cor.mat[order(cor.mat[,ncol(cor.mat)],decreasing=FALSE),])
   ymiddle=seq(from=0,to=length(out$mods$vnames),length=nrow(xright))
  offSet<-.5 

######################## copied from append out
  par(mar=c(6,17,6,2))
    
    plot(c(min(0,min(cor.mat)),(max(cor.mat)+.1)),y=c(-.5,(length(out$mods$vnames)+.5)),type="n",xlab="Importance",
        main="Importance using the change in AUC\nwhen each predictor is permuted",ylab="",yaxt="n",cex.lab=3,cex.main=3,cex.axis=2)
    grid()
      if(out$dat$split.type!="crossValidation"){
         rect(xleft=0,ybottom=ymiddle,xright=xright[,ncol(xright)],ytop=ymiddle+offSet,col="blue",lwd=2)
      }                         
     if(out$dat$split.type=="test" | out$dat$split.type=="eval"){
        rect(xleft=0,ybottom=ymiddle-offSet,xright=xright[,1],ytop=ymiddle,col="lightblue",lwd=2)
        legend("bottomright" ,legend=c("train",ifelse(out$dat$split.type=="eval","eval. split","test")),fill=c("blue","lightblue"),bg="white",cex=2.5)
      }                         
      if(out$dat$split.type=="crossValidation"){ 
        cor.mat<-cor.mat[order(cor.mat[,ncol(cor.mat)],decreasing=FALSE),]
        boxplot(t(cor.mat[,1:(ncol(cor.mat)-1)]),horizontal =TRUE,add=TRUE,at=ymiddle,yaxt="n",col="lightblue",ylab="n")
        points(y=ymiddle,x=cor.mat[,ncol(cor.mat)],cex=3,pch=8,lwd=3,col="darkslateblue")
        legend(x="bottomright",legend=c("CV","Train"),pch=c(22,8),pt.cex=c(3,3.5),pt.lwd=c(2,3),pt.bg=c("lightblue","darkslateblue"),col=c("black","darkslateblue"),cex=2.5)
      }
 ############################### copied from appendOut 
    Offset=ifelse(out$dat$split.type=="none",.25,0)
    axis(2,at=seq(from=0,to=length(out$mods$vnames),length=length(out$mods$vnames))+Offset,labels=rownames(xright),las=2,cex=2.5,cex.lab=2.5,cex.axis=2.5)
    title(ylab="Variables",line=15,cex.lab=3,font.lab=2)
} 

PermutePredict<-function(pred.names,dat,pred,modelFit,Model,resp){

    AUC<-matrix(NA,nrow=length(pred.names),ncol=5) 
    
     for(j in 1:5){ #do the permutation 5 times to remove some of the random chance component
     for (i in 1:length(pred.names)){
           indx<-match(pred.names[i],names(dat))
           Dat<-dat
           Dat[,indx]<-Dat[sample(1:dim(dat)[1]),indx]
           options(warn=-1)
           new.pred<-as.vector(pred.fct(model=modelFit,x=Dat,Model=Model))
           #have to use ROC here because auc in presence absence incorrectly assumes auc will be greater than .5
           AUC[i,j]<-roc(resp,new.pred)
           options(warn=0)
           
        } }
        AUC<-apply(AUC,1,mean)
      return(AUC)
}   