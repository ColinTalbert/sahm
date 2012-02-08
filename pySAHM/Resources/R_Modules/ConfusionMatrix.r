confusion.matrix<-function(Stats,split.type){

     par(oma=c(5,3,5,3),mar=c(10,5,5,2))
  if(split.type=="none") lo<-layout(matrix(data=c(1,2), nrow=1, ncol=2), c(4,1.5), 1)
   else {lo<-layout(matrix(data=c(1,2,3), nrow=1, ncol=3), c(4.5,4.5,1), 1)
       if(split.type=="crossValidation"){
                                 a<-lapply(Stats[names(Stats)!="train"],function(lst){lst$Cmx})
                                  cmx<-a[[1]]
                                  for(i in 2:length(a)) cmx<-cmx+a[[i]]
                                  csv.stats<-apply(do.call("rbind",(lapply(Stats[names(Stats)!="train"],function(lst){
                                       return(c(lst$Sens,lst$Specf,lst$Pcc,lst$Kappa))}))),2,mean)
                                Stats$crossValidation<-list(Cmx=cmx,Sens=csv.stats[1],Specf=csv.stats[2],Pcc=csv.stats[3],Kappa=csv.stats[4])
                                Stats<-list("crossValidation"=Stats$crossValidation,"train"=Stats$train)
            }
          }

zlim<-c(min(unlist(lapply(Stats,function(lst){100*lst$Cmx/sum(lst$Cmx)}))),max(unlist(lapply(Stats,function(lst){100*lst$Cmx/sum(lst$Cmx)}))))

  for(i in length(Stats):1){
      image((1:2),c(2,4),matrix(data=c(100*Stats[[i]]$Cmx[2]/sum(Stats[[i]]$Cmx),100*Stats[[i]]$Cmx[4]/sum(Stats[[i]]$Cmx),100*Stats[[i]]$Cmx[1]/sum(Stats[[i]]$Cmx),100*Stats[[i]]$Cmx[3]/sum(Stats[[i]]$Cmx)),nrow=2),
               zlim=zlim,xaxt="n",yaxt="n",xlab="",
               ylab="",main=paste("Confusion matrix for \n", names(Stats)[i], "data",sep=" "),col=heat.colors(50)[50:1],cex.lab=1.5,cex.main=1.8)
          mtext("Absence",side=2,at=2,cex=1.2,lwd=1.3)
          mtext("Presence",side=2,at=4,cex=1.2,lwd=1.3)
          mtext("Presence",side=1,at=1,cex=1.2,line=.5,lwd=1.3)
          mtext("Absence",side=1,at=2,cex=1.2,line=.5,lwd=1.3)
          text(x=c(1,1,2,2),y=c(2,4,2,4),
          labels=c(paste(signif(100*Stats[[i]]$Cmx[2]/sum(Stats[[i]]$Cmx),digits=3),"%\n(",Stats[[i]]$Cmx[2],")",sep=""),
                   paste(signif(100*Stats[[i]]$Cmx[1]/sum(Stats[[i]]$Cmx),digits=3),"%\n(",Stats[[i]]$Cmx[1],")",sep=""),
                   paste(signif(100*Stats[[i]]$Cmx[4]/sum(Stats[[i]]$Cmx),digits=3),"%\n(",Stats[[i]]$Cmx[4],")",sep=""),
                   paste(signif(100*Stats[[i]]$Cmx[3]/sum(Stats[[i]]$Cmx),digits=3),"%\n(",Stats[[i]]$Cmx[3],")",sep="")),cex=2)
              abline(h=3,lwd=5)
              abline(v=1.5,lwd=5)
         mtext(paste("Pct Correctly Classified: ",signif(Stats[[i]]$Pcc,digits=3),
         "          Sensitivity: ",signif(Stats[[i]]$Sens,digits=3),
         "\n                Specificity:   ",signif(Stats[[i]]$Specf,digits=3),
         " Cohen's Kappa: ",signif(Stats[[i]]$Kappa,digits=3),sep=""),side=1,line=4,cex=1.1)
        box()
    }
  mtext("Observed",1,outer=TRUE,lwd=2,cex=2)
  mtext("Predicted",2,outer=TRUE,lwd=2,cex=2)
  
### color scale
 image(1,seq(from=zlim[1],to=zlim[2],length=50),
               matrix(data=seq(from=zlim[1],to=zlim[2],length=50), ncol=50,nrow=1),
              col=heat.colors(50)[50:1],
              xlab="",ylab="",zlim=zlim,
              xaxt="n")

}

