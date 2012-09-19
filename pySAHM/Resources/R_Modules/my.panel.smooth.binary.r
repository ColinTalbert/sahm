my.panel.smooth<-function (x, y, col = par("col"), bg = NA, pch = par("pch"),famly="binomial",
    cex = 1, col.smooth = "red", span = 2/3, iter = 3, weights=rep(1,times=length(y)),cex.mult,Ylab,plot.it=TRUE,lin=1,...)
{
#This function fits a gam to show the relationship between a binary response and the specified predictor
#similar to a lowess smooth but appropraite for binary response.  Occasionally gam fails or doesn't converge
#this is indicated by the null deviance being less than the fit deviance.  When this occurs I instead fit a glm
#Weights have to be set here so that the relationship is clear unfortunately the intercept isn't correct hopefully
#this won't be needed once weights are accepted modified to return values for the csv in Colin's widget
#Written by Marian Talbert 5/22/2012
     family<-switch(famly,
            "binomial"=binomial,
            "poisson"=poisson)
          
    o<-order(x)
    x<-x[o]
    y<-y[o]
    col<-col[o]
    bg<-bg[o]
      if(sum(y==0)/sum(y==1)>1.2) wgt<-c(sum(y==1)/sum(y==0),1)[factor(y,levels=c(0,1))]
      else wgt<-rep(1,times=length(y))
    
    if(!is.factor(x)) {
         options(warn=2)
          g<-try(gam(y~s(x,2),weights=wgt,family=family),silent=TRUE)
          options(warn=-1)
          dev.broke<-try((1-g$dev/g$null.deviance)<0,silent=TRUE)
              if(class(dev.broke)=="try-error") dev.broke=TRUE
          if("try-error"%in%class(g) | dev.broke){
              gam.failed=TRUE
              g<-glm(y~x+x^2,weights=wgt,family=family)
              y.fit<-predict(g,type="response")
          }  else {
              y.fit<-predict.gam(g,type="response")
              gam.failed=FALSE
          }
     } else{
        g<-glm(y~x,weights=wgt,family=family)
              y.fit<-predict(g,type="response")
            
             if(plot.it){
             barplot(prop.table(table(y,x),margin=2),main="Proportion of Absence by Category",col=c("blue","red"),...)
              mtext(paste("% dev exp ",round(100*(1-g$dev/g$null.deviance),digits=1),sep=""),side=2,line=lin,cex=cex.mult*.7)
              }
             return(100*(1-g$dev/g$null.deviance)) 
     } 
 
    if(plot.it){
    plot(x,y,ylab="",xlab="",type="n",cex.axis=.7*cex.mult)
    title(main=paste(ifelse(gam.failed,"GLM","GAM")," showing predictor response relationship",sep=""),cex.main=.8*cex.mult)
          if(famly=="poisson") 
              points(x, y, pch = pch,bg="black",col="black",cex = cex*cex.mult)
          else points(x, y, pch = pch,bg=c("blue","red")[factor(y,levels=c(0,1))],col=c("blue4","red4")[factor(y,levels=c(0,1))],cex = cex*cex.mult)
          segments(x0=x[1:(length(x)-1)],y0=y.fit[1:(length(x)-1)],x1=x[2:length(x)],y1=y.fit[2:length(x)],col="red",cex=3*cex.mult,lwd=cex.mult)
          if(missing(Ylab)) mtext(paste("% dev exp ",round(100*(1-g$dev/g$null.deviance),digits=1),sep=""),side=2,line=lin,cex=cex.mult*.7)
          return(gam.failed)
    } else return(100*(1-g$dev/g$null.deviance)) 
    
}

