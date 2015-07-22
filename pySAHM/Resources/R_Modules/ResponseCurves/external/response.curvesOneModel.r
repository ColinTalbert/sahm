response.curvesOneModel<-function(fitLst,model,vals=NULL){
    
        #How to check that models and data match
        dat<-fitLst$dat$ma$train$dat[,-1]
        resp<-fitLst$dat$ma$train$dat[,1]
        VarNames<-names(dat)
        myPredict <- function (x, y, ...) { 
          out <- predict(x, y, type='response', args=c("outputformat=logistic"), ...);
          return (out)
        }
        #response(m_me,  at=mean, expand=0, fun=myPredict)
        #response(m_glm, at=mean, expand=0, fun=myPredict)
        
         mins  <- sapply(dat, min,  na.rm=TRUE)
         maxs  <- sapply(dat, max,  na.rm=TRUE)
         means <- sapply(dat, mean, na.rm=TRUE)
         n <- 50
         
        if(is.null(vals)) vals<-matrix(means,nrow=1)
         else vals<-rbind(means,vals)
         
        
        # Cols<-c("red","blue","green","blueviolet","darkgoldenrod1","aquamarine","violetred","slateblue")
           
            par(mfrow=c(1,ncol(dat)),mar=c(0,0,0,0),oma=c(3,5,3,0),xpd=TRUE)
            y.lim<-c(0,1)
              nRow<-1
                
                 for (pIdx in 1:ncol(dat)) {
                            plotR<- (names(dat)[pIdx]%in%fitLst$mods$vnames)
                         for(v in 1:nrow(vals)){
                          test <- do.call("rbind", replicate(n, vals[v,], simplify=FALSE))
                          test[,pIdx] <- seq(mins[pIdx], maxs[pIdx], length.out=n)
                          test<-as.data.frame(test)
                          test<-rbind(test,vals[v,])
                          colnames(test)<-names(means)
                           Response<-pred.fct(fitLst$mods$final.mod, test,model)
                           lR<-length(Response)
                            if(v==1){
                            plot(test[1:(lR-1),pIdx],Response[1:(lR-1)], ylim = y.lim, xlab = "",
                                ylab = "", type = ifelse(plotR,"l","n"), lwd=2,cex=3,cex.main=3,cex.axis=1.2,yaxt=ifelse(pIdx==1,"s","n"),
                                xaxt="n",main="")
                              
                             }
                             if(v!=1 & plotR){
                             lines(test[1:(lR-1),pIdx],Response[1:(lR-1)], ylim = y.lim, xlab = "",
                                ylab = "", type = "l", lwd=2,cex=3,cex.main=3,cex.axis=1.2,yaxt=ifelse(pIdx==1,"s","n"),
                                xaxt="n",main="",col=Cols[v-1])
                                segments(x0=vals[v,pIdx],y0=0,y1=Response[lR],x1=vals[v,pIdx],col=Cols[v-1],lty=2)
                             }   
                                mtext(names(dat)[pIdx],line=1,cex=1.2)
                               # if(pIdx==1) mtext(model,side=2,outer=TRUE,at=seq(from=1/(2*nRow),to=(1-1/(2*nRow)),length=nRow)[j+1],line=3,cex=1.2)
                           }
                 } 
  }         
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
      