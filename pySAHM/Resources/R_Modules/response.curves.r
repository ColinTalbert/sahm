###############################################################################
##
## Copyright (C) 2010-2012, USGS Fort Collins Science Center. 
## All rights reserved.
## Contact: talbertc@usgs.gov
##
## This file is part of the Software for Assisted Habitat Modeling package
## for VisTrails.
##
## "Redistribution and use in source and binary forms, with or without 
## modification, are permitted provided that the following conditions are met:
##
##  - Redistributions of source code must retain the above copyright notice, 
##    this list of conditions and the following disclaimer.
##  - Redistributions in binary form must reproduce the above copyright 
##    notice, this list of conditions and the following disclaimer in the 
##    documentation and/or other materials provided with the distribution.
##  - Neither the name of the University of Utah nor the names of its 
##    contributors may be used to endorse or promote products derived from 
##    this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
## THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
## PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
## CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
## EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
## PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
## OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
## OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
## ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
##
## Although this program has been used by the U.S. Geological Survey (USGS), 
## no warranty, expressed or implied, is made by the USGS or the 
## U.S. Government as to the accuracy and functioning of the program and 
## related program material nor shall the fact of distribution constitute 
## any such warranty, and no responsibility is assumed by the USGS 
## in connection therewith.
##
## Any use of trade, firm, or product names is for descriptive purposes only 
## and does not imply endorsement by the U.S. Government.
###############################################################################

response.curves<-function(out,Model,pred.dat=NULL,cv=FALSE){
      attach(out$input)
      on.exit(detach(out$input))
      bname<-out$dat$bname
        
      if(Model%in%c("brt","mars","rf"))  nvar <- nrow(out$mods$summary)
      if(Model%in%c("maxent","udc")) nvar <- out$mods$n.vars.final
      if(Model=="glm")              nvar <- out$mods$n.vars.final-length(grep(":", attr(terms(formula(out$mods$final.mod[[1]])),"term.labels")))
               
            pcol <- ceiling(sqrt(nvar))
            prow <- ceiling(nvar/pcol)
      
                     
        #this little section is borrowed from BIOMOD because rf partial plot 
        #does something odd with the y axis
        dat<-out$dat$ma$train$dat[,-1]
        resp<-out$dat$ma$train$dat[,1]
        Xp <- as.data.frame(matrix(NA, nc = ncol(dat), nr = nrow(dat),
        dimnames = list(NULL, colnames(dat))))
        for (i in 1:ncol(dat)) {
            if (is.numeric(dat[, i])) {
                Xp[, i] <- mean(dat[, i])
            }
            else {
                Xp[, i] <- as.factor(rep(names(which.max(summary(dat[,
                    i]))), nrow(dat)))
                levels(Xp[, i]) <- levels(dat[, i])
            }
    }
       
     dir.create(file.path(out$input$output.dir,"responseCurves"))
     rsp.dat<-NA
              
      for (k in c(1,2)){
          if(k==1){ jpeg(file.path(out$input$output.dir,"responseCurves","all_response_curves.jpg"),width=2000,height=2000,quality=100)
                    par(oma=c(2,8,4,2),mfrow=c(prow,pcol))}                   
         for (i in sort(match(out$mods$vnames,names(dat)))) {
                if (k==2){ jpeg(filename=file.path(out$input$output.dir,"responseCurves",paste(names(dat)[i],".jpg",sep="")),width=2000,height=2000,quality=100)
                 par(mar=c(7,10,8,1))
                }
                if (!is.factor(dat[, i])) {
                    xr <- range(dat[, i])
                    Xp1 <- Xp
                    Xp1[, i] <- seq(xr[1], xr[2], len = nrow(dat))
                }
                else {
                    Xp1 <- Xp
                    Nrepcat <- floor(nrow(dat)/length(levels(dat[,
                      i])))
                    Xp1[, i] <- as.factor(c(rep(levels(dat[, i])[1],
                      nrow(dat) - (Nrepcat * length(levels(dat[,
                        i])))), rep(levels(dat[, i]), each = Nrepcat)))
                }
                Xf<-matrix(nrow=nrow(Xp1),ncol=max(length(out$mods$final.mod),1))
                for(j in 1:max(1,length(out$mods$final.mod))){
                      Xf[,j] <- pred.fct(out$mods$final.mod[[j]], as.data.frame(Xp1),Model)
                 }
                      y.lim<-c(0,1)
                      #I'm not quite sure what the logical limits are for poisson but 0 1 doesn't work I might be able to take the max
                      #of the predicted values but I'm not sure this would work for every situation
                      if(out$input$model.family=="poisson") y.lim=range(apply(Xf,1,mean))
                       plot(Xp1[, i],apply(Xf,1,mean), ylim = y.lim, xlab = "",
                      ylab = "", type = "l", main = names(dat)[i],lwd=ifelse(k==1,4,6),cex=3,cex.main=6,xaxt="n",yaxt="n")
                        if(k==2) mtext("Predicted Value", side=2,line=6,cex=6)
                        axis(1,labels=FALSE)
                        axis(2,labels=FALSE)
                        axis(1,line=2,lwd=0,cex.axis=4)
                        axis(2,line=1,lwd=0,cex.axis=4)

                      rug(dat[resp==1,i],col="red",lwd=2)
                      rug(dat[resp==0,i],col="blue",lwd=2)
               if(k==2) graphics.off() 
               if(k==2) {if(i==min(sort(match(out$mods$vnames,names(dat))))) rsp.dat<-cbind(Xp1[, i],apply(Xf,1,mean))
                        else rsp.dat<-cbind(rsp.dat,Xp1[, i],apply(Xf,1,mean))
                            colnames(rsp.dat)[(ncol(rsp.dat)-1):(ncol(rsp.dat))]<-c(names(dat)[i],basename(out$input$output.dir))  
                        }             
               } 
         if(k==1){
         mtext("Predicted Value",side=2,cex=5,line=2,outer=TRUE)
         graphics.off()}     
       }
       write.csv(rsp.dat,file=file.path(out$dat$bnameExpanded,"ResponseCurves.csv"))
                
  }         
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
      