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
 
      if(Model%in%c("brt","mars"))  nvar <- nrow(out$mods$summary)
      if(Model=="glm")              nvar <- out$mods$n.vars.final-length(grep(":", attr(terms(formula(out$mods$final.mod)),"term.labels")))
      if(Model=="rf")               nvar <- nrow(out$mods$summary)           
            pcol <- min(ceiling(sqrt(nvar)),4)
            prow <- min(ceiling(nvar/pcol),3)

            pdf(paste(bname,"_response_curves.pdf",sep=""),width=11,height=8.5,onefile=T)
                par(oma=c(2,2,4,2))
                for(i in 1:length(out$mods$final.mod)){
                    if(Model=="brt") out$mods$r.curves[[i]] <- gbm.plot(out$mods$final.mod[[i]],plotit=F,plot.layout=c(prow,pcol))
                    if(Model=="mars"){
                         out$mods$r.curves[[i]] <- mars.plot(out$mods$final.mod[[i]],plot.it=F)
                         out$mods$summary$Var<- rownames(out$mods$summary)
                    }     
                 } 
                 
                 col.scale<-rainbow(length(out$mods$final.mod)) 
                  
                for (i in 1:nrow(out$mods$summary)){
                     resp.indx<-lapply(out$mods$r.curves,function(lst,x){match(x,lst$names)},x=out$mods$summary$Var[i])
                     extract.rsp<-function(a,b){if(!is.na(b)) range(a$resp[[b]])}
                     resp.rng<-mapply(extract.rsp,a=out$mods$r.curve,b=resp.indx)
                       #
                     x.lims<-range(dat[,match(out$mods$summary$Var[i],names(dat))])
                     y.lims<-range(resp.rng)
                     if(Model=="brt") 
                         plot(x.lims,y.lims,type="n",xlab=paste(out$mods$summary$Var[i],"  (",round(out$mods$summary$x[i],digits=2),"%)",sep=""),
                         ylab="fitted function")
                     else plot(x.lims,y.lims,type="n",xlab=out$mods$summary$Var[i],ylab="fitted function")   
                        for(j in 1:length(out$mods$final.mod)){
                           k<-match(out$mods$summary$Var[i],out$mods$r.curves[[j]]$names)
                           if(!is.na(k)){
                               lines(x=out$mods$r.curves[[j]]$preds[[k]],y=out$mods$r.curves[[j]]$resp[[k]],type="l",col=col.scale[j],lwd=1.3)
                           }
                     }
                 }
                par(mfrow=c(1,1))
                #for(i in 1:min(nrow(int),2)) gbm.perspec(fit,int$var1.index[i],int$var2.index[i])
                if(!out$input$PsdoAbs & Model=="brt") {
                    if(!is.null(out$mods$interactions)){
                        for(i in 1:ifelse(is.null(nrow(out$mods$interactions)),1,nrow(out$mods$interactions))){
                            gbm.perspec(out$mods$final.mod[[1]],out$mods$interactions[[1]]$v1[i],out$mods$interactions[[1]]$v2[i],pas.data=out$dat$ma$train)
                               mtext(paste("BRT interaction plots for",basename(ma.name)),outer=T,side=3,cex=1.5) 
                        }
                    }
                }
            graphics.off()
           
    }
 
 
        pcol <- min(ceiling(sqrt(nvar)),4)
        prow <- min(ceiling(nvar/pcol),3)
                    term=seq(1:length(attr(terms(formula(out$mods$final.mod)),"term.labels")))
                  if(length(grep(":", attr(terms(formula(out$mods$final.mod)),"term.labels")))>0) term<-term[-c((grep(":", attr(terms(formula(out$mods$final.mod)),"term.labels"))))]
                 
        pdf(paste(bname,"_response_curves.pdf",sep=""),width=11,height=8.5,onefile=T)
          #svg(paste(bname,"_response_curves.svg",sep=""),width=11,height=8.5,onefile=T)
            par(oma=c(2,2,4,2),mfrow=c(prow,pcol))
            r.curves <-my.termplot(out$mods$final.mod,plot.it=T,terms=term,rug=TRUE)
            mtext(paste("GLM response curves for",basename(ma.name)),outer=T,side=3,cex=1.3)
           
           #############################
            par(mfrow=c(1,1))
            graphics.off()
        } else r.curves<-my.termplot(out$mods$final.mod,plot.it=F)

     }
if(Model=="rf"){
 if(responseCurveForm=="pdf"){
                    nvar <- nrow(out$mods$summary)
                    pcol <- min(ceiling(sqrt(nvar)),4)
                    prow <- min(ceiling(nvar/pcol),3)
                    pdf(paste(bname,"_response_curves.pdf",sep=""),width=11,height=8.5,onefile=T)
                    par(oma=c(2,2,4,2),mfrow=c(prow,pcol))
                    } 
                    
#this little section is borrowed from BIOMOD because rf partial plot 
#does something odd with the y axis
dat<-out$dat$ma$train$dat[,-1]
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
     for (i in 1:ncol(dat)) {
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
            Xf<-matrix(nrow=nrow(Xp1),ncol=length(out$mods$final.mod))
            for(j in 1:length(out$mods$final.mod)){
                  Xf[,j] <- predict(out$mods$final.mod[[j]], as.data.frame(Xp1), type = "prob")[,2]
             }
                   plot(Xp1[, i],apply(Xf,1,mean), ylim = c(0, 1), xlab = "",
                  ylab = "", type = "l", main = names(dat)[i])       
           } 
            graphics.off()

    }
    
}