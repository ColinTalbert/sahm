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

 if(cv==TRUE){ #avoid producing pdfs here we just want the values for later
  debug.mode=FALSE
  responseCurveForm="no"
  }  else{
     attach(out$input)
      on.exit(detach(out$input))
      bname<-out$dat$bname
  }

 if(Model=="mars"){
        if(is.null(responseCurveForm)){
           responseCurveForm<-0}

          if(debug.mode | responseCurveForm=="pdf"){

            nvar <- nrow(out$mods$summary)
            pcol <- min(ceiling(sqrt(nvar)),4)
            prow <- min(ceiling(nvar/pcol),3)

            r.curves <- mars.plot(out$mods$final.mod,plot.layout=c(prow,pcol),file.name=paste(bname,"_response_curves.pdf",sep=""))

             } else r.curves<-mars.plot(out$mods$final.mod,plot.it=F)
     }

 if(Model=="glm") {if(debug.mode | responseCurveForm=="pdf"){
       
        nvar <- length(coef(out$mods$final.mod))-1
        pcol <- min(ceiling(sqrt(nvar)),4)
        prow <- min(ceiling(nvar/pcol),3)
                    term=seq(1:(length(coef(out$mods$final.mod))-1))
                  if(length(grep(":", names(coef(out$mods$final.mod))))>0) term<-term[-c((grep(":", names(coef(out$mods$final.mod)))-1))]
        pdf(paste(bname,"_response_curves.pdf",sep=""),width=11,height=8.5,onefile=T)
          #svg(paste(bname,"_response_curves.svg",sep=""),width=11,height=8.5,onefile=T)
            par(oma=c(2,2,4,2),mfrow=c(prow,pcol))
            r.curves <-my.termplot(out$mods$final.mod,plot.it=T,terms=NULL,rug=TRUE)
            mtext(paste("GLM response curves for",basename(ma.name)),outer=T,side=3,cex=1.3)
           ################### New work
           #starting by just looking at one element of the list later use an lapply
           # if(!is.null(out$cv$resp.curves))
            #        for(i in 1:length(r.curves$names)){
            #              plot(r.curves$pred[[i]],r.curves$resp[[i]],type="l")
             #             for(j in 1:length(out$cv$resp)){
              #             indx<-na.omit(match(r.curves$names[i],names(out$cv$resp[[i]]$pred)))
              #             lines(out$cv$resp.curves[[j]]$pred[indx][[1]],out$cv$resp.curves[[j]]$resp[indx][[1]],col="grey",lty="dotdash")
               #            }
               #          lines(r.curves$pred[[i]],r.curves$resp[[i]],type="l",col="red",lwd=2)
                #           }


                     # plot(r.curves$pred[1][[1]], r.curves$resp[1][[1]])
                    #      points(out$cv$resp.curves$"1"$pred[1][[1]][1],out$cv$resp.curves$"1"$resp[1][[1]][1])

           #############################
            par(mfrow=c(1,1))
            graphics.off()
        } else r.curves<-my.termplot(out$mods$final.mod,plot.it=F)

     }
if(Model=="rf"){

                r.curves <- list(names=row.names(out$mods$summary),preds=list(),resp=list())
                inc <- 10/length(r.curves$names)
                assign("r.curves",r.curves,envir=.GlobalEnv)

            if(is.null(responseCurveForm)){
              responseCurveForm<-0}

         if(debug.mode | responseCurveForm=="pdf"){
                    nvar <- nrow(out$mods$summary)
                    pcol <- min(ceiling(sqrt(nvar)),4)
                    prow <- min(ceiling(nvar/pcol),3)
                    pdf(paste(bname,"_response_curves.pdf",sep=""),width=11,height=8.5,onefile=T)
                    par(oma=c(2,2,4,2),mfrow=c(prow,pcol))
                    }
                for(i in 1:length(r.curves$names)){
                        assign("i",i,envir=.GlobalEnv)
                                x<-partialPlot(out$mods$final.mod,out$dat$Subset$dat,r.curves$names[i],n.pt=50,plot=T,main="",
                                        xlab=r.curves$names[i])
                            r.curves$preds[[i]] <- x$x
                            r.curves$resp[[i]] <- x$y
                        cat(paste("Progress:",round(70+i*inc,1),"%\n",sep=""));flush.console()  ### print time
                        }
                if(debug.mode) graphics.off()


    }
if(Model=="brt"){
        if(is.null(responseCurveForm)){
        responseCurveForm<-0}

        if(debug.mode | responseCurveForm=="pdf"){
            nvar <- nrow(out$mods$final.mod$contributions)
            pcol <- min(ceiling(sqrt(nvar)),4)
            prow <- min(ceiling(nvar/pcol),3)

            pdf(paste(bname,"_response_curves.pdf",sep=""),width=11,height=8.5,onefile=T)
                par(oma=c(2,2,4,2))
                out$mods$r.curves <- gbm.plot(out$mods$final.mod,plotit=T,plot.layout=c(prow,pcol))
                mtext(paste("BRT response curves for",basename(ma.name)),outer=T,side=3,cex=1.3)

                par(mfrow=c(1,1))
                #for(i in 1:min(nrow(int),2)) gbm.perspec(fit,int$var1.index[i],int$var2.index[i])
                if(!is.null(out$mods$interactions)){
                    for(i in 1:nrow(out$mods$interactions)) {gbm.perspec(out$mods$final.mod,out$mods$interactions$v1[i],out$mods$interactions$v2[i])
                    mtext(paste("BRT interaction plots for",basename(ma.name)),outer=T,side=3,cex=1.5) }
                    }
            graphics.off()
            } else {
                r.curves <- gbm.plot(out$mods$final.mod,plotit=F)
            }
    }
    if(cv==FALSE) r.curves<-NULL
    return(r.curves)
    
}