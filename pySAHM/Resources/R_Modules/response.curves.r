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

        pdf(paste(bname,"_response_curves.pdf",sep=""),width=11,height=8.5,onefile=T)
            par(oma=c(2,2,4,2),mfrow=c(prow,pcol))
            r.curves <-my.termplot(out$mods$final.mod,plot.it=T)
            mtext(paste("GLM response curves for",basename(ma.name)),outer=T,side=3,cex=1.3)
           ################### New work
           #starting by just looking at one element of the list later use an lapply
            if(!is.null(out$cv$resp.curves))
                    for(i in 1:length(r.curves$names)){
                          plot(r.curves$pred[[i]],r.curves$resp[[i]],type="l")
                          for(j in 1:length(out$cv$resp)){
                           indx<-na.omit(match(r.curves$names[i],names(out$cv$resp[[i]]$pred)))
                           lines(out$cv$resp.curves[[j]]$pred[indx][[1]],out$cv$resp.curves[[j]]$resp[indx][[1]],col="grey",lty="dotdash")
                           }
                         lines(r.curves$pred[[i]],r.curves$resp[[i]],type="l",col="red",lwd=2)
                           }


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