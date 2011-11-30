response.curves<-function(out,Model,bname){
 attach(out$input)
 on.exit(detach(out$input))
 bname<-out$dat$bname
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
            par(mfrow=c(1,1))
            graphics.off()
        } else out$mods$r.curves<-my.termplot(out$mods$final.mod,plot.it=F)

     }
}