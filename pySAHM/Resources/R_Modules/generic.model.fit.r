generic.model.fit<-function(out,Model){
  if(Model=="mars"){ out$mods$final.mod<-mars.glm(data=out$dat$ma$train$dat, mars.x=c(2:ncol(out$dat$ma$train$dat)), mars.y=1, mars.degree=out$input$mars.degree, family=out$input$model.family,
          site.weights=out$dat$ma$train$weight, penalty=out$input$mars.penalty)
          fit_contribs <- mars.contribs(out$mods$final.mod)


          x<-fit_contribs$deviance.table
          x <- x[x[,2]!=0,]
          x <- x[order(x[,4]),]
          row.names(x) <- x[,1]
          x$df <- -1*x$df
          x <- x[,-1]
          cat("Summary of Model:","\n")
          print(out$mods$summary <- x)

           out$mods$final.mod$contributions$var<-names(out$dat$ma$train$dat)[-1]
             out$mods$n.vars.final<-nrow(out$mods$summary)

          }

  if(Model=="glm") {
          penalty <- if(out$input$simp.method=="AIC") 2 else log(nrow(out$dat$ma$ma))
          scope.glm <- list(lower=as.formula(paste("response","~1")),
          upper=as.formula(paste("response","~",paste(out$dat$used.covs,collapse='+'))))

          mymodel.glm.step <- step(glm(as.formula(paste("response","~1")),family=out$input$model.family,data=out$dat$ma$train$dat,weights=out$dat$ma$train$weight,na.action="na.exclude"),
          direction='both',scope=scope.glm,trace=0,k=penalty)

          out$mods$final.mod<-mymodel.glm.step

    cat("\n","Finished with stepwise GLM","\n")
    cat("Summary of Model:","\n")
    print(out$mods$summary <- summary(mymodel.glm.step))
    if(length(coef(out$mods$final.mod))==1) stop("Null model was selected.  \nEvaluation metrics and plots will not be produced")

    if(!is.null(out$dat$bad.factor.cols)){
        capture.output(cat("\nWarning: the following categorical response variables were removed from consideration\n",
            "because they had only one level:",paste(out$dat$bad.factor.cols,collapse=","),"\n"),
            file=paste(bname,"_output.txt",sep=""),append=T)
        }
        #storing number of variables in final model
        out$mods$n.vars.final<-length(attr(terms(formula(out$mods$final.mod)),"term.labels"))
         }
  return(out)
 }