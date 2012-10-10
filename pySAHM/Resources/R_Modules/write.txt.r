write.txt<-function(out,t0){
  model.label<-switch(out$input$script.name,
    mars="\nMARS Model Results\n",
    glm="Generalized Linear Model Results\n",
    rf="Random Forest Modeling Results\n",
    brt="\nBoosted Regression Tree Modeling Results\n")

  txt0 <- paste(model.label,"\n",
              "Data:\n",out$input$ma.name,"\n",
              "\n\t n(pres)=                       ",out$dat$nPresAbs$train[2],
              if(out$input$script.name!="maxlike")"\n\t n(abs) =                       ",out$dat$nPresAbs$train[1],
              "\n\t n covariates considered =      ",length(out$dat$used.covs),"\n",
              "\n\t total time for model fitting = ",round((unclass(Sys.time())-t0)/60,2),"min\n",sep="")
              
   capture.output(cat(txt0),file=paste(out$dat$bname,"_output.txt",sep=""))
       }