# A set of "pluggable" R functions for automated model fitting of glm models to presence/absence data #
#
# Modified 12-2-09 to:  remove categorical covariates from consideration if they only contain one level
#                       ID factor variables based on "categorical" prefix and look for tifs in subdir
#                       Give progress reports
#                       write large output tif files in blocks to alleviate memory issues
#                       various bug fixes
#
#
# Modified 3-4-09 to use a list object for passing of arguements and data
#


# Libraries required to run this program #
#   PresenceAbsence - for ROC plots

#   rgdal - for geotiff i/o
#   sp - used by rdgal library
#   raster for geotiff o
options(error=NULL)

FitModels <- function(ma.name,tif.dir=NULL,output.dir=NULL,debug.mode=FALSE,script.name,...){
      
       Call<-match.call()
    # This function fits a stepwise GLM model to presence-absence data.
    # written by Alan Swanson, 2008-2009
    # # Maintained and edited by Marian Talbert September 2010-
    # Arguements.
    # ma.name: is the name of a .csv file with a model array.  full path must be included unless it is in the current
    #  R working directory # THIS FILE CAN NOW INCLUDE AN OPTIONAL COLUMN OF SITE WEIGHTS WHICH MUST BE LABELED "site.weights"
    # tif.dir: is the directory containing geotiffs for each covariate.  only required if geotiffs output of the 
    #   response surface is requested #    # cov.list.name: is the name of a text file with the names of covariates to be included in models (one per line).
    # output.dir: is the directory that output files will be stored in.  if not given, files will go to the current working directory. 
    # response.col: column number of the model array containing a binary 0/1 response.  all other columns will be considered explanatory variables.
    # make.p.tif: T if a geotiff of the response surface is desired.
    # make.binary.tif: T if a geotiff of the response surface is desired.
    # simp.method: model simplification method.  valid methods include: "AIC" and "BIC". NOT CURRENTLY FUNCTIONAL 
    # debug.mode: if T, output is directed to the console during the run.  also, a pdf is generated which contains response curve plots and perspective plots
    #    showing the effects of interactions deemed important.  if F, output is diverted to a text file and the console is kept clear 
    #    in either case, a set of standard output files are created in the output directory.
    # 
    t0 <- unclass(Sys.time())

    out <- list(
      input=lapply(as.list(Call[2:length(Call)]),eval), #with optional args this definition might be a problem but since called from the command line it works
      dat = list(), #just captures output from read.ma
      mods=list(final.mod=NULL,
                r.curves=NULL,
                tif.output=list(prob=NULL,bin=NULL),
                auc.output=NULL,
                interactions=NULL,  # not used #
                summary=NULL))

    #print warnings as they occur
    options(warn=1)
    
      Model=script.name
   #Load Libraries
      chk.libs(Model)
   #Read in data, perform several checks and store all of the information in the out list
      out <- read.ma(out)
     ############################# READ.MA ########################

    # check output dir #
    if(file.access(out$input$output.dir,mode=2)!=0) stop(paste("output directory",output.dir,"is not writable"))

    # generate a filename for output #
          if(debug.mode==T){
            outfile <- paste(bname<-paste(out$input$output.dir,paste("/",Model,"_",sep=""),n<-1,sep=""),"_output.txt",sep="")
            while(file.access(outfile)==0) outfile<-paste(bname<-paste(out$input$output.dir,paste("/",Model,"_",sep=""),n<-n+1,sep=""),"_output.txt",sep="")
            capture.output(cat("temp"),file=outfile) # reserve the new basename #
            } else bname<-paste(out$input$output.dir,paste("/",Model,sep=""),sep="")
            out$dat$bname <- bname
         if(!debug.mode) {sink(logname <- paste(bname,"_log.txt",sep=""));on.exit(sink)} else logname<-NULL


          cat("\nbegin processing of model array:",out$input$ma.name,"\n")
          cat("\nfile basename set to:",out$dat$bname,"\n")
          assign("out",out,envir=.GlobalEnv)
          if(!debug.mode) {sink();cat("Progress:20%\n");flush.console();sink(logname,append=T)} else {cat("\n");cat("20%\n")}  ### print time
    ##############################################################################################################
    #  Begin model fitting #
    ##############################################################################################################
          cat("\n","Fitting",toupper(Model),"model","\n")
          flush.console()
          
    # Fit the desired model#
       out<-generic.model.fit(out,Model)

    #Run cross validation if specified might need separate cv functions for each model
        if(out$dat$split.type=="crossValidation") out<-cv.fct(out$mods$final.mod, out, sp.no = 1, prev.stratify = F)

              assign("out",out,envir=.GlobalEnv)
              t3 <- unclass(Sys.time())

    #Capturing a bunch of output
         txt0 <- paste("\n", toupper(Model),"Model Results\n","\n","Data:\n",ma.name,"\n","\n\t n(pres)=",
            out$dat$nPresAbs$train[2],"\n\t n(abs)=",out$dat$nPresAbs$train[1],"\n\t n covariates considered=",length(out$dat$used.covs),
            "\n",
            "\n   total time for model fitting=",round((unclass(Sys.time())-t0)/60,2),"min\n",sep="")

        capture.output(cat(txt0),file=paste(bname,"_output.txt",sep=""),append=TRUE)
        cat("\n","Finished with", toupper(Model),"\n")
        if(!is.null(out$dat$bad.factor.cols)){
            cat("\nWarning: the following categorical response variables were removed from consideration\n",
                "because they had only one level:",paste(out$dat$bad.factor.cols,collapse=","),"\n\n")
            }
        cat("\n","Storing output...","\n","\n")
        capture.output(cat("\n\nSummary of Model:\n"),file=paste(bname,"_output.txt",sep=""),append=TRUE)
        capture.output(print(out$mods$summary),file=paste(bname,"_output.txt",sep=""),append=TRUE)
        if(!is.null(out$dat$bad.factor.cols)){
            capture.output(cat("\nWarning: the following categorical response variables were removed from consideration\n",
                "because they had only one level:",paste(out$dat$bad.factor.cols,collapse=","),"\n"),
                file=paste(bname,"_output.txt",sep=""),append=T)
            }
        cat("40%\n")
    
    
    ##############################################################################################################
    #  Begin model output #
    ##############################################################################################################


    pred.vals<-function(x,model,Model){
    x$pred<-pred.fct(model,x$dat[,-1],Model)
    return(x)}

    #getting the predictions for the test/train or cross validation splits into the object at the correct list location
    if(out$dat$split.type!="crossValidation") out$dat$ma<-(lapply(X=out$dat$ma,FUN=pred.vals,model=out$mods$final.mod,Model=Model))
       else out$dat$ma$train$pred<-out$mods$final.mod$fitted$response #produces the same thing as pred.mars(out$mods$final.mod,out$dat$ma$train$dat[2:ncol(out$dat$ma$train$dat)])

    #Just for the training set for Random Forest we have to take out of bag predictions rather than the regular predictions
    if(Model=="rf") out$dat$ma$train$pred<-tweak.p(as.vector(predict(out$mods$final.mod,type="prob")[,2]))
    #producing auc and residual plots model summary information and accross model evaluation metric
      out$mods$auc.output<-make.auc.plot.jpg(out=out)

              if(!debug.mode) {sink();cat("Progress:70%\n");flush.console();sink(logname,append=T)} else cat("70%\n")

  # Response curves #
      response.curves(out,Model)

     assign("out",out,envir=.GlobalEnv)

   #Save Workspace
   save.image(paste(output.dir,"modelWorkspace",sep="\\"))
          t4 <- unclass(Sys.time())
          cat("\nfinished with final model summarization, t=",round(t4-t3,2),"sec\n");flush.console()
          if(!debug.mode) {sink();cat("Progress:80%\n");flush.console();sink(logname,append=T)} else cat("70%\n")

    # Make .tif of predictions #
    if(out$input$make.p.tif==T | out$input$make.binary.tif==T){
        if((n.var <- out$mods$n.vars.final)<1){
            stop("Error producing geotiff output:  null model selected by stepwise procedure - pointless to make maps")
            } else {
            cat("\nproducing prediction maps...","\n","\n");flush.console()
            proc.tiff(model=out$mods$final.mod,vnames=names(out$dat$ma$train$dat)[-1],
                tif.dir=out$dat$tif.dir$dname,filenames=out$dat$tif.ind,pred.fct=pred.fct,factor.levels=out$dat$ma$factor.levels,make.binary.tif=make.binary.tif,
                thresh=out$mods$auc.output$thresh,make.p.tif=make.p.tif,outfile.p=paste(out$dat$bname,"_prob_map.tif",sep=""),
                outfile.bin=paste(out$dat$bname,"_bin_map.tif",sep=""),tsize=50.0,NAval=-3000,
                fnames=out$dat$tif.names,logname=logname,out=out,Model=Model)
            }

            if(make.p.tif) out$mods$tif.output$prob <- paste(out$dat$bname,"_prob_map.tif",sep="")
            if(make.binary.tif) out$mods$tif.output$bin <- paste(out$dat$bname,"_bin_map.tif",sep="")
            t5 <- unclass(Sys.time())
            cat("\nfinished with prediction maps, t=",round(t5-t4,2),"sec\n");flush.console()
          }


    if(debug.mode) assign("out",out,envir=.GlobalEnv)

    
    cat(paste("\ntotal time=",round((unclass(Sys.time())-t0)/60,2),"min\n\n\n",sep=""))
    if(!debug.mode) {
        sink();on.exit();unlink(paste(bname,"_log.txt",sep=""))
        }
    cat("Progress:100%\n");flush.console()
    if(debug.mode) assign("fit",out$mods$final.mod,envir=.GlobalEnv)
    invisible(out)
    }
