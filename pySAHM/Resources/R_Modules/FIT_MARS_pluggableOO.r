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
#   XML - for XML i/o
#   rgdal - for geotiff i/o
#   sp - used by rdgal library
#   raster for geotiff o
options(error=NULL)

fit.mars.fct <- function(ma.name,tif.dir=NULL,output.dir=NULL,response.col="^response.binary",make.p.tif=T,make.binary.tif=T,
      mars.degree=1,mars.penalty=2,responseCurveForm=NULL,debug.mode=T,script.name="mars.r",opt.methods=2,save.model=TRUE,UnitTest=FALSE,MESS=FALSE){
      
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
    #    except for final output of an xml file.  in either case, a set of standard output files are created in the output directory.
    # 

    # Value:
    # returns nothing but generates a number of output files in the directory
    # "output.dir" named above.  These output files consist of:
    #
    # glm_output.txt:  a text file with fairly detailed results of the final model.
    # glm_output.xml:  an xml-formatted text file with results from the final model.
    # glm_response_curves.xml:  an xml-formatted text file with response curves for
    #   each covariate in the final model.
    # glm_prob_map.tif:  a geotiff of the response surface
    # glm_bin_map.tif:  a geotiff of the binary response surface.  threhold is based on the roc curve at the point where sensitivity=specificity.
    # glm_log.txt:   a file containing text output diverted from the console when debug.mode=F
    # glm_auc_plot.jpg:  a jpg file of a ROC plot.
    # glm_response_curves.pdf:  an pdf file with response curves for
    #   each covariate in the final model and perspective plots showing the effect of interactions deemed significant.
    #   only produced when debug.mode=T
    # #  seed=NULL                                 # sets a seed for the algorithm, any inegeger is acceptable
    #  opt.methods=2                             # sets the method used for threshold optimization used in the
    #                                            # the evaluation statistics module
    #  save.model=FALSE                          # whether the model will be used to later produce tifs
    # when debug.mode is true, these filenames will include a number in them so that they will not overwrite preexisting files. eg brt_1_output.txt.
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
                summary=NULL),
      time=list(strt=unclass(Sys.time()),end=NULL),
      error.mssg=list(NULL),
      ec=0    # error count #
      )

      # load libaries #
      out <- check.libs(list("PresenceAbsence","rgdal","XML","sp","survival","mda","raster","tcltk2","foreign","ade4"),out)
      
      # exit program now if there are missing libraries #
      if(!is.null(out$error.mssg[[1]])) stop("There are missing libraries")

     ############################# READ.MA ########################
    if(UnitTest!=FALSE) options(warn=2)
    out <- read.ma(out)
    if(UnitTest==1) return(out)
    
    # check output dir #
    if(file.access(out$input$output.dir,mode=2)!=0) stop(paste("output directory",output.dir,"is not writable"))

    # generate a filename for output #
          if(debug.mode==T){
            outfile <- paste(bname<-paste(out$input$output.dir,"/mars_",n<-1,sep=""),"_output.txt",sep="")
            while(file.access(outfile)==0) outfile<-paste(bname<-paste(out$input$output.dir,"/mars_",n<-n+1,sep=""),"_output.txt",sep="")
            capture.output(cat("temp"),file=outfile) # reserve the new basename #
            } else bname<-paste(out$input$output.dir,"/mars",sep="")
            out$dat$bname <- bname
            

    options(warn=1)

    cat("\nbegin processing of model array:",out$input$ma.name,"\n")
    cat("\nfile basename set to:",out$dat$bname,"\n")
    assign("out",out,envir=.GlobalEnv)
    if(!debug.mode) {sink();cat("Progress:20%\n");flush.console();sink(logname,append=T)} else {cat("\n");cat("20%\n")}  ### print time
    ##############################################################################################################
    #  Begin model fitting #
    ##############################################################################################################

    # Fit null GLM and run stepwise, then print results #
    cat("\n","Fitting MARS model","\n")
    flush.console()

    out$mods$final.mod <- mars.glm(data=out$dat$ma$train$dat, mars.x=c(2:ncol(out$dat$ma$train$dat)), mars.y=1, mars.degree=out$input$mars.degree, family=out$input$model.family,
          site.weights=out$dat$ma$train$weight, penalty=out$input$mars.penalty)
      

  out$mods$final.mod$contributions$var<-names(out$dat$ma$train$dat)[-1]

    assign("out",out,envir=.GlobalEnv)
    t3 <- unclass(Sys.time())
    fit_contribs <- try(mars.contribs(out$mods$final.mod))
    if(class(fit_contribs)=="try-error") stop(paste("Error summarizing MARS model:",fit_contribs))
       
    x<-fit_contribs$deviance.table
    x <- x[x[,2]!=0,]
    x <- x[order(x[,4]),]
    row.names(x) <- x[,1]
    x$df <- -1*x$df
    x <- x[,-1]
    
     txt0 <- paste("\nMARS Model Results\n","\n","Data:\n",ma.name,"\n","\n\t n(pres)=",
        out$dat$nPresAbs$train[2],"\n\t n(abs)=",out$dat$nPresAbs$train[1],"\n\t n covariates considered=",length(out$dat$ma$used.covs),
        "\n",
        "\n   total time for model fitting=",round((unclass(Sys.time())-t0)/60,2),"min\n",sep="")

    capture.output(cat(txt0),file=paste(bname,"_output.txt",sep=""))
    
    cat("\n","Finished with MARS","\n")
    cat("Summary of Model:","\n")
    print(out$mods$summary <- x)
    if(!is.null(out$dat$bad.factor.cols)){
        cat("\nWarning: the following categorical response variables were removed from consideration\n",
            "because they had only one level:",paste(out$dat$bad.factor.cols,collapse=","),"\n\n")
        }
    cat("\n","Storing output...","\n","\n")
    #flush.console()
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
           
    # Store .jpg ROC plot #

    pred.fct<-function(x,model){
    x$pred<-pred.mars(model,x$dat[,-1])
    return(x)}
    
    #getting the predictions for the test/train or cross validation splits into the object at the correct list location
    out$dat$ma<-(lapply(X=out$dat$ma,FUN=pred.fct,model=out$mods$final.mod))

      out$mods$auc.output<-make.auc.plot.jpg(out=out)

    if(!debug.mode) {sink();cat("Progress:70%\n");flush.console();sink(logname,append=T)} else cat("70%\n")
    
    # Response curves #
    
    if(is.null(responseCurveForm)){
    responseCurveForm<-0}    
    
    if(debug.mode | responseCurveForm=="pdf"){
        nvar <- nrow(out$mods$summary)
        pcol <- min(ceiling(sqrt(nvar)),4)
        prow <- min(ceiling(nvar/pcol),3)
        r.curves <- try(mars.plot(fit,plot.layout=c(prow,pcol),file.name=paste(bname,"_response_curves.pdf",sep="")))

        } else r.curves<-try(mars.plot(fit,plot.it=F))
        
        if(class(r.curves)!="try-error") stop(paste("ERROR: problem fitting response curves",r.curves))

        pred.fct<-pred.mars

     assign("out",out,envir=.GlobalEnv)

 save.image(paste(output.dir,"modelWorkspace",sep="\\"))
    t4 <- unclass(Sys.time())
    cat("\nfinished with final model summarization, t=",round(t4-t3,2),"sec\n");flush.console()
    if(!debug.mode) {sink();cat("Progress:80%\n");flush.console();sink(logname,append=T)} else cat("70%\n")   
    # Make .tif of predictions #

    if(out$input$make.p.tif==T | out$input$make.binary.tif==T){
        if((n.var <- nrow(out$mods$summary))<1){
            mssg <- "Error producing geotiff output:  null model selected by stepwise procedure - pointless to make maps"
            class(mssg)<-"try-error"
            } else {
            cat("\nproducing prediction maps...","\n","\n");flush.console()
            proc.tiff(model=out$mods$final.mod,vnames=names(out$dat$ma$train$dat)[-1],
                tif.dir=out$dat$tif.dir$dname,filenames=out$dat$tif.ind,pred.fct=pred.mars,factor.levels=out$dat$ma$factor.levels,make.binary.tif=make.binary.tif,
                thresh=out$mods$auc.output$thresh,make.p.tif=make.p.tif,outfile.p=paste(out$dat$bname,"_prob_map.tif",sep=""),
                outfile.bin=paste(out$dat$bname,"_bin_map.tif",sep=""),tsize=50.0,NAval=-3000,
                fnames=out$dat$tif.names,logname=logname,out=out)
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
################################################################################
###########          End fit.mars.fct       ####################################


#set defaults
make.p.tif=T
make.binary.tif=T
mars.degree=1
mars.penalty=2
script.name="mars.r"
opt.methods=2
save.model=TRUE
MESS=FALSE

# Interpret command line argurments #
# Make Function Call #
Args <- commandArgs(trailingOnly=FALSE)

    for (i in 1:length(Args)){
     if(Args[i]=="-f") ScriptPath<-Args[i+1]
     }

    print(Args)
    for (arg in Args) {
    	argSplit <- strsplit(arg, "=")
    	argSplit[[1]][1]
    	argSplit[[1]][2]
    	if(argSplit[[1]][1]=="c") csv <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="o") output <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="rc") responseCol <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="mpt") make.p.tif <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="mbt")  make.binary.tif <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="deg") mars.degree <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="pen") mars.penalty <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="om")  opt.methods <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="savm")  save.model <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="mes")  MESS <- argSplit[[1]][2]
    }
	print(csv)
	print(output)
	print(responseCol)

ScriptPath<-dirname(ScriptPath)
source(paste(ScriptPath,"LoadRequiredCode.r",sep="\\"))
source(paste(ScriptPath,"MARS.helper.fcts.r",sep="\\"))
print(ScriptPath)

make.p.tif<-as.logical(make.p.tif)
make.binary.tif<-as.logical(make.binary.tif)
save.model<-make.p.tif | make.binary.tif
opt.methods<-as.numeric(opt.methods)

fit.mars.fct(ma.name=csv,
        tif.dir=NULL,output.dir=output,
        response.col=responseCol,make.p.tif=make.p.tif,make.binary.tif=make.binary.tif,
            mars.degree=mars.degree,mars.penalty=mars.penalty,debug.mode=F,responseCurveForm="pdf",
            script.name="mars.r",save.model=save.model,opt.methods=as.numeric(opt.methods),MESS=MESS)
