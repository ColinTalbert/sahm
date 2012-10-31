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


options(error=NULL)

FitModels <- function(ma.name,tif.dir=NULL,output.dir=NULL,debug.mode=FALSE,script.name,make.p.tif=TRUE,make.binary.tif=TRUE,...){
      
       Call<-match.call()
    # This function fits a generic model to presence-pseudoabsence, presence-absence or count data.
    # written by Marian Talbert, 2011-2012
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

    # Setting up the list that holds everything.  This is quite different for each model
        out <- list(
          input=lapply(as.list(Call[2:length(Call)]),eval), #with optional args this definition might be a problem but since called from the command line it works
          dat = list(), #just captures output from read.ma
          mods=list(final.mod=NULL,
                    tif.output=list(prob=NULL,bin=NULL),
                    auc.output=NULL,
                    interactions=NULL,  # not used #
                    summary=NULL))

    if(is.null(out$input$seed)) out$input$seed<-round(runif(1,min=-((2^32)/2-1),max=((2^32)/2-1)))
    set.seed(as.numeric(out$input$seed))
   #print warnings as they occur
        options(warn=1)
    
        Model=script.name

         # generate a filename for output #
              if(debug.mode==T){
                outfile <- paste(bname<-paste(out$input$output.dir,paste("/",Model,"_",sep=""),n<-1,sep=""),"_output.txt",sep="")
                while(file.access(outfile)==0) outfile<-paste(bname<-paste(out$input$output.dir,paste("/",Model,"_",sep=""),n<-n+1,sep=""),"_output.txt",sep="")
                capture.output(paste(toupper(Model),"Results"),file=outfile) # reserve the new basename #
                } else bname<-file.path(out$input$output.dir,Model)

   #Load Libraries
              chk.libs(Model)
   #Read in data, perform several checks and store all of the information in the out list
             out <- read.ma(out)
             out$dat$bname <- bname
             if(out$input$script.name=="rf" & out$input$model.family=="poisson") stop("Random Forest not implemented for count data")
            
   #writing out the header info to the CSV so in case of a break we know what broke
             out<-place.save(out)
              out$dat$split.label<-out$dat$split.type
   #check output dir #
              if(file.access(out$input$output.dir,mode=2)!=0) stop(paste("output directory",output.dir,"is not writable"))

              cat("\nbegin processing of model array:",out$input$ma.name,"\n")
              cat("\nfile basename set to:",out$dat$bname,"\n")
              assign("out",out,envir=.GlobalEnv)
              cat("Progress:20%\n");flush.console();
             cat("\n","Fitting",toupper(Model),"model","\n")
             flush.console()
      
    #Fit the desired model#
               out<-generic.model.fit(out,Model,t0)
          
    #Making Predictions
               pred.vals<-function(x,model,Model){
              x$pred<-pred.fct(model,x$dat[,2:ncol(x$dat)],Model)
              return(x)}
       
              #getting the predictions for the test/train or cross validation splits into the object at the correct list location
                  if(out$dat$split.type!="crossValidation") out$dat$ma<-(lapply(X=out$dat$ma,FUN=pred.vals,model=out$mods$final.mod,Model=Model))
                     else out$dat$ma$train$pred<-pred.vals(out$dat$ma$train,out$mods$final.mod,Model=Model)$pred  
                   #Maxlike needs to remove incomplete cases which are removed in read.ma if there's a data.frame to work with
                   if(Model=="maxlike") out$dat$ma$train$pred<-out$dat$ma$train$pred[out$dat$ma$train$compl]
                  #Just for the training set for Random Forest we have to take out of bag predictions rather than the regular predictions
                  if(Model=="rf" & !out$input$PsdoAbs) out$dat$ma$train$pred<-tweak.p(as.vector(predict(out$mods$final.mod[[1]],type="prob")[,2])) 
                                                  
    #Run Cross Validation if specified might need separate cv functions for each model
            if(out$dat$split.type=="crossValidation") out<-cv.fct(out$mods$final.mod, out=out, sp.no = 1, prev.stratify = F,Model=Model)
            
                  assign("out",out,envir=.GlobalEnv)
                  t3 <- unclass(Sys.time())

                  if(!is.null(out$dat$bad.factor.cols)){
                      capture.output(cat("\nWarning: the following categorical response variables were removed from consideration\n",
                          "because they had only one level:",paste(out$dat$bad.factor.cols,collapse=","),"\n"),
                          file=paste(bname,"_output.txt",sep=""),append=T)
                      }

                    if(nrow(out$dat$ma$train$dat)/(ncol(out$dat$ma$train$dat)-1)<10){
                    capture.output(cat(paste("\n Warning: You have approximately ", round(nrow(out$dat$ma$train$dat)/(ncol(out$dat$ma$train$dat)-1),digits=1),
                    " observations for every predictor\n consider reducing the number of predictors before continuing\n",sep="")),
                          file=paste(bname,"_output.txt",sep=""),append=T)
                    }
                  cat("40%\n")
                  
    #producing auc and residual plots model summary information and accross model evaluation metric
          out$mods$auc.output<-make.auc.plot.jpg(out=out)

              cat("Progress:70%\n");flush.console()

  #Response curves #
     if(Model!="maxlike") response.curves(out,Model)

     assign("out",out,envir=.GlobalEnv)

   #Save Workspace
   save.image(file.path(output.dir,"modelWorkspace"))
          t4 <- unclass(Sys.time())
          cat("\nfinished with final model summarization, t=",round(t4-t3,2),"sec\n");flush.console()
         cat("Progress:80%\n");flush.console()
    
    # Make .tif of predictions #
    if(out$input$make.p.tif==T | out$input$make.binary.tif==T){
        if((n.var <- out$mods$n.vars.final)<1){
            stop("Error producing geotiff output:  null model selected by stepwise procedure - pointless to make maps")
            } else {
            cat("\nproducing prediction maps...","\n","\n");flush.console()
                              proc.tiff(model=out$mods$final.mod,vnames=names(out$dat$ma$train$dat)[-1],
                tif.dir=out$dat$tif.dir$dname,filenames=out$dat$tif.ind,factor.levels=out$dat$factor.levels,make.binary.tif=make.binary.tif,
                thresh=out$mods$auc.output$thresh,make.p.tif=make.p.tif,outfile.p=paste(out$dat$bname,"_prob_map.tif",sep=""),
                outfile.bin=paste(out$dat$bname,"_bin_map.tif",sep=""),tsize=50.0,NAval=-3000,
                fnames=out$dat$tif.names,out=out,Model=Model)

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
