EvaluateNewData<-function(workspace=NULL,out.dir=NULL,b.tif=TRUE,p.tif=TRUE,mess=FALSE,new.tifs=NULL,produce.metrics=TRUE){

#This functions has several tasks that it will perform

#1. (Produce Maps for best models)It separates the step of model fit from producing probability 
#or binary surfaces and MESS maps
#the default is to read in a workspace and make predictions using the original tiffs supplied
#but if an mds with new tiff directories are supplied, the function will instead use these

#2. (Apply Model to a New Region) It can also project to a new region if a header is supplied specifying just new tiffs is supplied if a response is available for 
# the new region then it can produce evaluation metrics as well

#3. (Evaluate independent data or Evaluation Split) It will evaluate either data withheld for evaluation (using EvalSplit in the original mds) 
#or evaluate a model on a completely new region if an mds is available for the new region new.tifs should be the mds and produce.metrics should be set to true 

#Written by Marian Talbert 5/2012

    load(workspace)
    chk.libs(out$input$script.name)
       out1<-out
       try(rm(out,envir=.GlobalEnv),silent=TRUE)
       try(rm(out),silent=TRUE)
       out<-out1
    # generate a filename for output #
                out$input$output.dir<-out.dir
                out$input$MESS<-mess
                Model<-out$input$script.name
              out$dat$bname<-paste(out$input$output.dir,paste("/",Model,sep=""),sep="")
               if(!produce.metrics & !is.null(new.tifs)) out$input$NoResidMaps=TRUE
    
    #write a few details on the data to the output txt document
        txt0 <- paste("Original Model:\n\t",
                  switch(out$input$script.name,
                                brt="Boosted Regression Tree",
                                rf="Random Forest",
                                glm="Generalized Linear Model",
                                mars="Multivariate Adaptive Regression Spline"),
                  "\n",              
                "\nOriginal Data:\n\t",out1$input$ma.name,
                "\n\tn(pres)=",out1$dat$nPresAbs$train[2],
                "\n\tn(abs)=",out1$dat$nPresAbs$train[1],
                if(!is.null(new.tifs)) paste("\n\nModel applied to:\n\t",
                                new.tifs,sep=""),
                "\n",sep="")                
                
           capture.output(cat(txt0),file=paste(out$dat$bname,"_output.txt",sep=""))
           rm(out1) 
         ##################################################################################
         ###################### Producing Evalutaiton Metrics for the hold out data
                 #if evaluating on brand new data switch out the mds 
                 if(!is.null(new.tifs)){out$input$ma.name<-new.tifs
                    store.train<-out$dat$ma$train
                 }
                 hl=readLines(out$input$ma.name,1)
                 hl=strsplit(hl,',')
                 tif.info<-readLines(out$input$ma.name,3)
                 tif.info<-strsplit(tif.info,',')
                 temp<-tif.info[[2]]
                 paths<-matrix(as.character(tif.info[[3]]))
                 rownames(paths) <-tif.info[[1]][1:length(paths)]
                 
                 if(!is.null(new.tifs) & any(!is.na(match(hl[[1]],c("EvalSplit","Split")))))
                 stop("The input dataset to this module should not have any data split for testing or model evaluation")
             #since we can be missing any of xy or response we need to remove only what's included    
                 unused=na.omit(match(tolower(hl[[1]]),c("x","y",tolower(out$input$response.col))))  
                 if(any(!is.na(unused))) temp[unused]<-0
                 include<-as.numeric(temp)
             #if a new mds was supplied switch out the tiffs for map production    
                 if(!is.null(new.tifs)){
                 paths<-paths[include==1,]
                 path.check(paths)
                  out$dat$tif.ind<-paths}
             if(produce.metrics){   
                   if(is.null(new.tifs)){ 
                   #Determine if we will be evaluating a holdout set if so rename it to the split portion so evaluation metrics will be caluclated and reported
                   #Switching the name Eval.Split to split so we evaluate on final holdout data
                           if(!is.na(Eval.split<-match("evalsplit",tolower(unlist(hl))))){
                                  if(!is.na(Split<-match("split",tolower(unlist(hl))))) {hl[[1]][Split]<-"Unused"
                                    include[Split]<-0
                                  }
                                  hl[[1]][Eval.split]<-"Split"  
                         }
                   }
                                
                      
                      #if we have completely new data then the whole dataset should be used for testing and the original displayed as training data
                      if(!is.null(new.tifs)) {
                      out <- read.ma(out,hl=hl,include=include)
                         #read.ma wasn't designed for what it's doing here and overwrites a few things it shouldn't 
                         #these are reset correctly here
                          names(out$dat$ma)<-"test"
                          out$dat$ma$train<-store.train
                            out$dat$split.type=out$dat$split.label="eval"
                            out$dat$bname<-paste(out$input$output.dir,paste("/",Model,sep=""),sep="")
                      }
                  # Making Predictions
                           pred.vals<-function(x,model,Model){
                          x$pred<-pred.fct(model,x$dat[,2:ncol(x$dat)],Model)
                          return(x)}
                          
      
                         assign("out",out,envir=.GlobalEnv)
                          #getting the predictions for the test/train split
                          out$dat$ma<-(lapply(X=out$dat$ma,FUN=pred.vals,model=out$mods$final.mod,Model=Model))
                          #Just for the training set for Random Forest we have to take out of bag predictions rather than the regular predictions
                          if(Model=="rf") out$dat$ma$train$pred<-tweak.p(as.vector(predict(out$mods$final.mod,type="prob")[,2]))
      
                          #producing auc and residual plots model summary information and accross model evaluation metric
                      out$mods$auc.output<-make.auc.plot.jpg(out=out)
           }
          
     ################################ Making the tiff
   if(p.tif==T | b.tif==T){
        if((n.var <- out$mods$n.vars.final)<1){
            stop("Error producing geotiff output:  null model selected by stepwise procedure - pointless to make maps")
            } else {
            cat("\nproducing prediction maps...","\n","\n");flush.console()
               proc.tiff(model=out$mods$final.mod,vnames=names(out$dat$ma$train$dat)[-1],
                tif.dir=out$dat$tif.dir$dname,filenames=out$dat$tif.ind,pred.fct=pred.fct,factor.levels=out$dat$factor.levels,make.binary.tif=b.tif,
                thresh=out$mods$auc.output$thresh,make.p.tif=p.tif,outfile.p=paste(out$dat$bname,"_prob_map.tif",sep=""),
                outfile.bin=paste(out$dat$bname,"_bin_map.tif",sep=""),tsize=50.0,NAval=-3000,
                fnames=out$dat$tif.names,out=out,Model=Model)     
            }

     }

}

p.tif=T
b.tif=T
Mess=FALSE
new.tiffs=NULL
produce.metrics=TRUE


# Interpret command line argurments #
# Make Function Call #
Args <- commandArgs(trailingOnly=FALSE)

    for (i in 1:length(Args)){
     if(Args[i]=="-f") ScriptPath<-Args[i+1]
     }

    for (arg in Args) {
    	argSplit <- strsplit(arg, "=")
    	argSplit[[1]][1]
    	argSplit[[1]][2]
    	if(argSplit[[1]][1]=="ws") ws <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="o") out.dir <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="mes")  Mess <- argSplit[[1]][2]
   		if(argSplit[[1]][1]=="mpt") p.tif <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="mbt")  b.tif <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="c") new.tiffs <- argSplit[[1]][2]   #mds file header
 			if(argSplit[[1]][1]=="pmt")  produce.metrics <- argSplit[[1]][2]
    }

ScriptPath<-dirname(ScriptPath)
source(paste(ScriptPath,"LoadRequiredCode.r",sep="\\"))

EvaluateNewData(workspace=ws,out.dir=out.dir,b.tif=as.logical(b.tif),p.tif=as.logical(p.tif),mess=as.logical(Mess),new.tifs=new.tiffs,produce.metrics=as.logical(produce.metrics))




