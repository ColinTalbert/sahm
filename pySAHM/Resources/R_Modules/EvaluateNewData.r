EvaluateNewData<-function(workspace=NULL,out.dir=NULL,b.tif=TRUE,p.tif=TRUE,mess=FALSE,new.tifs=NULL,produce.metrics=TRUE,out=out){

#This functions has several tasks that it will perform

#1. (Produce Maps for best models)It separates the step of model fit from producing probability 
#or binary surfaces and MESS maps
#the default is to read in a workspace and make predictions using the original tiffs supplied
#but if an mds with new tiff directories are supplied, the function will instead use these

#2. (Apply Model to a New Region) It can also project to a new region if a header is supplied specifying just new tiffs is supplied if a response is available for 
# the new region then it can produce evaluation metrics as well

#3. (Evaluate independent data or Evaluation Split) It will evaluate either data withheld for evaluation (using EvalSplit in the original mds) 
#or evaluate a model on a completely new region if an mds is available for the new region new.tifs should be the mds and produce.metrics should be set to true 

#produce metrics is true if there is a new mds file with more than just a header

#Written by Marian Talbert 5/2012
     t0 <- unclass(Sys.time()) 
   
    chk.libs(out$input$script.name)
       out1<-out
       try(rm(out,envir=.GlobalEnv),silent=TRUE)
       try(rm(out),silent=TRUE)
       out<-out1
       out$input$ScriptPath=ScriptPath #this might not have been in the original, it's a new requirement
     
       on.exit(capture.output(cat("Model Failed\n\n"),file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE))  
       source(file.path(ScriptPath,paste(toupper(out$input$script.name),".helper.fcts.r",sep="")))
    # generate a filename for output #
                out$input$output.dir<-out.dir
                out$input$MESS<-mess
                Model<-out$input$script.name
              out$dat$bname<-paste(out$input$output.dir,paste("/","ApplyModel",sep=""),sep="")
               if(!produce.metrics & !is.null(new.tifs)) out$input$NoResidMaps=TRUE
         
    #write a few details on the data to the output txt document
        txt0 <- paste("Original Model:\n\t",
                  switch(out$input$script.name,
                                brt="Boosted Regression Tree",
                                rf="Random Forest",
                                glm="Generalized Linear Model",
                                mars="Multivariate Adaptive Regression Spline",
                                maxent="Maxent"),
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
                      store.train<-out$dat$ma$train
                 if(!is.null(new.tifs)){out$input$ma.name<-new.tifs
                    newDat<-try(read.csv(new.tifs,skip=4,nrows=2),silent=TRUE)
                    if(class(newDat)=="try-error") produce.metrics=FALSE
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
                      out$dat$tif.ind<-paths
                      out$dat$input$ParcTemplate<-tif.info[[3]][1]
                  }else{
                  #if we're missing a new mds we will evaluate the evaluation split switch it out here
                      if(!is.na(Eval.split<-match("evalsplit",tolower(unlist(hl))))){
                                      if(!is.na(Split<-match("split",tolower(unlist(hl))))) {
                                        hl[[1]][Split]<-"Unused"
                                        include[Split]<-0
                                      }
                                      hl[[1]][Eval.split]<-"Split"
                      
                      }
                  }
                 
                  #if produce metrics is true we're evaluationg on them if new.tifs is null we're evaluating on the evaluation split
             if(produce.metrics | is.null(new.tifs)){  
                                  
                   #Determine if we will be evaluating a holdout set if so rename it to the split portion so evaluation metrics will be caluclated and reported
                   #Switching the name Eval.Split to split so we evaluate on final holdout data
                                    
                                   out <- read.ma(out,hl=hl,include=include,evalNew=TRUE)
                                    out$dat$split.type=out$dat$split.label="eval"
                                    out$dat$bname<-paste(out$input$output.dir,paste("/","ApplyModel",sep=""),sep="")                      
                         
                      #if we have completely new data then the whole dataset should be used for testing and the original displayed as training data
                         #read.ma wasn't designed for what it's doing here and overwrites a few things it shouldn't 
                         #these are reset correctly here
                          if(produce.metrics) names(out$dat$ma)<-"test"
                              
                              out$dat$ma$train<-store.train
                          
                             
                        out <- place.save(out,Final.Model=TRUE)
                  # Making Predictions
                           pred.vals<-function(x,model,Model){
                          x$pred<-pred.fct(model,x$dat[,2:ncol(x$dat)],Model)
                          return(x)}
                     
                     
                          #getting the predictions for the new evaluation data 
                          #train predictions should be stored correctly in the original object
                          out$dat$ma$test<-pred.vals(x=out$dat$ma$test,model=out$mods$final.mod,Model=Model)
                          
                          #producing auc and residual plots model summary information and accross model evaluation metric   
                       out$dat$bnameExpanded=file.path(dirname(out$dat$bname),"ExpandedOutput")
                       assign("out",out,envir=.GlobalEnv)
                       dir.create(out$dat$bnameExpanded)
                       predsForOut<-cbind(resp=out$dat$ma$test$resp,pred=out$dat$ma$test$pred)
                       write.csv(predsForOut,file.path(out$dat$bnameExpanded,"predicted.csv"))
                       
                      out$mods$auc.output<-make.auc.plot.jpg(out=out)
                      response.curves(out,Model)
             
           }
        
     ################################ Making the tiff
   if(p.tif==T | b.tif==T){
        if((n.var <- out$mods$n.vars.final)<1){
            stop("Error producing geotiff output:  null model selected by stepwise procedure - pointless to make maps")
            } else {
           
            cat("\nproducing prediction maps...","\n","\n");flush.console()
            proc.tiff(model=out$mods$final.mod,vnames=names(out$dat$ma$train$dat)[-1],
                tif.dir=out$dat$tif.dir$dname,filenames=out$dat$tif.ind,factor.levels=out$dat$factor.levels,make.binary.tif=b.tif,
                thresh=out$mods$auc.output$thresh,make.p.tif=p.tif,outfile.p=paste(out$dat$bname,"_prob_map.tif",sep=""),
                outfile.bin=paste(out$dat$bname,"_bin_map.tif",sep=""),tsize=50.0,NAval=-3000,
                fnames=out$dat$tif.names,out=out,Model=Model)
                
            }

     }
    on.exit(capture.output(cat(paste("\nTotal time = ",round((unclass(Sys.time())-t0)/60,2)," min\n\n",sep="")),file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)) 
}

p.tif=T
b.tif=T
mess=FALSE
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
    	if(argSplit[[1]][1]=="mes")  mess <- argSplit[[1]][2]
   		if(argSplit[[1]][1]=="mpt") p.tif <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="mbt")  b.tif <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="c") new.tiffs <- argSplit[[1]][2]   #mds file header
 			if(argSplit[[1]][1]=="pmt")  produce.metrics <- argSplit[[1]][2]
    }

ScrptPath<-dirname(ScriptPath)
load(ws)
rm(ScriptPath)
ScriptPath<-ScrptPath
setwd(ScriptPath)
source(file.path(ScriptPath,"LoadRequiredCode.r"))
source(paste(toupper(out$input$script.name),".helper.fcts.r",sep=""))
    
EvaluateNewData(out.dir=out.dir,b.tif=as.logical(b.tif),p.tif=as.logical(p.tif),mess=as.logical(mess),new.tifs=new.tiffs,produce.metrics=as.logical(produce.metrics),out=out)




