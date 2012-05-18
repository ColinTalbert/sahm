EvaluateNewData<-function(workspace=NULL,out.dir=NULL,b.tif=TRUE,p.tif=TRUE,mess=FALSE,new.tifs=NULL,produce.metrics=TRUE){

#This functions separates the step of model fit from producing probability or binary surfaces and MESS maps
#the default is to read in a workspace and make predictions using the original tiffs supplied
#but if an mds with new tiff directories are supplied, the function will instead use these
#options are available for setting the threshold
#and true and false for whether to make binary or prob.tif

#my stuff to delete
#workspace=paste("C:\\VisTrails\\mtalbert_20110504T132851","modelWorkspace",sep="\\")
#new.tifs<-out$input$ma.name
#end delete

    load(workspace)

       out1<-out
       rm(out,envir=.GlobalEnv)
       rm(out)
       out<-out1

    # generate a filename for output #
                out$input$output.dir<-out.dir
                out$input$MESS<-mess
                Model<-out$input$script.name
              bname<-paste(out$input$output.dir,paste("/",Model,sep=""),sep="")


            # logname <- file(paste(bname,"_log.txt",sep=""), open="wt")
             #                sink(logname)
              #               sink(logname, type="message")
              out$dat$bname<-bname
    #Determine if we will be evaluating a holdout set if so rename it to the split portion so evaluation metrics will be caluclated and reported
   if(produce.metrics){
        hl=readLines(out$input$ma.name,1)
         hl=strsplit(hl,',')
         
         ##################################################################################
         ###################### Producing Evalutaiton Metrics for the hold out data
               if(!is.na(Eval.split<-match("evalsplit",tolower(unlist(hl))))){
                           tif.info<-readLines(out$input$ma.name,3)
                            tif.info<-strsplit(tif.info,',')
                            temp<-tif.info[[2]]
                            temp[1:3]<-0
                            include<-as.numeric(temp)
                       if(!is.na(Split<-match("split",tolower(unlist(hl))))) {hl[[1]][Split]<-"Unused"
                          include[Split]<-0
                          }
                           hl[[1]][Eval.split]<-"Split"
    
                           out <- read.ma(out,hl=hl,include=include)
    
                             out$dat$bname<-bname
                             out$dat$split.type="eval"
                # Making Predictions
                         pred.vals<-function(x,model,Model){
                        x$pred<-pred.fct(model,x$dat[,2:ncol(x$dat)],Model)
                        return(x)}
                           print("line 57 browser")
    
                       assign("out",out,envir=.GlobalEnv)
                        #getting the predictions for the test/train split
                        out$dat$ma<-(lapply(X=out$dat$ma,FUN=pred.vals,model=out$mods$final.mod,Model=Model))
                        #Just for the training set for Random Forest we have to take out of bag predictions rather than the regular predictions
                        if(Model=="rf") out$dat$ma$train$pred<-tweak.p(as.vector(predict(out$mods$final.mod,type="prob")[,2]))
    
                        #producing auc and residual plots model summary information and accross model evaluation metric
                    out$mods$auc.output<-make.auc.plot.jpg(out=out)
           }
      }
      if(!is.null(new.tifs)){
                 ma.name <- new.tifs
                      #get the paths off of the new mds file
                          tif.info<-readLines(ma.name,3)
                          tif.info<-strsplit(tif.info,',')
                          include<-(as.numeric(tif.info[[2]]))
                          paths<-as.character(tif.info[[3]])
    
                  paths<-paths[paths!=""]
                  browser()
    
                ma.cols <- match(names(out$dat$ma$train$dat)[-1],sub(".tif","",basename(paths)))
                paths<-paths[ma.cols]
                #checking that all tifs are present
                if(any(is.na(ma.cols))){
    
                                  stop("ERROR: the following geotiff(s) are missing in ",
                                        tif.dir,":  ",paste(ma.names[-r.col][is.na(ma.cols)],collapse=" ,"),sep="")
                                }
                #checking that we have read access to all tiffs
                 if(sum(file.access(paths),mode=0)!=0){
                                  stop("ERROR: the following geotiff(s) are missing : ",
                                        paths[(file.access(paths)!=0),][1],sep="")
    
                                }
                                out$dat$tif.ind<-paths
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
 			if(argSplit[[1]][1]=="ntfs") new.tiffs <- argSplit[[1]][2]   #mds file header
 			if(argSplit[[1]][1]=="pmt")  produce.metrics <- argSplit[[1]][2]
    }

EvaluateNewData(workspace=ws,out.dir=out.dir,b.tif=as.logical(b.tif),p.tif=as.logical(p.tif),mess=as.logical(mess),new.tifs=new.tiffs,produce.metrics=as.logical(produce.metrics))




