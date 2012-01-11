EvaluateNewData<-function(workspace=NULL,out.dir=NULL,make.binary.tif=NULL,make.p.tif=NULL,MESS=TRUE){

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
                out$input$MESS<-MESS
                Model<-out$input$script.name
              bname<-paste(out$input$output.dir,paste("/",Model,sep=""),sep="")


            # logname <- file(paste(bname,"_log.txt",sep=""), open="wt")
             #                sink(logname)
              #               sink(logname, type="message")
              out$dat$bname<-bname
    #Determine if we will be evaluating a holdout set if so rename it to the split portion so evaluation metrics will be caluclated and reported
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

     ################################ Making the tiff
   if(make.p.tif==T | make.binary.tif==T){
        if((n.var <- out$mods$n.vars.final)<1){
            stop("Error producing geotiff output:  null model selected by stepwise procedure - pointless to make maps")
            } else {
            cat("\nproducing prediction maps...","\n","\n");flush.console()

            proc.tiff(model=out$mods$final.mod,vnames=out$mods$vnames,
                tif.dir=out$dat$tif.dir$dname,filenames=out$dat$tif.ind,pred.fct=pred.fct,factor.levels=out$dat$ma$factor.levels,make.binary.tif=make.binary.tif,
                thresh=out$mods$auc.output$thresh,make.p.tif=make.p.tif,outfile.p=paste(out$dat$bname,"_prob_map.tif",sep=""),
                outfile.bin=paste(out$dat$bname,"_bin_map.tif",sep=""),tsize=50.0,NAval=-3000,
                fnames=out$dat$tif.names,out=out,Model=Model)
            }

     }

}




make.p.tif=T
make.binary.tif=T
MESS=FALSE

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
    	if(argSplit[[1]][1]=="mes")  MESS <- argSplit[[1]][2]
   		if(argSplit[[1]][1]=="mpt") make.p.tif <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="mbt")  make.binary.tif <- argSplit[[1]][2]
    }

make.p.tif=as.logical(make.p.tif)
make.binary.tif=as.logical(make.binary.tif)
MESS=as.logical(MESS)

EvaluateNewData(workspace=ws,out.dir=out.dir,make.binary.tif=make.binary.tif,make.p.tif=make.p.tif,MESS=MESS)



