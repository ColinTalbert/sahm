EvaluateNewData<-function(workspace=NULL,outDir=NULL,binary.tif=NULL,p.tif=NULL,mes=TRUE){

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
       rm(out)
       #change the new input arguements

       out1$input$make.binary.tif<-binary.tif
       out1$input$make.p.tif<-p.tif
       out1$input$MESS<-mes
       out1$input$output.dir<-outDir
       out1$input$final.model=TRUE
       out<-out1

    # generate a filename for output and reload the libraries #
                Model<-out$input$script.name
                bname<-paste(out$input$output.dir,paste("/",Model,sep=""),sep="")
               chk.libs(Model)
              out$dat$bname<-bname
    #Determine if we will be evaluating a holdout set if so rename it to the split portion so evaluation metrics will be caluclated and reported
    hl=readLines(out$input$ma.name,1)
     hl=strsplit(hl,',')
     if(!is.na(Eval.split<-match("evalsplit",tolower(unlist(hl))))) out$dat$split.type="eval"
     assign("out",out,envir=.GlobalEnv)
       out$dat$split.label=out$dat$split.type

     out<-place.save(out,Final.Model=out1$input$final.model)
     ##################################################################################
     ###################### Producing Evalutaiton Metrics for the hold out data
           if(out$dat$split.type=="eval"){
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
                              out$dat$split.type="test"
                              out$dat$split.label="eval"
                         out$dat$bname<-bname
                        }
                        
                           pred.vals<-function(x,model,Model){
              x$pred<-pred.fct(model,x$dat[,2:ncol(x$dat)],Model)
              return(x)}

              #getting the predictions for the test/train or cross validation splits into the object at the correct list location

              if(out$dat$split.type!="crossValidation") out$dat$ma<-(lapply(X=out$dat$ma,FUN=pred.vals,model=out$mods$final.mod,Model=Model))
                 else out$dat$ma$train$pred<-pred.vals(out$dat$ma$train,out$mods$final.mod,Model=Model)$pred  #produces the same thing as pred.mars(out$mods$final.mod,out$dat$ma$train$dat[2:ncol(out$dat$ma$train$dat)])

                   assign("out",out,envir=.GlobalEnv)
                    #getting the predictions for the test/train split
                    out$dat$ma<-(lapply(X=out$dat$ma,FUN=pred.vals,model=out$mods$final.mod,Model=Model))
                    #Just for the training set for Random Forest we have to take out of bag predictions rather than the regular predictions
                    if(Model=="rf") out$dat$ma$train$pred<-tweak.p(as.vector(predict(out$mods$final.mod,type="prob")[,2]))

             out$mods$auc.output<-make.auc.plot.jpg(out=out)

      assign("out",out,envir=.GlobalEnv)
      
           #producing auc and residual plots model summary information and across model evaluation metric

                
     ################################ Making the tiff
   if(out$input$make.p.tif==T | out$input$make.binary.tif==T){
        if((n.var <- out$mods$n.vars.final)<1){
            stop("Error producing geotiff output:  null model selected by stepwise procedure - pointless to make maps")
            } else {
            cat("\nproducing prediction maps...","\n","\n");flush.console()

               proc.tiff(model=out$mods$final.mod,vnames=names(out$dat$ma$train$dat)[-1],
                tif.dir=out$dat$tif.dir$dname,filenames=out$dat$tif.ind,pred.fct=pred.fct,factor.levels=out$dat$ma$factor.levels,make.binary.tif=out$input$make.binary.tif,
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

source(paste(ScriptPath,"LoadRequiredCode.r",sep="\\"))

EvaluateNewData(workspace=ws,outDir=out.dir,binary.tif=make.binary.tif,p.tif=make.p.tif,mes=MESS)



