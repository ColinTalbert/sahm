PredictModel<-function(workspace=NULL,new.tifs=NULL,thresh=NULL,make.btif=NULL,make.ptif=NULL){

#This functions separates the step of model fit from producing probability or binary surfaces
#the default is to read in a workspace and make predictions using the original tiffs supplied
#but if an mds with new tiff directories are supplied, the function will instead use these
#options are available for setting the threshold
#and true and false for whether to make binary or prob.tif

#my stuff to delete
#workspace=paste("C:\\VisTrails\\mtalbert_20110504T132851","modelWorkspace",sep="\\")
#new.tifs<-out$input$ma.name
#end delete

load(workspace)

if(!is.null(new.tifs)){

  ma.name <- new.tifs
      #get the paths off of the new mds file
          tif.info<-readLines(ma.name,3)
          tif.info<-strsplit(tif.info,',')
          include<-(as.numeric(tif.info[[2]]))
          paths<-as.character(tif.info[[3]])

  paths<-paths[paths!=""]
#this line needs to be changed eventually to deal with locations of model fit covaries from different models
if(out$input$model.source.file=="brt.r") {model.covs<-levels(summary(out$mods$final.mod,plotit=FALSE)[,1])
pred.fct=brt.predict
}
ma.cols <- match(model.covs,sub(".tif","",basename(paths)))
paths<-paths[ma.cols]
#checking that all tifs are present
if(any(is.na(ma.cols))){
                  out$ec <- out$ec+1
                  out$error.mssg[[out$ec]] <- paste("ERROR: the following geotiff(s) are missing in ",
                        tif.dir,":  ",paste(ma.names[-r.col][is.na(ma.cols)],collapse=" ,"),sep="")
                  return(out)
                }
#checking that we have read access to all tiffs
 if(sum(file.access(paths),mode=0)!=0){
                  out$ec <- out$ec+1
                  out$error.mssg[[out$ec]] <- paste("ERROR: the following geotiff(s) are missing : ",
                        paths[(file.access(paths)!=0),][1],sep="")
                return(out)
                }
                out$dat$tif.ind<-paths
                }
                


if(!is.null(thresh))out$mods$auc.output$thresh<-thresh
if(!is.null(make.btif)) make.binary.tif<-make.btif
if(!is.null(make.ptif)) make.p.tif<-make.ptif

mssg <- proc.tiff(model=out$mods$final.mod,vnames=as.character(out$mods$final.mod$contributions$var),
            tif.dir=out$dat$tif.dir$dname,filenames=out$dat$tif.ind,pred.fct=pred.fct,factor.levels=out$dat$ma$factor.levels,make.binary.tif=make.binary.tif,
            thresh=out$mods$auc.output$thresh,make.p.tif=make.p.tif,outfile.p=paste(out$dat$bname,"_prob_map.tif",sep=""),
            outfile.bin=paste(out$dat$bname,"_bin_map.tif",sep=""),tsize=50.0,NAval=-3000)


}


new.tifs=NULL
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
    	if(argSplit[[1]][1]=="new.tifs") new.tifs <- argSplit[[1]][2]
    }
    
PredictModel(workspace=ws,new.tifs=new.tifs)


