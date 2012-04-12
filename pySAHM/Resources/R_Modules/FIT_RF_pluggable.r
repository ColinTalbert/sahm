
 # Interpret command line argurments #
# Make Function Call #
#Set defaults for optional commands
make.p.tif=T
make.binary.tif=T
responseCurveForm="pdf"
n.trees=1000
mtry=NULL
samp.replace=FALSE
sampsize=NULL
nodesize=NULL
maxnodes=NULL
importance=FALSE
localImp=FALSE
nPerm=1
proximity=NULL
oob.prox=proximity
norm.votes=TRUE
do.trace=FALSE
keep.forest=NULL
keep.inbag=FALSE
make.r.curves=T
seed=NULL
opt.methods=2
save.model=TRUE
seed=NULL
MESS=FALSE
xtest=NULL
ytest=NULL


Args <- commandArgs(trailingOnly=FALSE)

    for (i in 1:length(Args)){
     if(Args[i]=="-f") ScriptPath<-Args[i+1]
     }

    for (arg in Args) {
    	argSplit <- strsplit(arg, "=")
    	argSplit[[1]][1]
    	argSplit[[1]][2]
    	if(argSplit[[1]][1]=="c") csv <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="o") output <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="rc") responseCol <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="mpt") make.p.tif <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="mbt")  make.binary.tif <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="ntree")  n.trees <- argSplit[[1]][2]
      if(argSplit[[1]][1]=="mtry")  mtry <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="sampR")  samp.replace <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="sampS")  sampsize <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="nodeS")  nodesize <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="maxN")  maxnodes <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="impt")  importance <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="locImp")  localImp <- argSplit[[1]][2]
 		  if(argSplit[[1]][1]=="nPerm")  nPerm <- argSplit[[1]][2]
 		  if(argSplit[[1]][1]=="prox")  proximity <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="oopp")  oop.prox <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="NVot")  norm.votes <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="Trce")  do.trace <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="kf")  keep.forest <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="Kbag")  keep.inbag <- argSplit[[1]][2]
 		  if(argSplit[[1]][1]=="curves")  make.r.curves <- argSplit[[1]][2]
 		  if(argSplit[[1]][1]=="om")  opt.methods <- argSplit[[1]][2]
      if(argSplit[[1]][1]=="savm")  save.model <- argSplit[[1]][2]
      if(argSplit[[1]][1]=="mes")  MESS <- argSplit[[1]][2]
      if(argSplit[[1]][1]=="seed")  seed <- argSplit[[1]][2]

 		  
    }
	print(csv)
	print(output)
	print(responseCol)
	
make.p.tif<-as.logical(make.p.tif)
make.binary.tif<-as.logical(make.binary.tif)
samp.replace<-as.logical(samp.replace)
importance<-as.logical(importance)
localImp<-as.logical(localImp)
norm.votes<-as.logical(norm.votes)
do.trace<-as.logical(do.trace)
keep.inbag<-as.logical(keep.inbag)
make.r.curves<-as.logical(make.r.curves)
save.model<-make.p.tif | make.binary.tif
n.trees<-as.numeric(n.trees)
opt.methods<-as.numeric(opt.methods)
ScriptPath<-dirname(ScriptPath)
MESS<-as.logical(MESS)

source(paste(ScriptPath,"RF.helper.fcts.r",sep="\\"))
source(paste(ScriptPath,"LoadRequiredCode.r",sep="\\"))

FitModels(ma.name=csv,tif.dir=NULL,output.dir=output,response.col=responseCol,make.p.tif=make.p.tif,make.binary.tif=make.binary.tif,
      debug.mode=F,responseCurveForm="pdf",xtest=xtest,ytest=ytest,n.trees=n.trees,mtry=mtry,samp.replace=samp.replace, sampsize=sampsize,
      nodesize=nodesize,maxnodes=maxnodes,importance=importance,
      localImp=localImp,nPerm=nPerm,proximity=proximity,oob.prox=oob.prox,norm.votes=norm.votes,do.trace=do.trace,keep.forest=keep.forest,
      keep.inbag=keep.inbag,make.r.curves=make.r.curves,
      seed=seed,script.name="rf",opt.methods=opt.methods,save.model=save.model,MESS=MESS)

 