
#set defaults
make.p.tif=T
make.binary.tif=T
mars.degree=1
mars.penalty=2
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
MESS<-as.logical(MESS)

FitModels(ma.name=csv,
        tif.dir=NULL,output.dir=output,
        response.col=responseCol,make.p.tif=make.p.tif,make.binary.tif=make.binary.tif,
            mars.degree=mars.degree,mars.penalty=mars.penalty,debug.mode=F,responseCurveForm="pdf",
            script.name="mars",save.model=save.model,opt.methods=as.numeric(opt.methods),MESS=MESS)
