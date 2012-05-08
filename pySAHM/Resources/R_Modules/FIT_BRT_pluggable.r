###############################################################################
##
## Copyright (C) 20010-2012, USGS Fort Collins Science Center. 
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

make.p.tif=T
make.binary.tif=T

tc=NULL
n.folds=3
alpha=1

learning.rate = NULL
bag.fraction = 0.5
prev.stratify = TRUE
max.trees = 10000
tolerance.method = "auto"
tolerance = 0.001
seed=NULL
opt.methods=2
save.model=TRUE
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
    	if(argSplit[[1]][1]=="c") csv <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="o") output <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="rc") responseCol <- argSplit[[1]][2]
   		if(argSplit[[1]][1]=="mpt") make.p.tif <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="mbt")  make.binary.tif <- argSplit[[1]][2]
      if(argSplit[[1]][1]=="tc")  tc <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="nf")  n.folds <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="alp")  alpha <- argSplit[[1]][2]
      if(argSplit[[1]][1]=="lr")  learning.rate <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="bf")  bag.fraction <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="ps")  prev.stratify <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="mt")  max.trees <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="om")  opt.methods <- argSplit[[1]][2]
 			if(argSplit[[1]][1]=="seed")  seed <- argSplit[[1]][2]
 		  if(argSplit[[1]][1]=="savm")  save.model <- argSplit[[1]][2]
 		  if(argSplit[[1]][1]=="tolm")  tolerance.method <- argSplit[[1]][2]
 		  if(argSplit[[1]][1]=="tol")  tolerance <- argSplit[[1]][2]
 		  if(argSplit[[1]][1]=="mes")  MESS <- argSplit[[1]][2]
 			
    }
	print(csv)
	print(output)
	print(responseCol)

ScriptPath<-dirname(ScriptPath)
source(paste(ScriptPath,"LoadRequiredCode.r",sep="\\"))
source(paste(ScriptPath,"BRT.helper.fcts.r",sep="\\"))

alpha<-as.numeric(alpha)
make.p.tif<-as.logical(make.p.tif)
make.binary.tif<-as.logical(make.binary.tif)
prev.stratify<-as.logical(prev.stratify)
save.model<-make.p.tif | make.binary.tif
opt.methods<-as.numeric(opt.methods)
MESS<-as.logical(MESS)
tolerance=as.numeric(tolerance)
bag.fraction<-as.numeric(bag.fraction)
max.trees<-as.numeric(max.trees)
n.folds<-as.numeric(n.folds)

    FitModels(ma.name=csv,
		tif.dir=NULL,
		output.dir=output,
		response.col=responseCol,
		make.p.tif=make.p.tif,make.binary.tif=make.binary.tif,
		simp.method="cross-validation",debug.mode=F,responseCurveForm="pdf",tc=tc,n.folds=n.folds,alpha=alpha,script.name="brt",
		learning.rate =learning.rate, bag.fraction = bag.fraction,prev.stratify = prev.stratify,max.trees = max.trees,seed=seed,
    save.model=save.model,opt.methods=opt.methods,MESS=MESS,tolerance.method = tolerance.method,tolerance=tolerance)



