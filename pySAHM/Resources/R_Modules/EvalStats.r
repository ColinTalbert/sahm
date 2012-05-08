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

EvaluationStats<-function(out,thresh,train,train.pred,opt.methods=opt.methods){
    response<-out$dat$ma$ma.test[,1]

     if(out$input$model.source.file=="rf.r") {pred<-tweak.p(as.vector(predict(out$mods$final.mod,newdata=out$dat$ma$ma.test[,-1],type="prob")[,2]))
      modelname="Random Forest"
     }

    if(out$input$model.source.file=="mars.r") {pred<-mars.predict(out$mods$final.mod,out$dat$ma$ma.test)$prediction[,1]
         modelname="MARS"
    }

    if(out$input$model.source.file=="glm.r")  {pred=glm.predict(out$mods$final.mod,out$dat$ma$ma.test)
        modelname="GLM"
    }

    if(out$input$model.source.file=="brt.r") {pred=predict.gbm(out$mods$final.mod,out$dat$ma$ma.test,out$mods$final.mod$target.trees,type="response")
       modelname="BRT"
      }
      
    auc.output<-make.auc.plot.jpg(out$dat$ma$ma.test,pred=pred,plotname=paste(out$dat$bname,"_auc_plot.jpg",sep=""),
          modelname=modelname,test.split=TRUE,thresh=thresh,train=train,train.pred,opt.methods=opt.methods,weight=out$dat$ma$test.weights,out=out)

                out$mods$auc.output<-auc.output

}
