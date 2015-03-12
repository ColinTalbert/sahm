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

pred.fct<-function(model,x,Model){
#Written by Marian Talbert to predict given a dataframe of input (with the expeption of maxlike which works on full rasters)
# model = the model fit object
# x     = a data frame without the response column obviously
# Model = one of mars, glm, rf, brt, maxlike at present 

  y <- rep(NA,nrow(x))

  if(Model%in%c("glm","mars")){
 
   if("list"%in%class(model)) y <- try(as.vector(predict(model[[1]],x,type="response")),silent=TRUE)
        else  y<- try(as.vector(predict(model,x,type="response")),silent=TRUE)
      
     }
 
 
  if(Model=="brt"){
         # retrieve key items from the global environment #
          # make predictions from complete data only #
          #y <- rep(NA,nrow(x))
          #y[complete.cases(x)] <- predict.gbm(model, x[complete.cases(x),],model$target.trees,type="response")
          if(class(model[[1]])=="gbm"){
                           prd<-function(model,x){
                              preds <- rep(NA,nrow(x))
                              preds[complete.cases(x)]<-predict.gbm(model,newdata=x[complete.cases(x),],n.trees=model$target.trees,type="response")
                              return(preds) 
                           }         
                  #getting the predictions from each split of the data then taking out one column and getting the average
                          lst.preds<-try(lapply(model,FUN=prd,x=x))
                         y<-try(apply(do.call("rbind",lst.preds),2,mean))
          }  else{
                  # make predictions from full data #
                  y[complete.cases(x)] <- try(predict.gbm(model,x[complete.cases(x),],model$target.trees,type="response"),silent=TRUE)        
          }
  }
  if(Model=="rf"){
       # retrieve key items from the global environment #
          # make predictions from complete data only #
           if(class(model[[1]])=="randomForest"){
                  #getting the predictions from each split of the data then taking out one column and getting the average
                          lst.preds<-try(lapply(lapply(model,FUN=predict,newdata=x[complete.cases(x),],type="vote"),"[",,2))
                         y[complete.cases(x)]<-try(apply(do.call("rbind",lst.preds),2,mean))
                         	y[y==1]<-max(y[y<1],na.rm=TRUE)
	                        y[y==0]<-min(y[y>0],na.rm=TRUE)
                         }  else{
                y[complete.cases(x)] <- try(as.vector(predict(model,newdata=x[complete.cases(x),],type="vote")[,2]),silent=TRUE)
              }  
   }
   if(Model=="maxent"){

   y[complete.cases(x)]<-try(maxent.predict(model,x[complete.cases(x),]),silent=TRUE)
   }
    if(class(y)=="try-error") stop("Predicting the response for the new values failed.  One probable cause is that you are trying to predict to factor levels that were not present during model fitting.")
return(y)
}