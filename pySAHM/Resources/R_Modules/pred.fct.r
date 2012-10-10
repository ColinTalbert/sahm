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

  if(Model=="glm"){

          y <- try(as.vector(predict(model,x,type="response")),silent=TRUE)
          if(class(y)=="try-error") stop("Predicting the response for the new values failed.  One probable cause is that you are trying to predict to factor levels that were not present during model fitting.")
          # encode missing values as -1.
          y[is.na(y)]<- NaN
     }

  if(Model=="mars"){
        # retrieve key items from the global environment #
        # make predictionss.
        y <- rep(NA,nrow(x))
        y[complete.cases(x)] <- try(as.vector(mars.predict(model,x[complete.cases(x),])$prediction[,1]),silent=true)

      #which(is.na(y)
        # encode missing values as -1.
        y[is.na(y)]<- NaN
  }
  if(Model=="brt"){
         # retrieve key items from the global environment #
          # make predictions from complete data only #
          #y <- rep(NA,nrow(x))
          #y[complete.cases(x)] <- predict.gbm(model, x[complete.cases(x),],model$target.trees,type="response")

          # make predictions from full data #
          y <- try(predict.gbm(model,x,model$target.trees,type="response"),silent=TRUE)
          # encode missing values as -1.
           a<-complete.cases(x)
          y[!(a)]<- NaN
      }

  if(Model=="rf"){
       # retrieve key items from the global environment #
          # make predictions from complete data only #
          y <- rep(NA,nrow(x))
          y[complete.cases(x)] <- try(as.vector(predict(model,newdata=x[complete.cases(x),],type="prob")[,2]),silent=TRUE)

          # make predictions from full data #

          # encode missing values as -1.
          y[is.na(y)]<- NaN
   }
    if(class(y)=="try-error") stop("Predicting the response for the new values failed.  One probable cause is that you are trying to predict to factor levels that were not present during model fitting.")
return(y)
}