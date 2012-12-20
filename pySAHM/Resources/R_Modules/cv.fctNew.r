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

"cv.fct" <-
function (fit.object, out, sp.no = 1, prev.stratify = F,Model)
{
#
# j. leathwick/j. elith - August 2006
#
# version 3.1 - developed in R 2.3.1 using mda 0.3-1
#
# function to perform k-fold cross validation
# with full model perturbation for each subset
#
# requires mda library from Cran
# requires functions mw and calibration
#
# takes a mars/glm object produced by mars.glm
# and first assesses the full model, and then
# randomly subsets the dataset into nk folds and drops
# each subset in turn, fitting on remaining data
# and predicting for withheld data
#
# caters for both single species and community models via the argument sp.no
# for the first, sp.no can be left on its default of 1
# for community models, sp.no can be varied from 1 to n.spp
#
# modified 29/9/04 to
#   1. return mars analysis details for audit trail
#   2. calculate roc and calibration on subsets as well as full data
#      returning the mean and se of the ROC scores
#      and the mean calibration statistics
#
# modified 8/10/04 to add prevalence stratification
# modified 7th January to test for binomial family and return if not
#
# updated 15th March to cater for both binomial and poisson families
#
# updated 16th June 2005 to calculate residual deviance
#
attach(out$input)
on.exit(detach(out$input))


data<-out$dat$ma$train
n.cases<-nrow(out$dat$ma$train$dat)
xdat<-out$dat$ma$train$dat[,2:ncol(out$dat$ma$train$dat)]
ydat<-out$dat$ma$train$dat[,1]
u_i<-out$dat$ma$train$pred
family<-out$input$model.family
site.weights<-out$dat$ma$train$weight

  ############################################################
 
 y_i <- ydat

  if (family == "binomial" | family=="bernoulli") {
    full.resid.deviance <- calc.deviance(y_i,u_i, weights = site.weights, family="binomial")
    full.test <- roc(y_i, u_i)
    full.calib <- calibration(y_i, u_i)
  }

  if (family=="poisson") {
    full.resid.deviance <- calc.deviance(y_i,u_i, weights = site.weights, family="poisson")                      
    full.test <- cor(y_i, u_i)
    full.calib <- calibration(y_i, u_i, family = "poisson")
  }

# set up for results storage
   nk<-length(out$dat$ma)-1
  subset.test <- rep(0,nk)
  subset.calib <- as.data.frame(matrix(0,ncol=5,nrow=nk))
  names(subset.calib) <- c("intercept","slope","test1","test2","test3")
  subset.resid.deviance <- rep(0,nk)

# now setup for withholding random subsets

  pred.values <- rep(0, n.cases)
  fitted.values <- rep(0, n.cases)

  print("", quote = FALSE)
  print("Creating predictions for subsets...", quote = F)

 selector<-out$dat$selector
 resp.curves<-vector("list",nk)
   out.cv<-list()
   names(resp.curves)<-seq(1:nk)
 #############################################
 #Start cross validation Fold loop
 
for (i in 1:nk) {
              cat(i," ")
              model.mask <- selector != i  #used to fit model on majority of data
              pred.mask <- selector == i   #used to identify the with-held subset
              assign("species.subset", ydat[model.mask], pos = 1)
              assign("predictor.subset", xdat[model.mask, ], pos = 1)
              
              dat<-cbind(predictor.subset,species.subset)
              names(dat)<-names(data)
              
              # fit new model
              cv.final.mod<-model.fit(dat=out$dat$ma$train$dat[model.mask,],out=out,Model=Model,weight=out$dat$ma$train$weight[model.mask],Fold=i)                        
              fitted.values[pred.mask]<-out$dat$ma[[i]]$pred<-pred.fct(model=cv.final.mod,x=xdat[pred.mask,],Model)
                    
                     out$dat$ma[[i]]$thresh <- as.numeric(optimal.thresholds(data.frame(ID=1:nrow(out$dat$ma$train$dat[model.mask,]),pres.abs=out$dat$ma$train$dat[model.mask,]$response,
                          pred=pred.fct(model=cv.final.mod,x=xdat[model.mask,],Model)),opt.methods=out$input$opt.methods))[2]
              y_i <- ydat[pred.mask]
              u_i <- fitted.values[pred.mask]
              weights.subset <- site.weights[pred.mask]

              if (family == "binomial" | family=="bernoulli") {
                subset.resid.deviance[i] <- calc.deviance(y_i,u_i,weights = weights.subset, family="binomial")
                subset.test[i] <- roc(y_i,u_i)
                subset.calib[i,] <- calibration(y_i, u_i)
              }

              if (family=="poisson"){
                subset.resid.deviance[i] <- calc.deviance(y_i,u_i,weights = weights.subset, family="poisson")
                subset.test[i] <- cor(y_i, u_i)
                subset.calib[i,] <- calibration(y_i, u_i, family = family)
    }

  } #end of Cross Validation Fold Loop
  #################################################
 
  cat("","\n")

  y_i <- ydat
  u_i <- fitted.values

  if (family=="binomial" | family=="bernoulli") {
    cv.resid.deviance <- calc.deviance(y_i,u_i,weights = site.weights, family="binomial")
    cv.test <- roc(y_i, u_i)
    cv.calib <- calibration(y_i, u_i)
  }

  if (family=="poisson"){
    cv.resid.deviance <- calc.deviance(y_i,u_i,weights = site.weights, family="poisson")
    cv.test <- cor(y_i, u_i)
    cv.calib <- calibration(y_i, u_i, family = "poisson")
  }

  subset.test.mean <- mean(subset.test)
  subset.test.se <- sqrt(var(subset.test))/sqrt(nk)

  subset.test <- list(test.scores = subset.test, subset.test.mean = subset.test.mean,
    subset.test.se = subset.test.se)

  subset.calib.mean <- apply(subset.calib[,c(1:2)],2,mean)
  names(subset.calib.mean) <- names(subset.calib)[c(1:2)] #mean only of parameters

  subset.calib <- list(subset.calib = subset.calib,
    subset.calib.mean = subset.calib.mean)

  subset.deviance.mean <- mean(subset.resid.deviance)
  subset.deviance.se <- sqrt(var(subset.resid.deviance))/sqrt(nk)

   subset.deviance <- list(subset.deviances = subset.resid.deviance, subset.deviance.mean = subset.deviance.mean,
    subset.deviance.se = subset.deviance.se)
  
  cv.list<-list(full.resid.deviance = full.resid.deviance,
    full.test = full.test, full.calib = full.calib, pooled.deviance = cv.resid.deviance, pooled.test = cv.test,
    pooled.calib = cv.calib,subset.deviance = subset.deviance, subset.test = subset.test, subset.calib = subset.calib,resp.curves=resp.curves)
   out$cv<-cv.list
  return(out)
}
