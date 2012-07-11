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

tweak.p <- function(p){
	p[p==1]<-max(p[p<1])
	p[p==0]<-min(p[p>0])
	return(p)
	}
	
	RFResponseCurve<-function (all.splits, pred.data, x.var, which.class, w, plot = TRUE, add = FALSE,
    n.pt = min(length(unique(pred.data[, xname])), 51), rug = TRUE,
    xlab = deparse(substitute(x.var)), ylab = "", main = paste("Partial Dependence on",
        deparse(substitute(x.var))), ...)
{ 
# Edited by Marian Talbert to average predictions for all splits of used/available in order to produce
# Response curves following the approach of Barbet-Massin 2012
  
     x<-all.splits[[1]]
    classRF <- x$type != "regression"
    if (is.null(x$forest))
        stop("The randomForest object must contain the forest.\n")
    x.var <- substitute(x.var)
    xname <- if (is.character(x.var))
        x.var
    else {
        if (is.name(x.var))
            deparse(x.var)
        else {
            eval(x.var,envir=parent.frame())
        }
    }
    xv <- pred.data[, xname]
    n <- nrow(pred.data)
    if (missing(w))
        w <- rep(1, n)
    if (classRF) {
        if (missing(which.class)) {
            focus <- 1
        }
        else {
            focus <- charmatch(which.class, colnames(x$votes))
            if (is.na(focus))
                stop(which.class, "is not one of the class labels.")
        }
    }
    if (is.factor(xv) && !is.ordered(xv)) {
        x.pt <- levels(xv)
        y.pt <- numeric(length(x.pt))
        for (i in seq(along = x.pt)) {
            x.data <- pred.data
            x.data[, xname] <- factor(rep(x.pt[i], n), levels = x.pt)
            if (classRF) {
           
               for(j in 1:(length(all.splits)-1)){
                 if(j==1) pr = predict(all.splits[[j]], x.data, type = "prob")/(length(all.splits)-1)
                 else {
                    new.pr<-predict(all.splits[[i]], x.data, type = "prob")/(length(all.splits)-1)
                    pr=pr+new.pr/(length(all.splits)-1)
                 }
                }
                y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] >
                  0, pr[, focus], .Machine$double.eps)) - rowMeans(log(ifelse(pr >
                  0, pr, .Machine$double.eps))), w, na.rm = TRUE)
            }
            else y.pt[i] <- weighted.mean(predict(x, x.data),
                w, na.rm = TRUE)
        }
        if (add) {
            points(1:length(x.pt), y.pt, type = "h", lwd = 2,
                ...)
        }
        else {
            if (plot)
                barplot(y.pt, width = rep(1, length(y.pt)), col = "blue",
                  xlab = xlab, ylab = ylab, main = main, names.arg = x.pt,
                  ...)
        }
    }
    else {
        if (is.ordered(xv))
            xv <- as.numeric(xv)
        x.pt <- seq(min(xv), max(xv), length = n.pt)
        y.pt <- numeric(length(x.pt))
        for (i in seq(along = x.pt)) {
            x.data <- pred.data
            x.data[, xname] <- rep(x.pt[i], n)
            if (classRF) {
               for(j in 1:(length(all.splits)-1)){
                 if(j==1) pr = predict(all.splits[[j]], x.data, type = "prob")/(length(all.splits)-1)
                 else {
                    new.pr<-predict(all.splits[[j]], x.data, type = "prob")/(length(all.splits)-1)
                    pr=pr+new.pr
                 }
                }
                y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] ==
                  0, .Machine$double.eps, pr[, focus])) - rowMeans(log(ifelse(pr ==
                  0, .Machine$double.eps, pr))), w, na.rm = TRUE)
            }
            else {
                y.pt[i] <- weighted.mean(predict(x, x.data),
                  w, na.rm = TRUE)
            }
        }
        if (add) {
            lines(x.pt, y.pt, ...)
        }
        else {
            if (plot)
                plot(x.pt, y.pt, type = "l", xlab = xlab, ylab = ylab,
                  main = main, ...)
        }
        if (rug && plot) {
            if (n.pt > 10) {
                rug(quantile(xv, seq(0.1, 0.9, by = 0.1)), side = 1)
            }
            else {
                rug(unique(xv, side = 1))
            }
        }
    }
    invisible(list(x = x.pt, y = y.pt))
}
