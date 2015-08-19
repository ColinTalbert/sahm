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

calcStat<-function(x,family,has.split){

#Written by Marian Talbert 2011
    auc.data<-data.frame(ID=1:nrow(x$dat),pres.abs=x$dat[,1],pred=x$pred)
    
    p.bar <- sum(auc.data$pres.abs * x$weight) / sum(x$weight)
       n.pres=sum(auc.data$pres.abs>=1)
        n.abs=nrow(auc.data)-n.pres
        if(has.split & !is.null(x$Split)){ #we're in the train split here for pseudoabs
              dev.vect<-vector()
              null.dev<-vector()
               for(i in 1:(length(unique(x$Split))-1)){
                 dev.vect[i]<-calc.deviance(c(rep(0,times=sum(x$Split==i)),x$resp[x$resp==1]),
                          c(x$pred[x$Split==i],x$pred[x$resp==1]))
                 null.dev[i]<-calc.deviance(c(rep(0,times=sum(x$Split==i)),x$resp[x$resp==1]),
                          rep(mean(c(rep(1,times=sum(x$resp==1)),rep(0,times=sum(x$Split==i)))),times=(sum(x$resp==1)+sum(x$Split==i))))
               }
               null.dev<-mean(null.dev)
               dev.fit<-mean(dev.vect)
        }
        else if(has.split & is.null(x$Split)){ #in the test split for pseudoabs deviance shouldn't be calculated because calibration is wrong
              null.dev=NA
              dev.fit=NA
        }
        else{     
        null.dev=calc.deviance(auc.data$pres.abs, rep(p.bar,times=length(auc.data$pres.abs)), x$weight, family=family) #*nrow(x$dat)
                                             if(is.nan(null.dev)) null.dev=NA
        dev.fit=calc.deviance(auc.data$pres.abs, x$pred, x$weight, family=family) #*nrow(x$dat) Elith does not include this it might cause a weighting issue when averaging I'm not sure if I should include it
                                            if(is.nan(dev.fit)) dev.fit=NA
        }                                    
        dev.exp=null.dev - dev.fit
        pct.dev.exp=dev.exp/null.dev*100
        correlation=cor(auc.data$pres.abs,x$pred)
        #have to use roc here because auc in the PresenceAbsence package incorretly assumes that the value must be greater than .5
        #this isn't necessarily true for an independent evaluation set
        auc.fit<-roc(auc.data$pres.abs,auc.data$pred)
        calibration.stats<-calibration(auc.data$pres.abs, x$pred, family =family)

        if(family%in%c("binomial","bernoulli")){
            cmx <- cmx(auc.data,threshold=x$thresh)
            PCC <- pcc(cmx,st.dev=F)*100
            SENS <- sensitivity(cmx,st.dev=F)
            SPEC <- specificity(cmx,st.dev=F)
            KAPPA <- Kappa(cmx,st.dev=F)
            TSS <- SENS+SPEC-1

            return(list(n.pres=n.pres,n.abs=n.abs,null.dev=null.dev,dev.fit=dev.fit,dev.exp=dev.exp,pct.dev.exp=pct.dev.exp,correlation=correlation,auc.data=auc.data,
            auc.fit=auc.fit,Cmx=cmx,Pcc=PCC,Sens=SENS,Specf=SPEC,Kappa=KAPPA,Tss=TSS,calibration.stats=calibration.stats,thresh=x$thresh))
          }

        if(family=="poisson"){
           prediction.error<-sum((auc.data$pres.abs-auc.data$pred)^2)
            return(list(n.pres=n.pres,n.abs=n.abs,null.dev=null.dev,dev.fit=dev.fit,dev.exp=dev.exp,pct.dev.exp=pct.dev.exp,correlation=correlation,auc.data=auc.data,
            auc.fit=auc.fit,prediction.error=prediction.error,calibration.stats=calibration.stats))
            }

    }