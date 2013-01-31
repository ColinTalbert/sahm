maxent.predict<-function(model,dat){

if(names(model[[1]])[1]=="Raw.coef")
    {attach(model[[1]])
        on.exit(detach(model[[1]]))
    } else{attach(model)
       on.exit(detach(model))
}

#These all default to zero in case the feature was excluded
Raw <- Quad <- Prod <- Forw <- Rev <- Thresh <- 0

if(!is.null(Raw.coef)){
  Raw<- model.matrix(as.formula(paste("~",paste(Raw.coef[,1],collapse=" + "),sep="")),dat)
  Raw<-apply(t(Raw)*Raw.mult,2,sum)
  }
if(!is.null(Quad.coef)){  
  Quad<- model.matrix(as.formula(paste("~ I(",paste(Quad.coef[,1],collapse=") + I("),")",sep="")),dat)
  Quad<-apply(t(Quad)*Quad.mult,2,sum)
  }
if(!is.null(Prod.coef)){  
  Prod<- model.matrix(as.formula(paste("~ ",paste(Prod.coef[,1],collapse=" + "),sep="")),dat)
  Prod<-apply(t(Prod)*Prod.mult,2,sum)
  }
if(!is.null(Fwd.Hinge)){
  Forw<- model.matrix(as.formula(paste("~ ",paste("-1 + ",paste("I(",paste(Fwd.Hinge[,1],paste(Fwd.Hinge[,1],
      paste("(",Fwd.Hinge[,3],")",sep=""),sep=">"),sep="*("),collapse=")) + "),"))",sep=""),sep="")),dat)
  Forw.Cst<-Forw!=0
  Forw<-apply(t(Forw)*FH.mult,2,sum)+apply(t(Forw.Cst)*FH.cnst,2,sum)
  }
if(!is.null(Rev.Hinge)){
  Rev<- model.matrix(as.formula(paste("~ ",paste("-1 + ",paste("I(",paste(Rev.Hinge[,1],paste(Rev.Hinge[,1],
       paste("(",Rev.Hinge[,4],")",sep=""),sep="<"),sep="*("),collapse=")) + "),"))",sep=""),sep="")),dat)
  Rev.Cst<-Rev!=0
  Rev <- apply(t(Rev)*Rev.mult,2,sum) + apply(t(Rev.Cst)*Rev.cnst,2,sum)
  }
if(!is.null(Thresh.val)){
  Thresh.val[,1]<-gsub("=","==",Thresh.val[,1])
  Thresh<-model.matrix(as.formula(paste("~ ",paste("I",Thresh.val[,1],collapse=" + ",sep=""),sep="")),dat)
  Thresh<-apply(t(Thresh[,2:ncol(Thresh)])*Thresh.cnst,2,sum)
  }

S<-Raw + Quad + Prod + Forw + Rev + Thresh - normalizers[1,2]

qx<-  exp(S)/normalizers[2,2]
prediction<-qx*exp(entropy)/(1+qx*exp(entropy))
 return(prediction)
}