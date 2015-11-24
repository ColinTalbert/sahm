read.maxent<-function(lambdas){
  lambdas <- read.csv(lambdas,header=FALSE)
  normalizers<-lambdas[(nrow(lambdas)-3):nrow(lambdas),]
    entropy<-normalizers[4,2]
    lambdas<-lambdas[1:(nrow(lambdas)-4),]
    fctType <- rep("raw",times=nrow(lambdas))
    fctType[grep("`",as.character(lambdas[,1]))] <- "reverse.hinge"
    fctType[grep("'",as.character(lambdas[,1]))] <- "forward.hinge"
    fctType[grep("\\^",as.character(lambdas[,1]))]<-"quadratic"
    fctType[grep("[*]",as.character(lambdas[,1]))]<-"product"
    fctType[grep("[(]",as.character(lambdas[,1]))]<-"threshold"
 
  #make these all default to NULL in case the feature type was turned off
  Raw.coef<-Quad.coef<-Prod.coef<-Fwd.Hinge<-Rev.Hinge<-Thresh.val<-Raw.mult<-Quad.mult<-
      Prod.mult<-FH.mult<-FH.cnst<-Rev.mult<-Rev.cnst<-Thresh.cnst<-NULL
      
  if(any(fctType=="raw")){ 
       "Raw.coef"<-lambdas[fctType=="raw",]
       Raw.mult<-c(-sum(Raw.coef[,2]*Raw.coef[,3]/(Raw.coef[,4]-Raw.coef[,3])), Raw.coef[,2]/(Raw.coef[,4]-Raw.coef[,3]))
       Raw.mult[is.nan(Raw.mult)]<-0
  }
  if(any(fctType=="quadratic")){
        "Quad.coef"<-lambdas[fctType=="quadratic",]
         Quad.mult<-c(-sum(Quad.coef[,2]*Quad.coef[,3]/(Quad.coef[,4]-Quad.coef[,3])), Quad.coef[,2]/(Quad.coef[,4]-Quad.coef[,3]))
         }
  if(any(fctType=="product")){ 
       "Prod.coef"<-lambdas[fctType=="product",]
        Prod.coef[,1]<-gsub("[*]",":",Prod.coef[,1])
        Prod.mult<-c(-sum(Prod.coef[,2]*Prod.coef[,3]/(Prod.coef[,4]-Prod.coef[,3])), Prod.coef[,2]/(Prod.coef[,4]-Prod.coef[,3]))
  }
  if(any(fctType=="forward.hinge")){ 
       "Fwd.Hinge"<-lambdas[fctType=="forward.hinge",]
       Fwd.Hinge[,1]<-gsub("'","",Fwd.Hinge[,1])
       FH.mult<-Fwd.Hinge[,2]/(Fwd.Hinge[,4]-Fwd.Hinge[,3])
       FH.cnst<- -Fwd.Hinge[,2]*Fwd.Hinge[,3]/(Fwd.Hinge[,4]-Fwd.Hinge[,3])
  }
  if(any(fctType=="reverse.hinge")){ 
    "Rev.Hinge"<-lambdas[fctType=="reverse.hinge",]
     Rev.Hinge[,1]<-gsub("`","",Rev.Hinge[,1])
     Rev.mult<- -Rev.Hinge[,2]/(Rev.Hinge[,4]-Rev.Hinge[,3])
     Rev.cnst<-Rev.Hinge[,2]*Rev.Hinge[,4]/(Rev.Hinge[,4]-Rev.Hinge[,3])
  }
  if(any(fctType=="threshold")){ 
      "Thresh.val"<-lambdas[fctType=="threshold",]
       Thresh.cnst<-Thresh.val[,2]
  }
  
 
  retn.lst<-list(Raw.coef=Raw.coef,Quad.coef=Quad.coef,Prod.coef=Prod.coef,Fwd.Hinge=Fwd.Hinge,Rev.Hinge=Rev.Hinge,Thresh.val=Thresh.val,Raw.mult=Raw.mult,Quad.mult=Quad.mult,
      Prod.mult=Prod.mult,FH.mult=FH.mult,FH.cnst=FH.cnst,Rev.mult=Rev.mult,Rev.cnst=Rev.cnst,Thresh.cnst=Thresh.cnst,normalizers=normalizers,entropy=entropy)
  return(retn.lst)
}

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