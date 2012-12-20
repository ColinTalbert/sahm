maxent.predict<-function(model,dat){

if(names(model[[1]])[1]=="Raw.coef")
    {attach(model[[1]])
        on.exit(detach(model[[1]]))
    } else{attach(model)
       on.exit(detach(model))
}
Raw<- model.matrix(as.formula(paste("~",paste(Raw.coef[,1],collapse=" + "),sep="")),dat)
Quad<- model.matrix(as.formula(paste("~ I(",paste(Quad.coef[,1],collapse=") + I("),")",sep="")),dat)
Prod<- model.matrix(as.formula(paste("~ ",paste(Prod.coef[,1],collapse=" + "),sep="")),dat)
Forw<- model.matrix(as.formula(paste("~ ",paste("-1 + ",paste("I(",paste(Fwd.Hinge[,1],paste(Fwd.Hinge[,1],paste("(",Fwd.Hinge[,3],")",sep=""),sep=">"),sep="*("),collapse=")) + "),"))",sep=""),sep="")),dat)
Rev<- model.matrix(as.formula(paste("~ ",paste("-1 + ",paste("I(",paste(Rev.Hinge[,1],paste(Rev.Hinge[,1],paste("(",Rev.Hinge[,4],")",sep=""),sep="<"),sep="*("),collapse=")) + "),"))",sep=""),sep="")),dat)
#using Forw.Cst<- model.matrix(as.formula(paste("~ ",paste("-1 + ",paste("I(",paste(Fwd.Hinge[,1],paste("(",Fwd.Hinge[,3],")",sep=""),sep="<"),collapse=") + "),")",sep=""),sep="")),dat)
#doesn't work because it inclused both true and false for the first indicator I need to ask how to get rid of this but e-mail is down right now
#I'm keeping a place saver for this -1 with an empty paste
Forw.Cst<-Forw!=0
Rev.Cst<-Rev!=0
Thresh<-model.matrix(as.formula(paste("~ ",paste("I",Thresh.val[,1],collapse=" + ",sep=""),sep="")),dat)


S<-apply(t(Raw)*Raw.mult,2,sum)+
   apply(t(Quad)*Quad.mult,2,sum)+
   apply(t(Prod)*Prod.mult,2,sum)+
   apply(t(Forw)*FH.mult,2,sum)+
   apply(t(Forw.Cst)*FH.cnst,2,sum)+
   apply(t(Rev)*Rev.mult,2,sum)+
   apply(t(Rev.Cst)*Rev.cnst,2,sum)+
   apply(t(Thresh[,2:ncol(Thresh)])*Thresh.cnst,2,sum)-normalizers[1,2]

qx<-  exp(S)/normalizers[2,2]
prediction<-qx*exp(entropy)/(1+qx*exp(entropy))
 return(prediction)
}