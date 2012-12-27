read.maxent<-function(lambdas){

  lambdas <- read.csv(lambdas,header=FALSE)
  normalizers<-lambdas[(nrow(lambdas)-3):nrow(lambdas),]
    entropy<-normalizers[4,2]
    lambdas<-lambdas[1:(nrow(lambdas)-4),]
    variableNames <-names(x)
    fctType <- rep("raw",times=nrow(lambdas)-4)
    fctType[grep("`",as.character(lambdas[,1]))] <- "reverse.hinge"
    fctType[grep("'",as.character(lambdas[,1]))] <- "forward.hinge"
    fctType[grep("\\^",as.character(lambdas[,1]))]<-"quadratic"
    fctType[grep("[*]",as.character(lambdas[,1]))]<-"product"
    fctType[grep("[(]",as.character(lambdas[,1]))]<-"threshold"
  
  #make these all default to NULL in case the feature type was turned off
  Raw.coef <- Quad.coef <- Prod.coef <- Fwd.Hinge <- Rev.Hinge <- Thresh.val <-NULL 
  if(any(fctType=="raw")) "Raw.coef"<-lambdas[fctType=="raw",]
  if(any(fctType=="quadratic")) "Quad.coef"<-lambdas[fctType=="quadratic",]
  if(any(fctType=="product")) "Prod.coef"<-lambdas[fctType=="product",]
  if(any(fctType=="forward.hinge")) "Fwd.Hinge"<-lambdas[fctType=="forward.hinge",]
  if(any(fctType=="reverse.hinge")) "Rev.Hinge"<-lambdas[fctType=="reverse.hinge",]
  if(any(fctType=="threshold")) "Thresh.val"<-lambdas[fctType=="threshold",]
   Prod.coef[,1]<-gsub("[*]",":",Prod.coef[,1])
   Fwd.Hinge[,1]<-gsub("'","",Fwd.Hinge[,1])
   Rev.Hinge[,1]<-gsub("`","",Rev.Hinge[,1])

  Raw.mult<-c(-sum(Raw.coef[,2]*Raw.coef[,3]/(Raw.coef[,4]-Raw.coef[,3])), Raw.coef[,2]/(Raw.coef[,4]-Raw.coef[,3]))
  Quad.mult<-c(-sum(Quad.coef[,2]*Quad.coef[,3]/(Quad.coef[,4]-Quad.coef[,3])), Quad.coef[,2]/(Quad.coef[,4]-Quad.coef[,3]))
  Prod.mult<-c(-sum(Prod.coef[,2]*Prod.coef[,3]/(Prod.coef[,4]-Prod.coef[,3])), Prod.coef[,2]/(Prod.coef[,4]-Prod.coef[,3]))
  FH.mult<-Fwd.Hinge[,2]/(Fwd.Hinge[,4]-Fwd.Hinge[,3])
  FH.cnst<- -Fwd.Hinge[,2]*Fwd.Hinge[,3]/(Fwd.Hinge[,4]-Fwd.Hinge[,3])
  Rev.mult<- -Rev.Hinge[,2]/(Rev.Hinge[,4]-Rev.Hinge[,3])
  Rev.cnst<-Rev.Hinge[,2]*Rev.Hinge[,4]/(Rev.Hinge[,4]-Rev.Hinge[,3])
  Thresh.cnst<-Thresh.val[,2]
  retn.lst<-list(Raw.coef=Raw.coef,Quad.coef=Quad.coef,Prod.coef=Prod.coef,Fwd.Hinge=Fwd.Hinge,Rev.Hinge=Rev.Hinge,Thresh.val=Thresh.val,Raw.mult=Raw.mult,Quad.mult=Quad.mult,
      Prod.mult=Prod.mult,FH.mult=FH.mult,FH.cnst=FH.cnst,Rev.mult=Rev.mult,Rev.cnst=Rev.cnst,Thresh.cnst=Thresh.cnst,normalizers=normalizers,entropy=entropy)
  return(retn.lst)
}