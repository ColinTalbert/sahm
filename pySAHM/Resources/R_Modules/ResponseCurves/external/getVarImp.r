getVarImp<-function(Dir){
  File<-file.path(Dir,"ExpandedOutput","VariableImportance.csv")
  VarImp<-read.csv(File,row.names=1)
  if(ncol(VarImp)==1) return(as.vector(VarImp))
  VarImp<-apply(VarImp[,1:(ncol(VarImp)-1)],1,mean)
  return(VarImp)
}