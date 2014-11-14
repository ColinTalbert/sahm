read.hsc<-function(hsc){
      JSON<-readLines(hsc)
      mechModel<-fromJSON(JSON)
      mechModel
}

hsc.predict<-function(model,dat){
      
      if(is.null(names(model))) model<-model[[1]]
    
      for(i in 1:length(model)){
             len<-length(model[[i]])
             Form<-(paste("function(x){",paste(model[[i]][[1]][2],"*","I(","x"),
                          "<=",model[[i]][[1]][1],")","+",
                           ifelse(len>1,expandMiddle(model[[i]],"x"),""),
                           paste(model[[i]][[len]][2],"*","I(","x"),">",model[[i]][[len]][1],")}")
                  )
        Fct<-eval(parse(text=Form))
        if(i==1) Pred<-Fct(dat[,which(names(dat)==names(model)[i],arr.ind=TRUE)])
         else Pred<-cbind(Pred,Fct(dat[,which(names(dat)==names(model)[i],arr.ind=TRUE)]))     
      }
   
    #for now take the product across all maybe latter something else...
    Pred<-apply(Pred,1,prod)  
    return(Pred)
}
     
expandMiddle<-function(lst,lstName){
   for(j in 2:length(lst)){
       m<-(lst[[j]][2]-lst[[j-1]][2])/(lst[[j]][1]-lst[[j-1]][1])
       b<-lst[[j-1]][2]
       InRange<-paste("I(",lstName,">",lst[[j-1]][1],"&",lstName,"<=",lst[[j]][1],")")
       if(j==2) P <- paste(InRange,"*(",m,"*(",lstName,"-",lst[[j-1]][1],")+",b,")")
       else P <- paste(P,paste(InRange,"*(",m,"*(",lstName,"-",lst[[j-1]][1],")+",b,")"),sep="+")
  }
  P<-paste(P,"+")
  return(P)
}
 
