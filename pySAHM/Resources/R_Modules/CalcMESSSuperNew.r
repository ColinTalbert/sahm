CalcMESS<-function(rast,train.dat){
              #if anything is out of range, return it before calculating sums                       
              min.train<-train.dat[1,] #because we sorted
              max.train<-train.dat[nrow(train.dat),]
              browser()
              output<-matrix(data=NA,nrow=nrow(rast),ncol=ncol(rast))
              for(k in 1:length(min.train)) output[,k]<-my.min(rast.val=rast[,k],min.train[,k],max.train[,k])
              output<-apply(output,1,min)
              if(sum(output>0)==0) return(output)
              f<-matrix(dat=NA,nrow=sum(output>0),ncol=ncol(rast))
              #start.time<-Sys.time()
              #for(k in 1:length(min.train)) f[,k]<-100*apply(outer(train.dat[,k],rast[output>0,k],"<"),2,sum)/nrow(train.dat)
              #end.time<-Sys.time()-start.time
              #start.time<-Sys.time()
              for(k in 1:length(min.train))  {
                    f[,k]<-100*mapply(vecSum,rast[output>0,k],MoreArgs=list(vect=train.dat[,k]))/nrow(train.dat)
                    f[,k]<-I(f[,k]<=50)*2*f[,k]+I(f[,k]>50)*2*(100-f[,k])
              }
              #end.time<-Sys.time()-start.time
              f<-apply(f,1,min)
               
              output[output>0]<-f
              return(output)
}
my.min<-function(rast.val,min.train,max.train){
pmin((rast.val-min.train),(max.train-rast.val))/(max.train-min.train)*100
}

