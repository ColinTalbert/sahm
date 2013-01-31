CalcMESS<-function(rast,train.dat){
              #if anything is out of range, return it before calculating sums                       
              min.train<-train.dat[1,] #because we sorted
              max.train<-train.dat[nrow(train.dat),]
              output<-data.frame(matrix(data=NA,nrow=nrow(rast),ncol=ncol(rast)))
              for(k in 1:length(min.train)) output[,k]<-my.min(rast.val=rast[,k],min.train[,k],max.train[,k])
              Indx<-apply(output,1,which.min)
              output<-apply(output,1,min)
              if(sum(output>0)==0) return(data.frame(Indx=Indx,output=output))
              
              f<-data.frame(matrix(dat=NA,nrow=sum(output>0),ncol=ncol(rast)))
              
              for(k in 1:length(min.train))  {
                    f[,k]<-100*mapply(vecSum,rast[output>0,k],MoreArgs=list(vect=train.dat[,k]))/nrow(train.dat)
                    f[,k]<-I(f[,k]<=50)*2*f[,k]+I(f[,k]>50)*2*(100-f[,k])
              }
              Indx[output>0]<-apply(f,1,which.min)
              f<-apply(f,1,min)
              output[output>0]<-f
              return(data.frame(Indx=Indx,output=output))
}
my.min<-function(rast.val,min.train,max.train){
pmin((rast.val-min.train),(max.train-rast.val))/(max.train-min.train)*100
}

vecSum<-function(v,vect)sum(v>vect)